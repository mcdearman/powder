//! Memory: reference counts, and a collector for what counts cannot free.
//!
//! Every value on the heap has a count of the references to it and is
//! freed when the count reaches nothing, which is when the last reference
//! is let go of. That is prompt, and costs nothing when nothing is let go
//! of, but values that refer to each other in a circle keep each other's
//! counts up after nothing else refers to them. Those are found by trial
//! deletion (Bacon and Rajan, "Concurrent Cycle Collection in Reference
//! Counted Systems", 2001, the synchronous collector):
//!
//! - A value whose count goes down but not to nothing may be the last way
//!   into a circle, so it is noted as a possible root.
//! - To collect, the counts of everything reachable from the roots are
//!   taken down by the references among them. What is left with a count
//!   is referred to from outside, and it and all it reaches are put back
//!   as they were. What is left with none is garbage, and is freed.
//!
//! Collecting looks only at what the roots reach, not at the whole heap.

use std::cell::{Cell, RefCell};
use std::mem::ManuallyDrop;
use std::ops::Deref;
use std::ptr::NonNull;

#[derive(Clone, Copy, PartialEq)]
enum Color {
    /// In use, or free of suspicion.
    Black,
    /// Being looked at: its count has had the references from others
    /// being looked at taken off.
    Gray,
    /// Garbage, as far as is known.
    White,
    /// A possible root of a circle.
    Purple,
}

struct Header {
    count: Cell<usize>,
    color: Cell<Color>,
    /// Whether it is among the possible roots.
    buffered: Cell<bool>,
    /// Whether what it held has been let go of, and only its memory waits
    /// to be freed, which it does while it is among the roots.
    dead: Cell<bool>,
    how: &'static How,
}

/// What is done with each value another refers to.
type Visit<'a> = &'a mut dyn FnMut(NonNull<Header>);

/// What can be done with a value without knowing its type.
struct How {
    trace: unsafe fn(NonNull<Header>, Visit),
    drop: unsafe fn(NonNull<Header>),
    free: unsafe fn(NonNull<Header>),
}

#[repr(C)]
struct Boxed<T> {
    header: Header,
    value: ManuallyDrop<T>,
}

/// A reference from one value on the heap to another.
pub struct Edge(NonNull<Header>);

/// What is kept on the heap says what it refers to.
pub trait Trace: 'static {
    fn trace(&self, visit: &mut dyn FnMut(Edge));
}

/// How much has been made and freed, and how.
#[derive(Clone, Copy, Default)]
pub struct Stats {
    pub made: u64,
    pub freed_by_count: u64,
    pub freed_in_circles: u64,
    pub collections: u64,
}

thread_local! {
    static ROOTS: RefCell<Vec<NonNull<Header>>> = const { RefCell::new(Vec::new()) };
    /// While garbage is freed, letting go of a reference changes no count:
    /// the counts were settled by the collection.
    static SWEEPING: Cell<bool> = const { Cell::new(false) };
    /// Whether a value is being let go of, and the values let go of
    /// meanwhile, which wait their turn.
    static RELEASING: Cell<bool> = const { Cell::new(false) };
    static WAITING: RefCell<Vec<NonNull<Header>>> = const { RefCell::new(Vec::new()) };
    static STATS: Cell<Stats> = const { Cell::new(Stats { made: 0, freed_by_count: 0, freed_in_circles: 0, collections: 0 }) };
}

fn note(change: impl FnOnce(&mut Stats)) {
    STATS.with(|stats| {
        let mut now = stats.get();
        change(&mut now);
        stats.set(now);
    });
}

pub fn stats() -> Stats {
    STATS.with(Cell::get)
}

/// How many possible roots are waiting to be looked at.
pub fn pending() -> usize {
    ROOTS.with(|roots| roots.borrow().len())
}

/// A counted reference to a value on the heap.
pub struct Gc<T: Trace> {
    at: NonNull<Boxed<T>>,
}

impl<T: Trace> Gc<T> {
    const HOW: &'static How = &How { trace: trace_of::<T>, drop: drop_of::<T>, free: free_of::<T> };

    pub fn new(value: T) -> Self {
        note(|stats| stats.made += 1);
        let boxed = Box::new(Boxed {
            header: Header { count: Cell::new(1), color: Cell::new(Color::Black), buffered: Cell::new(false), dead: Cell::new(false), how: Self::HOW },
            value: ManuallyDrop::new(value),
        });
        Gc { at: NonNull::from(Box::leak(boxed)) }
    }

    fn header(&self) -> &Header {
        // SAFETY: a reference keeps what it refers to.
        unsafe { &self.at.as_ref().header }
    }

    pub fn edge(&self) -> Edge {
        Edge(self.at.cast())
    }

    /// The value, to change, if this is the only reference to it.
    pub fn get_mut(&mut self) -> Option<&mut T> {
        if self.header().count.get() == 1 {
            // SAFETY: nothing else refers to it.
            Some(unsafe { &mut self.at.as_mut().value })
        } else {
            None
        }
    }
}

impl<T: Trace> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        // SAFETY: a reference keeps what it refers to.
        unsafe { &self.at.as_ref().value }
    }
}

impl<T: Trace> Clone for Gc<T> {
    fn clone(&self) -> Self {
        let header = self.header();
        header.count.set(header.count.get() + 1);
        // What is being used is no root of garbage.
        header.color.set(Color::Black);
        Gc { at: self.at }
    }
}

impl<T: Trace> Drop for Gc<T> {
    fn drop(&mut self) {
        if SWEEPING.with(Cell::get) {
            return;
        }
        let at: NonNull<Header> = self.at.cast();
        let header = self.header();
        header.count.set(header.count.get() - 1);
        if header.count.get() == 0 {
            // SAFETY: the last reference is this one, now gone.
            unsafe { release(at) }
        } else if header.color.get() != Color::Purple {
            header.color.set(Color::Purple);
            if !header.buffered.get() {
                header.buffered.set(true);
                ROOTS.with(|roots| roots.borrow_mut().push(at));
            }
        }
    }
}

unsafe fn trace_of<T: Trace>(at: NonNull<Header>, visit: Visit) {
    // SAFETY: the header is the start of a box of this type.
    let boxed = unsafe { at.cast::<Boxed<T>>().as_ref() };
    boxed.value.trace(&mut |edge| visit(edge.0));
}

unsafe fn drop_of<T>(at: NonNull<Header>) {
    // SAFETY: as above, and the value is let go of once.
    unsafe { ManuallyDrop::drop(&mut at.cast::<Boxed<T>>().as_mut().value) }
}

unsafe fn free_of<T>(at: NonNull<Header>) {
    // SAFETY: as above; the value was let go of before.
    drop(unsafe { Box::from_raw(at.cast::<Boxed<T>>().as_ptr()) });
}

/// Let go of what a value holds, and free it unless it is among the roots,
/// where it is freed when the roots are next looked at.
///
/// What it holds may be let go of in turn, as deep as a list is long.
/// That is not done by this calling itself: a value let go of while
/// another is being let go of waits in a list, and they are taken from
/// the list one at a time.
unsafe fn release(at: NonNull<Header>) {
    if RELEASING.with(|releasing| releasing.replace(true)) {
        WAITING.with(|waiting| waiting.borrow_mut().push(at));
        return;
    }
    let mut next = Some(at);
    while let Some(at) = next {
        // SAFETY: the caller's: nothing refers to it any more.
        unsafe {
            let header = at.as_ref();
            header.color.set(Color::Black);
            header.dead.set(true);
            (header.how.drop)(at);
            note(|stats| stats.freed_by_count += 1);
            if !header.buffered.get() {
                (header.how.free)(at);
            }
        }
        next = WAITING.with(|waiting| waiting.borrow_mut().pop());
    }
    RELEASING.with(|releasing| releasing.set(false));
}

fn children(at: NonNull<Header>, mut visit: impl FnMut(NonNull<Header>)) {
    // SAFETY: only what is alive is looked at.
    unsafe { (at.as_ref().how.trace)(at, &mut visit) }
}

/// Free the values that only refer to each other. To be called where
/// nothing on the heap is in the middle of being changed. Gives how many
/// values were looked at, and how many of them were freed.
pub fn collect() -> (usize, usize) {
    let roots = ROOTS.with(|roots| std::mem::take(&mut *roots.borrow_mut()));
    if roots.is_empty() {
        return (0, 0);
    }
    let mut looked = 0;
    note(|stats| stats.collections += 1);
    // SAFETY: every root is alive or, if dead, has only its memory left,
    // which is freed here and nowhere else.
    unsafe {
        // Take off, from everything the roots reach, the references among
        // them.
        let mut suspects = Vec::new();
        let mut work = Vec::new();
        for &root in &roots {
            let header = root.as_ref();
            if header.dead.get() {
                (header.how.free)(root);
            } else if header.color.get() == Color::Purple {
                suspects.push(root);
                work.push(root);
                while let Some(at) = work.pop() {
                    let header = at.as_ref();
                    if header.color.get() != Color::Gray {
                        header.color.set(Color::Gray);
                        looked += 1;
                        children(at, |child| {
                            let count = &child.as_ref().count;
                            count.set(count.get() - 1);
                            work.push(child);
                        });
                    }
                }
            } else {
                header.buffered.set(false);
            }
        }
        // What still has a count is referred to from outside: it and all
        // it reaches are put back. What has none is garbage so far.
        for &root in &suspects {
            work.push(root);
            while let Some(at) = work.pop() {
                let header = at.as_ref();
                if header.color.get() != Color::Gray {
                    continue;
                }
                if header.count.get() > 0 {
                    header.color.set(Color::Black);
                    let mut back = vec![at];
                    while let Some(live) = back.pop() {
                        children(live, |child| {
                            let header = child.as_ref();
                            header.count.set(header.count.get() + 1);
                            if header.color.get() != Color::Black {
                                header.color.set(Color::Black);
                                back.push(child);
                            }
                        });
                    }
                } else {
                    header.color.set(Color::White);
                    children(at, |child| work.push(child));
                }
            }
        }
        // Gather the garbage, and then free it: what it holds first, with
        // no count changed, and then its memory.
        let mut garbage = Vec::new();
        for &root in &suspects {
            root.as_ref().buffered.set(false);
            work.push(root);
            while let Some(at) = work.pop() {
                let header = at.as_ref();
                if header.color.get() == Color::White && !header.buffered.get() {
                    header.color.set(Color::Black);
                    children(at, |child| work.push(child));
                    garbage.push(at);
                }
            }
        }
        SWEEPING.with(|sweeping| sweeping.set(true));
        for &at in &garbage {
            (at.as_ref().how.drop)(at);
        }
        SWEEPING.with(|sweeping| sweeping.set(false));
        for &at in &garbage {
            (at.as_ref().how.free)(at);
        }
        note(|stats| stats.freed_in_circles += garbage.len() as u64);
        (looked, garbage.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A value that may refer to another.
    struct Node {
        next: RefCell<Option<Gc<Node>>>,
    }

    impl Trace for Node {
        fn trace(&self, visit: &mut dyn FnMut(Edge)) {
            if let Some(next) = &*self.next.borrow() {
                visit(next.edge());
            }
        }
    }

    fn node(next: Option<Gc<Node>>) -> Gc<Node> {
        Gc::new(Node { next: RefCell::new(next) })
    }

    /// Nodes that refer to each other in a circle, and a reference to one.
    fn circle(size: usize) -> Gc<Node> {
        let first = node(None);
        let mut last = first.clone();
        for _ in 1..size {
            last = node(Some(last));
        }
        *first.next.borrow_mut() = Some(last);
        first
    }

    fn left() -> u64 {
        let now = stats();
        now.made - now.freed_by_count - now.freed_in_circles
    }

    #[test]
    fn what_refers_to_nothing_else_is_freed_by_its_count() {
        let chain = node(Some(node(Some(node(None)))));
        assert_eq!(left(), 3);
        drop(chain);
        assert_eq!((left(), stats().freed_by_count, stats().freed_in_circles), (0, 3, 0));
    }

    #[test]
    fn a_circle_is_freed_by_collecting() {
        drop(circle(5));
        assert_eq!(left(), 5);
        assert_eq!(collect(), (5, 5));
        assert_eq!((left(), stats().freed_in_circles), (0, 5));
    }

    #[test]
    fn a_circle_of_one_is_freed() {
        drop(circle(1));
        collect();
        assert_eq!(left(), 0);
    }

    #[test]
    fn a_circle_referred_to_from_outside_is_kept() {
        let kept = circle(4);
        // Letting go of a second reference makes it a possible root.
        drop(kept.clone());
        assert_eq!(collect(), (4, 0));
        assert_eq!(left(), 4);
        // And its counts are as they were: it is freed when let go of.
        drop(kept);
        collect();
        assert_eq!(left(), 0);
    }

    #[test]
    fn what_a_circle_refers_to_outside_itself_is_kept_if_referred_to() {
        let outside = node(None);
        let ring = circle(3);
        // One of the circle's refers to the outside one too, by way of a
        // node of its own.
        let spur = node(Some(outside.clone()));
        let also = node(Some(ring.clone()));
        *spur.next.borrow_mut() = Some(outside.clone());
        drop(also);
        drop(ring);
        drop(spur);
        collect();
        // The circle is gone, and the outside one is still here, alone.
        assert_eq!(left(), 1);
        assert!(outside.next.borrow().is_none());
    }

    #[test]
    fn garbage_hanging_off_a_circle_goes_with_it() {
        let ring = circle(2);
        let tail = node(Some(node(None)));
        // A node in the circle's way out: the circle refers to a chain.
        let joint = node(Some(ring.clone()));
        let holder = Gc::new(Pair(RefCell::new(Some(joint)), RefCell::new(Some(tail))));
        drop(ring);
        drop(holder);
        collect();
        assert_eq!(left(), 0);
    }

    /// A value that may refer to two others.
    struct Pair(RefCell<Option<Gc<Node>>>, RefCell<Option<Gc<Node>>>);

    impl Trace for Pair {
        fn trace(&self, visit: &mut dyn FnMut(Edge)) {
            for held in [&self.0, &self.1] {
                if let Some(node) = &*held.borrow() {
                    visit(node.edge());
                }
            }
        }
    }

    #[test]
    fn a_value_let_go_of_while_a_possible_root_is_freed_once() {
        let a = node(None);
        drop(a.clone());
        drop(a);
        assert_eq!(left(), 0);
        collect();
        assert_eq!(left(), 0);
    }

    #[test]
    fn many_circles_are_freed_together() {
        for size in 1..50 {
            drop(circle(size));
        }
        collect();
        assert_eq!(left(), 0);
    }
}
