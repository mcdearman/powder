//! The machine that runs Powder's bytecode.
//!
//! The bytecode is AxCut compiled for registers (see
//! `src/Powder/Bytecode.hs` in the compiler, which is the description of
//! the format). A value is in a register, and an instruction takes what it
//! uses out of its registers: in AxCut a value is used once, and what is
//! used twice was copied first by an instruction for that.
//!
//! Nothing is called and returned from. A method is gone to with its
//! arguments in the first registers, and what a computation comes back to
//! is a consumer, which is a frame on a stack the machine keeps for
//! itself, in chunks, and grows as it needs. So how deep a program goes is
//! not bounded by the stack the machine itself runs on, which never grows:
//! the loop that runs instructions calls nothing that calls it back, and
//! letting go of a long list is a loop too.
//!
//! Memory is counted, not traced. A value that is shared holds a count of
//! who holds it, and an instruction uses up what it takes, so the only
//! places a count goes up or a value is let go are the instructions that
//! copy and drop and the ones that copy out of an object that is still
//! shared. Values that refer to each other
//! in a circle, which counts alone would never free, are found and freed
//! by the collector in `gc.rs`, between instructions.

mod foreign;
mod gc;

// The machine allocates and frees small things all the time, which is what
// this allocator is quick at.
#[global_allocator]
static ALLOCATOR: mimalloc::MiMalloc = mimalloc::MiMalloc;

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{FromPrimitive, One, ToPrimitive, Zero};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, IsTerminal as _, Seek, SeekFrom, Write};
use gc::Gc;
use smallvec::SmallVec;
use std::rc::Rc;

// Values ---------------------------------------------------------------------

#[derive(Clone)]
enum Value {
    /// An integer that fits in sixty-four bits.
    Int(i64),
    /// One that does not.
    Big(Rc<BigInt>),
    Double(f64),
    /// A constructed value with no fields: which constructor of its type.
    Tag(u32),
    Data(Gc<Data>),
    Object(Gc<RefCell<Cell>>),
    /// A consumer: a frame on the stack of them, by its chunk and its
    /// place there. It is used once, and is not counted.
    Frame(u32, u32),
    /// Bytes side by side, of a number fixed when they are made.
    Bytes(Rc<RefCell<Vec<u8>>>),
}

/// The values a constructed value or an object holds. Most hold few, and
/// those are kept in the value itself, with nothing more to allocate.
type Held = SmallVec<[Value; 4]>;

struct Data {
    tag: u32,
    fields: Held,
}

/// What an object is. A thunk is `Delayed`, then `Working`, then `Done`.
enum Cell {
    Made { captured: Held, method: u8, parameters: usize, block: usize },
    Delayed { captured: Held, block: usize },
    Working,
    Done(Value),
    /// A thunk whose working out something raised went past: forced, it
    /// raises that again.
    Raises(Value),
}

impl Value {
    /// What it refers to on the heap, if it is a reference.
    fn edge(&self, visit: &mut dyn FnMut(gc::Edge)) {
        match self {
            Value::Data(data) => visit(data.edge()),
            Value::Object(object) => visit(object.edge()),
            _ => {}
        }
    }
}

impl gc::Trace for Data {
    fn trace(&self, visit: &mut dyn FnMut(gc::Edge)) {
        self.fields.iter().for_each(|field| field.edge(visit));
    }
}

impl gc::Trace for RefCell<Cell> {
    fn trace(&self, visit: &mut dyn FnMut(gc::Edge)) {
        match &*self.borrow() {
            Cell::Made { captured, .. } | Cell::Delayed { captured, .. } => captured.iter().for_each(|held| held.edge(visit)),
            Cell::Done(value) | Cell::Raises(value) => value.edge(visit),
            Cell::Working => {}
        }
    }
}

const RET: u8 = 0;
/// The method of a function: an argument and the consumer of the result.
const AP: u8 = 1;

// The image ------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Debug)]
enum Prim {
    IntAdd, IntSub, IntMul, IntQuot, IntRem, IntEq, IntLt, IntLe, IntNarrow, IntToDouble,
    DoubleAdd, DoubleSub, DoubleMul, DoubleDiv, DoublePow, DoubleAtan2, DoubleEq, DoubleLt, DoubleLe,
    DoubleTruncate, DoubleFun, DoubleShowAt, FloatShowAt, FloatNarrow, DoubleMantissa, DoubleExponent,
    DoubleEncode, DoubleFromRatio, CharCategory, CharUpper, CharLower, CharTitle, MatchFail, PutChar, GetChar, Exit, PathChar, FileOpen, HGetChar, FileClose, HIsEOF,
    ArgCount, ArgChar, FileSeek, FileTruncate, FileFlush, IsTerminal, EnvChar, CellNew, CellRead, CellWrite,
    PeekInt, PeekDouble, PokeInt, PokeDouble, ErrnoGet, ErrnoSet, CallbackInt, CallbackDouble, CallbackReturnInt,
    CallbackReturnDouble,
    IntAnd, IntOr, IntXor, IntShiftL, IntShiftR, BitsCount, BitsLowest, BitsHighest,
    BytesNew, BytesWrite, BytesCopy, BytesRead, BytesLength, BytesCompare, Raise, HandlerPush, HandlerPop,
}

fn prim_named(name: &str) -> Result<Prim, String> {
    use Prim::*;
    Ok(match name {
        "primIntAdd" => IntAdd, "primIntSub" => IntSub, "primIntMul" => IntMul, "primIntQuot" => IntQuot,
        "primIntRem" => IntRem, "primIntEq" => IntEq, "primIntLt" => IntLt, "primIntLe" => IntLe,
        "primIntNarrow" => IntNarrow, "primIntToDouble" => IntToDouble, "primDoubleAdd" => DoubleAdd,
        "primDoubleSub" => DoubleSub, "primDoubleMul" => DoubleMul, "primDoubleDiv" => DoubleDiv,
        "primDoublePow" => DoublePow, "primDoubleAtan2" => DoubleAtan2, "primDoubleEq" => DoubleEq,
        "primDoubleLt" => DoubleLt, "primDoubleLe" => DoubleLe, "primDoubleTruncate" => DoubleTruncate,
        "primDoubleFun" => DoubleFun, "primDoubleShowAt" => DoubleShowAt, "primFloatShowAt" => FloatShowAt,
        "primFloatNarrow" => FloatNarrow, "primDoubleMantissa" => DoubleMantissa,
        "primDoubleExponent" => DoubleExponent, "primDoubleEncode" => DoubleEncode, "primDoubleFromRatio" => DoubleFromRatio, "primCharCategory" => CharCategory, "primCharUpper" => CharUpper,
        "primCharLower" => CharLower, "primCharTitle" => CharTitle, "primMatchFail" => MatchFail,
        "primPutChar" => PutChar, "primGetChar" => GetChar, "primExit" => Exit, "primPathChar" => PathChar,
        "primFileOpen" => FileOpen, "primHGetChar" => HGetChar, "primFileClose" => FileClose,
        "primHIsEOF" => HIsEOF, "primArgCount" => ArgCount, "primArgChar" => ArgChar,
        "primFileSeek" => FileSeek, "primFileTruncate" => FileTruncate, "primFileFlush" => FileFlush,
        "primIsTerminal" => IsTerminal, "primEnvChar" => EnvChar, "primCellNew" => CellNew,
        "primCellRead" => CellRead, "primCellWrite" => CellWrite,
        "primPeekInt" => PeekInt, "primPeekDouble" => PeekDouble, "primPokeInt" => PokeInt,
        "primPokeDouble" => PokeDouble, "primErrnoGet" => ErrnoGet, "primErrnoSet" => ErrnoSet,
        "primCallbackInt" => CallbackInt, "primCallbackDouble" => CallbackDouble,
        "primCallbackReturnInt" => CallbackReturnInt, "primCallbackReturnDouble" => CallbackReturnDouble,
        "primIntAnd" => IntAnd, "primIntOr" => IntOr, "primIntXor" => IntXor, "primIntShiftL" => IntShiftL,
        "primIntShiftR" => IntShiftR, "primBitsCount" => BitsCount, "primBitsLowest" => BitsLowest,
        "primBitsHighest" => BitsHighest, "primBytesNew" => BytesNew, "primBytesWrite" => BytesWrite,
        "primBytesCopy" => BytesCopy, "primBytesRead" => BytesRead, "primBytesLength" => BytesLength,
        "primBytesCompare" => BytesCompare, "primRaise" => Raise, "primHandlerPush" => HandlerPush,
        "primHandlerPop" => HandlerPop,
        other => return Err(format!("the image uses a primitive this machine has not got: {other}")),
    })
}

/// A register: where a value is.
type Reg = u16;

/// What an object is, apart from what it holds: whether it is a thunk,
/// its method, how many parameters that has, and where its code is.
#[derive(Clone, Copy)]
struct Made {
    thunk: bool,
    method: u8,
    parameters: usize,
    block: usize,
}

/// An instruction, as the image has it: what it is, three registers, and
/// a number. See `Powder.Bytecode`, which is the only description of them.
#[derive(Clone, Copy)]
struct Instr {
    op: u8,
    a: u8,
    b: u8,
    c: u8,
    imm: u32,
}

mod op {
    pub const HALT: u8 = 0;
    pub const MOVE: u8 = 1;
    pub const COPY: u8 = 2;
    pub const DROP: u8 = 3;
    pub const DATA: u8 = 4;
    pub const SWITCH: u8 = 5;
    pub const CLOSURE: u8 = 6;
    pub const THUNK: u8 = 7;
    pub const FRAME: u8 = 8;
    pub const INVOKE: u8 = 9;
    pub const FORCE: u8 = 10;
    pub const INT: u8 = 11;
    pub const CONST: u8 = 12;
    pub const GLOBAL: u8 = 13;
    pub const PRIM: u8 = 14;
    pub const PRIM1: u8 = 15;
    pub const PRIM2: u8 = 16;
    pub const FOREIGN: u8 = 17;
    pub const REC: u8 = 18;
    pub const LAST: u8 = REC;
}

/// A clause of a switch: where the list of its fields' registers starts,
/// how many, and where its code is.
type Clause = (usize, usize, usize);

/// An object of a recursive group: its register, its shape, where the
/// list of the registers it holds starts and how many, and which of the
/// group's objects it holds after those.
type Mutual = (Reg, usize, usize, usize, Box<[usize]>);

struct Image {
    code: Vec<Instr>,
    /// For each instruction that takes several registers, where their list
    /// starts in `lists`.
    lists_at: Vec<u32>,
    lists: Vec<u8>,
    constants: Vec<Value>,
    prims: Vec<Prim>,
    foreigns: Vec<(String, String, char)>,
    shapes: Vec<Made>,
    switches: Vec<Box<[Clause]>>,
    recs: Vec<Box<[Mutual]>>,
    /// How many registers the program uses.
    registers: usize,
    definitions: Vec<usize>,
    entry: usize,
    /// Where a call from C is answered, which starts with the action that
    /// answers it in the first register.
    callback: usize,
    /// What C may call: its name, the letters of its arguments and of its
    /// result, and the definition that is the action answering it.
    exports: Vec<(String, String, char, usize)>,
}

impl Image {
    /// The registers an instruction takes several of.
    fn list(&self, pc: usize, count: u8) -> &[u8] {
        let start = self.lists_at[pc] as usize;
        &self.lists[start..start + count as usize]
    }
}

struct Reader<'a> {
    bytes: &'a [u8],
    at: usize,
}

impl Reader<'_> {
    fn byte(&mut self) -> Result<u8, String> {
        let b = *self.bytes.get(self.at).ok_or("the image ends too soon")?;
        self.at += 1;
        Ok(b)
    }
    fn word(&mut self) -> Result<u32, String> {
        let bytes = self.bytes.get(self.at..self.at + 4).ok_or("the image ends too soon")?;
        self.at += 4;
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }
    fn number(&mut self) -> Result<usize, String> {
        Ok(self.word()? as usize)
    }
    fn many<T>(&mut self, mut each: impl FnMut(&mut Self) -> Result<T, String>) -> Result<Vec<T>, String> {
        let n = self.number()?;
        if n > self.bytes.len() {
            return Err("the image says it has more than it can".into());
        }
        (0..n).map(|_| each(self)).collect()
    }
}

fn load(bytes: &[u8]) -> Result<Image, String> {
    let mut r = Reader { bytes, at: 0 };
    if bytes.len() < 4 || &bytes[..4] != b"PWDR" {
        return Err("this is not an image of a Powder program".into());
    }
    r.at = 4;
    let version = r.number()?;
    if version != 4 {
        return Err(format!("the image is of version {version}, and this machine runs version 4"));
    }
    let registers = r.number()?;
    if registers > 256 {
        return Err("the image uses more registers than there can be".into());
    }
    let strings = r.many(|r| {
        let n = r.number()?;
        let text = r.bytes.get(r.at..r.at + n).ok_or("the image ends too soon")?;
        r.at += n;
        String::from_utf8(text.to_vec()).map_err(|_| "a string in the image is not text".to_string())
    })?;
    let string = |i: usize| strings.get(i).cloned().ok_or_else(|| "the image names a string it has not got".to_string());
    let constants = r.many(|r| {
        let text = string(r.number()?)?;
        Ok(integer(text.parse::<BigInt>().map_err(|_| "a number in the image is not one")?))
    })?;
    let prims = r.many(|r| prim_named(&string(r.number()?)?))?;
    let foreigns = r.many(|r| Ok((string(r.number()?)?, string(r.number()?)?, char::from_u32(r.word()?).ok_or("a C function's result is not a letter")?)))?;
    let shapes = r.many(|r| {
        let (kind, method) = (r.byte()?, r.byte()?);
        let parameters = u16::from_le_bytes([r.byte()?, r.byte()?]) as usize;
        Ok(Made { thunk: kind == 1, method, parameters, block: r.number()? })
    })?;
    let n = r.number()?;
    let lists = bytes.get(r.at..r.at + n).ok_or("the image ends too soon")?.to_vec();
    r.at += n;
    let switches = r.many(|r| Ok(r.many(|r| Ok((r.number()?, r.number()?, r.number()?)))?.into_boxed_slice()))?;
    let recs = r.many(|r| {
        Ok(r.many(|r| {
            let to = r.byte()? as Reg;
            let (shape, start, count) = (r.number()?, r.number()?, r.number()?);
            Ok((to, shape, start, count, r.many(|r| r.number())?.into_boxed_slice()))
        })?
        .into_boxed_slice())
    })?;
    let code = r.many(|r| {
        let (op, a, b, c) = (r.byte()?, r.byte()?, r.byte()?, r.byte()?);
        if op > op::LAST {
            return Err(format!("the image has an instruction this machine has not got: {op}"));
        }
        Ok(Instr { op, a, b, c, imm: r.word()? })
    })?;
    let lists_at = (0..code.len()).map(|_| r.word()).collect::<Result<Vec<_>, _>>()?;
    let definitions = r.many(|r| {
        let _name = r.number()?;
        r.number()
    })?;
    let (entry, callback) = (r.number()?, r.number()?);
    let exports = r.many(|r| Ok((string(r.number()?)?, string(r.number()?)?, char::from_u32(r.word()?).ok_or("a C function's result is not a letter")?, r.number()?)))?;
    // Everything the code names is there: what is past the end of a table
    // is found here, and not while the program runs.
    let places = code.len();
    let bad = |what: &str| Err(format!("the image is malformed: {what}"));
    for (pc, i) in code.iter().enumerate() {
        let start = lists_at[pc] as usize;
        let lists_ok = match i.op {
            op::DATA | op::CLOSURE | op::THUNK | op::FRAME | op::PRIM | op::FOREIGN => start + i.c as usize <= lists.len(),
            _ => true,
        };
        let named_ok = match i.op {
            op::SWITCH => (i.imm as usize) < switches.len(),
            op::CLOSURE | op::THUNK | op::FRAME => (i.imm as usize) < shapes.len(),
            op::CONST => (i.imm as usize) < constants.len(),
            op::GLOBAL => (i.imm as usize) < definitions.len(),
            op::PRIM | op::PRIM1 | op::PRIM2 => (i.imm as usize) < prims.len(),
            op::FOREIGN => (i.imm as usize) < foreigns.len(),
            op::REC => (i.imm as usize) < recs.len(),
            _ => true,
        };
        if !lists_ok || !named_ok {
            return bad("an instruction names what the image has not got");
        }
    }
    if shapes.iter().any(|s| s.block >= places)
        || switches.iter().flatten().any(|&(start, count, at)| at >= places || start + count > lists.len())
        || recs.iter().any(|group| group.iter().any(|(_, shape, start, count, others)| *shape >= shapes.len() || start + count > lists.len() || others.iter().any(|&o| o >= group.len())))
        || definitions.iter().chain([&entry, &callback]).any(|&at| at >= places)
    {
        return bad("a place past the end of the code");
    }
    Ok(Image { code, lists_at, lists, constants, prims, foreigns, shapes, switches, recs, registers, definitions, entry, callback, exports })
}

// The stack of frames ----------------------------------------------------------

/// The consumers that are waiting: what a computation will be given to
/// when it is done. A consumer is made before what it waits for is begun
/// and is called once, by which time every consumer made after it has
/// been called or will never be, so they are a stack.
///
/// The stack is not the machine's own. It is in chunks, each a run of
/// values, and grows by another chunk when one is full; a frame is in one
/// chunk, a word saying where its code is and how much it holds, and then
/// what it holds. Calling a frame takes what it holds back out and lets
/// go of the frame and of every frame above it.
struct Frames {
    chunks: Vec<Vec<Value>>,
    /// The chunk the top of the stack is in.
    current: usize,
}

/// How many values a chunk has room for, unless one frame needs more.
const CHUNK: usize = 8192;

/// What a frame that keeps a thunk's value says in place of its code.
const UPDATE: i64 = -1;

impl Frames {
    fn new() -> Self {
        Frames { chunks: vec![Vec::with_capacity(CHUNK)], current: 0 }
    }

    /// Push a frame, and give the value that is the way to it.
    fn push(&mut self, header: i64, held: impl ExactSizeIterator<Item = Value>) -> Value {
        let size = 1 + held.len();
        if self.chunks[self.current].len() + size > self.chunks[self.current].capacity() {
            // The next chunk, which may be one that was let go of before.
            if !self.chunks[self.current].is_empty() {
                self.current += 1;
            }
            if self.current == self.chunks.len() {
                self.chunks.push(Vec::with_capacity(size.max(CHUNK)));
            } else if self.chunks[self.current].capacity() < size {
                self.chunks[self.current] = Vec::with_capacity(size);
            }
        }
        let chunk = &mut self.chunks[self.current];
        let at = chunk.len();
        chunk.push(Value::Int(header));
        chunk.extend(held);
        Value::Frame(self.current as u32, at as u32)
    }

    /// Call a frame: let go of every frame above it, take what it holds
    /// out into some registers, and let go of it. Gives what it said of
    /// its code.
    fn pop(&mut self, chunk: u32, at: u32, into: &mut [Value]) -> Result<i64, String> {
        let gone = || "the image is malformed: a consumer was called that is not there".to_string();
        let (chunk, at) = (chunk as usize, at as usize);
        if chunk > self.current {
            return Err(gone());
        }
        while self.current > chunk {
            self.chunks[self.current].clear();
            self.current -= 1;
        }
        let frames = &mut self.chunks[chunk];
        let Some(Value::Int(header)) = frames.get(at) else { return Err(gone()) };
        let header = *header;
        // A frame that keeps a thunk's value holds two things; any other
        // says how many beside where its code is.
        let held = if header == UPDATE { 2 } else { (header & 0xFFFF) as usize };
        if at + 1 + held > frames.len() || held > into.len() {
            return Err(gone());
        }
        frames.truncate(at + 1 + held);
        for (to, from) in into.iter_mut().zip(frames.drain(at + 1..)) {
            *to = from;
        }
        frames.truncate(at);
        Ok(if header == UPDATE { UPDATE } else { header >> 16 })
    }
}

// Numbers --------------------------------------------------------------------

/// An integer as a value: in sixty-four bits if it fits.
fn integer(n: BigInt) -> Value {
    match n.to_i64() {
        Some(i) => Value::Int(i),
        None => Value::Big(Rc::new(n)),
    }
}

fn big(v: &Value) -> Result<BigInt, String> {
    match v {
        Value::Int(i) => Ok(BigInt::from(*i)),
        Value::Big(b) => Ok((**b).clone()),
        _ => Err("type error: expected an integer".into()),
    }
}

/// The low sixty-four bits of an integer.
fn low(v: &Value) -> Result<u64, String> {
    match v {
        Value::Int(i) => Ok(*i as u64),
        _ => Ok(big(v)?.mod_floor(&(BigInt::one() << 64)).to_u64().unwrap_or(0)),
    }
}

/// Bytes, and a run of them that is all there: where it starts and ends.
fn bytes(v: &Value) -> Result<&Rc<RefCell<Vec<u8>>>, String> {
    match v {
        Value::Bytes(b) => Ok(b),
        _ => Err("type error: expected bytes".into()),
    }
}

fn run(of: &Rc<RefCell<Vec<u8>>>, at: &Value, count: usize) -> Result<std::ops::Range<usize>, String> {
    let at = usize::try_from(small(at)?).map_err(|_| "bytes: out of range")?;
    match at.checked_add(count) {
        Some(end) if end <= of.borrow().len() => Ok(at..end),
        _ => Err("bytes: out of range".into()),
    }
}

fn how_many(v: &Value) -> Result<usize, String> {
    usize::try_from(small(v)?).map_err(|_| "bytes: out of range".into())
}

fn small(v: &Value) -> Result<i64, String> {
    match v {
        Value::Int(i) => Ok(*i),
        Value::Big(_) => Err("a number is too big for what it is used as".into()),
        _ => Err("type error: expected an integer".into()),
    }
}

fn double(v: &Value) -> Result<f64, String> {
    match v {
        Value::Double(x) => Ok(*x),
        _ => Err("type error: expected a floating-point number".into()),
    }
}

/// A double as Haskell's `show` writes it: the fewest digits that read
/// back as the same number, plainly from a tenth up to ten million and
/// with an exponent outside that.
fn show_double(digits_and_exponent: String, negative: bool, x_is: (bool, bool, bool)) -> String {
    let (nan, infinite, zero) = x_is;
    if nan {
        return "NaN".into();
    }
    let sign = if negative { "-" } else { "" };
    if infinite {
        return format!("{sign}Infinity");
    }
    if zero {
        return format!("{sign}0.0");
    }
    // Rust writes d.ddde<n>; the digits, and the place of the point.
    let (mantissa, exponent) = digits_and_exponent.split_once('e').unwrap_or((&digits_and_exponent, "0"));
    let digits: String = mantissa.chars().filter(|c| c.is_ascii_digit()).collect();
    let e: i32 = exponent.parse::<i32>().unwrap_or(0) + 1;
    if (1..=7).contains(&e) {
        let e = e as usize;
        let (whole, fraction) = if digits.len() > e { (digits[..e].to_string(), digits[e..].to_string()) } else { (format!("{digits}{}", "0".repeat(e - digits.len())), String::new()) };
        format!("{sign}{whole}.{}", if fraction.is_empty() { "0" } else { &fraction })
    } else if e == 0 {
        format!("{sign}0.{digits}")
    } else {
        let (first, rest) = digits.split_at(1);
        format!("{sign}{first}.{}e{}", if rest.is_empty() { "0" } else { rest }, e - 1)
    }
}

fn shown64(x: f64) -> String {
    show_double(format!("{:e}", x.abs()), x.is_sign_negative(), (x.is_nan(), x.is_infinite(), x == 0.0))
}

fn shown32(x: f32) -> String {
    show_double(format!("{:e}", x.abs()), x.is_sign_negative(), (x.is_nan(), x.is_infinite(), x == 0.0))
}

/// The two integers a double is made of: it is the first times two to the
/// second, the first with fifty-three bits.
fn decode(x: f64) -> (i64, i64) {
    if x == 0.0 || x.is_nan() || x.is_infinite() {
        // Haskell gives something for the last two; nothing depends on what.
        return (0, 0);
    }
    let bits = x.to_bits();
    let exponent = ((bits >> 52) & 0x7ff) as i64;
    let fraction = (bits & ((1 << 52) - 1)) as i64;
    let (mut m, mut e) = if exponent == 0 { (fraction, -1074) } else { (fraction | (1 << 52), exponent - 1075) };
    // A number too small to have all its bits is given them by shifting.
    while m < (1 << 52) {
        m <<= 1;
        e -= 1;
    }
    (if x < 0.0 { -m } else { m }, e)
}

/// A double from two integers: the first times two to the second.
fn encode(m: &BigInt, e: i64) -> f64 {
    if m.is_zero() {
        return 0.0;
    }
    // Keep sixty-four bits of the first, which is more than a double has,
    // and note in the last of them whether anything was let go.
    let extra = (m.bits() as i64 - 64).max(0);
    let mut kept: BigInt = m >> (extra as usize);
    if extra > 0 && !(m - (&kept << (extra as usize))).is_zero() {
        kept |= BigInt::one();
    }
    let mut x = kept.to_f64().unwrap_or(0.0);
    let mut left = e + extra;
    while left > 0 {
        let step = left.min(1000);
        x *= 2f64.powi(step as i32);
        left -= step;
    }
    while left < 0 {
        let step = (-left).min(1000);
        x /= 2f64.powi(step as i32);
        left += step;
    }
    x
}

fn character(v: &Value) -> Result<char, String> {
    Ok(char::from_u32(small(v)? as u32).unwrap_or(char::REPLACEMENT_CHARACTER))
}

/// A character's other case, where that is one character; itself where it
/// is several, as Haskell's simple case mapping has it.
fn single(c: char, map: impl Fn(char) -> Vec<char>) -> i64 {
    match map(c).as_slice() {
        [one] => *one as i64,
        _ => c as i64,
    }
}

/// Which of Unicode's general categories a character is in, in the order
/// Haskell's `GeneralCategory` lists them.
fn category(c: char) -> i64 {
    use unicode_general_category::GeneralCategory::*;
    match unicode_general_category::get_general_category(c) {
        UppercaseLetter => 0, LowercaseLetter => 1, TitlecaseLetter => 2, ModifierLetter => 3, OtherLetter => 4,
        NonspacingMark => 5, SpacingMark => 6, EnclosingMark => 7, DecimalNumber => 8, LetterNumber => 9,
        OtherNumber => 10, ConnectorPunctuation => 11, DashPunctuation => 12, OpenPunctuation => 13,
        ClosePunctuation => 14, InitialPunctuation => 15, FinalPunctuation => 16, OtherPunctuation => 17,
        MathSymbol => 18, CurrencySymbol => 19, ModifierSymbol => 20, OtherSymbol => 21, SpaceSeparator => 22,
        LineSeparator => 23, ParagraphSeparator => 24, Control => 25, Format => 26, Surrogate => 27,
        PrivateUse => 28, Unassigned => 29,
        _ => 29,
    }
}

/// The double nearest the ratio of two integers.
fn ratio(n: &BigInt, d: &BigInt) -> f64 {
    if d.is_zero() {
        return if n.is_zero() { f64::NAN } else if n.sign() == num_bigint::Sign::Minus { f64::NEG_INFINITY } else { f64::INFINITY };
    }
    if n.is_zero() {
        return 0.0;
    }
    let negative = (n.sign() == num_bigint::Sign::Minus) != (d.sign() == num_bigint::Sign::Minus);
    let (n, d) = (n.magnitude().clone(), d.magnitude().clone());
    // Scale so that the quotient has at least sixty-six bits, and note
    // whether anything was left over, so that halfway is told from just over.
    let shift = (66 + d.bits() as i64 - n.bits() as i64).max(0);
    let scaled = n << (shift as usize);
    let mut q = &scaled / &d;
    if !(&scaled % &d).is_zero() {
        q |= num_bigint::BigUint::one();
    }
    let x = encode(&BigInt::from(q), -shift);
    if negative { -x } else { x }
}

// The machine ----------------------------------------------------------------

struct Machine {
    image: Image,
    globals: Vec<Gc<RefCell<Cell>>>,
    out: BufWriter<std::io::Stdout>,
    input: BufReader<std::io::Stdin>,
    path: String,
    // The files that are open, by number, and how many ever were: a number
    // is not used twice. What is read is read ahead; what is written is not
    // held back.
    files: HashMap<i64, BufReader<File>>,
    opened: i64,
    // The program's cells, each a place for a value that can be changed.
    cells: Vec<Value>,
    // The environment, each entry written NAME=value.
    environment: Vec<String>,
    arguments: Vec<String>,
    name: String,
    // The actions that answer calls from C, by number; and the calls being
    // answered, the innermost last: each its arguments, and its result.
    answers: Vec<Value>,
    calls: Vec<(Vec<foreign::Number>, foreign::Number)>,
    // The registers, and the stack of consumers.
    regs: Vec<Value>,
    stack: Frames,
    // The catches that are running, the innermost last: each its handler,
    // and how many thunks were being worked out when it began. And those
    // thunks, the one begun last last.
    handlers: Vec<(Value, usize)>,
    forcing: Vec<Gc<RefCell<Cell>>>,
    // How many possible roots of circles there are to be before they are
    // next looked at.
    due: usize,
}

/// How many possible roots there are to be before the first collection.
const FIRST_COLLECTION: usize = 16384;

/// Read one character of UTF-8, or nothing at the end.
fn read_char(from: &mut dyn BufRead) -> Option<char> {
    let mut first = [0u8; 1];
    if from.read(&mut first).ok()? == 0 {
        return None;
    }
    let n = match first[0] {
        b if b < 0x80 => return Some(b as char),
        b if b >> 5 == 0b110 => 1,
        b if b >> 4 == 0b1110 => 2,
        b if b >> 3 == 0b11110 => 3,
        _ => return Some(char::REPLACEMENT_CHARACTER),
    };
    let mut rest = [0u8; 3];
    if from.read_exact(&mut rest[..n]).is_err() {
        return Some(char::REPLACEMENT_CHARACTER);
    }
    let mut bytes = vec![first[0]];
    bytes.extend_from_slice(&rest[..n]);
    Some(std::str::from_utf8(&bytes).ok().and_then(|s| s.chars().next()).unwrap_or(char::REPLACEMENT_CHARACTER))
}

/// Give back what was read ahead of where the program has read to, so that
/// what is done to the file next is done there.
#[allow(clippy::seek_from_current)]
fn settle(file: &mut BufReader<File>) {
    if !file.buffer().is_empty() {
        let _ = file.seek(SeekFrom::Current(0));
    }
}

fn code_of(c: Option<char>) -> Value {
    Value::Int(c.map_or(-1, |c| c as i64))
}

fn truth(b: bool) -> Value {
    Value::Tag(b as u32)
}

const UNIT: Value = Value::Tag(0);

impl Machine {
    fn finish(&mut self, status: i32) -> ! {
        let _ = self.out.flush();
        // How memory went, for whoever asks.
        if std::env::var_os("POWDER_GC_STATS").is_some() {
            // What is left is to be what the program still had, so the
            // circles let go of since the last look are freed first.
            gc::collect();
            let stats = gc::stats();
            eprintln!(
                "gc: made {}, freed by count {}, freed in circles {}, left {}, collections {}",
                stats.made,
                stats.freed_by_count,
                stats.freed_in_circles,
                stats.made - stats.freed_by_count - stats.freed_in_circles,
                stats.collections
            );
        }
        std::process::exit(status)
    }

    fn primitive(&mut self, p: Prim, a: &[Value]) -> Result<Value, String> {
        use Prim::*;
        Ok(match (p, a) {
            (IntAdd, [Value::Int(x), Value::Int(y)]) => x.checked_add(*y).map_or_else(|| integer(BigInt::from(*x) + y), Value::Int),
            (IntSub, [Value::Int(x), Value::Int(y)]) => x.checked_sub(*y).map_or_else(|| integer(BigInt::from(*x) - y), Value::Int),
            (IntMul, [Value::Int(x), Value::Int(y)]) => x.checked_mul(*y).map_or_else(|| integer(BigInt::from(*x) * y), Value::Int),
            (IntAdd, [x, y]) => integer(big(x)? + big(y)?),
            (IntSub, [x, y]) => integer(big(x)? - big(y)?),
            (IntMul, [x, y]) => integer(big(x)? * big(y)?),
            (IntQuot | IntRem, [x, y]) => {
                let (x, y) = (big(x)?, big(y)?);
                if y.is_zero() {
                    return Err("division by zero".into());
                }
                // Both round towards zero, as Haskell's quot and rem do.
                integer(if p == IntQuot { x / y } else { x % y })
            }
            (IntEq, [Value::Int(x), Value::Int(y)]) => truth(x == y),
            (IntLt, [Value::Int(x), Value::Int(y)]) => truth(x < y),
            (IntLe, [Value::Int(x), Value::Int(y)]) => truth(x <= y),
            (IntEq, [x, y]) => truth(big(x)? == big(y)?),
            (IntLt, [x, y]) => truth(big(x)? < big(y)?),
            (IntLe, [x, y]) => truth(big(x)? <= big(y)?),
            (IntNarrow, [Value::Int(x)]) => Value::Int(*x),
            (IntNarrow, [x]) => {
                let modulus = BigInt::one() << 64;
                let low = big(x)?.mod_floor(&modulus).to_u64().unwrap_or(0);
                Value::Int(low as i64)
            }
            (IntToDouble, [x]) => Value::Double(big(x)?.to_f64().unwrap_or(f64::NAN)),
            (IntAnd, [Value::Int(x), Value::Int(y)]) => Value::Int(x & y),
            (IntOr, [Value::Int(x), Value::Int(y)]) => Value::Int(x | y),
            (IntXor, [Value::Int(x), Value::Int(y)]) => Value::Int(x ^ y),
            (IntAnd, [x, y]) => integer(big(x)? & big(y)?),
            (IntOr, [x, y]) => integer(big(x)? | big(y)?),
            (IntXor, [x, y]) => integer(big(x)? ^ big(y)?),
            (IntShiftL, [x, by]) => {
                let by = small(by)?.max(0);
                match x {
                    // Where nothing is moved off the end, it is as it was.
                    Value::Int(x) if by < 63 && (x << by) >> by == *x => Value::Int(x << by),
                    _ => integer(big(x)? << usize::try_from(by).map_err(|_| "a number is moved too far")?),
                }
            }
            (IntShiftR, [Value::Int(x), by]) => Value::Int(x >> small(by)?.clamp(0, 63)),
            (IntShiftR, [x, by]) => integer(big(x)? >> usize::try_from(small(by)?.max(0)).map_err(|_| "a number is moved too far")?),
            (BitsCount, [x]) => Value::Int(low(x)?.count_ones() as i64),
            (BitsLowest, [x]) => Value::Int(low(x)?.trailing_zeros() as i64),
            (BitsHighest, [x]) => Value::Int(63 - low(x)?.leading_zeros() as i64),
            (BytesNew, [n]) => Value::Bytes(Rc::new(RefCell::new(vec![0u8; usize::try_from(small(n)?).map_err(|_| "bytes cannot be fewer than none")?]))),
            (BytesLength, [b]) => Value::Int(bytes(b)?.borrow().len() as i64),
            (BytesRead, [b, at]) => {
                let b = bytes(b)?;
                let place = run(b, at, 1)?;
                let byte = b.borrow()[place.start];
                Value::Int(byte as i64)
            }
            (BytesWrite, [b, at, x]) => {
                let b = bytes(b)?;
                let place = run(b, at, 1)?;
                b.borrow_mut()[place.start] = low(x)? as u8;
                UNIT
            }
            (BytesCopy, [to, at, from, at_from, count]) => {
                let (to, from, count) = (bytes(to)?, bytes(from)?, how_many(count)?);
                let (into, out_of) = (run(to, at, count)?, run(from, at_from, count)?);
                // The two may be the same bytes.
                if Rc::ptr_eq(to, from) {
                    to.borrow_mut().copy_within(out_of, into.start);
                } else {
                    to.borrow_mut()[into].copy_from_slice(&from.borrow()[out_of]);
                }
                UNIT
            }
            (BytesCompare, [one, at, other, at_other, count]) => {
                let (one, other, count) = (bytes(one)?, bytes(other)?, how_many(count)?);
                let (a, b) = (run(one, at, count)?, run(other, at_other, count)?);
                let order = one.borrow()[a].cmp(&other.borrow()[b]);
                Value::Int(order as i64)
            }
            (DoubleAdd, [x, y]) => Value::Double(double(x)? + double(y)?),
            (DoubleSub, [x, y]) => Value::Double(double(x)? - double(y)?),
            (DoubleMul, [x, y]) => Value::Double(double(x)? * double(y)?),
            (DoubleDiv, [x, y]) => Value::Double(double(x)? / double(y)?),
            (DoublePow, [x, y]) => Value::Double(double(x)?.powf(double(y)?)),
            (DoubleAtan2, [x, y]) => Value::Double(double(x)?.atan2(double(y)?)),
            (DoubleEq, [x, y]) => truth(double(x)? == double(y)?),
            (DoubleLt, [x, y]) => truth(double(x)? < double(y)?),
            (DoubleLe, [x, y]) => truth(double(x)? <= double(y)?),
            (DoubleTruncate, [x]) => {
                let x = double(x)?;
                integer(BigInt::from_f64(x.trunc()).ok_or("a number that is not finite has no integer part")?)
            }
            (DoubleFun, [which, x]) => {
                let x = double(x)?;
                Value::Double(match small(which)? {
                    0 => x.exp(), 1 => x.ln(), 2 => x.sqrt(), 3 => x.sin(), 4 => x.cos(), 5 => x.tan(),
                    6 => x.asin(), 7 => x.acos(), 8 => x.atan(), 9 => x.sinh(), 10 => x.cosh(), 11 => x.tanh(),
                    12 => x.asinh(), 13 => x.acosh(), 14 => x.atanh(),
                    _ => return Err("no such function of a double".into()),
                })
            }
            (DoubleShowAt, [x, i]) => code_of(shown64(double(x)?).chars().nth(small(i)?.max(0) as usize)),
            (FloatShowAt, [x, i]) => code_of(shown32(double(x)? as f32).chars().nth(small(i)?.max(0) as usize)),
            (FloatNarrow, [x]) => Value::Double(double(x)? as f32 as f64),
            (DoubleMantissa, [x]) => Value::Int(decode(double(x)?).0),
            (DoubleExponent, [x]) => Value::Int(decode(double(x)?).1),
            (DoubleEncode, [m, e]) => Value::Double(encode(&big(m)?, small(e).unwrap_or(0).clamp(-100000, 100000))),
            (DoubleFromRatio, [n, d]) => Value::Double(ratio(&big(n)?, &big(d)?)),
            (CharCategory, [c]) => Value::Int(match small(c)? {
                // Half a pair is no character to Rust, and is one to Unicode.
                0xD800..=0xDFFF => 27,
                code => char::from_u32(code as u32).map_or(29, category),
            }),
            (CharUpper, [c]) => Value::Int(single(character(c)?, |x| x.to_uppercase().collect())),
            (CharLower, [c]) => Value::Int(single(character(c)?, |x| x.to_lowercase().collect())),
            (CharTitle, [c]) => {
                // The few characters that are two letters in one have a title case of their own.
                let c = character(c)?;
                Value::Int(match c as u32 {
                    0x01C4..=0x01C6 => 0x01C5,
                    0x01C7..=0x01C9 => 0x01C8,
                    0x01CA..=0x01CC => 0x01CB,
                    0x01F1..=0x01F3 => 0x01F2,
                    _ => single(c, |x| x.to_uppercase().collect()),
                })
            }
            (MatchFail, []) => return Err("pattern match failure".into()),
            (PutChar, [h, c]) => {
                let c = char::from_u32(small(c)? as u32).unwrap_or(char::REPLACEMENT_CHARACTER);
                let mut bytes = [0u8; 4];
                let text = c.encode_utf8(&mut bytes).as_bytes();
                match small(h)? {
                    2 => {
                        let _ = self.out.flush();
                        let _ = std::io::stderr().write_all(text);
                    }
                    h => match self.files.get_mut(&h) {
                        Some(file) => {
                            settle(file);
                            let _ = file.get_mut().write_all(text);
                        }
                        None => {
                            let _ = self.out.write_all(text);
                        }
                    },
                }
                UNIT
            }
            (GetChar, []) => {
                // What was written is seen before what is read is waited for.
                let _ = self.out.flush();
                code_of(read_char(&mut self.input))
            }
            (Exit, [n]) => self.finish(small(n)? as i32),
            (PathChar, [c]) => {
                self.path.push(char::from_u32(small(c)? as u32).unwrap_or(char::REPLACEMENT_CHARACTER));
                UNIT
            }
            (FileOpen, [mode]) => {
                let path = std::mem::take(&mut self.path);
                let mut options = std::fs::OpenOptions::new();
                let opened = match small(mode)? {
                    0 => options.read(true),
                    1 => options.write(true).create(true).truncate(true),
                    2 => options.append(true).create(true),
                    _ => options.read(true).write(true).create(true),
                }
                .open(&path)
                .and_then(|file| if file.metadata()?.is_dir() { Err(std::io::ErrorKind::Other.into()) } else { Ok(file) });
                match opened {
                    Ok(file) => {
                        let handle = 3 + self.opened;
                        self.opened += 1;
                        self.files.insert(handle, BufReader::new(file));
                        Value::Int(handle)
                    }
                    Err(e) => Value::Int(match e.kind() {
                        std::io::ErrorKind::NotFound => -1,
                        std::io::ErrorKind::PermissionDenied => -2,
                        _ => -4,
                    }),
                }
            }
            (HGetChar, [h]) => match self.files.get_mut(&small(h)?) {
                Some(file) => code_of(read_char(file)),
                None => Value::Int(-1),
            },
            (FileClose, [h]) => {
                self.files.remove(&small(h)?);
                UNIT
            }
            (HIsEOF, [h]) => match self.files.get_mut(&small(h)?) {
                Some(file) => truth(file.fill_buf().map_or(true, |b| b.is_empty())),
                None => {
                    let _ = self.out.flush();
                    truth(self.input.fill_buf().map_or(true, |b| b.is_empty()))
                }
            },
            (FileSeek, [h, whence, offset]) => {
                let (whence, offset) = (small(whence)?, small(offset)?);
                Value::Int(match self.files.get_mut(&small(h)?) {
                    Some(file) => match whence {
                        0 if offset < 0 => -1,
                        0 => file.seek(SeekFrom::Start(offset as u64)).map_or(-1, |p| p as i64),
                        1 => file.seek(SeekFrom::Current(offset)).map_or(-1, |p| p as i64),
                        2 => file.seek(SeekFrom::End(offset)).map_or(-1, |p| p as i64),
                        _ => file.get_ref().metadata().map_or(-1, |m| m.len() as i64),
                    },
                    None => -1,
                })
            }
            (FileTruncate, [h, size]) => {
                let size = small(size)?;
                Value::Int(match self.files.get_mut(&small(h)?) {
                    Some(file) if size >= 0 => {
                        settle(file);
                        file.get_ref().set_len(size as u64).map_or(-1, |()| 0)
                    }
                    _ => -1,
                })
            }
            (FileFlush, [h]) => {
                if small(h)? == 1 {
                    let _ = self.out.flush();
                }
                UNIT
            }
            (IsTerminal, [h]) => truth(match small(h)? {
                0 => std::io::stdin().is_terminal(),
                1 => std::io::stdout().is_terminal(),
                2 => std::io::stderr().is_terminal(),
                h => self.files.get(&h).is_some_and(|file| file.get_ref().is_terminal()),
            }),
            (EnvChar, [i, j]) => {
                let (i, j) = (small(i)?, small(j)?.max(0) as usize);
                code_of(usize::try_from(i).ok().and_then(|i| self.environment.get(i)).and_then(|entry| entry.chars().nth(j)))
            }
            (CellNew, [x]) => {
                self.cells.push(x.clone());
                Value::Int(self.cells.len() as i64 - 1)
            }
            (CellRead, [n]) => usize::try_from(small(n)?).ok().and_then(|n| self.cells.get(n)).cloned().ok_or("there is no such cell")?,
            (CellWrite, [n, x]) => {
                let cell = usize::try_from(small(n)?).ok().and_then(|n| self.cells.get_mut(n)).ok_or("there is no such cell")?;
                *cell = x.clone();
                UNIT
            }
            // SAFETY: the program says what is at the address, as a C
            // program would, and is as wrong as one would be if it is not.
            (PeekInt, [kind, address]) => {
                let at = small(address)? as usize;
                Value::Int(unsafe {
                    match small(kind)? {
                        0 => (at as *const i8).read_unaligned() as i64,
                        1 => (at as *const i16).read_unaligned() as i64,
                        2 => (at as *const i32).read_unaligned() as i64,
                        3 => (at as *const i64).read_unaligned(),
                        4 => (at as *const u8).read_unaligned() as i64,
                        5 => (at as *const u16).read_unaligned() as i64,
                        6 => (at as *const u32).read_unaligned() as i64,
                        _ => return Ok(integer(BigInt::from((at as *const u64).read_unaligned()))),
                    }
                })
            }
            (PeekDouble, [kind, address]) => {
                let at = small(address)? as usize;
                Value::Double(unsafe { if small(kind)? == 8 { (at as *const f32).read_unaligned() as f64 } else { (at as *const f64).read_unaligned() } })
            }
            (PokeInt, [kind, address, value]) => {
                let at = small(address)? as usize;
                // An integer too wide for the place is cut to it, as C does.
                let value = (big(value)? & BigInt::from(u64::MAX)).to_u64().unwrap_or(0);
                unsafe {
                    match small(kind)? {
                        0 | 4 => (at as *mut u8).write_unaligned(value as u8),
                        1 | 5 => (at as *mut u16).write_unaligned(value as u16),
                        2 | 6 => (at as *mut u32).write_unaligned(value as u32),
                        _ => (at as *mut u64).write_unaligned(value),
                    }
                }
                UNIT
            }
            (PokeDouble, [kind, address, value]) => {
                let (at, value) = (small(address)? as usize, double(value)?);
                unsafe {
                    if small(kind)? == 8 {
                        (at as *mut f32).write_unaligned(value as f32)
                    } else {
                        (at as *mut f64).write_unaligned(value)
                    }
                }
                UNIT
            }
            (CallbackInt, [i]) => {
                let i = small(i)? as usize;
                match self.calls.last().and_then(|(taken, _)| taken.get(i)) {
                    Some(foreign::Number::Long(n)) => Value::Int(*n),
                    _ => return Err("there is no such integer argument of a call from C".into()),
                }
            }
            (CallbackDouble, [i]) => {
                let i = small(i)? as usize;
                match self.calls.last().and_then(|(taken, _)| taken.get(i)) {
                    Some(foreign::Number::Double(x)) => Value::Double(*x),
                    _ => return Err("there is no such double argument of a call from C".into()),
                }
            }
            (CallbackReturnInt, [value]) => {
                let value = foreign::Number::Long(small(value)?);
                self.calls.last_mut().ok_or("there is no call from C to answer")?.1 = value;
                UNIT
            }
            (CallbackReturnDouble, [value]) => {
                let value = foreign::Number::Double(double(value)?);
                self.calls.last_mut().ok_or("there is no call from C to answer")?.1 = value;
                UNIT
            }
            (ErrnoGet, []) => Value::Int(foreign::errno() as i64),
            (ErrnoSet, [value]) => {
                foreign::set_errno(small(value)? as i32);
                UNIT
            }
            (ArgCount, []) => Value::Int(self.arguments.len() as i64),
            (ArgChar, [i, j]) => {
                let (i, j) = (small(i)?, small(j)?.max(0) as usize);
                let text = if i < 0 { Some(&self.name) } else { self.arguments.get(i as usize) };
                code_of(text.and_then(|t| t.chars().nth(j)))
            }
            _ => return Err(format!("type error: {p:?} was given what it cannot take")),
        })
    }

    fn object(made: &Made, captured: Held) -> Cell {
        if made.thunk {
            Cell::Delayed { captured, block: made.block }
        } else {
            Cell::Made { captured, method: made.method, parameters: made.parameters, block: made.block }
        }
    }

    /// Make what C may call C's, before anything runs.
    fn export(&mut self) -> Result<(), String> {
        for (name, shape, result, definition) in self.image.exports.clone() {
            let action = self.globals.get(definition).ok_or("the image exports a definition it has not got")?.clone();
            self.answers.push(Value::Object(action));
            foreign::export(&name, foreign::closure(&shape, result, self.answers.len() - 1)?);
        }
        Ok(())
    }

    /// Answer a call from C: run the action of a number, which takes the
    /// call's arguments and gives its result by the primitives for that.
    /// It runs in registers of its own, above what is on the stack of
    /// frames: the call it is answering is in the middle of an
    /// instruction, whose registers are still wanted.
    fn answer(&mut self, which: usize, taken: Vec<foreign::Number>) -> foreign::Number {
        let Some(action) = self.answers.get(which).cloned() else { return foreign::Number::Long(0) };
        self.calls.push((taken, foreign::Number::Long(0)));
        let mut fresh = vec![Value::Tag(0); self.regs.len()];
        fresh[0] = action;
        let kept = std::mem::replace(&mut self.regs, fresh);
        let start = self.image.callback;
        if let Err(message) = self.run(start) {
            eprintln!("runtime error: {message}");
            self.finish(1)
        }
        self.regs = kept;
        self.calls.pop().map_or(foreign::Number::Long(0), |(_, gave)| gave)
    }

    /// What is in a register, taken out of it.
    #[inline(always)]
    fn take(&mut self, r: Reg) -> Value {
        std::mem::replace(&mut self.regs[r as usize], Value::Tag(0))
    }

    /// Call a C function with the values in some registers, each as the
    /// kind its shape says.
    fn foreign_call(&mut self, symbol: &str, shape: &str, result: char, arguments: &[Reg]) -> Result<Value, String> {
        let given = shape
            .chars()
            .zip(arguments.iter())
            .map(|(kind, &r)| {
                let v = self.take(r);
                if kind == 'D' || kind == 'F' { double(&v).map(foreign::Number::Double) } else { small(&v).map(foreign::Number::Long) }
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(match (result, foreign::call(symbol, shape, result, &given)?) {
            ('V', _) => UNIT,
            (_, foreign::Number::Double(x)) => Value::Double(x),
            (kind, foreign::Number::Long(n)) => Value::Int(match kind {
                'I' => n as i32 as i64,
                'U' => n as u32 as i64,
                's' => n as i16 as i64,
                'S' => n as u16 as i64,
                'c' => n as i8 as i64,
                'C' => n as u8 as i64,
                _ => n,
            }),
        })
    }

    /// Give what is raised to the innermost catch that is running, and say
    /// where to go on. The thunks begun since it began are not finished,
    /// and will not be: each raises the same when it is next forced.
    fn raise(&mut self, raised: Value) -> Result<usize, String> {
        let (handler, height) = self.handlers.pop().ok_or("something was raised with no catch running, not even the outermost")?;
        for thunk in self.forcing.drain(height..) {
            *thunk.borrow_mut() = Cell::Raises(raised.clone());
        }
        self.regs[0] = raised;
        self.regs[1] = UNIT;
        self.call(handler, AP, 2)
    }

    /// Call a method of something, with its arguments in the first
    /// registers: what it captured goes into the registers after them, and
    /// where its code is is given. A thunk's value coming back to the
    /// frame that keeps it is kept, and passed on to the consumer of the
    /// force.
    fn call(&mut self, mut target: Value, method: u8, arguments: usize) -> Result<usize, String> {
        let malformed = |what: &str| format!("the image is malformed: {what}");
        loop {
            match target {
                Value::Frame(chunk, at) => {
                    if method != RET || arguments != 1 {
                        return Err(malformed("a consumer called as what it is not"));
                    }
                    let header = self.stack.pop(chunk, at, &mut self.regs[1..])?;
                    if header != UPDATE {
                        return Ok(header as usize);
                    }
                    // What the frame held is in the two registers after the
                    // value: the thunk, and who wanted it.
                    let (thunk, consumer) = (self.take(1), self.take(2));
                    let Value::Object(thunk) = thunk else { return Err(malformed("a value kept in what is not a thunk")) };
                    *thunk.borrow_mut() = Cell::Done(self.regs[0].clone());
                    self.forcing.pop();
                    target = consumer;
                }
                Value::Object(mut cell) => {
                    // What nothing else refers to is used up by the call:
                    // what it holds is moved out, not copied.
                    return if let Some(only) = cell.get_mut() {
                        match only.get_mut() {
                            Cell::Made { captured, method: has, parameters, block } if *has == method && *parameters == arguments => {
                                for (to, from) in self.regs[arguments..].iter_mut().zip(captured.drain(..)) {
                                    *to = from;
                                }
                                Ok(*block)
                            }
                            _ => Err(malformed("a call of what is not an object with that method")),
                        }
                    } else {
                        match &*cell.borrow() {
                            Cell::Made { captured, method: has, parameters, block } if *has == method && *parameters == arguments => {
                                for (to, from) in self.regs[arguments..].iter_mut().zip(captured.iter()) {
                                    *to = from.clone();
                                }
                                Ok(*block)
                            }
                            _ => Err(malformed("a call of what is not an object with that method")),
                        }
                    };
                }
                _ => return Err(malformed("a call of what is not an object")),
            }
        }
    }

    /// Run from a place in the code, with what it starts with in the
    /// first registers, until it halts. Nothing here calls itself: what a
    /// computation comes back to is a frame on the stack of them, which is
    /// the machine's to grow, and not this function's own stack.
    fn run(&mut self, start: usize) -> Result<(), String> {
        let malformed = |what: &str| format!("the image is malformed: {what}");
        let mut pc = start;
        let mut since = 0u32;
        loop {
            // Between instructions nothing is half made, so here is where
            // circles of garbage are looked for, when enough values have
            // been let go of that might be the way into one.
            since += 1;
            if since >= 4096 {
                since = 0;
                if gc::pending() > self.due {
                    // Looking costs as much as there is to look at. Where
                    // little of it turns out to be garbage, look less
                    // often; where much does, more often again.
                    let (looked, freed) = gc::collect();
                    self.due = if freed * 8 < looked { self.due.saturating_mul(2).min(1 << 26) } else { (self.due / 2).max(FIRST_COLLECTION) };
                }
            }
            let i = self.image.code[pc];
            let here = pc;
            pc += 1;
            let (a, b, c) = (i.a as Reg, i.b as Reg, i.c as Reg);
            match i.op {
                op::HALT => return Ok(()),
                op::MOVE => {
                    let value = self.take(b);
                    self.regs[a as usize] = value;
                }
                op::COPY => {
                    let value = self.regs[b as usize].clone();
                    self.regs[a as usize] = value;
                }
                op::DROP => self.regs[a as usize] = Value::Tag(0),
                op::DATA => {
                    let made = if i.c == 0 {
                        Value::Tag(i.imm)
                    } else {
                        let start = self.image.lists_at[here] as usize;
                        let held: Held = (start..start + i.c as usize).map(|at| self.take(self.image.lists[at] as Reg)).collect();
                        Value::Data(Gc::new(Data { tag: i.imm, fields: held }))
                    };
                    self.regs[a as usize] = made;
                }
                op::SWITCH => {
                    let (tag, mut fields) = match self.take(a) {
                        Value::Tag(tag) => (tag, Held::new()),
                        // If nothing else holds it, its fields are moved out.
                        Value::Data(mut data) => match data.get_mut() {
                            Some(mine) => (mine.tag, std::mem::take(&mut mine.fields)),
                            None => (data.tag, data.fields.clone()),
                        },
                        _ => return Err(malformed("a switch on what is not a constructed value")),
                    };
                    let &(start, count, target) = self.image.switches[i.imm as usize].get(tag as usize).ok_or_else(|| malformed("a switch has no clause for a constructor"))?;
                    if count != fields.len() {
                        return Err(malformed("a clause names the wrong number of fields"));
                    }
                    for (at, field) in (start..start + count).zip(fields.drain(..)) {
                        self.regs[self.image.lists[at] as usize] = field;
                    }
                    pc = target;
                }
                op::CLOSURE | op::THUNK => {
                    let start = self.image.lists_at[here] as usize;
                    let held: Held = (start..start + i.c as usize).map(|at| self.take(self.image.lists[at] as Reg)).collect();
                    let made = self.image.shapes[i.imm as usize];
                    self.regs[a as usize] = Value::Object(Gc::new(RefCell::new(Self::object(&made, held))));
                }
                op::FRAME => {
                    let start = self.image.lists_at[here] as usize;
                    let block = self.image.shapes[i.imm as usize].block;
                    let (regs, lists) = (&mut self.regs, &self.image.lists);
                    let held = (start..start + i.c as usize).map(|at| std::mem::replace(&mut regs[lists[at] as usize], Value::Tag(0)));
                    let frame = self.stack.push(((block as i64) << 16) | i.c as i64, held);
                    self.regs[a as usize] = frame;
                }
                op::REC => {
                    let group = self.image.recs[i.imm as usize].clone();
                    let cells: Vec<_> = group.iter().map(|_| Gc::new(RefCell::new(Cell::Working))).collect();
                    for ((_, shape, start, count, theirs), cell) in group.iter().zip(&cells) {
                        let mut held: Held = (*start..start + count).map(|at| self.take(self.image.lists[at] as Reg)).collect();
                        held.extend(theirs.iter().map(|&o| Value::Object(cells[o].clone())));
                        let made = self.image.shapes[*shape];
                        *cell.borrow_mut() = Self::object(&made, held);
                    }
                    for ((to, ..), cell) in group.iter().zip(cells) {
                        self.regs[*to as usize] = Value::Object(cell);
                    }
                }
                op::INT => self.regs[a as usize] = Value::Int(i.imm as i32 as i64),
                op::CONST => self.regs[a as usize] = self.image.constants[i.imm as usize].clone(),
                op::GLOBAL => self.regs[a as usize] = Value::Object(self.globals[i.imm as usize].clone()),
                op::PRIM | op::PRIM1 | op::PRIM2 => {
                    let prim = self.image.prims[i.imm as usize];
                    let given: Held = match i.op {
                        op::PRIM1 => std::iter::once(self.take(b)).collect(),
                        op::PRIM2 => [self.take(b), self.take(c)].into_iter().collect(),
                        _ => {
                            let start = self.image.lists_at[here] as usize;
                            (start..start + i.c as usize).map(|at| self.take(self.image.lists[at] as Reg)).collect()
                        }
                    };
                    match prim {
                        Prim::Raise => {
                            let raised = given.into_iter().next().ok_or_else(|| malformed("a raise of nothing"))?;
                            pc = self.raise(raised)?;
                        }
                        Prim::HandlerPush => {
                            let handler = given.into_iter().next().ok_or_else(|| malformed("a catch with no handler"))?;
                            self.handlers.push((handler, self.forcing.len()));
                            self.regs[a as usize] = UNIT;
                        }
                        Prim::HandlerPop => {
                            self.handlers.pop();
                            self.regs[a as usize] = UNIT;
                        }
                        _ => {
                            let result = self.primitive(prim, &given)?;
                            self.regs[a as usize] = result;
                        }
                    }
                }
                op::FOREIGN => {
                    let (symbol, shape, result) = self.image.foreigns[i.imm as usize].clone();
                    let arguments: Vec<Reg> = self.image.list(here, i.c).iter().map(|&r| r as Reg).collect();
                    if symbol == "wrapper" {
                        // A C function made of an action of the program's.
                        let action = arguments.first().map(|&r| self.take(r)).ok_or_else(|| malformed("a C function made of nothing"))?;
                        self.answers.push(action);
                        self.regs[a as usize] = Value::Int(foreign::closure(&shape, result, self.answers.len() - 1)?);
                    } else {
                        self.regs[a as usize] = self.foreign_call(&symbol, &shape, result, &arguments)?;
                    }
                }
                op::INVOKE => {
                    let target = self.take(c);
                    pc = self.call(target, i.b, c as usize)?;
                }
                // Work a value out if it is a thunk not yet worked out, which
                // is to run its body with a frame that will keep the value;
                // otherwise give it to the consumer.
                op::FORCE => {
                    let (mut x, k) = (self.take(a), self.take(b));
                    pc = loop {
                        let Value::Object(cell) = &x else {
                            self.regs[0] = x;
                            break self.call(k, RET, 1)?;
                        };
                        let state = std::mem::replace(&mut *cell.borrow_mut(), Cell::Working);
                        match state {
                            Cell::Delayed { captured, block } => {
                                self.forcing.push(cell.clone());
                                let update = self.stack.push(UPDATE, [x, k].into_iter());
                                self.regs[0] = update;
                                for (to, from) in self.regs[1..].iter_mut().zip(captured) {
                                    *to = from;
                                }
                                break block;
                            }
                            Cell::Done(v) => {
                                *cell.borrow_mut() = Cell::Done(v.clone());
                                x = v;
                            }
                            Cell::Working => return Err("a value depends on itself".into()),
                            Cell::Raises(raised) => {
                                *cell.borrow_mut() = Cell::Raises(raised.clone());
                                break self.raise(raised)?;
                            }
                            other => {
                                *cell.borrow_mut() = other;
                                self.regs[0] = x;
                                break self.call(k, RET, 1)?;
                            }
                        }
                    };
                }
                _ => return Err(malformed("an instruction this machine has not got")),
            }
        }
    }
}

/// The machine, for calls from C: they come from inside a call the machine
/// made, on its thread, and have nothing else to find it by.
static mut MACHINE: *mut Machine = std::ptr::null_mut();

fn answer(which: usize, taken: Vec<foreign::Number>) -> foreign::Number {
    // SAFETY: set before anything runs, to a machine that outlives the
    // run; and the machine holds nothing across a call of C's.
    let machine = unsafe { &mut *MACHINE };
    machine.answer(which, taken)
}

fn main() {
    let mut arguments = std::env::args().skip(1);
    let Some(path) = arguments.next() else {
        eprintln!("usage: powder-vm IMAGE [ARGS]");
        std::process::exit(2);
    };
    let bytes = std::fs::read(&path).unwrap_or_else(|e| {
        eprintln!("powder-vm: {path}: {e}");
        std::process::exit(2);
    });
    let rest: Vec<String> = arguments.collect();
    let image = load(&bytes).unwrap_or_else(|e| {
        eprintln!("powder-vm: {path}: {e}");
        std::process::exit(2);
    });
    let globals = image.definitions.iter().map(|&block| Gc::new(RefCell::new(Cell::Delayed { captured: Held::new(), block }))).collect();
    // Room for what a call brings with it, past the registers the program
    // names: what a frame held, with the value it is given.
    let registers = vec![Value::Tag(0); image.registers + 4];
    let mut machine = Machine {
        image,
        globals,
        out: BufWriter::new(std::io::stdout()),
        input: BufReader::new(std::io::stdin()),
        path: String::new(),
        files: HashMap::new(),
        opened: 0,
        cells: Vec::new(),
        environment: std::env::vars_os().map(|(name, value)| format!("{}={}", name.to_string_lossy(), value.to_string_lossy())).collect(),
        arguments: rest,
        name: path,
        answers: Vec::new(),
        calls: Vec::new(),
        regs: registers,
        stack: Frames::new(),
        handlers: Vec::new(),
        forcing: Vec::new(),
        // POWDER_GC=off leaves circles be, which is for measuring what
        // looking for them costs.
        due: if std::env::var_os("POWDER_GC").is_some_and(|v| v == "off") { usize::MAX } else { FIRST_COLLECTION },
    };
    // SAFETY: the machine is in one place for as long as it runs, and C
    // calls back only from inside a call the machine made.
    unsafe { MACHINE = &mut machine };
    foreign::answer_with(answer);
    let outcome = foreign::load_libraries().and_then(|()| machine.export()).and_then(|()| {
        let entry = machine.image.entry;
        machine.run(entry)
    });
    let _ = machine.out.flush();
    match outcome {
        Ok(()) => machine.finish(0),
        Err(message) => {
            eprintln!("runtime error: {message}");
            // Nothing is let go of on the way out: the system takes it all back.
            machine.finish(1)
        }
    }
}
