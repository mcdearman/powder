//! Calling C: a function found by name in what the machine is linked
//! with, or one whose address the program has, called at the type the
//! program says.
//!
//! A type is said by a letter for each argument and one for the result:
//! `c C s S I U L Q` for the integers, signed and not, of 8, 16, 32 and 64
//! bits, `F` and `D` for float and double, `P` for a pointer, and `V` for
//! no result. The call is made by libffi.

use libffi::middle::{Arg, Cif, CodePtr, Type};
use libffi::low::ffi_cif;
use libffi::middle::Closure;
use std::cell::Cell;
use std::collections::HashMap;
use std::ffi::{c_void, CString};
use std::sync::Mutex;

/// An argument or a result: an integer or a double.
#[derive(Clone, Copy)]
pub enum Number {
    Long(i64),
    Double(f64),
}

thread_local! {
    /// What errno was after the last call: nothing the machine does
    /// between two calls of the program's is to change what the program
    /// sees.
    static KEPT: Cell<i32> = const { Cell::new(0) };
}

fn errno_place() -> *mut i32 {
    // SAFETY: the C library's own way to its errno.
    unsafe {
        #[cfg(target_os = "macos")]
        return libc::__error();
        #[cfg(not(target_os = "macos"))]
        return libc::__errno_location();
    }
}

pub fn errno() -> i32 {
    KEPT.with(Cell::get)
}

pub fn set_errno(value: i32) {
    KEPT.with(|kept| kept.set(value));
}

/// Load the libraries the environment names in `POWDER_FOREIGN_LIBS`,
/// between colons, so that what is in them can be called by name.
pub fn load_libraries() -> Result<(), String> {
    let Some(named) = std::env::var_os("POWDER_FOREIGN_LIBS") else { return Ok(()) };
    for file in named.to_string_lossy().split(':').filter(|f| !f.is_empty()) {
        let path = CString::new(file).map_err(|_| format!("the library {file} could not be loaded"))?;
        // SAFETY: loading a library the user named.
        if unsafe { libc::dlopen(path.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL) }.is_null() {
            return Err(format!("the library {file} could not be loaded"));
        }
    }
    Ok(())
}

fn type_of(kind: char) -> Option<Type> {
    Some(match kind {
        'c' => Type::i8(),
        'C' => Type::u8(),
        's' => Type::i16(),
        'S' => Type::u16(),
        'I' => Type::i32(),
        'U' => Type::u32(),
        'L' => Type::i64(),
        'Q' => Type::u64(),
        'F' => Type::f32(),
        'D' => Type::f64(),
        'P' => Type::pointer(),
        'V' => Type::void(),
        _ => return None,
    })
}

/// An argument as C is given it.
enum Slot {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
    Pointer(*mut c_void),
}

/// The functions the program made C's, by name, each with its address.
static EXPORTED: Mutex<Option<HashMap<String, usize>>> = Mutex::new(None);

fn find(symbol: &str) -> Result<*mut c_void, String> {
    // A function the program made C's is found before anything of C's.
    if let Some(made) = EXPORTED.lock().ok().and_then(|known| known.as_ref().and_then(|names| names.get(symbol).copied())) {
        return Ok(made as *mut c_void);
    }
    let name = CString::new(symbol).map_err(|_| format!("there is no C function called {symbol}"))?;
    // SAFETY: looking a name up in what is already loaded.
    let found = unsafe { libc::dlsym(libc::RTLD_DEFAULT, name.as_ptr()) };
    if found.is_null() {
        Err(format!("there is no C function called {symbol}"))
    } else {
        Ok(found)
    }
}

/// Call a C function, given what its arguments are and what it gives. The
/// function is the one of a name; or for `&name`, nothing is called and
/// the address of the name is given; or for `dynamic`, the function is
/// the one whose address is the first argument.
pub fn call(symbol: &str, shape: &str, result: char, arguments: &[Number]) -> Result<Number, String> {
    let unfit = || format!("a C function of this type cannot be called: {symbol}");
    if let Some(name) = symbol.strip_prefix('&') {
        return Ok(Number::Long(find(name)? as i64));
    }
    let (function, shape, arguments) = match (symbol, arguments) {
        ("dynamic", [Number::Long(address), rest @ ..]) if shape.starts_with('P') => (*address as usize as *mut c_void, &shape[1..], rest),
        ("dynamic", _) => return Err("a C function called by its address needs the address first".into()),
        _ => (find(symbol)?, shape, arguments),
    };
    let mut types = Vec::with_capacity(arguments.len());
    let mut slots = Vec::with_capacity(arguments.len());
    for (kind, argument) in shape.chars().zip(arguments) {
        types.push(type_of(kind).filter(|_| kind != 'V').ok_or_else(unfit)?);
        slots.push(match (kind, *argument) {
            ('c', Number::Long(n)) => Slot::I8(n as i8),
            ('C', Number::Long(n)) => Slot::U8(n as u8),
            ('s', Number::Long(n)) => Slot::I16(n as i16),
            ('S', Number::Long(n)) => Slot::U16(n as u16),
            ('I', Number::Long(n)) => Slot::I32(n as i32),
            ('U', Number::Long(n)) => Slot::U32(n as u32),
            ('L', Number::Long(n)) => Slot::I64(n),
            ('Q', Number::Long(n)) => Slot::U64(n as u64),
            ('P', Number::Long(n)) => Slot::Pointer(n as usize as *mut c_void),
            ('F', Number::Double(x)) => Slot::F32(x as f32),
            ('D', Number::Double(x)) => Slot::F64(x),
            _ => return Err(unfit()),
        });
    }
    if slots.len() != shape.chars().count() {
        return Err(unfit());
    }
    let given: Vec<Arg> = slots
        .iter()
        .map(|slot| match slot {
            Slot::I8(v) => Arg::new(v),
            Slot::U8(v) => Arg::new(v),
            Slot::I16(v) => Arg::new(v),
            Slot::U16(v) => Arg::new(v),
            Slot::I32(v) => Arg::new(v),
            Slot::U32(v) => Arg::new(v),
            Slot::I64(v) => Arg::new(v),
            Slot::U64(v) => Arg::new(v),
            Slot::F32(v) => Arg::new(v),
            Slot::F64(v) => Arg::new(v),
            Slot::Pointer(v) => Arg::new(v),
        })
        .collect();
    let cif = Cif::new(types, type_of(result).ok_or_else(unfit)?);
    let code = CodePtr::from_ptr(function);
    // SAFETY: the program declared the function to have this type, which
    // is all that can be known of it. A result narrower than a word comes
    // back widened to one, so every integer is read as a whole word.
    unsafe {
        *errno_place() = errno();
        let gave = match result {
            'D' => Number::Double(cif.call::<f64>(code, &given)),
            'F' => Number::Double(cif.call::<f32>(code, &given) as f64),
            'V' => {
                cif.call::<()>(code, &given);
                Number::Long(0)
            }
            _ => Number::Long(cif.call::<i64>(code, &given)),
        };
        set_errno(*errno_place());
        Ok(gave)
    }
}

// C calling back --------------------------------------------------------------

/// What a C function made here is: the letters of its type, and the number
/// of the action that answers it.
struct Made {
    shape: String,
    result: char,
    which: usize,
}

/// Where calls from C go: the machine's, given the number of the action and
/// the arguments. It is set once, before any C function is made.
type Answerer = fn(usize, Vec<Number>) -> Number;

static ANSWERER: Mutex<Option<Answerer>> = Mutex::new(None);

pub fn answer_with(answerer: Answerer) {
    if let Ok(mut place) = ANSWERER.lock() {
        *place = Some(answerer);
    }
}

/// What libffi calls when C calls a function made here.
unsafe extern "C" fn answer(_cif: &ffi_cif, result: &mut u64, arguments: *const *const c_void, made: &Made) {
    let mut taken = Vec::with_capacity(made.shape.len());
    for (i, kind) in made.shape.chars().enumerate() {
        // SAFETY: libffi gives as many places as the type has arguments,
        // each holding what its letter says.
        let at = unsafe { *arguments.add(i) };
        taken.push(unsafe {
            match kind {
                'c' => Number::Long(*(at as *const i8) as i64),
                'C' => Number::Long(*(at as *const u8) as i64),
                's' => Number::Long(*(at as *const i16) as i64),
                'S' => Number::Long(*(at as *const u16) as i64),
                'I' => Number::Long(*(at as *const i32) as i64),
                'U' => Number::Long(*(at as *const u32) as i64),
                'F' => Number::Double(*(at as *const f32) as f64),
                'D' => Number::Double(*(at as *const f64)),
                _ => Number::Long(*(at as *const i64)),
            }
        });
    }
    let answerer = ANSWERER.lock().ok().and_then(|place| *place);
    let kept = errno();
    let gave = answerer.map_or(Number::Long(0), |answerer| answerer(made.which, taken));
    set_errno(kept);
    // A narrow result goes back widened to a whole word.
    match (made.result, gave) {
        ('D', Number::Double(x)) => *result = x.to_bits(),
        // SAFETY: the place for a result is at least a word.
        ('F', Number::Double(x)) => unsafe { *(result as *mut u64 as *mut f32) = x as f32 },
        ('c', Number::Long(n)) => *result = n as i8 as i64 as u64,
        ('s', Number::Long(n)) => *result = n as i16 as i64 as u64,
        ('I', Number::Long(n)) => *result = n as i32 as i64 as u64,
        ('C', Number::Long(n)) => *result = n as u8 as u64,
        ('S', Number::Long(n)) => *result = n as u16 as u64,
        ('U', Number::Long(n)) => *result = n as u32 as u64,
        ('V', _) => {}
        (_, Number::Long(n)) => *result = n as u64,
        _ => {}
    }
}

/// Make a C function of a type, answered by the action of a number, and
/// give its address. It is never let go of.
pub fn closure(shape: &str, result: char, which: usize) -> Result<i64, String> {
    let unfit = || "a C function of this type cannot be made".to_string();
    let types = shape.chars().map(|kind| type_of(kind).filter(|_| kind != 'V').ok_or_else(unfit)).collect::<Result<Vec<_>, _>>()?;
    let cif = Cif::new(types, type_of(result).ok_or_else(unfit)?);
    let made: &'static Made = Box::leak(Box::new(Made { shape: shape.to_string(), result, which }));
    let closure: &'static Closure<'static> = Box::leak(Box::new(Closure::new(cif, answer, made)));
    Ok(*closure.code_ptr() as usize as i64)
}

/// Make a function C's under a name: one the program calls by that name,
/// or takes the address of, is this one; and if a library that is loaded
/// has a place for its address, called `powder_export_` and the name, the
/// address is put there, for the library's own code to call it by.
pub fn export(name: &str, address: i64) {
    if let Ok(mut known) = EXPORTED.lock() {
        known.get_or_insert_with(HashMap::new).insert(name.to_string(), address as usize);
    }
    if let Ok(place) = CString::new(format!("powder_export_{name}")) {
        // SAFETY: a library that has the place has it for this.
        unsafe {
            let found = libc::dlsym(libc::RTLD_DEFAULT, place.as_ptr());
            if !found.is_null() {
                *(found as *mut usize) = address as usize;
            }
        }
    }
}
