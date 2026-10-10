//! Calling C: a function found by name in what the machine is linked
//! with, or one whose address the program has, called at the type the
//! program says.
//!
//! A type is said by a letter for each argument and one for the result:
//! `c C s S I U L Q` for the integers, signed and not, of 8, 16, 32 and 64
//! bits, `F` and `D` for float and double, `P` for a pointer, and `V` for
//! no result. The call is made by libffi.

use libffi::middle::{Arg, Cif, CodePtr, Type};
use std::cell::Cell;
use std::ffi::{c_void, CString};

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

fn find(symbol: &str) -> Result<*mut c_void, String> {
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
