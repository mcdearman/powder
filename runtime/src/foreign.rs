//! Calling C: a function found by name in what the machine is linked
//! with, called with numbers and giving a number.
//!
//! Every type a call can have is written out: up to three arguments, each
//! a double, a float or a 64-bit integer, and a result that is one of
//! those or nothing. A narrower integer is passed in the wider one, as the
//! calling conventions have it, and the caller narrows what comes back. A
//! float is a double to the machine, and is made one on the way in and
//! back.
//!
//! This file is written by tools/foreign-tables.py. It is not to be
//! edited.

use std::ffi::CString;

/// An argument or a result: an integer or a double.
#[derive(Clone, Copy)]
pub enum Number {
    Long(i64),
    Double(f64),
}

/// Call the function of a name, given what its arguments are, each a double
/// (`D`), a float (`F`) or an integer (`L`), and whether it gives one of
/// those or nothing (`V`).
#[rustfmt::skip]
pub fn call(symbol: &str, shape: &str, result: char, arguments: &[Number]) -> Result<Number, String> {
    let name = CString::new(symbol).map_err(|_| format!("there is no C function called {symbol}"))?;
    // SAFETY: looking a name up in what is already loaded.
    let found = unsafe { libc::dlsym(libc::RTLD_DEFAULT, name.as_ptr()) };
    if found.is_null() {
        return Err(format!("there is no C function called {symbol}"));
    }
    use Number::{Double, Long};
    type Found = *mut libc::c_void;
    // SAFETY: the program declared the function to have this type, which is
    // all that can be known of it.
    unsafe {
        match (shape, result, arguments) {
            ("", 'D', []) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn() -> f64>(found)())),
            ("", 'L', []) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn() -> i64>(found)())),
            ("", 'F', []) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn() -> f32>(found)() as f64)),
            ("", 'V', []) => { std::mem::transmute::<Found, unsafe extern "C" fn()>(found)(); Ok(Long(0)) },
            ("D", 'D', [Double(a0)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64) -> f64>(found)(*a0))),
            ("D", 'L', [Double(a0)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64) -> i64>(found)(*a0))),
            ("D", 'F', [Double(a0)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64) -> f32>(found)(*a0) as f64)),
            ("D", 'V', [Double(a0)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64)>(found)(*a0); Ok(Long(0)) },
            ("L", 'D', [Long(a0)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64) -> f64>(found)(*a0))),
            ("L", 'L', [Long(a0)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64) -> i64>(found)(*a0))),
            ("L", 'F', [Long(a0)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64) -> f32>(found)(*a0) as f64)),
            ("L", 'V', [Long(a0)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64)>(found)(*a0); Ok(Long(0)) },
            ("F", 'D', [Double(a0)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32) -> f64>(found)(*a0 as f32))),
            ("F", 'L', [Double(a0)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32) -> i64>(found)(*a0 as f32))),
            ("F", 'F', [Double(a0)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32) -> f32>(found)(*a0 as f32) as f64)),
            ("F", 'V', [Double(a0)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32)>(found)(*a0 as f32); Ok(Long(0)) },
            ("DD", 'D', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64) -> f64>(found)(*a0, *a1))),
            ("DD", 'L', [Double(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64) -> i64>(found)(*a0, *a1))),
            ("DD", 'F', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64) -> f32>(found)(*a0, *a1) as f64)),
            ("DD", 'V', [Double(a0), Double(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("DL", 'D', [Double(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64) -> f64>(found)(*a0, *a1))),
            ("DL", 'L', [Double(a0), Long(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64) -> i64>(found)(*a0, *a1))),
            ("DL", 'F', [Double(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64) -> f32>(found)(*a0, *a1) as f64)),
            ("DL", 'V', [Double(a0), Long(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("DF", 'D', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32) -> f64>(found)(*a0, *a1 as f32))),
            ("DF", 'L', [Double(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32) -> i64>(found)(*a0, *a1 as f32))),
            ("DF", 'F', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32) -> f32>(found)(*a0, *a1 as f32) as f64)),
            ("DF", 'V', [Double(a0), Double(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32)>(found)(*a0, *a1 as f32); Ok(Long(0)) },
            ("LD", 'D', [Long(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64) -> f64>(found)(*a0, *a1))),
            ("LD", 'L', [Long(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64) -> i64>(found)(*a0, *a1))),
            ("LD", 'F', [Long(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64) -> f32>(found)(*a0, *a1) as f64)),
            ("LD", 'V', [Long(a0), Double(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("LL", 'D', [Long(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64) -> f64>(found)(*a0, *a1))),
            ("LL", 'L', [Long(a0), Long(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64) -> i64>(found)(*a0, *a1))),
            ("LL", 'F', [Long(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64) -> f32>(found)(*a0, *a1) as f64)),
            ("LL", 'V', [Long(a0), Long(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("LF", 'D', [Long(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32) -> f64>(found)(*a0, *a1 as f32))),
            ("LF", 'L', [Long(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32) -> i64>(found)(*a0, *a1 as f32))),
            ("LF", 'F', [Long(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32) -> f32>(found)(*a0, *a1 as f32) as f64)),
            ("LF", 'V', [Long(a0), Double(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32)>(found)(*a0, *a1 as f32); Ok(Long(0)) },
            ("FD", 'D', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64) -> f64>(found)(*a0 as f32, *a1))),
            ("FD", 'L', [Double(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64) -> i64>(found)(*a0 as f32, *a1))),
            ("FD", 'F', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64) -> f32>(found)(*a0 as f32, *a1) as f64)),
            ("FD", 'V', [Double(a0), Double(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64)>(found)(*a0 as f32, *a1); Ok(Long(0)) },
            ("FL", 'D', [Double(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64) -> f64>(found)(*a0 as f32, *a1))),
            ("FL", 'L', [Double(a0), Long(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64) -> i64>(found)(*a0 as f32, *a1))),
            ("FL", 'F', [Double(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64) -> f32>(found)(*a0 as f32, *a1) as f64)),
            ("FL", 'V', [Double(a0), Long(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64)>(found)(*a0 as f32, *a1); Ok(Long(0)) },
            ("FF", 'D', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32) -> f64>(found)(*a0 as f32, *a1 as f32))),
            ("FF", 'L', [Double(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32) -> i64>(found)(*a0 as f32, *a1 as f32))),
            ("FF", 'F', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32) -> f32>(found)(*a0 as f32, *a1 as f32) as f64)),
            ("FF", 'V', [Double(a0), Double(a1)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32)>(found)(*a0 as f32, *a1 as f32); Ok(Long(0)) },
            ("DDD", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("DDD", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("DDD", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("DDD", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("DDL", 'D', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("DDL", 'L', [Double(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("DDL", 'F', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, i64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("DDL", 'V', [Double(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("DDF", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f32) -> f64>(found)(*a0, *a1, *a2 as f32))),
            ("DDF", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f32) -> i64>(found)(*a0, *a1, *a2 as f32))),
            ("DDF", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f32) -> f32>(found)(*a0, *a1, *a2 as f32) as f64)),
            ("DDF", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f64, f32)>(found)(*a0, *a1, *a2 as f32); Ok(Long(0)) },
            ("DLD", 'D', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("DLD", 'L', [Double(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("DLD", 'F', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("DLD", 'V', [Double(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("DLL", 'D', [Double(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("DLL", 'L', [Double(a0), Long(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("DLL", 'F', [Double(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, i64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("DLL", 'V', [Double(a0), Long(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("DLF", 'D', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f32) -> f64>(found)(*a0, *a1, *a2 as f32))),
            ("DLF", 'L', [Double(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f32) -> i64>(found)(*a0, *a1, *a2 as f32))),
            ("DLF", 'F', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f32) -> f32>(found)(*a0, *a1, *a2 as f32) as f64)),
            ("DLF", 'V', [Double(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, i64, f32)>(found)(*a0, *a1, *a2 as f32); Ok(Long(0)) },
            ("DFD", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f64) -> f64>(found)(*a0, *a1 as f32, *a2))),
            ("DFD", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f64) -> i64>(found)(*a0, *a1 as f32, *a2))),
            ("DFD", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f64) -> f32>(found)(*a0, *a1 as f32, *a2) as f64)),
            ("DFD", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f64)>(found)(*a0, *a1 as f32, *a2); Ok(Long(0)) },
            ("DFL", 'D', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, i64) -> f64>(found)(*a0, *a1 as f32, *a2))),
            ("DFL", 'L', [Double(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, i64) -> i64>(found)(*a0, *a1 as f32, *a2))),
            ("DFL", 'F', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, i64) -> f32>(found)(*a0, *a1 as f32, *a2) as f64)),
            ("DFL", 'V', [Double(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, i64)>(found)(*a0, *a1 as f32, *a2); Ok(Long(0)) },
            ("DFF", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f32) -> f64>(found)(*a0, *a1 as f32, *a2 as f32))),
            ("DFF", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f32) -> i64>(found)(*a0, *a1 as f32, *a2 as f32))),
            ("DFF", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f32) -> f32>(found)(*a0, *a1 as f32, *a2 as f32) as f64)),
            ("DFF", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f64, f32, f32)>(found)(*a0, *a1 as f32, *a2 as f32); Ok(Long(0)) },
            ("LDD", 'D', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("LDD", 'L', [Long(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("LDD", 'F', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("LDD", 'V', [Long(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LDL", 'D', [Long(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("LDL", 'L', [Long(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("LDL", 'F', [Long(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, i64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("LDL", 'V', [Long(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LDF", 'D', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f32) -> f64>(found)(*a0, *a1, *a2 as f32))),
            ("LDF", 'L', [Long(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f32) -> i64>(found)(*a0, *a1, *a2 as f32))),
            ("LDF", 'F', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f32) -> f32>(found)(*a0, *a1, *a2 as f32) as f64)),
            ("LDF", 'V', [Long(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f64, f32)>(found)(*a0, *a1, *a2 as f32); Ok(Long(0)) },
            ("LLD", 'D', [Long(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("LLD", 'L', [Long(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("LLD", 'F', [Long(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("LLD", 'V', [Long(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LLL", 'D', [Long(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("LLL", 'L', [Long(a0), Long(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("LLL", 'F', [Long(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, i64) -> f32>(found)(*a0, *a1, *a2) as f64)),
            ("LLL", 'V', [Long(a0), Long(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LLF", 'D', [Long(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f32) -> f64>(found)(*a0, *a1, *a2 as f32))),
            ("LLF", 'L', [Long(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f32) -> i64>(found)(*a0, *a1, *a2 as f32))),
            ("LLF", 'F', [Long(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f32) -> f32>(found)(*a0, *a1, *a2 as f32) as f64)),
            ("LLF", 'V', [Long(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, i64, f32)>(found)(*a0, *a1, *a2 as f32); Ok(Long(0)) },
            ("LFD", 'D', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f64) -> f64>(found)(*a0, *a1 as f32, *a2))),
            ("LFD", 'L', [Long(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f64) -> i64>(found)(*a0, *a1 as f32, *a2))),
            ("LFD", 'F', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f64) -> f32>(found)(*a0, *a1 as f32, *a2) as f64)),
            ("LFD", 'V', [Long(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f64)>(found)(*a0, *a1 as f32, *a2); Ok(Long(0)) },
            ("LFL", 'D', [Long(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, i64) -> f64>(found)(*a0, *a1 as f32, *a2))),
            ("LFL", 'L', [Long(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, i64) -> i64>(found)(*a0, *a1 as f32, *a2))),
            ("LFL", 'F', [Long(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, i64) -> f32>(found)(*a0, *a1 as f32, *a2) as f64)),
            ("LFL", 'V', [Long(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, i64)>(found)(*a0, *a1 as f32, *a2); Ok(Long(0)) },
            ("LFF", 'D', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f32) -> f64>(found)(*a0, *a1 as f32, *a2 as f32))),
            ("LFF", 'L', [Long(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f32) -> i64>(found)(*a0, *a1 as f32, *a2 as f32))),
            ("LFF", 'F', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f32) -> f32>(found)(*a0, *a1 as f32, *a2 as f32) as f64)),
            ("LFF", 'V', [Long(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(i64, f32, f32)>(found)(*a0, *a1 as f32, *a2 as f32); Ok(Long(0)) },
            ("FDD", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f64) -> f64>(found)(*a0 as f32, *a1, *a2))),
            ("FDD", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f64) -> i64>(found)(*a0 as f32, *a1, *a2))),
            ("FDD", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f64) -> f32>(found)(*a0 as f32, *a1, *a2) as f64)),
            ("FDD", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f64)>(found)(*a0 as f32, *a1, *a2); Ok(Long(0)) },
            ("FDL", 'D', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, i64) -> f64>(found)(*a0 as f32, *a1, *a2))),
            ("FDL", 'L', [Double(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, i64) -> i64>(found)(*a0 as f32, *a1, *a2))),
            ("FDL", 'F', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, i64) -> f32>(found)(*a0 as f32, *a1, *a2) as f64)),
            ("FDL", 'V', [Double(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, i64)>(found)(*a0 as f32, *a1, *a2); Ok(Long(0)) },
            ("FDF", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f32) -> f64>(found)(*a0 as f32, *a1, *a2 as f32))),
            ("FDF", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f32) -> i64>(found)(*a0 as f32, *a1, *a2 as f32))),
            ("FDF", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f32) -> f32>(found)(*a0 as f32, *a1, *a2 as f32) as f64)),
            ("FDF", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f64, f32)>(found)(*a0 as f32, *a1, *a2 as f32); Ok(Long(0)) },
            ("FLD", 'D', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f64) -> f64>(found)(*a0 as f32, *a1, *a2))),
            ("FLD", 'L', [Double(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f64) -> i64>(found)(*a0 as f32, *a1, *a2))),
            ("FLD", 'F', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f64) -> f32>(found)(*a0 as f32, *a1, *a2) as f64)),
            ("FLD", 'V', [Double(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f64)>(found)(*a0 as f32, *a1, *a2); Ok(Long(0)) },
            ("FLL", 'D', [Double(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, i64) -> f64>(found)(*a0 as f32, *a1, *a2))),
            ("FLL", 'L', [Double(a0), Long(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, i64) -> i64>(found)(*a0 as f32, *a1, *a2))),
            ("FLL", 'F', [Double(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, i64) -> f32>(found)(*a0 as f32, *a1, *a2) as f64)),
            ("FLL", 'V', [Double(a0), Long(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, i64)>(found)(*a0 as f32, *a1, *a2); Ok(Long(0)) },
            ("FLF", 'D', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f32) -> f64>(found)(*a0 as f32, *a1, *a2 as f32))),
            ("FLF", 'L', [Double(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f32) -> i64>(found)(*a0 as f32, *a1, *a2 as f32))),
            ("FLF", 'F', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f32) -> f32>(found)(*a0 as f32, *a1, *a2 as f32) as f64)),
            ("FLF", 'V', [Double(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, i64, f32)>(found)(*a0 as f32, *a1, *a2 as f32); Ok(Long(0)) },
            ("FFD", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f64) -> f64>(found)(*a0 as f32, *a1 as f32, *a2))),
            ("FFD", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f64) -> i64>(found)(*a0 as f32, *a1 as f32, *a2))),
            ("FFD", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f64) -> f32>(found)(*a0 as f32, *a1 as f32, *a2) as f64)),
            ("FFD", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f64)>(found)(*a0 as f32, *a1 as f32, *a2); Ok(Long(0)) },
            ("FFL", 'D', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, i64) -> f64>(found)(*a0 as f32, *a1 as f32, *a2))),
            ("FFL", 'L', [Double(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, i64) -> i64>(found)(*a0 as f32, *a1 as f32, *a2))),
            ("FFL", 'F', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, i64) -> f32>(found)(*a0 as f32, *a1 as f32, *a2) as f64)),
            ("FFL", 'V', [Double(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, i64)>(found)(*a0 as f32, *a1 as f32, *a2); Ok(Long(0)) },
            ("FFF", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f32) -> f64>(found)(*a0 as f32, *a1 as f32, *a2 as f32))),
            ("FFF", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f32) -> i64>(found)(*a0 as f32, *a1 as f32, *a2 as f32))),
            ("FFF", 'F', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f32) -> f32>(found)(*a0 as f32, *a1 as f32, *a2 as f32) as f64)),
            ("FFF", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<Found, unsafe extern "C" fn(f32, f32, f32)>(found)(*a0 as f32, *a1 as f32, *a2 as f32); Ok(Long(0)) },
            _ => Err(format!("a C function of this type cannot be called: {symbol}")),
        }
    }
}
