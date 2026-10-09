//! Calling C: a function found by name in what the machine is linked
//! with, called with numbers and giving a number.
//!
//! Every type a call can have is written out: up to three arguments, each
//! a double or a 64-bit integer, and a result that is one of those or
//! nothing. A narrower integer is passed in the wider one, as the calling
//! conventions have it, and the caller narrows what comes back.
//!
//! This file is written by a program. It is not to be edited.

use std::ffi::CString;

/// An argument or a result: an integer or a double.
#[derive(Clone, Copy)]
pub enum Number {
    Long(i64),
    Double(f64),
}

/// Call the function of a name, given what its arguments are, each a double
/// (`D`) or an integer (`L`), and whether it gives a double, an integer or
/// nothing (`V`).
pub fn call(symbol: &str, shape: &str, result: char, arguments: &[Number]) -> Result<Number, String> {
    let name = CString::new(symbol).map_err(|_| format!("there is no C function called {symbol}"))?;
    // SAFETY: looking a name up in what is already loaded.
    let found = unsafe { libc::dlsym(libc::RTLD_DEFAULT, name.as_ptr()) };
    if found.is_null() {
        return Err(format!("there is no C function called {symbol}"));
    }
    use Number::{Double, Long};
    // SAFETY: the program declared the function to have this type, which is
    // all that can be known of it.
    unsafe {
        match (shape, result, arguments) {
            ("", 'D', []) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn() -> f64>(found)())),
            ("", 'L', []) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn() -> i64>(found)())),
            ("", 'V', []) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn()>(found)(); Ok(Long(0)) },
            ("D", 'D', [Double(a0)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64) -> f64>(found)(*a0))),
            ("D", 'L', [Double(a0)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64) -> i64>(found)(*a0))),
            ("D", 'V', [Double(a0)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64)>(found)(*a0); Ok(Long(0)) },
            ("L", 'D', [Long(a0)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64) -> f64>(found)(*a0))),
            ("L", 'L', [Long(a0)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64) -> i64>(found)(*a0))),
            ("L", 'V', [Long(a0)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64)>(found)(*a0); Ok(Long(0)) },
            ("DD", 'D', [Double(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64) -> f64>(found)(*a0, *a1))),
            ("DD", 'L', [Double(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64) -> i64>(found)(*a0, *a1))),
            ("DD", 'V', [Double(a0), Double(a1)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("DL", 'D', [Double(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64) -> f64>(found)(*a0, *a1))),
            ("DL", 'L', [Double(a0), Long(a1)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64) -> i64>(found)(*a0, *a1))),
            ("DL", 'V', [Double(a0), Long(a1)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("LD", 'D', [Long(a0), Double(a1)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64) -> f64>(found)(*a0, *a1))),
            ("LD", 'L', [Long(a0), Double(a1)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64) -> i64>(found)(*a0, *a1))),
            ("LD", 'V', [Long(a0), Double(a1)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("LL", 'D', [Long(a0), Long(a1)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64) -> f64>(found)(*a0, *a1))),
            ("LL", 'L', [Long(a0), Long(a1)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64) -> i64>(found)(*a0, *a1))),
            ("LL", 'V', [Long(a0), Long(a1)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64)>(found)(*a0, *a1); Ok(Long(0)) },
            ("DDD", 'D', [Double(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("DDD", 'L', [Double(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("DDD", 'V', [Double(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("DDL", 'D', [Double(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("DDL", 'L', [Double(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("DDL", 'V', [Double(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, f64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("DLD", 'D', [Double(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("DLD", 'L', [Double(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("DLD", 'V', [Double(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("DLL", 'D', [Double(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("DLL", 'L', [Double(a0), Long(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("DLL", 'V', [Double(a0), Long(a1), Long(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(f64, i64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LDD", 'D', [Long(a0), Double(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("LDD", 'L', [Long(a0), Double(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("LDD", 'V', [Long(a0), Double(a1), Double(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LDL", 'D', [Long(a0), Double(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("LDL", 'L', [Long(a0), Double(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("LDL", 'V', [Long(a0), Double(a1), Long(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, f64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LLD", 'D', [Long(a0), Long(a1), Double(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64, f64) -> f64>(found)(*a0, *a1, *a2))),
            ("LLD", 'L', [Long(a0), Long(a1), Double(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64, f64) -> i64>(found)(*a0, *a1, *a2))),
            ("LLD", 'V', [Long(a0), Long(a1), Double(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64, f64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            ("LLL", 'D', [Long(a0), Long(a1), Long(a2)]) => Ok(Double(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64, i64) -> f64>(found)(*a0, *a1, *a2))),
            ("LLL", 'L', [Long(a0), Long(a1), Long(a2)]) => Ok(Long(std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64, i64) -> i64>(found)(*a0, *a1, *a2))),
            ("LLL", 'V', [Long(a0), Long(a1), Long(a2)]) => { std::mem::transmute::<*mut libc::c_void, unsafe extern "C" fn(i64, i64, i64)>(found)(*a0, *a1, *a2); Ok(Long(0)) },
            _ => Err(format!("a C function of this type cannot be called: {symbol}")),
        }
    }
}
