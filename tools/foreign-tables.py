#!/usr/bin/env python3
"""Write the tables of C call types: src/Powder/SKI/Foreign.hs for the
machines written in Haskell, and runtime/src/foreign.rs for the one in
Rust. Run from the repository's root."""
import itertools

KINDS = "DLF"
RESULTS = "DLFV"
shapes = ["".join(c) for n in range(0, 4) for c in itertools.product(KINDS, repeat=n)]

# Haskell --------------------------------------------------------------------
ty = {"D": "Double", "L": "Int64", "F": "Float", "V": "()"}
out = ['''-- | Calling C: a function found by name in what the program is linked
-- with, called with numbers and giving a number.
--
-- There is no making a call of a type decided as the program runs, short
-- of a library for it, so every type a call can have is written out: up
-- to three arguments, each a double, a float or a 64-bit integer, and a
-- result that is one of those or nothing. A narrower integer is passed in
-- the wider one, as the calling conventions have it, and the caller
-- narrows what comes back. A float is a double to the machines, and is
-- made one on the way in and back.
--
-- This module is written by tools/foreign-tables.py. It is not to be
-- edited.
module Powder.SKI.Foreign (callForeign) where

import Control.Exception (IOException, try)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr, castFunPtr)
import System.Posix.DynamicLinker (DL (Default), dlsym)

''']


def name(shape, result):
    return "call" + (shape or "N") + "to" + result


for shape in shapes:
    for result in RESULTS:
        t = " -> ".join([ty[c] for c in shape] + ["IO " + ty[result]])
        out.append(f'foreign import ccall "dynamic" {name(shape, result)} :: FunPtr ({t}) -> {t}\n')
out.append('''
-- | Call the function of a name, given what its arguments are, each a
-- double (@D@), a float (@F@) or an integer (@L@), and whether it gives
-- one of those or nothing (@V@). Says what is wrong if it cannot.
callForeign :: String -> String -> Char -> [Either Int64 Double] -> IO (Either String (Either Int64 Double))
callForeign symbol shape result arguments =
  try (dlsym Default symbol) >>= \\case
    Left (_ :: IOException) -> pure (Left ("there is no C function called " ++ symbol))
    Right found -> case (shape, result, arguments) of
''')
for shape in shapes:
    pats = ", ".join(("Left " if c == "L" else "Right ") + f"a{i}" for i, c in enumerate(shape))
    args = "".join((f" (realToFrac a{i})" if c == "F" else f" a{i}") for i, c in enumerate(shape))
    for result in RESULTS:
        wrap = {"D": "Right . Right <$>", "L": "Right . Left <$>", "F": "Right . Right . realToFrac <$>", "V": "Right (Left 0) <$"}[result]
        out.append(f"      (\"{shape}\", '{result}', [{pats}]) -> {wrap} {name(shape, result)} (castFunPtr found){args}\n")
out.append('      _ -> pure (Left ("a C function of this type cannot be called: " ++ symbol))\n')
open("src/Powder/SKI/Foreign.hs", "w").write("".join(out))

# Rust -----------------------------------------------------------------------
rs = {"D": "f64", "L": "i64", "F": "f32"}
out = ['''//! Calling C: a function found by name in what the machine is linked
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
''']
for shape in shapes:
    pats = ", ".join(("Long" if c == "L" else "Double") + f"(a{i})" for i, c in enumerate(shape))
    args = ", ".join((f"*a{i} as f32" if c == "F" else f"*a{i}") for i, c in enumerate(shape))
    params = ", ".join(rs[c] for c in shape)
    for result in RESULTS:
        gives = "" if result == "V" else f" -> {rs[result]}"
        call = f'std::mem::transmute::<Found, unsafe extern "C" fn({params}){gives}>(found)({args})'
        body = {"D": f"Ok(Double({call}))", "L": f"Ok(Long({call}))", "F": f"Ok(Double({call} as f64))", "V": f"{{ {call}; Ok(Long(0)) }}"}[result]
        out.append(f"            (\"{shape}\", '{result}', [{pats}]) => {body},\n")
out.append('''            _ => Err(format!("a C function of this type cannot be called: {symbol}")),
        }
    }
}
''')
open("runtime/src/foreign.rs", "w").write("".join(out))
