//! The machine that runs Powder's bytecode.
//!
//! The bytecode is AxCut written down (see `src/Powder/Bytecode.hs` in the
//! compiler, which is the description of the format). There is one
//! environment, a row of values, and every instruction works on the end
//! of it: nothing names where a value is but the instruction that makes
//! the row again.
//!
//! Memory is counted, not traced. A value that is shared holds a count of
//! who holds it, and the environment is linear: an instruction uses up
//! what it takes, so the only places a count goes up or a value is let go
//! are the instruction that makes the row again and the ones that copy
//! out of an object that is still shared. Values that refer to themselves
//! are not yet collected; a collector for those is to come.

mod foreign;

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{FromPrimitive, One, ToPrimitive, Zero};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, IsTerminal as _, Seek, SeekFrom, Write};
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
    Data(Rc<Data>),
    Object(Rc<RefCell<Cell>>),
}

struct Data {
    tag: u32,
    fields: Vec<Value>,
}

/// What an object is. A thunk is `Delayed`, then `Working`, then `Done`.
enum Cell {
    Made { captured: Vec<Value>, method: u8, parameters: usize, block: usize },
    Delayed { captured: Vec<Value>, block: usize },
    Working,
    Done(Value),
    /// The consumer a thunk's body is given: it keeps the value in the
    /// thunk and passes it on to the consumer of the force.
    Update { thunk: Rc<RefCell<Cell>>, consumer: Value },
}

const RET: u8 = 0;

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
        other => return Err(format!("the image uses a primitive this machine has not got: {other}")),
    })
}

struct Made {
    thunk: bool,
    method: u8,
    parameters: usize,
    block: usize,
}

enum Instruction {
    Substitute(Vec<usize>),
    Let { tag: u32, arity: usize },
    Switch(Vec<(usize, usize)>),
    New { made: Made, captured: usize },
    Invoke(u8),
    Force,
    Lit(Value),
    Global(usize),
    Prim(Prim, usize),
    Foreign { symbol: String, shape: String, result: char },
    /// For each object: what it is, how many values it takes off the
    /// environment, and which of the objects it captures after those.
    Rec(Vec<(Made, usize, Vec<usize>)>),
    Halt,
}

struct Image {
    blocks: Vec<Vec<Instruction>>,
    definitions: Vec<usize>,
    entry: usize,
    /// The block that answers a call from C, which starts with the action
    /// that answers it in the environment.
    callback: usize,
    /// What C may call: its name, the letters of its arguments and of its
    /// result, and the definition that is the action answering it.
    exports: Vec<(String, String, char, usize)>,
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
    fn number(&mut self) -> Result<usize, String> {
        let (mut n, mut shift) = (0usize, 0);
        loop {
            let b = self.byte()?;
            n |= ((b & 127) as usize) << shift;
            if b & 128 == 0 {
                return Ok(n);
            }
            shift += 7;
        }
    }
    fn signed(&mut self) -> Result<i64, String> {
        let (mut n, mut shift) = (0u64, 0);
        loop {
            let b = self.byte()?;
            n |= ((b & 127) as u64) << shift;
            if b & 128 == 0 {
                break;
            }
            shift += 7;
        }
        Ok(if n & 1 == 1 { -(((n + 1) >> 1) as i64) } else { (n >> 1) as i64 })
    }
    fn made(&mut self) -> Result<(bool, u8), String> {
        Ok((self.byte()? == 1, self.byte()?))
    }
}

fn load(bytes: &[u8]) -> Result<Image, String> {
    let mut r = Reader { bytes, at: 0 };
    if bytes.len() < 4 || &bytes[..4] != b"PWDR" {
        return Err("this is not an image of a Powder program".into());
    }
    r.at = 4;
    let version = r.number()?;
    if version != 2 {
        return Err(format!("the image is of version {version}, and this machine runs version 2"));
    }
    let mut strings = Vec::new();
    for _ in 0..r.number()? {
        let n = r.number()?;
        let text = bytes.get(r.at..r.at + n).ok_or("the image ends too soon")?;
        strings.push(String::from_utf8(text.to_vec()).map_err(|_| "a string in the image is not text")?);
        r.at += n;
    }
    let string = |i: usize| strings.get(i).cloned().ok_or_else(|| "the image names a string it has not got".to_string());
    let mut blocks = Vec::new();
    for _ in 0..r.number()? {
        let mut block = Vec::new();
        for _ in 0..r.number()? {
            block.push(match r.byte()? {
                0 => {
                    let n = r.number()?;
                    Instruction::Substitute((0..n).map(|_| r.number()).collect::<Result<_, _>>()?)
                }
                1 => Instruction::Let { tag: r.number()? as u32, arity: r.number()? },
                2 => {
                    let n = r.number()?;
                    Instruction::Switch((0..n).map(|_| Ok((r.number()?, r.number()?))).collect::<Result<_, String>>()?)
                }
                3 => {
                    let (thunk, method) = r.made()?;
                    let captured = r.number()?;
                    Instruction::New { made: Made { thunk, method, parameters: r.number()?, block: r.number()? }, captured }
                }
                4 => Instruction::Invoke(r.byte()?),
                5 => Instruction::Force,
                6 => Instruction::Lit(Value::Int(r.signed()?)),
                7 => {
                    let text = string(r.number()?)?;
                    Instruction::Lit(integer(text.parse::<BigInt>().map_err(|_| "a number in the image is not one")?))
                }
                8 => Instruction::Global(r.number()?),
                9 => Instruction::Prim(prim_named(&string(r.number()?)?)?, r.number()?),
                10 => Instruction::Foreign { symbol: string(r.number()?)?, shape: string(r.number()?)?, result: r.byte()? as char },
                11 => {
                    let n = r.number()?;
                    let mut objects = Vec::new();
                    for _ in 0..n {
                        let (thunk, method) = r.made()?;
                        let mine = r.number()?;
                        let others = r.number()?;
                        let theirs = (0..others).map(|_| r.number()).collect::<Result<Vec<_>, _>>()?;
                        objects.push((Made { thunk, method, parameters: r.number()?, block: r.number()? }, mine, theirs));
                    }
                    Instruction::Rec(objects)
                }
                12 => Instruction::Halt,
                other => return Err(format!("the image has an instruction this machine does not know: {other}")),
            });
        }
        blocks.push(block);
    }
    let n = r.number()?;
    let definitions = (0..n).map(|_| r.number()).collect::<Result<_, _>>()?;
    let entry = r.number()?;
    let callback = r.number()?;
    let mut exports = Vec::new();
    for _ in 0..r.number()? {
        exports.push((string(r.number()?)?, string(r.number()?)?, r.byte()? as char, r.number()?));
    }
    Ok(Image { blocks, definitions, entry, callback, exports })
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
    globals: Vec<Rc<RefCell<Cell>>>,
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
    frames: Vec<(Vec<foreign::Number>, foreign::Number)>,
}

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
                match self.frames.last().and_then(|(taken, _)| taken.get(i)) {
                    Some(foreign::Number::Long(n)) => Value::Int(*n),
                    _ => return Err("there is no such integer argument of a call from C".into()),
                }
            }
            (CallbackDouble, [i]) => {
                let i = small(i)? as usize;
                match self.frames.last().and_then(|(taken, _)| taken.get(i)) {
                    Some(foreign::Number::Double(x)) => Value::Double(*x),
                    _ => return Err("there is no such double argument of a call from C".into()),
                }
            }
            (CallbackReturnInt, [value]) => {
                let value = foreign::Number::Long(small(value)?);
                self.frames.last_mut().ok_or("there is no call from C to answer")?.1 = value;
                UNIT
            }
            (CallbackReturnDouble, [value]) => {
                let value = foreign::Number::Double(double(value)?);
                self.frames.last_mut().ok_or("there is no call from C to answer")?.1 = value;
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

    fn object(made: &Made, captured: Vec<Value>) -> Cell {
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
    fn answer(&mut self, which: usize, taken: Vec<foreign::Number>) -> foreign::Number {
        let Some(action) = self.answers.get(which).cloned() else { return foreign::Number::Long(0) };
        self.frames.push((taken, foreign::Number::Long(0)));
        let block = self.image.callback;
        if let Err(message) = self.run(block, vec![action]) {
            eprintln!("runtime error: {message}");
            self.finish(1)
        }
        self.frames.pop().map_or(foreign::Number::Long(0), |(_, gave)| gave)
    }

    /// Run from a block, with an environment, until it halts.
    fn run(&mut self, start: usize, mut env: Vec<Value>) -> Result<(), String> {
        let malformed = |what: &str| format!("the image is malformed: {what}");
        let mut block = start;
        let mut at = 0;
        loop {
            // The image is not changed while it runs, so an instruction can
            // be looked at while the rest of the machine is changed.
            let instruction: *const Instruction = &self.image.blocks[block][at];
            at += 1;
            // SAFETY: `self.image` outlives the loop and nothing writes to it.
            match unsafe { &*instruction } {
                Instruction::Halt => return Ok(()),
                Instruction::Substitute(sources) => {
                    env = sources.iter().map(|&i| env[i].clone()).collect();
                }
                Instruction::Let { tag, arity } => {
                    let fields = env.split_off(env.len() - arity);
                    env.push(if fields.is_empty() { Value::Tag(*tag) } else { Value::Data(Rc::new(Data { tag: *tag, fields })) });
                }
                Instruction::Switch(clauses) => {
                    let (tag, fields) = match env.pop() {
                        Some(Value::Tag(tag)) => (tag, Vec::new()),
                        // If nothing else holds it, its fields are moved out.
                        Some(Value::Data(data)) => match Rc::try_unwrap(data) {
                            Ok(data) => (data.tag, data.fields),
                            Err(shared) => (shared.tag, shared.fields.clone()),
                        },
                        _ => return Err(malformed("a switch on what is not a constructed value")),
                    };
                    let &(count, target) = clauses.get(tag as usize).ok_or_else(|| malformed("a switch has no clause for a constructor"))?;
                    if count != fields.len() {
                        return Err(malformed("a clause names the wrong number of fields"));
                    }
                    env.extend(fields);
                    block = target;
                    at = 0;
                }
                Instruction::New { made, captured } => {
                    let held = env.split_off(env.len() - captured);
                    env.push(Value::Object(Rc::new(RefCell::new(Self::object(made, held)))));
                }
                Instruction::Rec(objects) => {
                    let cells: Vec<_> = objects.iter().map(|_| Rc::new(RefCell::new(Cell::Working))).collect();
                    let total: usize = objects.iter().map(|(_, mine, _)| mine).sum();
                    let mut values = env.split_off(env.len() - total).into_iter();
                    for ((made, mine, theirs), cell) in objects.iter().zip(&cells) {
                        let mut held: Vec<Value> = values.by_ref().take(*mine).collect();
                        held.extend(theirs.iter().map(|&i| Value::Object(cells[i].clone())));
                        *cell.borrow_mut() = Self::object(made, held);
                    }
                    env.extend(cells.into_iter().map(Value::Object));
                }
                Instruction::Lit(v) => env.push(v.clone()),
                Instruction::Global(i) => env.push(Value::Object(self.globals[*i].clone())),
                Instruction::Prim(p, n) => {
                    let arguments = env.split_off(env.len() - n);
                    let result = self.primitive(*p, &arguments)?;
                    env.push(result);
                }
                // A C function made of an action of the program's.
                Instruction::Foreign { symbol, shape, result } if symbol == "wrapper" => {
                    let action = env.pop().ok_or_else(|| malformed("a C function made of nothing"))?;
                    self.answers.push(action);
                    env.push(Value::Int(foreign::closure(shape, *result, self.answers.len() - 1)?));
                }
                Instruction::Foreign { symbol, shape, result } => {
                    let arguments = env.split_off(env.len() - shape.len());
                    let given = shape
                        .chars()
                        .zip(&arguments)
                        .map(|(kind, v)| if kind == 'D' || kind == 'F' { double(v).map(foreign::Number::Double) } else { small(v).map(foreign::Number::Long) })
                        .collect::<Result<Vec<_>, _>>()?;
                    env.push(match (result, foreign::call(symbol, shape, *result, &given)?) {
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
                    });
                }
                Instruction::Force | Instruction::Invoke(_) => {
                    // What is called, how, and with what.
                    let (mut target, mut method, mut arguments) = match unsafe { &*instruction } {
                        Instruction::Force => {
                            let (Some(x), Some(k), true) = (env.pop(), env.pop(), env.is_empty()) else {
                                return Err(malformed("a force with more in the environment than a value and its consumer"));
                            };
                            match self.force(x, k)? {
                                Entered::Body(captured, to) => {
                                    env = captured;
                                    block = to;
                                    at = 0;
                                    continue;
                                }
                                Entered::Call(k, v) => (k, RET, vec![v]),
                            }
                        }
                        Instruction::Invoke(m) => {
                            let object = env.pop().ok_or_else(|| malformed("an invoke with nothing in the environment"))?;
                            (object, *m, std::mem::take(&mut env))
                        }
                        _ => unreachable!(),
                    };
                    // A thunk's value coming back is kept and passed on,
                    // which may be to another thunk's consumer.
                    loop {
                        let Value::Object(cell) = &target else {
                            return Err(malformed("a call of what is not an object"));
                        };
                        let next = match &*cell.borrow() {
                            Cell::Made { captured, method: has, parameters, block: to } => {
                                if *has != method || *parameters != arguments.len() {
                                    return Err(malformed("an object was called by a method it has not got"));
                                }
                                arguments.extend(captured.iter().cloned());
                                block = *to;
                                at = 0;
                                None
                            }
                            Cell::Update { thunk, consumer } if method == RET && arguments.len() == 1 => {
                                *thunk.borrow_mut() = Cell::Done(arguments[0].clone());
                                Some(consumer.clone())
                            }
                            _ => return Err(malformed("a call of what is not an object with that method")),
                        };
                        match next {
                            None => break,
                            Some(consumer) => {
                                target = consumer;
                                method = RET;
                            }
                        }
                    }
                    env = std::mem::take(&mut arguments);
                }
            }
        }
    }

    /// Work a value out if it is a thunk not yet worked out, which is to
    /// run its body; otherwise say what its consumer is to be given.
    fn force(&mut self, mut x: Value, k: Value) -> Result<Entered, String> {
        loop {
            let Value::Object(cell) = &x else {
                return Ok(Entered::Call(k, x));
            };
            let state = std::mem::replace(&mut *cell.borrow_mut(), Cell::Working);
            match state {
                Cell::Delayed { captured, block } => {
                    let update = Value::Object(Rc::new(RefCell::new(Cell::Update { thunk: cell.clone(), consumer: k })));
                    let mut env = vec![update];
                    env.extend(captured);
                    return Ok(Entered::Body(env, block));
                }
                Cell::Done(v) => {
                    *cell.borrow_mut() = Cell::Done(v.clone());
                    x = v;
                }
                Cell::Working => return Err("a value depends on itself".into()),
                other => {
                    *cell.borrow_mut() = other;
                    return Ok(Entered::Call(k, x));
                }
            }
        }
    }
}

enum Entered {
    /// A thunk's body, with the environment it runs in.
    Body(Vec<Value>, usize),
    /// A consumer, and the value it is to be given.
    Call(Value, Value),
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
    // Letting go of a long list goes as deep as the list is long, so the
    // machine is given room for that until it frees memory by a loop.
    let machine = std::thread::Builder::new().stack_size(1 << 30).spawn(move || {
        let image = load(&bytes).unwrap_or_else(|e| {
            eprintln!("powder-vm: {path}: {e}");
            std::process::exit(2);
        });
        let globals = image.definitions.iter().map(|&block| Rc::new(RefCell::new(Cell::Delayed { captured: Vec::new(), block }))).collect();
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
            frames: Vec::new(),
        };
        // SAFETY: the machine is in one place for as long as the thread
        // runs, and C calls back only from inside a call the machine made.
        unsafe { MACHINE = &mut machine };
        foreign::answer_with(answer);
        let outcome = foreign::load_libraries().and_then(|()| machine.export()).and_then(|()| {
            let entry = machine.image.entry;
            machine.run(entry, Vec::new())
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
    });
    let _ = machine.expect("the machine could not be started").join();
}
