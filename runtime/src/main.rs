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
use std::io::{BufRead, BufReader, BufWriter, Write};
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
    DoubleEncode, DoubleFromRatio, MatchFail, PutChar, GetChar, Exit, PathChar, FileOpen, HGetChar, FileClose, HIsEOF,
    ArgCount, ArgChar,
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
        "primDoubleExponent" => DoubleExponent, "primDoubleEncode" => DoubleEncode, "primDoubleFromRatio" => DoubleFromRatio, "primMatchFail" => MatchFail,
        "primPutChar" => PutChar, "primGetChar" => GetChar, "primExit" => Exit, "primPathChar" => PathChar,
        "primFileOpen" => FileOpen, "primHGetChar" => HGetChar, "primFileClose" => FileClose,
        "primHIsEOF" => HIsEOF, "primArgCount" => ArgCount, "primArgChar" => ArgChar,
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
    if version != 1 {
        return Err(format!("the image is of version {version}, and this machine runs version 1"));
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
    Ok(Image { blocks, definitions, entry: r.number()? })
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

enum Open {
    Reading(BufReader<File>),
    Writing(File),
}

struct Machine {
    image: Image,
    globals: Vec<Rc<RefCell<Cell>>>,
    out: BufWriter<std::io::Stdout>,
    input: BufReader<std::io::Stdin>,
    path: String,
    files: HashMap<i64, Open>,
    arguments: Vec<String>,
    name: String,
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
                        Some(Open::Writing(file)) => {
                            let _ = file.write_all(text);
                        }
                        _ => {
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
                let opened = match small(mode)? {
                    0 => File::open(&path).map(|f| Open::Reading(BufReader::new(f))),
                    1 => File::create(&path).map(Open::Writing),
                    _ => std::fs::OpenOptions::new().append(true).create(true).open(&path).map(Open::Writing),
                };
                match opened {
                    Ok(file) => {
                        let handle = 3 + self.files.len() as i64;
                        self.files.insert(handle, file);
                        Value::Int(handle)
                    }
                    Err(_) => Value::Int(-1),
                }
            }
            (HGetChar, [h]) => match self.files.get_mut(&small(h)?) {
                Some(Open::Reading(file)) => code_of(read_char(file)),
                _ => Value::Int(-1),
            },
            (FileClose, [h]) => {
                // The entry stays, so that no later file is given its number.
                if let Some(file) = self.files.get_mut(&small(h)?) {
                    if let Open::Writing(f) = file {
                        let _ = f.flush();
                    }
                    *file = Open::Reading(BufReader::new(File::open("/dev/null").map_err(|e| e.to_string())?));
                }
                UNIT
            }
            (HIsEOF, [h]) => match self.files.get_mut(&small(h)?) {
                Some(Open::Reading(file)) => truth(file.fill_buf().map_or(true, |b| b.is_empty())),
                Some(Open::Writing(_)) => truth(true),
                None => {
                    let _ = self.out.flush();
                    truth(self.input.fill_buf().map_or(true, |b| b.is_empty()))
                }
            },
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

    /// Run from a block until the program halts.
    fn run(&mut self) -> Result<(), String> {
        let malformed = |what: &str| format!("the image is malformed: {what}");
        let mut env: Vec<Value> = Vec::new();
        let mut block = self.image.entry;
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
                Instruction::Foreign { symbol, shape, result } => {
                    let arguments = env.split_off(env.len() - shape.len());
                    let given = shape
                        .chars()
                        .zip(&arguments)
                        .map(|(kind, v)| if kind == 'D' { double(v).map(foreign::Number::Double) } else { small(v).map(foreign::Number::Long) })
                        .collect::<Result<Vec<_>, _>>()?;
                    let class = if *result == 'D' || *result == 'V' { *result } else { 'L' };
                    env.push(match (result, foreign::call(symbol, shape, class, &given)?) {
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
            arguments: rest,
            name: path,
        };
        let outcome = machine.run();
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
