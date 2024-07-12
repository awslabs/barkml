use base64::Engine;
use logos::{Lexer, Logos};
use std::fmt;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub(crate) enum Integer {
    Generic(i64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
#[logos(skip r"[ \t\f]+")] // Ignore this regex pattern between tokens
pub enum Token {
    Error,

    #[token("\n")]
    #[token("\n\r")]
    NewLine,

    #[token("true")]
    #[token("True")]
    #[token("yes")]
    #[token("Yes")]
    #[token("On")]
    #[token("on")]
    True,
    #[token("false")]
    #[token("False")]
    #[token("no")]
    #[token("No")]
    #[token("Off")]
    #[token("off")]
    False,

    #[token("!")]
    Exclaim,
    #[token("$")]
    Dollar,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("=")]
    Assign,
    #[token(":")]
    Colon,
    #[token("?")]
    Question,
    #[token(",")]
    Comma,

    // Keywords
    #[token("bool")]
    KeyBool,
    #[token("string")]
    KeyString,
    #[token("int")]
    KeyInt,
    #[token("i8")]
    KeyInt8,
    #[token("u8")]
    KeyUInt8,
    #[token("i16")]
    KeyInt16,
    #[token("u16")]
    KeyUInt16,
    #[token("i32")]
    KeyInt32,
    #[token("u32")]
    KeyUInt32,
    #[token("i64")]
    KeyInt64,
    #[token("u64")]
    KeyUInt64,
    #[token("float")]
    KeyFloat,
    #[token("f64")]
    KeyFloat64,
    #[token("f32")]
    KeyFloat32,
    #[token("bytes")]
    KeyBytes,
    #[token("version")]
    KeyVersion,
    #[token("require")]
    KeyRequire,
    #[token("label")]
    KeyLabel,
    #[token("array")]
    KeyArray,
    #[token("table")]
    KeyTable,
    #[token("section")]
    KeySection,
    #[token("block")]
    KeyBlock,

    // Unused but reserved
    #[token("module")]
    KeyModule,
    #[token("use")]
    KeyUse,
    #[token("as")]
    KeyAs,
    #[token("schema")]
    KeySchema,

    // Integer
    #[regex(r"[+-]?[0-9][0-9_]*(i64|i32|i16|i8|u64|u32|u16|u8)?", decimal::integer)]
    #[regex(
        r"[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*(i64|i32|i16|i8|u64|u32|u16|u8)?",
        hexadecimal::integer
    )]
    #[regex(r"[+-]?0o[0-7][0-7_]*(i64|i32|i16|i8|u64|u32|u16|u8)?", octal::integer)]
    #[regex(
        r"[+-]?0b[0-1][0-1_]*(i64|i32|i16|i8|u64|u32|u16|u8)?",
        binary::integer
    )]
    Int(Integer),

    // Floating-point
    #[regex(
        r"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?(f64|f32)?",
        float
    )]
    Float(HashableFloat),

    #[regex(r"m'[^'\r\n]*'", macro_string)]
    MacroString(String),
    #[regex(r"b'[-A-Za-z0-9+/]*={0,3}'", byte_string)]
    ByteString(Vec<u8>),
    #[regex(r"'[^'\n\r]*'", quote_string)]
    #[regex(r#""[^"\n\r]*""#, quote_string)]
    String(String),

    #[regex(r"[a-zA-Z][a-zA-Z0-9_\-]*", |x| x.slice().to_string())]
    Identifier(String),
    #[regex(r"m\![a-zA-Z][a-zA-Z0-9_\-]*", |x| x.slice().trim_start_matches("m!").to_string())]
    MacroIdentifier(String),
    #[regex(r"\![a-zA-Z][a-zA-Z0-9_\-]*", |x| x.slice().trim_start_matches('!').to_string())]
    LabelIdentifier(String),
    #[regex(r"\$[a-zA-Z][a-zA-Z0-9_\-]*", |x| x.slice().trim_start_matches('$').to_string())]
    ControlIdentifier(String),

    #[regex(r"[0-9]+\.[0-9]+\.[0-9]+(\-[a-z0-9]+(\+[a-z0-9]+)?)?", version_literal)]
    Version(semver::Version),

    #[regex(
        r"(=|~|\^|>|<|<=|>=)[0-9]+(\.[0-9]+(\.[0-9]+(\-[a-z0-9]+(\+[a-z0-9]+)?)?)?)?",
        version_require
    )]
    Require(semver::VersionReq),

    #[regex(r"(#[ \t\f]*[^\n\r]+[\n\r])*", line_comment)]
    LineComment(String),
    #[regex(r"\/\*[^\/\*]*\*\/", multiline_comment)]
    MultiLineComment(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum HashableFloat {
    Generic(f64),
    Float32(f32),
    Float64(f64),
}

impl Eq for HashableFloat {}

impl Hash for HashableFloat {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Float32(float) => state.write(float.to_be_bytes().as_slice()),
            Self::Float64(float) | Self::Generic(float) => {
                state.write(float.to_be_bytes().as_slice())
            }
        }
    }
}

impl From<HashableFloat> for f32 {
    fn from(val: HashableFloat) -> Self {
        match val {
            HashableFloat::Float32(value) => value,
            HashableFloat::Float64(value) | HashableFloat::Generic(value) => value as f32,
        }
    }
}

impl From<HashableFloat> for f64 {
    fn from(val: HashableFloat) -> Self {
        match val {
            HashableFloat::Float32(value) => value as f64,
            HashableFloat::Float64(value) | HashableFloat::Generic(value) => value,
        }
    }
}

fn multiline_comment(lexer: &mut Lexer<Token>) -> Option<String> {
    let slice = lexer.slice();
    let comment = slice.trim_start_matches("/*").trim_end_matches("*/").trim();
    Some(comment.to_string())
}

fn line_comment(lexer: &mut Lexer<Token>) -> Option<String> {
    let slice = lexer.slice();
    let comment = slice
        .lines()
        .map(|x| x.trim_start_matches('#').trim())
        .collect::<Vec<_>>()
        .join("\n");
    Some(comment)
}

fn version_require(lexer: &mut Lexer<Token>) -> Option<semver::VersionReq> {
    let slice = lexer.slice();
    let value = semver::VersionReq::parse(slice).ok()?;
    Some(value)
}

fn version_literal(lexer: &mut Lexer<Token>) -> Option<semver::Version> {
    let slice = lexer.slice();
    let value = semver::Version::parse(slice).ok()?;
    Some(value)
}

fn quote_string(lexer: &mut Lexer<Token>) -> Option<String> {
    let slice = lexer.slice();
    let value = slice
        .trim_start_matches('"')
        .trim_start_matches('\'')
        .trim_end_matches('\'')
        .trim_end_matches('"');
    Some(value.to_string())
}

fn byte_string(lexer: &mut Lexer<Token>) -> Option<Vec<u8>> {
    let slice = lexer.slice();
    let value = base64::engine::general_purpose::STANDARD
        .decode(slice)
        .ok()?;
    Some(value)
}

fn macro_string(lexer: &mut Lexer<Token>) -> Option<String> {
    let slice = lexer.slice();
    Some(
        slice
            .trim_start_matches("m'")
            .trim_end_matches('\'')
            .to_string(),
    )
}

fn float(lexer: &mut Lexer<Token>) -> Option<HashableFloat> {
    let slice = lexer.slice();
    if slice.ends_with("f64") {
        let slice = slice.trim_end_matches("f64");
        return Some(HashableFloat::Float64(slice.parse().ok()?));
    }
    if slice.ends_with("f32") {
        let slice = slice.trim_end_matches("f32");
        return Some(HashableFloat::Float32(slice.parse().ok()?));
    }
    Some(HashableFloat::Generic(slice.parse().ok()?))
}

macro_rules! number {
    ($radix_name: ident : $radix: literal [ $($type: ident as $wrap: ident where $suffix: literal),* ] ) => {
        pub(crate) mod $radix_name {
            pub fn integer(lexer: &mut logos::Lexer<$crate::lang::Token>) -> Option<super::Integer> {
                let slice = lexer.slice().replace('_', "");
                let slice = slice.as_str();
                let slice = slice.trim_start_matches("0x").trim_start_matches("0o").trim_start_matches("0b");
                $(
                    if slice.ends_with($suffix) {
                        let slice = slice.trim_end_matches($suffix);
                        let value = $type::from_str_radix(slice, $radix).ok()?;
                        return Some(super::Integer::$wrap(value));
                    }
                )*
                let value = i64::from_str_radix(slice, $radix).ok()?;
                Some(super::Integer::Generic(value))
            }
        }
    }
}

number!(decimal: 10 [
    i64 as I64 where "i64",
    u64 as U64 where "u64",
    i32 as I32 where "i32",
    u32 as U32 where "u32",
    i16 as I16 where "i16",
    u16 as U16 where "u16",
    i8 as I8 where "i8",
    u8 as U8 where "u8"
]);

number!(hexadecimal: 16 [
    i64 as I64 where "i64",
    u64 as U64 where "u64",
    i32 as I32 where "i32",
    u32 as U32 where "u32",
    i16 as I16 where "i16",
    u16 as U16 where "u16",
    i8 as I8 where "i8",
    u8 as U8 where "u8"
]);
number!(octal: 8 [
    i64 as I64 where "i64",
    u64 as U64 where "u64",
    i32 as I32 where "i32",
    u32 as U32 where "u32",
    i16 as I16 where "i16",
    u16 as U16 where "u16",
    i8 as I8 where "i8",
    u8 as U8 where "u8"
]);
number!(binary: 2 [
    i64 as I64 where "i64",
    u64 as U64 where "u64",
    i32 as I32 where "i32",
    u32 as U32 where "u32",
    i16 as I16 where "i16",
    u16 as U16 where "u16",
    i8 as I8 where "i8",
    u8 as U8 where "u8"
]);
