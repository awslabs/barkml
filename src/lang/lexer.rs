use base64::Engine;
use logos::{Lexer, Logos};
use snafu::ResultExt;
use std::fmt;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Integer {
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
#[logos(error = super::error::Error)]
#[logos(skip r"[ \t\f\n\r]+")] // Ignore this regex pattern between tokens
pub enum Token {
    Error,

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
    #[token("null", priority = 10)]
    #[token("nil", priority = 10)]
    #[token("none", priority = 10)]
    KeyNull,
    #[token("bool", priority = 10)]
    KeyBool,
    #[token("string", priority = 10)]
    KeyString,
    #[token("int", priority = 10)]
    KeyInt,
    #[token("i8", priority = 10)]
    KeyInt8,
    #[token("u8", priority = 10)]
    KeyUInt8,
    #[token("i16", priority = 10)]
    KeyInt16,
    #[token("u16", priority = 10)]
    KeyUInt16,
    #[token("i32", priority = 10)]
    KeyInt32,
    #[token("u32", priority = 10)]
    KeyUInt32,
    #[token("i64", priority = 10)]
    KeyInt64,
    #[token("u64", priority = 10)]
    KeyUInt64,
    #[token("float", priority = 10)]
    KeyFloat,
    #[token("f64", priority = 10)]
    KeyFloat64,
    #[token("f32", priority = 10)]
    KeyFloat32,
    #[token("bytes", priority = 10)]
    KeyBytes,
    #[token("version", priority = 10)]
    KeyVersion,
    #[token("require", priority = 10)]
    KeyRequire,
    #[token("label", priority = 10)]
    KeyLabel,
    #[token("array", priority = 10)]
    KeyArray,
    #[token("table", priority = 10)]
    KeyTable,
    #[token("section", priority = 10)]
    KeySection,
    #[token("block", priority = 10)]
    KeyBlock,

    // Unused but reserved
    #[token("module", priority = 10)]
    KeyModule,
    #[token("use", priority = 10)]
    KeyUse,
    #[token("as", priority = 10)]
    KeyAs,
    #[token("schema", priority = 10)]
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

    #[regex(r"[a-zA-Z][a-zA-Z0-9_\-]*", |x| x.slice().to_string(), priority = 5)]
    Identifier(String),
    #[regex(r"m\![a-zA-Z][a-zA-Z0-9_\-\.]*", |x| x.slice().trim_start_matches("m!").to_string(), priority = 6)]
    MacroIdentifier(String),
    #[regex(r"\![a-zA-Z][a-zA-Z0-9_\-]*", |x| x.slice().trim_start_matches('!').to_string(), priority = 7)]
    LabelIdentifier(String),
    #[regex(r"\$[a-zA-Z][a-zA-Z0-9_\-]*", |x| x.slice().trim_start_matches('$').to_string(), priority = 8)]
    ControlIdentifier(String),

    #[regex(r"([0-9]+\.){2}[0-9]+([-A-Za-z0-9\.]+)?", version_literal)]
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
pub enum HashableFloat {
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

fn version_require(lexer: &mut Lexer<Token>) -> super::error::Result<semver::VersionReq> {
    let slice = lexer.slice();
    semver::VersionReq::parse(slice).map_err(|e| super::error::Error::InvalidRequire {
        reason: e.to_string(),
    })
}

fn version_literal(lexer: &mut Lexer<Token>) -> super::error::Result<semver::Version> {
    let slice = lexer.slice();
    semver::Version::parse(slice).map_err(|e| super::error::Error::InvalidVersion {
        reason: e.to_string(),
    })
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

fn byte_string(lexer: &mut Lexer<Token>) -> super::error::Result<Vec<u8>> {
    let slice = lexer
        .slice()
        .trim_start_matches("b'")
        .trim_end_matches('\'');
    base64::engine::general_purpose::STANDARD
        .decode(slice)
        .context(super::error::InvalidBase64Snafu)
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

fn float(lexer: &mut Lexer<Token>) -> super::error::Result<HashableFloat> {
    let slice = lexer.slice();
    if slice.ends_with("f64") {
        let slice = slice.trim_end_matches("f64");
        return Ok(HashableFloat::Float64(
            slice.parse().context(super::error::InvalidFloatSnafu)?,
        ));
    }
    if slice.ends_with("f32") {
        let slice = slice.trim_end_matches("f32");
        return Ok(HashableFloat::Float32(
            slice.parse().context(super::error::InvalidFloatSnafu)?,
        ));
    }
    Ok(HashableFloat::Generic(
        slice.parse().context(super::error::InvalidFloatSnafu)?,
    ))
}

macro_rules! number {
    ($radix_name: ident : $radix: literal [ $($type: ident as $wrap: ident where $suffix: literal),* ] ) => {
        pub(crate) mod $radix_name {
            use snafu::ResultExt;
            pub fn integer(lexer: &mut logos::Lexer<$crate::lang::Token>) -> $crate::lang::error::Result<super::Integer> {
                let slice = lexer.slice().replace('_', "");
                let slice = slice.as_str();
                let slice = slice.trim_start_matches("0x").trim_start_matches("0o").trim_start_matches("0b");
                $(
                    if slice.ends_with($suffix) {
                        let slice = slice.trim_end_matches($suffix);
                        let value = $type::from_str_radix(slice, $radix).context($crate::lang::error::InvalidIntegerSnafu)?;
                        return Ok(super::Integer::$wrap(value));
                    }
                )*
                let value = i64::from_str_radix(slice, $radix).context($crate::lang::error::InvalidIntegerSnafu)?;
                Ok(super::Integer::Generic(value))
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

#[cfg(test)]
mod test {
    use logos::Logos;

    use super::Token;

    #[test]
    fn byte_string() {
        let tokens = Token::lexer("b'aGVsbG8='").collect::<Vec<_>>();
        println!("received: {:?}", tokens);
    }
}
