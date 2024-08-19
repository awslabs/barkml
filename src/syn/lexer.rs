use crate::{error, Result};
use base64::Engine;
use logos::{Lexer, Logos, Skip};
use snafu::ResultExt;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::ast::Location;

/// Stores the integer with precision retained
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Integer {
    Signed(i64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    Unsigned(u64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

/// Handles incrementing the line and column counters
fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    Skip
}

// Creates a source location
fn base_callback(lex: &mut Lexer<Token>) -> Location {
    Location {
        module: None,
        line: lex.extras.line,
        column: lex.span().start - lex.extras.column,
    }
}

/// Represents the allowed tokens in a barkml file
#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
#[logos(error = error::Error)]
#[logos(extras = Location)]
#[logos(skip r"[ \t\f\r]+")] // Ignore this regex pattern between tokens
pub enum Token {
    #[regex(r"\n", newline_callback)]
    Newline,

    Error(Location),

    #[token("true", base_callback)]
    #[token("True", base_callback)]
    #[token("yes", base_callback)]
    #[token("Yes", base_callback)]
    #[token("On", base_callback)]
    #[token("on", base_callback)]
    True(Location),
    #[token("false", base_callback)]
    #[token("False", base_callback)]
    #[token("no", base_callback)]
    #[token("No", base_callback)]
    #[token("Off", base_callback)]
    #[token("off", base_callback)]
    False(Location),

    #[token("!", base_callback)]
    Exclaim(Location),
    #[token("$", base_callback)]
    Dollar(Location),

    #[token("[", base_callback)]
    LBracket(Location),
    #[token("]", base_callback)]
    RBracket(Location),
    #[token("{", base_callback)]
    LBrace(Location),
    #[token("}", base_callback)]
    RBrace(Location),
    #[token("(", base_callback)]
    LParen(Location),
    #[token(")", base_callback)]
    RParen(Location),
    #[token("=", base_callback)]
    Assign(Location),
    #[token(":", base_callback)]
    Colon(Location),
    #[token("?", base_callback)]
    Question(Location),
    #[token(",", base_callback)]
    Comma(Location),

    // Keywords
    #[token("null", base_callback, priority = 10)]
    #[token("nil", base_callback, priority = 10)]
    #[token("none", base_callback, priority = 10)]
    KeyNull(Location),
    #[token("bool", base_callback, priority = 10)]
    KeyBool(Location),
    #[token("string", base_callback, priority = 10)]
    KeyString(Location),
    #[token("int", base_callback, priority = 10)]
    KeyInt(Location),
    #[token("uint", base_callback, priority = 10)]
    KeyUInt(Location),
    #[token("i8", base_callback, priority = 10)]
    KeyInt8(Location),
    #[token("u8", base_callback, priority = 10)]
    KeyUInt8(Location),
    #[token("i16", base_callback, priority = 10)]
    KeyInt16(Location),
    #[token("u16", base_callback, priority = 10)]
    KeyUInt16(Location),
    #[token("i32", base_callback, priority = 10)]
    KeyInt32(Location),
    #[token("u32", base_callback, priority = 10)]
    KeyUInt32(Location),
    #[token("i64", base_callback, priority = 10)]
    KeyInt64(Location),
    #[token("u64", base_callback, priority = 10)]
    KeyUInt64(Location),
    #[token("i128", base_callback, priority = 10)]
    KeyInt128(Location),
    #[token("u128", base_callback, priority = 10)]
    KeyUInt128(Location),
    #[token("float", base_callback, priority = 10)]
    KeyFloat(Location),
    #[token("f64", base_callback, priority = 10)]
    KeyFloat64(Location),
    #[token("f32", base_callback, priority = 10)]
    KeyFloat32(Location),
    #[token("bytes", base_callback, priority = 10)]
    KeyBytes(Location),
    #[token("version", base_callback, priority = 10)]
    KeyVersion(Location),
    #[token("require", base_callback, priority = 10)]
    KeyRequire(Location),
    #[token("label", base_callback, priority = 10)]
    KeyLabel(Location),
    #[token("array", base_callback, priority = 10)]
    KeyArray(Location),
    #[token("table", base_callback, priority = 10)]
    KeyTable(Location),
    #[token("section", base_callback, priority = 10)]
    KeySection(Location),
    #[token("block", base_callback, priority = 10)]
    KeyBlock(Location),

    // Unused but reserved
    #[token("module", base_callback, priority = 10)]
    KeyModule(Location),
    #[token("use", base_callback, priority = 10)]
    KeyUse(Location),
    #[token("as", base_callback, priority = 10)]
    KeyAs(Location),
    #[token("schema", base_callback, priority = 10)]
    KeySchema(Location),

    // Integer
    #[regex(
        r"[+-]?[0-9][0-9_]*(i128|i64|i32|i16|i8|u128|u64|u32|u16|u8)?",
        decimal::integer
    )]
    #[regex(
        r"[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*(i128|i64|i32|i16|i8|u128|u64|u32|u16|u8)?",
        hexadecimal::integer
    )]
    #[regex(
        r"[+-]?0o[0-7][0-7_]*(i128|i64|i32|i16|i8|u128|u64|u32|u16|u8)?",
        octal::integer
    )]
    #[regex(
        r"[+-]?0b[0-1][0-1_]*(i128|i64|i32|i16|i8|u128|u64|u32|u16|u8)?",
        binary::integer
    )]
    Int((Location, Integer)),

    // Floating-point
    #[regex(
        r"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?(f64|f32)?",
        float
    )]
    Float((Location, HashableFloat)),

    #[regex(r"m'[^']*'", macro_string)]
    MacroString((Location, String)),
    #[regex(r"b'[-A-Za-z0-9+/]*={0,3}'", byte_string)]
    ByteString((Location, Vec<u8>)),
    #[regex(r"'[^']*'", quote_string)]
    #[regex(r#""[^"]*""#, quote_string)]
    String((Location, String)),

    #[regex(r"[a-zA-Z][a-zA-Z0-9_\-]*", |x| {
        (base_callback(x), x.slice().to_string()) }, priority = 5
    )]
    Identifier((Location, String)),
    #[regex(r"m\![a-zA-Z][a-zA-Z0-9_\-\.]*", |x| {
        (base_callback(x), x.slice().trim_start_matches("m!").to_string()) }
    , priority = 6)]
    MacroIdentifier((Location, String)),
    #[regex(r"\![a-zA-Z][a-zA-Z0-9_\-]*", |x| {
        (base_callback(x), x.slice().trim_start_matches('!').to_string())
    }, priority = 7)]
    LabelIdentifier((Location, String)),
    #[regex(r"\$[a-zA-Z][a-zA-Z0-9_\-]*", |x| {
        (base_callback(x), x.slice().trim_start_matches('$').to_string()) }, priority = 8
    )]
    ControlIdentifier((Location, String)),

    #[regex(r"([0-9]+\.){2}[0-9]+([-A-Za-z0-9\.]+)?", version_literal)]
    Version((Location, semver::Version)),

    #[regex(
        r"(=|~|\^|>|<|<=|>=)[0-9]+(\.[0-9]+(\.[0-9]+(\-[a-z0-9]+(\+[a-z0-9]+)?)?)?)?",
        version_require
    )]
    Require((Location, semver::VersionReq)),

    #[regex(r"(#[ \t\f]*[^\n\r]+[\n\r])*", line_comment)]
    LineComment((Location, String)),
    #[regex(r"\/\*[^\/\*]*\*\/", multiline_comment)]
    MultiLineComment((Location, String)),
}

impl Token {
    pub fn location(&self, module: Option<String>) -> Location {
        match self {
            Self::Error(source)
            | Self::True(source)
            | Self::False(source)
            | Self::Exclaim(source)
            | Self::Dollar(source)
            | Self::LBracket(source)
            | Self::RBracket(source)
            | Self::LBrace(source)
            | Self::RBrace(source)
            | Self::LParen(source)
            | Self::RParen(source)
            | Self::Assign(source)
            | Self::Colon(source)
            | Self::Question(source)
            | Self::Comma(source)
            | Self::KeyNull(source)
            | Self::KeyBool(source)
            | Self::KeyString(source)
            | Self::KeyInt(source)
            | Self::KeyInt8(source)
            | Self::KeyInt16(source)
            | Self::KeyInt32(source)
            | Self::KeyInt64(source)
            | Self::KeyUInt8(source)
            | Self::KeyUInt16(source)
            | Self::KeyUInt32(source)
            | Self::KeyUInt64(source)
            | Self::KeyFloat(source)
            | Self::KeyFloat32(source)
            | Self::KeyFloat64(source)
            | Self::KeyBytes(source)
            | Self::KeyVersion(source)
            | Self::KeyRequire(source)
            | Self::KeyLabel(source)
            | Self::KeyArray(source)
            | Self::KeyTable(source)
            | Self::KeySection(source)
            | Self::KeyBlock(source)
            | Self::KeyModule(source)
            | Self::KeyUse(source)
            | Self::KeyAs(source)
            | Self::KeySchema(source)
            | Self::Int((source, ..))
            | Self::Float((source, ..))
            | Self::MacroString((source, ..))
            | Self::LabelIdentifier((source, ..))
            | Self::ByteString((source, ..))
            | Self::String((source, ..))
            | Self::Identifier((source, ..))
            | Self::MacroIdentifier((source, ..))
            | Self::ControlIdentifier((source, ..))
            | Self::Version((source, ..))
            | Self::Require((source, ..))
            | Self::LineComment((source, ..))
            | Self::MultiLineComment((source, ..)) => {
                let mut src = source.clone();
                if let Some(module_name) = module.as_ref() {
                    src.set_module(module_name.as_str());
                }
                src
            }
            _ => Location::default(),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

fn multiline_comment(lexer: &mut Lexer<Token>) -> (Location, String) {
    let slice = lexer.slice();
    let comment = slice.trim_start_matches("/*").trim_end_matches("*/").trim();
    (base_callback(lexer), comment.to_string())
}

fn line_comment(lexer: &mut Lexer<Token>) -> (Location, String) {
    let slice = lexer.slice();
    let comment = slice
        .lines()
        .map(|x| x.trim_start_matches('#').trim())
        .collect::<Vec<_>>()
        .join("\n");
    (base_callback(lexer), comment.to_string())
}

fn version_require(lexer: &mut Lexer<Token>) -> Result<(Location, semver::VersionReq)> {
    let slice = lexer.slice();
    let location = base_callback(lexer);
    Ok((
        location.clone(),
        semver::VersionReq::parse(slice).map_err(|e| error::Error::Require {
            location: location.clone(),
            reason: e.to_string(),
        })?,
    ))
}

fn version_literal(lexer: &mut Lexer<Token>) -> Result<(Location, semver::Version)> {
    let slice = lexer.slice();
    let location = base_callback(lexer);
    Ok((
        location.clone(),
        semver::Version::parse(slice).map_err(|e| error::Error::Version {
            location: location.clone(),
            reason: e.to_string(),
        })?,
    ))
}

fn quote_string(lexer: &mut Lexer<Token>) -> (Location, String) {
    let slice = lexer.slice();
    let value = slice
        .trim_start_matches('"')
        .trim_start_matches('\'')
        .trim_end_matches('\'')
        .trim_end_matches('"');
    (base_callback(lexer), value.to_string())
}

fn byte_string(lexer: &mut Lexer<Token>) -> Result<(Location, Vec<u8>)> {
    let slice = lexer
        .slice()
        .trim_start_matches("b'")
        .trim_end_matches('\'');
    let location = base_callback(lexer);
    Ok((
        location.clone(),
        base64::engine::general_purpose::STANDARD
            .decode(slice)
            .context(error::Base64Snafu {
                location: location.clone(),
            })?,
    ))
}

fn macro_string(lexer: &mut Lexer<Token>) -> (Location, String) {
    let slice = lexer.slice();
    (
        base_callback(lexer),
        slice
            .trim_start_matches("m'")
            .trim_end_matches('\'')
            .to_string(),
    )
}

fn float(lexer: &mut Lexer<Token>) -> Result<(Location, HashableFloat)> {
    let slice = lexer.slice();
    let location = base_callback(lexer);
    if slice.ends_with("f64") {
        let slice = slice.trim_end_matches("f64");
        return Ok((
            location.clone(),
            HashableFloat::Float64(slice.parse().context(error::FloatSnafu {
                location: location.clone(),
            })?),
        ));
    }
    if slice.ends_with("f32") {
        let slice = slice.trim_end_matches("f32");
        return Ok((
            location.clone(),
            HashableFloat::Float32(slice.parse().context(error::FloatSnafu {
                location: location.clone(),
            })?),
        ));
    }
    Ok((
        location.clone(),
        HashableFloat::Generic(slice.parse().context(error::FloatSnafu {
            location: location.clone(),
        })?),
    ))
}

macro_rules! number {
    ($radix_name: ident : $radix: literal [ $($type: ident as $wrap: ident where $suffix: literal),* ] ) => {
        pub(crate) mod $radix_name {
            use snafu::ResultExt;
            pub fn integer(lexer: &mut logos::Lexer<$crate::syn::Token>) -> $crate::Result<($crate::ast::Location, super::Integer)> {
                let location = $crate::syn::lexer::base_callback(lexer);
                let slice = lexer.slice().replace('_', "");
                let slice = slice.as_str();
                let slice = slice.trim_start_matches("0x").trim_start_matches("0o").trim_start_matches("0b");
                $(
                    if slice.ends_with($suffix) {
                        let slice = slice.trim_end_matches($suffix);
                        let value = $type::from_str_radix(slice, $radix).context($crate::error::IntegerSnafu {
                            location: location.clone(),
                        })?;
                        return Ok((location.clone(), super::Integer::$wrap(value)));
                    }
                )*
                let value = i64::from_str_radix(slice, $radix).context($crate::error::IntegerSnafu {
                    location: location.clone(),
                })?;
                Ok((location.clone(), super::Integer::Signed(value)))
            }
        }
    }
}

number!(decimal: 10 [
    u64 as Unsigned where "uint",
    i128 as I128 where "i128",
    u128 as U128 where "u128",
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
    u64 as Unsigned where "uint",
    i128 as I128 where "i128",
    u128 as U128 where "u128",
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
    u64 as Unsigned where "uint",
    i128 as I128 where "i128",
    u128 as U128 where "u128",
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
    u64 as Unsigned where "uint",
    i128 as I128 where "i128",
    u128 as U128 where "u128",
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
