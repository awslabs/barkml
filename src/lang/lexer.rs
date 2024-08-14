use base64::Engine;
use logos::{Lexer, Logos, Skip};
use snafu::ResultExt;
use std::fmt::Formatter;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

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

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line, self.column))
    }
}

fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    Skip
}

fn base_callback(lex: &mut Lexer<Token>) -> Position {
    Position {
        line: lex.extras.line,
        column: lex.span().start - lex.extras.column,
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
#[logos(error = super::error::Error)]
#[logos(extras = Position)]
#[logos(skip r"[ \t\f\r]+")] // Ignore this regex pattern between tokens
pub enum Token {
    #[regex(r"\n", newline_callback)]
    Newline,

    Error(Position),

    #[token("true", base_callback)]
    #[token("True", base_callback)]
    #[token("yes", base_callback)]
    #[token("Yes", base_callback)]
    #[token("On", base_callback)]
    #[token("on", base_callback)]
    True(Position),
    #[token("false", base_callback)]
    #[token("False", base_callback)]
    #[token("no", base_callback)]
    #[token("No", base_callback)]
    #[token("Off", base_callback)]
    #[token("off", base_callback)]
    False(Position),

    #[token("!", base_callback)]
    Exclaim(Position),
    #[token("$", base_callback)]
    Dollar(Position),
    #[token("[", base_callback)]
    LBracket(Position),
    #[token("]", base_callback)]
    RBracket(Position),
    #[token("{", base_callback)]
    LBrace(Position),
    #[token("}", base_callback)]
    RBrace(Position),
    #[token("(", base_callback)]
    LParen(Position),
    #[token(")", base_callback)]
    RParen(Position),
    #[token("=", base_callback)]
    Assign(Position),
    #[token(":", base_callback)]
    Colon(Position),
    #[token("?", base_callback)]
    Question(Position),
    #[token(",", base_callback)]
    Comma(Position),

    // Keywords
    #[token("null", base_callback, priority = 10)]
    #[token("nil", base_callback, priority = 10)]
    #[token("none", base_callback, priority = 10)]
    KeyNull(Position),
    #[token("bool", base_callback, priority = 10)]
    KeyBool(Position),
    #[token("string", base_callback, priority = 10)]
    KeyString(Position),
    #[token("int", base_callback, priority = 10)]
    KeyInt(Position),
    #[token("uint", base_callback, priority = 10)]
    KeyUInt(Position),
    #[token("i8", base_callback, priority = 10)]
    KeyInt8(Position),
    #[token("u8", base_callback, priority = 10)]
    KeyUInt8(Position),
    #[token("i16", base_callback, priority = 10)]
    KeyInt16(Position),
    #[token("u16", base_callback, priority = 10)]
    KeyUInt16(Position),
    #[token("i32", base_callback, priority = 10)]
    KeyInt32(Position),
    #[token("u32", base_callback, priority = 10)]
    KeyUInt32(Position),
    #[token("i64", base_callback, priority = 10)]
    KeyInt64(Position),
    #[token("u64", base_callback, priority = 10)]
    KeyUInt64(Position),
    #[token("i128", base_callback, priority = 10)]
    KeyInt128(Position),
    #[token("u128", base_callback, priority = 10)]
    KeyUInt128(Position),
    #[token("float", base_callback, priority = 10)]
    KeyFloat(Position),
    #[token("f64", base_callback, priority = 10)]
    KeyFloat64(Position),
    #[token("f32", base_callback, priority = 10)]
    KeyFloat32(Position),
    #[token("bytes", base_callback, priority = 10)]
    KeyBytes(Position),
    #[token("version", base_callback, priority = 10)]
    KeyVersion(Position),
    #[token("require", base_callback, priority = 10)]
    KeyRequire(Position),
    #[token("label", base_callback, priority = 10)]
    KeyLabel(Position),
    #[token("array", base_callback, priority = 10)]
    KeyArray(Position),
    #[token("table", base_callback, priority = 10)]
    KeyTable(Position),
    #[token("section", base_callback, priority = 10)]
    KeySection(Position),
    #[token("block", base_callback, priority = 10)]
    KeyBlock(Position),

    // Unused but reserved
    #[token("module", base_callback, priority = 10)]
    KeyModule(Position),
    #[token("use", base_callback, priority = 10)]
    KeyUse(Position),
    #[token("as", base_callback, priority = 10)]
    KeyAs(Position),
    #[token("schema", base_callback, priority = 10)]
    KeySchema(Position),

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
    Int((Position, Integer)),

    // Floating-point
    #[regex(
        r"[+-]?[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?(f64|f32)?",
        float
    )]
    Float((Position, HashableFloat)),

    #[regex(r"m'[^']*'", macro_string)]
    MacroString((Position, String)),
    #[regex(r"b'[-A-Za-z0-9+/]*={0,3}'", byte_string)]
    ByteString((Position, Vec<u8>)),
    #[regex(r"'[^']*'", quote_string)]
    #[regex(r#""[^"]*""#, quote_string)]
    String((Position, String)),

    #[regex(r"[a-zA-Z][a-zA-Z0-9_\-]*", |x| {
        (base_callback(x), x.slice().to_string()) }, priority = 5
    )]
    Identifier((Position, String)),
    #[regex(r"m\![a-zA-Z][a-zA-Z0-9_\-\.]*", |x| {
        (base_callback(x), x.slice().trim_start_matches("m!").to_string()) }
    , priority = 6)]
    MacroIdentifier((Position, String)),
    #[regex(r"\![a-zA-Z][a-zA-Z0-9_\-]*", |x| {
        (base_callback(x), x.slice().trim_start_matches('!').to_string())
    }, priority = 7)]
    LabelIdentifier((Position, String)),
    #[regex(r"\$[a-zA-Z][a-zA-Z0-9_\-]*", |x| {
        (base_callback(x), x.slice().trim_start_matches('$').to_string()) }, priority = 8
    )]
    ControlIdentifier((Position, String)),

    #[regex(r"([0-9]+\.){2}[0-9]+([-A-Za-z0-9\.]+)?", version_literal)]
    Version((Position, semver::Version)),

    #[regex(
        r"(=|~|\^|>|<|<=|>=)[0-9]+(\.[0-9]+(\.[0-9]+(\-[a-z0-9]+(\+[a-z0-9]+)?)?)?)?",
        version_require
    )]
    Require((Position, semver::VersionReq)),

    #[regex(r"(#[ \t\f]*[^\n\r]+[\n\r])*", line_comment)]
    LineComment((Position, String)),
    #[regex(r"\/\*[^\/\*]*\*\/", multiline_comment)]
    MultiLineComment((Position, String)),
}

impl Token {
    pub fn position(&self) -> Position {
        match self {
            Self::Error(position)
            | Self::True(position)
            | Self::False(position)
            | Self::Exclaim(position)
            | Self::Dollar(position)
            | Self::LBracket(position)
            | Self::RBracket(position)
            | Self::LBrace(position)
            | Self::RBrace(position)
            | Self::LParen(position)
            | Self::RParen(position)
            | Self::Assign(position)
            | Self::Colon(position)
            | Self::Question(position)
            | Self::Comma(position)
            | Self::KeyNull(position)
            | Self::KeyBool(position)
            | Self::KeyString(position)
            | Self::KeyInt(position)
            | Self::KeyInt8(position)
            | Self::KeyInt16(position)
            | Self::KeyInt32(position)
            | Self::KeyInt64(position)
            | Self::KeyUInt8(position)
            | Self::KeyUInt16(position)
            | Self::KeyUInt32(position)
            | Self::KeyUInt64(position)
            | Self::KeyFloat(position)
            | Self::KeyFloat32(position)
            | Self::KeyFloat64(position)
            | Self::KeyBytes(position)
            | Self::KeyVersion(position)
            | Self::KeyRequire(position)
            | Self::KeyLabel(position)
            | Self::KeyArray(position)
            | Self::KeyTable(position)
            | Self::KeySection(position)
            | Self::KeyBlock(position)
            | Self::KeyModule(position)
            | Self::KeyUse(position)
            | Self::KeyAs(position)
            | Self::KeySchema(position)
            | Self::Int((position, ..))
            | Self::Float((position, ..))
            | Self::MacroString((position, ..))
            | Self::LabelIdentifier((position, ..))
            | Self::ByteString((position, ..))
            | Self::String((position, ..))
            | Self::Identifier((position, ..))
            | Self::MacroIdentifier((position, ..))
            | Self::ControlIdentifier((position, ..))
            | Self::Version((position, ..))
            | Self::Require((position, ..))
            | Self::LineComment((position, ..))
            | Self::MultiLineComment((position, ..)) => position.clone(),
            _ => Position::default(),
        }
    }
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

fn multiline_comment(lexer: &mut Lexer<Token>) -> (Position, String) {
    let slice = lexer.slice();
    let comment = slice.trim_start_matches("/*").trim_end_matches("*/").trim();
    (base_callback(lexer), comment.to_string())
}

fn line_comment(lexer: &mut Lexer<Token>) -> (Position, String) {
    let slice = lexer.slice();
    let comment = slice
        .lines()
        .map(|x| x.trim_start_matches('#').trim())
        .collect::<Vec<_>>()
        .join("\n");
    (base_callback(lexer), comment.to_string())
}

fn version_require(
    lexer: &mut Lexer<Token>,
) -> super::error::Result<(Position, semver::VersionReq)> {
    let slice = lexer.slice();
    let position = base_callback(lexer);
    Ok((
        position.clone(),
        semver::VersionReq::parse(slice).map_err(|e| super::error::Error::InvalidRequire {
            position: position.clone(),
            reason: e.to_string(),
        })?,
    ))
}

fn version_literal(lexer: &mut Lexer<Token>) -> super::error::Result<(Position, semver::Version)> {
    let slice = lexer.slice();
    let position = base_callback(lexer);
    Ok((
        position.clone(),
        semver::Version::parse(slice).map_err(|e| super::error::Error::InvalidVersion {
            position: position.clone(),
            reason: e.to_string(),
        })?,
    ))
}

fn quote_string(lexer: &mut Lexer<Token>) -> (Position, String) {
    let slice = lexer.slice();
    let value = slice
        .trim_start_matches('"')
        .trim_start_matches('\'')
        .trim_end_matches('\'')
        .trim_end_matches('"');
    (base_callback(lexer), value.to_string())
}

fn byte_string(lexer: &mut Lexer<Token>) -> super::error::Result<(Position, Vec<u8>)> {
    let slice = lexer
        .slice()
        .trim_start_matches("b'")
        .trim_end_matches('\'');
    let position = base_callback(lexer);
    Ok((
        position.clone(),
        base64::engine::general_purpose::STANDARD
            .decode(slice)
            .context(super::error::InvalidBase64Snafu {
                position: position.clone(),
            })?,
    ))
}

fn macro_string(lexer: &mut Lexer<Token>) -> (Position, String) {
    let slice = lexer.slice();
    (
        base_callback(lexer),
        slice
            .trim_start_matches("m'")
            .trim_end_matches('\'')
            .to_string(),
    )
}

fn float(lexer: &mut Lexer<Token>) -> super::error::Result<(Position, HashableFloat)> {
    let slice = lexer.slice();
    let position = base_callback(lexer);
    if slice.ends_with("f64") {
        let slice = slice.trim_end_matches("f64");
        return Ok((
            position,
            HashableFloat::Float64(slice.parse().context(super::error::InvalidFloatSnafu)?),
        ));
    }
    if slice.ends_with("f32") {
        let slice = slice.trim_end_matches("f32");
        return Ok((
            position,
            HashableFloat::Float32(slice.parse().context(super::error::InvalidFloatSnafu)?),
        ));
    }
    Ok((
        position,
        HashableFloat::Generic(slice.parse().context(super::error::InvalidFloatSnafu)?),
    ))
}

macro_rules! number {
    ($radix_name: ident : $radix: literal [ $($type: ident as $wrap: ident where $suffix: literal),* ] ) => {
        pub(crate) mod $radix_name {
            use snafu::ResultExt;
            pub fn integer(lexer: &mut logos::Lexer<$crate::lang::Token>) -> $crate::lang::error::Result<($crate::lang::Position, super::Integer)> {
                let position = $crate::lang::lexer::base_callback(lexer);
                let slice = lexer.slice().replace('_', "");
                let slice = slice.as_str();
                let slice = slice.trim_start_matches("0x").trim_start_matches("0o").trim_start_matches("0b");
                $(
                    if slice.ends_with($suffix) {
                        let slice = slice.trim_end_matches($suffix);
                        let value = $type::from_str_radix(slice, $radix).context($crate::lang::error::InvalidIntegerSnafu)?;
                        return Ok((position, super::Integer::$wrap(value)));
                    }
                )*
                let value = i64::from_str_radix(slice, $radix).context($crate::lang::error::InvalidIntegerSnafu)?;
                Ok((position, super::Integer::Signed(value)))
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
