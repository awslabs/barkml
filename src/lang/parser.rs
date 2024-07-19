use super::error::{self, Result};
use super::{HashableFloat, Integer, Token};
use crate::{Value, ValueType};
use indexmap::IndexMap;
use logos::{Lexer, SpannedIter};
use snafu::{ensure, OptionExt, ResultExt};
use std::iter::Peekable;
use std::ops::Range;

pub struct Parser<'source> {
    tokens: Peekable<SpannedIter<'source, Token>>,
    position: Range<usize>,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source, Token>) -> Self {
        Self {
            tokens: lexer.spanned().peekable(),
            position: Range::default(),
        }
    }

    pub fn parse(&mut self) -> Result<Value> {
        self.module()
    }

    fn next(&mut self) -> Option<(Token, Range<usize>)> {
        let x = self.tokens.next();
        if let Some((Ok(x), span)) = x.as_ref() {
            self.position = span.clone();
            Some((x.clone(), span.clone()))
        } else {
            None
        }
    }

    fn comment(&mut self) -> Result<Option<String>> {
        Ok(self
            .tokens
            .next_if(|(token, _)| {
                matches!(
                    token.as_ref(),
                    Ok(Token::LineComment(_) | Token::MultiLineComment(_))
                )
            })
            .and_then(|x| x.0.clone().ok())
            .and_then(|x| match x {
                Token::LineComment(comment) | Token::MultiLineComment(comment) => {
                    Some(comment.clone())
                }
                _ => None,
            }))
    }

    fn label(&mut self) -> Result<Option<String>> {
        Ok(self
            .tokens
            .next_if(|(token, _)| matches!(token.as_ref(), Ok(Token::LabelIdentifier(..))))
            .and_then(|x| x.0.clone().ok())
            .and_then(|x| match x {
                Token::LabelIdentifier(comment) => Some(comment.clone()),
                _ => None,
            }))
    }

    fn value_type(&mut self) -> Result<ValueType> {
        let (token, span) = self.next().context(error::EofSnafu {
            span: self.position.clone(),
        })?;
        let span = span.clone();
        match token {
            Token::KeyString => Ok(ValueType::String),
            Token::KeyInt | Token::KeyInt64 => Ok(ValueType::I64),
            Token::KeyInt8 => Ok(ValueType::I8),
            Token::KeyInt16 => Ok(ValueType::I16),
            Token::KeyInt32 => Ok(ValueType::I32),
            Token::KeyUInt64 => Ok(ValueType::U64),
            Token::KeyUInt32 => Ok(ValueType::U32),
            Token::KeyUInt16 => Ok(ValueType::U16),
            Token::KeyUInt8 => Ok(ValueType::U8),
            Token::KeyBool => Ok(ValueType::Bool),
            Token::KeyFloat64 | Token::KeyFloat => Ok(ValueType::F64),
            Token::KeyFloat32 => Ok(ValueType::F32),
            Token::KeyBytes => Ok(ValueType::Bytes),
            Token::KeyVersion => Ok(ValueType::Version),
            Token::KeyRequire => Ok(ValueType::Require),
            Token::KeyLabel => Ok(ValueType::Label),
            Token::KeyArray => {
                let (tok, s) = self.next().context(error::EofSnafu { span })?;
                ensure!(
                    tok == Token::LBracket,
                    error::ExpectedSnafu {
                        span: s.clone(),
                        expected: "[",
                        got: tok.clone()
                    }
                );
                let mut children = Vec::new();
                while let Some((tok, _)) = self.tokens.peek() {
                    let tok = tok.as_ref()?;
                    match tok {
                        Token::Comma => {
                            self.next();
                            continue;
                        }
                        Token::RBracket => {
                            self.next();
                            break;
                        }
                        _ => {
                            children.push(self.value_type()?);
                        }
                    }
                }
                Ok(ValueType::Array(children))
            }
            Token::KeyTable => {
                let (tok, s) = self
                    .next()
                    .context(error::EofSnafu { span: span.clone() })?;
                ensure!(
                    tok == Token::LBrace,
                    error::ExpectedSnafu {
                        span: s.clone(),
                        expected: "{",
                        got: tok.clone()
                    }
                );
                let mut children = IndexMap::new();
                while let Some((tok, _)) = self.tokens.peek() {
                    let tok = tok.as_ref()?;
                    match tok {
                        Token::Comma => {
                            self.next();
                            continue;
                        }
                        Token::RBrace => {
                            self.next();
                            break;
                        }
                        _ => {
                            let (id, id_span) = self
                                .next()
                                .context(error::EofSnafu { span: span.clone() })?;
                            let id = match id {
                                Token::Identifier(id) | Token::String(id) => Ok(id.clone()),
                                got => error::ExpectedSnafu {
                                    span: id_span.clone(),
                                    expected: "identifier or string value",
                                    got: got.clone(),
                                }
                                .fail(),
                            }?;
                            let (eq, eq_span) = self
                                .next()
                                .context(error::EofSnafu { span: span.clone() })?;
                            ensure!(
                                eq == Token::Colon,
                                error::ExpectedSnafu {
                                    span: eq_span.clone(),
                                    expected: ":",
                                    got: eq.clone()
                                }
                            );
                            let subtype = self.value_type()?;
                            children.insert(id, subtype);
                        }
                    }
                }
                Ok(ValueType::Table(children))
            }
            Token::KeyBlock => {
                let (tok, s) = self
                    .next()
                    .context(error::EofSnafu { span: span.clone() })?;
                ensure!(
                    tok == Token::LBrace,
                    error::ExpectedSnafu {
                        span: s.clone(),
                        expected: "{",
                        got: tok.clone()
                    }
                );
                let mut children = IndexMap::new();
                while let Some((tok, _)) = self.tokens.peek() {
                    let tok = tok.as_ref()?;
                    match tok {
                        Token::Comma => {
                            self.next();
                            continue;
                        }
                        Token::RBrace => {
                            self.next();
                            break;
                        }
                        _ => {
                            let (id, id_span) = self
                                .next()
                                .context(error::EofSnafu { span: span.clone() })?;
                            let id = match id {
                                Token::Identifier(id) | Token::String(id) => Ok(id.clone()),
                                got => error::ExpectedSnafu {
                                    span: id_span.clone(),
                                    expected: "identifier or string value",
                                    got: got.clone(),
                                }
                                .fail(),
                            }?;
                            let (eq, eq_span) = self
                                .next()
                                .context(error::EofSnafu { span: span.clone() })?;
                            ensure!(
                                eq == Token::Colon,
                                error::ExpectedSnafu {
                                    span: eq_span.clone(),
                                    expected: ":",
                                    got: eq.clone()
                                }
                            );
                            let subtype = self.value_type()?;
                            children.insert(id, subtype);
                        }
                    }
                }
                Ok(ValueType::Block(children))
            }
            Token::KeySection => {
                let (tok, s) = self
                    .next()
                    .context(error::EofSnafu { span: span.clone() })?;
                ensure!(
                    tok == Token::LBrace,
                    error::ExpectedSnafu {
                        span: s.clone(),
                        expected: "{",
                        got: tok.clone()
                    }
                );
                let mut children = IndexMap::new();
                while let Some((tok, _)) = self.tokens.peek() {
                    let tok = tok.as_ref()?;
                    match tok {
                        Token::Comma => {
                            self.next();
                            continue;
                        }
                        Token::RBrace => {
                            self.next();
                            break;
                        }
                        _ => {
                            let (id, id_span) = self
                                .next()
                                .context(error::EofSnafu { span: span.clone() })?;
                            let id = match id {
                                Token::Identifier(id) | Token::String(id) => Ok(id.clone()),
                                got => error::ExpectedSnafu {
                                    span: id_span.clone(),
                                    expected: "identifier or string value",
                                    got: got.clone(),
                                }
                                .fail(),
                            }?;
                            let (eq, eq_span) = self
                                .next()
                                .context(error::EofSnafu { span: span.clone() })?;
                            ensure!(
                                eq == Token::Colon,
                                error::ExpectedSnafu {
                                    span: eq_span.clone(),
                                    expected: ":",
                                    got: eq.clone()
                                }
                            );
                            let subtype = self.value_type()?;
                            children.insert(id, subtype);
                        }
                    }
                }
                Ok(ValueType::Section(children))
            }
            _ => error::OneOfSnafu {
                span: self.position.clone(),
                list: vec![
                    "string".to_string(),
                    "int".to_string(),
                    "i8".to_string(),
                    "i16".to_string(),
                    "i32".to_string(),
                    "i64".to_string(),
                    "u8".to_string(),
                    "u16".to_string(),
                    "u32".to_string(),
                    "u64".to_string(),
                    "float".to_string(),
                    "f64".to_string(),
                    "f32".to_string(),
                    "bool".to_string(),
                    "bytes".to_string(),
                    "version".to_string(),
                    "require".to_string(),
                    "array".to_string(),
                    "table".to_string(),
                    "block".to_string(),
                    "section".to_string(),
                ],
                got: token.clone(),
            }
            .fail(),
        }
    }

    fn value(&mut self) -> Result<(Value, ValueType)> {
        let comment = self.comment()?;
        let label = self.label()?;

        let (token, span) = self.next().context(error::EofSnafu {
            span: self.position.clone(),
        })?;
        match token {
            Token::False => Ok((
                Value::new_bool(false, None, None)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::Bool,
            )),
            Token::True => Ok((
                Value::new_bool(true, None, None)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::Bool,
            )),
            Token::Int(Integer::Generic(value)) => Ok((
                Value::new_int(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::I64,
            )),
            Token::Int(Integer::I64(value)) => Ok((
                Value::new_i64(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::I64,
            )),
            Token::Int(Integer::I32(value)) => Ok((
                Value::new_i32(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::I32,
            )),
            Token::Int(Integer::I16(value)) => Ok((
                Value::new_i16(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::I16,
            )),
            Token::Int(Integer::I8(value)) => Ok((
                Value::new_i8(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::I8,
            )),
            Token::Int(Integer::U64(value)) => Ok((
                Value::new_u64(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::U64,
            )),
            Token::Int(Integer::U32(value)) => Ok((
                Value::new_u32(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::U32,
            )),
            Token::Int(Integer::U16(value)) => Ok((
                Value::new_u16(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::U16,
            )),
            Token::Int(Integer::U8(value)) => Ok((
                Value::new_u8(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::U8,
            )),
            Token::Float(HashableFloat::Generic(value)) => Ok((
                Value::new_float(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::F64,
            )),
            Token::Float(HashableFloat::Float32(value)) => Ok((
                Value::new_f32(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::F32,
            )),
            Token::Float(HashableFloat::Float64(value)) => Ok((
                Value::new_f64(value, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::F64,
            )),
            Token::String(value) => Ok((
                Value::new_string(value.clone(), label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::String,
            )),
            Token::MacroString(value) => Ok((
                Value::new_macro(value.clone(), true, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::Macro,
            )),
            Token::MacroIdentifier(value) => Ok((
                Value::new_macro(value.clone(), false, label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::Macro,
            )),
            Token::ByteString(value) => Ok((
                Value::new_bytes(value.clone(), label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::Bytes,
            )),
            Token::Version(value) => Ok((
                Value::new_version(value.clone(), label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::Version,
            )),
            Token::Require(value) => Ok((
                Value::new_require(value.clone(), label, comment)
                    .context(error::ValueSnafu { span: span.clone() })?,
                ValueType::Require,
            )),
            Token::LBracket => {
                let mut children = Vec::new();
                let mut child_types = Vec::new();
                while let Some((token, _)) = self.tokens.peek() {
                    let token = token.as_ref()?;
                    match token {
                        Token::Comma => {
                            self.next();
                            continue;
                        }
                        Token::RBracket => {
                            self.next();
                            break;
                        }
                        _ => {
                            let (value, type_) = self.value()?;
                            children.push(value);
                            child_types.push(type_);
                        }
                    };
                }
                Ok((
                    Value::new_array(children, label, comment)
                        .context(error::ValueSnafu { span: span.clone() })?,
                    ValueType::Array(child_types),
                ))
            }
            Token::LBrace => {
                let mut children = IndexMap::new();
                let mut child_types = IndexMap::new();
                while let Some((token, child_span)) = self.tokens.peek() {
                    let token = token.as_ref()?;
                    let child_span = child_span.clone();
                    match token {
                        Token::Comma => {
                            self.next();
                            continue;
                        }
                        Token::RBrace => {
                            self.next();
                            break;
                        }
                        Token::Identifier(id) | Token::String(id) => {
                            let id = id.clone();
                            let (next_token, _) = self.next().context(error::EofSnafu {
                                span: child_span.clone(),
                            })?;
                            let vtype = if next_token == Token::Colon {
                                Some(self.value_type()?)
                            } else {
                                None
                            };
                            let (eq_tok, eq_span) = self.next().context(error::EofSnafu {
                                span: self.position.clone(),
                            })?;
                            ensure!(
                                eq_tok == Token::Assign,
                                error::ExpectedSnafu {
                                    span: eq_span.clone(),
                                    expected: "=",
                                    got: eq_tok.clone()
                                }
                            );
                            let (child, child_type) = self.value()?;
                            children.insert(id.clone(), child);
                            child_types.insert(id.clone(), vtype.unwrap_or(child_type.clone()));
                        }
                        _ => {
                            return error::OneOfSnafu {
                                span: child_span.clone(),
                                list: vec![
                                    ",".to_string(),
                                    "}".to_string(),
                                    "identifier".to_string(),
                                    "string".to_string(),
                                ],
                                got: token.clone(),
                            }
                            .fail()
                        }
                    }
                }
                Ok((
                    Value::new_table(children, label, comment)
                        .context(error::ValueSnafu { span })?,
                    ValueType::Table(child_types),
                ))
            }
            _ => error::ExpectedSnafu {
                span,
                expected: "value",
                got: token,
            }
            .fail(),
        }
    }

    fn statement(&mut self) -> Result<Value> {
        let comment = self.comment()?;

        let (token, span) = self.next().context(error::EofSnafu {
            span: self.position.clone(),
        })?;
        match token {
            Token::ControlIdentifier(id) => {
                let (mut eq, mut eq_span) = self
                    .next()
                    .context(error::EofSnafu { span: span.clone() })?;
                let type_ = if eq == Token::Colon {
                    let type_ = self.value_type()?;
                    (eq, eq_span) = self.next().context(error::EofSnafu {
                        span: eq_span.clone(),
                    })?;
                    Some(type_)
                } else {
                    None
                };
                ensure!(
                    eq == Token::Assign,
                    error::ExpectedSnafu {
                        expected: "=",
                        got: eq.clone(),
                        span: eq_span.clone()
                    }
                );
                let (value, vtype) = self.value()?;
                if let Some(type_) = type_.as_ref() {
                    ensure!(
                        vtype == *type_,
                        error::TypeSnafu {
                            span: span.clone(),
                            vtype: vtype.clone(),
                            atype: type_.clone()
                        }
                    );
                }
                Value::new_control(id.clone(), value, comment, type_)
                    .context(error::ValueSnafu { span: span.clone() })
            }
            Token::Identifier(id) | Token::String(id) => {
                let (front, _) = self
                    .tokens
                    .peek()
                    .context(error::EofSnafu { span: span.clone() })?;
                let front = front.as_ref()?;
                if *front == Token::Colon || *front == Token::Assign {
                    let (mut eq, mut eq_span) = self
                        .next()
                        .context(error::EofSnafu { span: span.clone() })?;
                    let type_ = if eq == Token::Colon {
                        let type_ = self.value_type()?;
                        (eq, eq_span) = self.next().context(error::EofSnafu {
                            span: eq_span.clone(),
                        })?;
                        Some(type_)
                    } else {
                        None
                    };
                    ensure!(
                        eq == Token::Assign,
                        error::ExpectedSnafu {
                            expected: "=",
                            got: eq.clone(),
                            span: eq_span.clone()
                        }
                    );
                    let (value, vtype) = self.value()?;
                    if let Some(type_) = type_.as_ref() {
                        ensure!(
                            vtype == *type_,
                            error::TypeSnafu {
                                span: span.clone(),
                                vtype: vtype.clone(),
                                atype: type_.clone()
                            }
                        );
                    }
                    Value::new_assignment(id.clone(), value, comment, type_)
                        .context(error::ValueSnafu { span: span.clone() })
                } else {
                    let mut labels: Vec<String> = Vec::new();
                    while let Some((label, label_span)) = self.tokens.peek() {
                        let label = label.clone()?;
                        let label_span = label_span.clone();
                        match label {
                            Token::LBrace => {
                                self.next();
                                break;
                            }
                            Token::Identifier(id) | Token::String(id) => {
                                self.next();
                                labels.push(id.clone());
                            }
                            _ => {
                                return error::OneOfSnafu {
                                    span: label_span.clone(),
                                    got: label.clone(),
                                    list: vec![
                                        "{".to_string(),
                                        "identifier".to_string(),
                                        "string".to_string(),
                                    ],
                                }
                                .fail();
                            }
                        }
                    }
                    let mut children = IndexMap::new();
                    while let Some((stmt, _)) = self.tokens.peek() {
                        let stmt = stmt.clone()?;
                        match stmt {
                            Token::RBrace => {
                                self.next();
                                break;
                            }
                            _ => {
                                let value = self.statement()?;
                                children.insert(value.inject_id().unwrap(), value);
                            }
                        }
                    }
                    Value::new_block(id.clone(), labels, children, comment)
                        .context(error::ValueSnafu { span: span.clone() })
                }
            }
            value => error::ExpectedSnafu {
                expected: "statement",
                got: value.clone(),
                span: span.clone(),
            }
            .fail(),
        }
    }

    fn module(&mut self) -> Result<Value> {
        let mut children = IndexMap::new();
        while let Some((token, span)) = self.tokens.peek() {
            let token = token.clone()?;
            let span = span.clone();
            let comment = self.comment()?;
            match token {
                Token::LBracket => {
                    self.next();
                    let (id, id_span) = self
                        .next()
                        .context(error::EofSnafu { span: span.clone() })?;
                    let id = match id {
                        Token::Identifier(id) => Ok(id),
                        Token::String(id) => Ok(id),
                        value => error::ExpectedSnafu {
                            span: id_span.clone(),
                            expected: "identifier or string",
                            got: value.clone(),
                        }
                        .fail(),
                    }?;
                    let (close, close_span) = self
                        .next()
                        .context(error::EofSnafu { span: span.clone() })?;
                    ensure!(
                        close == Token::RBracket,
                        error::ExpectedSnafu {
                            span: close_span.clone(),
                            expected: "]",
                            got: close.clone()
                        }
                    );
                    let mut statements = IndexMap::new();
                    while let Some((stmt, _)) = self.tokens.peek() {
                        let stmt = stmt.clone()?;
                        match stmt {
                            Token::LBracket => break,
                            _ => {
                                let value = self.statement()?;
                                statements.insert(value.inject_id().unwrap(), value);
                            }
                        }
                    }
                    let child = Value::new_section(id, statements, comment)
                        .context(error::ValueSnafu { span: span.clone() })?;
                    children.insert(child.inject_id().unwrap(), child);
                }
                _ => {
                    let value = self.statement()?;
                    children.insert(value.inject_id().unwrap(), value);
                }
            }
        }
        Value::new_module(children, None).context(error::ValueSnafu { span: 0..0 })
    }
}

#[cfg(test)]
mod test {
    use crate::lang::parser::Parser;
    use crate::lang::Token;
    use crate::{Value, ValueType};
    use indexmap::IndexMap;
    use logos::Logos;

    macro_rules! parser {
        ($input: expr) => {
            Parser::new(Token::lexer($input))
        };
    }
    #[test]
    fn comment() {
        let mut parser = parser!("# Line Comment\n");
        assert_eq!(parser.comment().unwrap(), Some("Line Comment".to_string()));
    }

    #[test]
    fn label() {
        let mut parser = parser!("!Hint");
        assert_eq!(parser.label().unwrap(), Some("Hint".to_string()));
    }

    #[test]
    fn values() {
        for (case, expected) in [
            (
                "'hello world'",
                (
                    Value::new_string("hello world".to_string(), None, None).unwrap(),
                    ValueType::String,
                ),
            ),
            (
                "-3",
                (Value::new_int(-3, None, None).unwrap(), ValueType::I64),
            ),
            (
                "-3i64",
                (Value::new_i64(-3, None, None).unwrap(), ValueType::I64),
            ),
            (
                "-3i32",
                (Value::new_i32(-3, None, None).unwrap(), ValueType::I32),
            ),
            (
                "-3i16",
                (Value::new_i16(-3, None, None).unwrap(), ValueType::I16),
            ),
            (
                "-3i8",
                (Value::new_i8(-3, None, None).unwrap(), ValueType::I8),
            ),
            (
                "3u64",
                (Value::new_u64(3, None, None).unwrap(), ValueType::U64),
            ),
            (
                "3u32",
                (Value::new_u32(3, None, None).unwrap(), ValueType::U32),
            ),
            (
                "3u16",
                (Value::new_u16(3, None, None).unwrap(), ValueType::U16),
            ),
            (
                "3u8",
                (Value::new_u8(3, None, None).unwrap(), ValueType::U8),
            ),
            (
                "-3.14",
                (Value::new_float(-3.14, None, None).unwrap(), ValueType::F64),
            ),
            (
                "-3.14f64",
                (Value::new_f64(-3.14, None, None).unwrap(), ValueType::F64),
            ),
            (
                "-3.14f32",
                (Value::new_f32(-3.14, None, None).unwrap(), ValueType::F32),
            ),
            (
                "false",
                (Value::new_bool(false, None, None).unwrap(), ValueType::Bool),
            ),
            (
                "1.2.3",
                (
                    Value::new_version(semver::Version::new(1, 2, 3), None, None).unwrap(),
                    ValueType::Version,
                ),
            ),
            (
                ">1.4",
                (
                    Value::new_require(semver::VersionReq::parse(">1.4").unwrap(), None, None)
                        .unwrap(),
                    ValueType::Require,
                ),
            ),
            (
                "m'hello'",
                (
                    Value::new_macro("hello".to_string(), true, None, None).unwrap(),
                    ValueType::Macro,
                ),
            ),
            (
                "m!hello",
                (
                    Value::new_macro("hello".to_string(), false, None, None).unwrap(),
                    ValueType::Macro,
                ),
            ),
            (
                "b'aGVsbG8='",
                (
                    Value::new_bytes(b"hello".to_vec(), None, None).unwrap(),
                    ValueType::Bytes,
                ),
            ),
            (
                "['hello', 3, true]",
                (
                    (Value::new_array(
                        vec![
                            Value::new_string("hello".to_string(), None, None).unwrap(),
                            Value::new_int(3, None, None).unwrap(),
                            Value::new_bool(true, None, None).unwrap(),
                        ],
                        None,
                        None,
                    )
                    .unwrap()),
                    ValueType::Array(vec![ValueType::String, ValueType::I64, ValueType::Bool]),
                ),
            ),
            (
                "{one = 'hello', two = 3, three = true}",
                (
                    Value::new_table(
                        IndexMap::from([
                            (
                                "one".to_string(),
                                Value::new_string("hello".to_string(), None, None).unwrap(),
                            ),
                            ("two".to_string(), Value::new_int(3, None, None).unwrap()),
                            (
                                "three".to_string(),
                                Value::new_bool(true, None, None).unwrap(),
                            ),
                        ]),
                        None,
                        None,
                    )
                    .unwrap(),
                    ValueType::Table(IndexMap::from([
                        ("one".to_string(), ValueType::String),
                        ("two".to_string(), ValueType::I64),
                        ("three".to_string(), ValueType::Bool),
                    ])),
                ),
            ),
        ] {
            let mut parser = parser!(case);
            assert_eq!(parser.value().unwrap(), expected);
        }
    }

    #[test]
    fn types() {
        for (case, expected) in [
            ("string", ValueType::String),
            ("int", ValueType::I64),
            ("i8", ValueType::I8),
            ("i16", ValueType::I16),
            ("i32", ValueType::I32),
            ("i64", ValueType::I64),
            ("u8", ValueType::U8),
            ("u16", ValueType::U16),
            ("u32", ValueType::U32),
            ("u64", ValueType::U64),
            ("float", ValueType::F64),
            ("f32", ValueType::F32),
            ("f64", ValueType::F64),
            ("bool", ValueType::Bool),
            ("version", ValueType::Version),
            ("require", ValueType::Require),
            ("bytes", ValueType::Bytes),
            (
                "array[string, int, bool]",
                ValueType::Array(vec![ValueType::String, ValueType::I64, ValueType::Bool]),
            ),
            (
                "table{one: string, two: int, three: bool}",
                ValueType::Table(IndexMap::from([
                    ("one".to_string(), ValueType::String),
                    ("two".to_string(), ValueType::I64),
                    ("three".to_string(), ValueType::Bool),
                ])),
            ),
            (
                "block{one: string, two: int, three: bool}",
                ValueType::Block(IndexMap::from([
                    ("one".to_string(), ValueType::String),
                    ("two".to_string(), ValueType::I64),
                    ("three".to_string(), ValueType::Bool),
                ])),
            ),
            (
                "section{one: string, two: int, three: bool}",
                ValueType::Section(IndexMap::from([
                    ("one".to_string(), ValueType::String),
                    ("two".to_string(), ValueType::I64),
                    ("three".to_string(), ValueType::Bool),
                ])),
            ),
        ] {
            let mut parser = parser!(case);
            assert_eq!(parser.value_type().unwrap(), expected);
        }
    }

    #[test]
    fn statements() {
        for (case, expected) in [
            (
                "$foo = 3",
                Value::new_control(
                    "foo".to_string(),
                    Value::new_int(3, None, None).unwrap(),
                    None,
                    None,
                )
                .unwrap(),
            ),
            (
                "$foo: f64 = 3.14",
                Value::new_control(
                    "foo".to_string(),
                    Value::new_f64(3.14, None, None).unwrap(),
                    None,
                    Some(ValueType::F64),
                )
                .unwrap(),
            ),
            (
                "# Comment\n$foo: f64 = !Hint 3.14",
                Value::new_control(
                    "foo".to_string(),
                    Value::new_f64(3.14, Some("Hint".to_string()), None).unwrap(),
                    Some("Comment".to_string()),
                    Some(ValueType::F64),
                )
                .unwrap(),
            ),
            (
                "foo = 3",
                Value::new_assignment(
                    "foo".to_string(),
                    Value::new_int(3, None, None).unwrap(),
                    None,
                    None,
                )
                .unwrap(),
            ),
            (
                "'foo': f64 = 3.14",
                Value::new_assignment(
                    "foo".to_string(),
                    Value::new_f64(3.14, None, None).unwrap(),
                    None,
                    Some(ValueType::F64),
                )
                .unwrap(),
            ),
            (
                "# Comment\n\"foo\": f64 = !Hint 3.14",
                Value::new_assignment(
                    "foo".to_string(),
                    Value::new_f64(3.14, Some("Hint".to_string()), None).unwrap(),
                    Some("Comment".to_string()),
                    Some(ValueType::F64),
                )
                .unwrap(),
            ),
            (
                "# Comment\ntest foo 'bar' {\n one = 3\n }\n",
                Value::new_block(
                    "test".to_string(),
                    vec!["foo".to_string(), "bar".to_string()],
                    IndexMap::from([(
                        "one".to_string(),
                        Value::new_assignment(
                            "one".to_string(),
                            Value::new_int(3, None, None).unwrap(),
                            None,
                            None,
                        )
                        .unwrap(),
                    )]),
                    Some("Comment".to_string()),
                )
                .unwrap(),
            ),
        ] {
            let mut parser = parser!(case);
            assert_eq!(parser.statement().unwrap(), expected);
        }
    }
}
