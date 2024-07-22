use super::error::{self, Result};
use super::read::{Read, TokenReader};
use super::{HashableFloat, Integer, Position, Token};
use crate::{Value, ValueType};
use indexmap::IndexMap;
use logos::Lexer;
use snafu::{ensure, OptionExt, ResultExt};

pub struct Parser<'source> {
    tokens: TokenReader<'source>,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source, Token>) -> Self {
        Self {
            tokens: TokenReader {
                lexer: lexer.peekable(),
                position: Position::default(),
            },
        }
    }

    pub fn parse(&mut self) -> Result<Value> {
        self.module()
    }

    fn comment(&mut self) -> Result<Option<String>> {
        if let Some(token) = self.tokens.peek()? {
            match token {
                Token::LineComment((_, comment)) | Token::MultiLineComment((_, comment)) => {
                    self.tokens.discard();
                    Ok(Some(comment.clone()))
                }
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn label(&mut self) -> Result<Option<String>> {
        if let Some(token) = self.tokens.peek()? {
            match token {
                Token::LabelIdentifier((_, label)) => {
                    self.tokens.discard();
                    Ok(Some(label.clone()))
                }
                _ => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn value_type(&mut self) -> Result<ValueType> {
        let token = self.tokens.next()?.context(error::EofSnafu {
            position: self.tokens.position(),
        })?;
        match token {
            Token::KeyString(_) => Ok(ValueType::String),
            Token::KeyInt(_) | Token::KeyInt64(_) => Ok(ValueType::I64),
            Token::KeyInt8(_) => Ok(ValueType::I8),
            Token::KeyInt16(_) => Ok(ValueType::I16),
            Token::KeyInt32(_) => Ok(ValueType::I32),
            Token::KeyUInt64(_) => Ok(ValueType::U64),
            Token::KeyUInt32(_) => Ok(ValueType::U32),
            Token::KeyUInt16(_) => Ok(ValueType::U16),
            Token::KeyUInt8(_) => Ok(ValueType::U8),
            Token::KeyNull(_) => Ok(ValueType::Null),
            Token::KeyBool(_) => Ok(ValueType::Bool),
            Token::KeyFloat64(_) | Token::KeyFloat(_) => Ok(ValueType::F64),
            Token::KeyFloat32(_) => Ok(ValueType::F32),
            Token::KeyBytes(_) => Ok(ValueType::Bytes),
            Token::KeyVersion(_) => Ok(ValueType::Version),
            Token::KeyRequire(_) => Ok(ValueType::Require),
            Token::KeyLabel(_) => Ok(ValueType::Label),
            Token::KeyArray(position) => {
                let tok = self.tokens.next()?.context(error::EofSnafu { position })?;
                ensure!(
                    matches!(tok, Token::LBracket(_)),
                    error::ExpectedSnafu {
                        position: tok.position(),
                        expected: "[",
                        got: tok.clone()
                    }
                );
                let mut children = Vec::new();
                while let Some(tok) = self.tokens.peek()? {
                    match tok {
                        Token::Comma(_) => {
                            self.tokens.discard();
                            continue;
                        }
                        Token::RBracket(_) => {
                            self.tokens.discard();
                            break;
                        }
                        _ => {
                            children.push(self.value_type()?);
                        }
                    }
                }
                Ok(ValueType::Array(children))
            }
            Token::KeyTable(position) => {
                let tok = self.tokens.next()?.context(error::EofSnafu { position })?;
                ensure!(
                    matches!(tok, Token::LBrace(_)),
                    error::ExpectedSnafu {
                        position: tok.position(),
                        expected: "{",
                        got: tok.clone()
                    }
                );
                let mut children = IndexMap::new();
                while let Some(tok) = self.tokens.peek()? {
                    match tok {
                        Token::Comma(_) => {
                            self.tokens.discard();
                            continue;
                        }
                        Token::RBrace(_) => {
                            self.tokens.discard();
                            break;
                        }
                        _ => {
                            let id = self.tokens.next()?.context(error::EofSnafu {
                                position: self.tokens.position(),
                            })?;
                            let id = match id {
                                Token::Identifier(id) | Token::String(id) => Ok(id.1.clone()),
                                got => error::ExpectedSnafu {
                                    position: got.position(),
                                    expected: "identifier or string value",
                                    got: got.clone(),
                                }
                                .fail(),
                            }?;
                            let eq = self.tokens.next()?.context(error::EofSnafu {
                                position: self.tokens.position(),
                            })?;
                            ensure!(
                                matches!(eq, Token::Colon(_)),
                                error::ExpectedSnafu {
                                    position: eq.position(),
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
            Token::KeyBlock(position) => {
                let tok = self.tokens.next()?.context(error::EofSnafu { position })?;
                ensure!(
                    matches!(tok, Token::LBrace(_)),
                    error::ExpectedSnafu {
                        position: tok.position(),
                        expected: "{",
                        got: tok.clone()
                    }
                );
                let mut children = IndexMap::new();
                while let Some(tok) = self.tokens.peek()? {
                    match tok {
                        Token::Comma(_) => {
                            self.tokens.discard();
                            continue;
                        }
                        Token::RBrace(_) => {
                            self.tokens.discard();
                            break;
                        }
                        _ => {
                            let id = self.tokens.next()?.context(error::EofSnafu {
                                position: self.tokens.position(),
                            })?;
                            let id = match id {
                                Token::Identifier(id) | Token::String(id) => Ok(id.1.clone()),
                                got => error::ExpectedSnafu {
                                    position: got.position(),
                                    expected: "identifier or string value",
                                    got: got.clone(),
                                }
                                .fail(),
                            }?;
                            let eq = self.tokens.next()?.context(error::EofSnafu {
                                position: self.tokens.position(),
                            })?;
                            ensure!(
                                matches!(eq, Token::Colon(_)),
                                error::ExpectedSnafu {
                                    position: eq.position(),
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
            Token::KeySection(position) => {
                let tok = self.tokens.next()?.context(error::EofSnafu { position })?;
                ensure!(
                    matches!(tok, Token::LBrace(_)),
                    error::ExpectedSnafu {
                        position: tok.position(),
                        expected: "{",
                        got: tok.clone()
                    }
                );
                let mut children = IndexMap::new();
                while let Some(tok) = self.tokens.peek()? {
                    match tok {
                        Token::Comma(_) => {
                            self.tokens.discard();
                            continue;
                        }
                        Token::RBrace(_) => {
                            self.tokens.discard();
                            break;
                        }
                        _ => {
                            let id = self.tokens.next()?.context(error::EofSnafu {
                                position: self.tokens.position(),
                            })?;
                            let id = match id {
                                Token::Identifier(id) | Token::String(id) => Ok(id.1.clone()),
                                got => error::ExpectedSnafu {
                                    position: got.position(),
                                    expected: "identifier or string value",
                                    got: got.clone(),
                                }
                                .fail(),
                            }?;
                            let eq = self.tokens.next()?.context(error::EofSnafu {
                                position: self.tokens.position(),
                            })?;
                            ensure!(
                                matches!(eq, Token::Colon(_)),
                                error::ExpectedSnafu {
                                    position: eq.position(),
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
                position: self.tokens.position(),
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

        let token = self.tokens.next()?.context(error::EofSnafu {
            position: self.tokens.position(),
        })?;
        match token {
            Token::KeyNull(position) => Ok((
                Value::new_null(label, comment).context(error::ValueSnafu { position })?,
                ValueType::Null,
            )),
            Token::False(position) => Ok((
                Value::new_bool(false, label, comment).context(error::ValueSnafu { position })?,
                ValueType::Bool,
            )),
            Token::True(position) => Ok((
                Value::new_bool(true, label, comment).context(error::ValueSnafu { position })?,
                ValueType::Bool,
            )),
            Token::Int((position, Integer::Generic(value))) => Ok((
                Value::new_int(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::I64,
            )),
            Token::Int((position, Integer::I64(value))) => Ok((
                Value::new_i64(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::I64,
            )),
            Token::Int((position, Integer::I32(value))) => Ok((
                Value::new_i32(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::I32,
            )),
            Token::Int((position, Integer::I16(value))) => Ok((
                Value::new_i16(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::I16,
            )),
            Token::Int((position, Integer::I8(value))) => Ok((
                Value::new_i8(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::I8,
            )),
            Token::Int((position, Integer::U64(value))) => Ok((
                Value::new_u64(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::U64,
            )),
            Token::Int((position, Integer::U32(value))) => Ok((
                Value::new_u32(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::U32,
            )),
            Token::Int((position, Integer::U16(value))) => Ok((
                Value::new_u16(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::U16,
            )),
            Token::Int((position, Integer::U8(value))) => Ok((
                Value::new_u8(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::U8,
            )),
            Token::Float((position, HashableFloat::Generic(value))) => Ok((
                Value::new_float(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::F64,
            )),
            Token::Float((position, HashableFloat::Float32(value))) => Ok((
                Value::new_f32(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::F32,
            )),
            Token::Float((position, HashableFloat::Float64(value))) => Ok((
                Value::new_f64(value, label, comment).context(error::ValueSnafu { position })?,
                ValueType::F64,
            )),
            Token::String((position, value)) => Ok((
                Value::new_string(value.clone(), label, comment)
                    .context(error::ValueSnafu { position })?,
                ValueType::String,
            )),
            Token::MacroString((position, value)) => Ok((
                Value::new_macro(value.clone(), true, label, comment)
                    .context(error::ValueSnafu { position })?,
                ValueType::Macro,
            )),
            Token::MacroIdentifier((position, value)) => Ok((
                Value::new_macro(value.clone(), false, label, comment)
                    .context(error::ValueSnafu { position })?,
                ValueType::Macro,
            )),
            Token::ByteString((position, value)) => Ok((
                Value::new_bytes(value.clone(), label, comment)
                    .context(error::ValueSnafu { position })?,
                ValueType::Bytes,
            )),
            Token::Version((position, value)) => Ok((
                Value::new_version(value.clone(), label, comment)
                    .context(error::ValueSnafu { position })?,
                ValueType::Version,
            )),
            Token::Require((position, value)) => Ok((
                Value::new_require(value.clone(), label, comment)
                    .context(error::ValueSnafu { position })?,
                ValueType::Require,
            )),
            Token::LBracket(position) => {
                let mut children = Vec::new();
                let mut child_types = Vec::new();
                while let Some(token) = self.tokens.peek()? {
                    match token {
                        Token::Comma(_) => {
                            self.tokens.discard();
                            continue;
                        }
                        Token::RBracket(_) => {
                            self.tokens.discard();
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
                        .context(error::ValueSnafu { position })?,
                    ValueType::Array(child_types),
                ))
            }
            Token::LBrace(position) => {
                let mut children = IndexMap::new();
                let mut child_types = IndexMap::new();
                while let Some(token) = self.tokens.peek()? {
                    match token {
                        Token::Comma(_) => {
                            self.tokens.discard();
                            continue;
                        }
                        Token::RBrace(_) => {
                            self.tokens.discard();
                            break;
                        }
                        Token::Identifier((position, id)) | Token::String((position, id)) => {
                            let id = id.clone();
                            let next_token = self.tokens.next()?.context(error::EofSnafu {
                                position: position.clone(),
                            })?;
                            let vtype = if matches!(next_token, Token::Comma(_)) {
                                Some(self.value_type()?)
                            } else {
                                None
                            };
                            let eq_tok = self.tokens.next()?.context(error::EofSnafu {
                                position: position.clone(),
                            })?;
                            ensure!(
                                matches!(eq_tok, Token::Assign(_)),
                                error::ExpectedSnafu {
                                    position: eq_tok.position(),
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
                                position,
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
                        .context(error::ValueSnafu { position })?,
                    ValueType::Table(child_types),
                ))
            }
            _ => error::ExpectedSnafu {
                position: token.position(),
                expected: "value",
                got: token,
            }
            .fail(),
        }
    }

    fn statement(&mut self) -> Result<Value> {
        let comment = self.comment()?;

        let token = self.tokens.next()?.context(error::EofSnafu {
            position: self.tokens.position(),
        })?;
        match token {
            Token::ControlIdentifier((position, id)) => {
                let mut eq = self.tokens.next()?.context(error::EofSnafu {
                    position: position.clone(),
                })?;
                let type_ = if matches!(eq, Token::Colon(_)) {
                    let type_ = self.value_type()?;
                    eq = self.tokens.next()?.context(error::EofSnafu {
                        position: eq.position(),
                    })?;
                    Some(type_)
                } else {
                    None
                };
                ensure!(
                    matches!(eq, Token::Assign(_)),
                    error::ExpectedSnafu {
                        position: eq.position(),
                        expected: "=",
                        got: eq.clone(),
                    }
                );
                let (value, vtype) = self.value()?;
                if let Some(type_) = type_.as_ref() {
                    ensure!(
                        vtype == *type_,
                        error::TypeSnafu {
                            position: position.clone(),
                            vtype: vtype.clone(),
                            atype: type_.clone()
                        }
                    );
                }
                Value::new_control(id.clone(), value, comment, type_).context(error::ValueSnafu {
                    position: position.clone(),
                })
            }
            Token::Identifier((position, id)) | Token::String((position, id)) => {
                let front = self.tokens.peek()?.context(error::EofSnafu {
                    position: position.clone(),
                })?;
                if matches!(front, Token::Colon(_)) || matches!(front, Token::Assign(_)) {
                    let mut eq = self.tokens.next()?.context(error::EofSnafu {
                        position: position.clone(),
                    })?;
                    let type_ = if matches!(eq, Token::Colon(_)) {
                        let type_ = self.value_type()?;
                        eq = self.tokens.next()?.context(error::EofSnafu {
                            position: eq.position(),
                        })?;
                        Some(type_)
                    } else {
                        None
                    };
                    ensure!(
                        matches!(eq, Token::Assign(_)),
                        error::ExpectedSnafu {
                            position: eq.position(),
                            expected: "=",
                            got: eq.clone(),
                        }
                    );
                    let (value, vtype) = self.value()?;
                    if let Some(type_) = type_.as_ref() {
                        ensure!(
                            vtype == *type_,
                            error::TypeSnafu {
                                position: position.clone(),
                                vtype: vtype.clone(),
                                atype: type_.clone()
                            }
                        );
                    }
                    Value::new_assignment(id.clone(), value, comment, type_).context(
                        error::ValueSnafu {
                            position: position.clone(),
                        },
                    )
                } else {
                    let mut labels: Vec<String> = Vec::new();
                    while let Some(label) = self.tokens.peek()? {
                        match label {
                            Token::LBrace(_) => {
                                self.tokens.discard();
                                break;
                            }
                            Token::Identifier((_, id)) | Token::String((_, id)) => {
                                self.tokens.discard();
                                labels.push(id.clone());
                            }
                            _ => {
                                return error::OneOfSnafu {
                                    position: label.position(),
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
                    while let Some(stmt) = self.tokens.peek()? {
                        match stmt {
                            Token::RBrace(_) => {
                                self.tokens.discard();
                                break;
                            }
                            _ => {
                                let value = self.statement()?;
                                children.insert(value.inject_id().unwrap(), value);
                            }
                        }
                    }
                    Value::new_block(id.clone(), labels, children, comment).context(
                        error::ValueSnafu {
                            position: position.clone(),
                        },
                    )
                }
            }
            value => error::ExpectedSnafu {
                expected: "statement",
                got: value.clone(),
                position: value.position(),
            }
            .fail(),
        }
    }

    fn module(&mut self) -> Result<Value> {
        let mut children = IndexMap::new();
        while let Some(token) = self.tokens.peek()? {
            let comment = self.comment()?;
            match token {
                Token::LBracket(position) => {
                    self.tokens.discard();
                    let id = self.tokens.next()?.context(error::EofSnafu {
                        position: position.clone(),
                    })?;
                    let id = match id {
                        Token::Identifier((_, id)) => Ok(id),
                        Token::String((_, id)) => Ok(id),
                        value => error::ExpectedSnafu {
                            position: value.position(),
                            expected: "identifier or string",
                            got: value.clone(),
                        }
                        .fail(),
                    }?;
                    let close = self.tokens.next()?.context(error::EofSnafu {
                        position: self.tokens.position(),
                    })?;
                    ensure!(
                        matches!(close, Token::RBracket(_)),
                        error::ExpectedSnafu {
                            position: close.position(),
                            expected: "]",
                            got: close.clone()
                        }
                    );
                    let mut statements = IndexMap::new();
                    while let Some(stmt) = self.tokens.peek()? {
                        match stmt {
                            Token::LBracket(_) => break,
                            _ => {
                                let value = self.statement()?;
                                statements.insert(value.inject_id().unwrap(), value);
                            }
                        }
                    }
                    let child =
                        Value::new_section(id, statements, comment).context(error::ValueSnafu {
                            position: position.clone(),
                        })?;
                    children.insert(child.inject_id().unwrap(), child);
                }
                _ => {
                    let value = self.statement()?;
                    children.insert(value.inject_id().unwrap(), value);
                }
            }
        }
        Value::new_module(children, None).context(error::ValueSnafu {
            position: Position::default(),
        })
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
