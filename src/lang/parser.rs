use crate::lang::{HashableFloat, Integer, Token};
use crate::{Float, Int, Value, ValueType};
use chumsky::prelude::*;
use std::collections::HashMap;

fn comment_parser() -> impl Parser<Token, Option<String>, Error = Simple<Token>> {
    recursive(|_| {
        let comment = select! {
            Token::LineComment(comment) => comment.clone(),
            Token::MultiLineComment(comment) => comment.clone(),
        };
        comment.or_not()
    })
}

fn label_parser() -> impl Parser<Token, Option<String>, Error = Simple<Token>> {
    recursive(|_| {
        let label = select! {
            Token::LabelIdentifier(label) => label.clone()
        };
        label.or_not()
    })
}

fn assignment_identifier() -> impl Parser<Token, String, Error = Simple<Token>> {
    recursive(|_| {
        select! {
            Token::Identifier(id) => id,
            Token::String(id) => id,
        }
    })
}

fn statement_id() -> impl Parser<Token, String, Error = Simple<Token>> {
    recursive(|_| {
        select! {
            Token::Identifier(id) => id,
        }
    })
}

fn labels() -> impl Parser<Token, Vec<String>, Error = Simple<Token>> {
    recursive(|_| assignment_identifier().separated_by(just(Token::Comma)))
}

fn identifier() -> impl Parser<Token, String, Error = Simple<Token>> {
    recursive(|_| {
        select! {
            Token::Identifier(id) => id,
            Token::ControlIdentifier(id) => id,
            Token::LabelIdentifier(id) => id,
        }
    })
}

fn value_parser() -> impl Parser<Token, Value, Error = Simple<Token>> {
    recursive(|ast| {
        let atom = comment_parser().then(label_parser()).then_with(|(comment, label)| {
            select! {
                Token::Int(Integer::Generic(value)) => Value::new_int(value, label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::I64(value)) => Value::new_precise_int(Int::I64(value), label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::I32(value)) => Value::new_precise_int(Int::I32(value), label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::I16(value)) => Value::new_precise_int(Int::I16(value), label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::I8(value)) => Value::new_precise_int(Int::I8(value), label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::U64(value)) => Value::new_precise_int(Int::U64(value), label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::U32(value)) => Value::new_precise_int(Int::U32(value), label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::U16(value)) => Value::new_precise_int(Int::U16(value), label.clone(), comment.clone()).unwrap(),
                Token::Int(Integer::U8(value)) => Value::new_precise_int(Int::U8(value), label.clone(), comment.clone()).unwrap(),
                Token::Float(HashableFloat::Generic(value)) => Value::new_float(value, label.clone(), comment.clone()).unwrap(),
                Token::Float(HashableFloat::Float64(value)) => Value::new_precise_float(Float::F64(value), label.clone(), comment.clone()).unwrap(),
                Token::Float(HashableFloat::Float32(value)) => Value::new_precise_float(Float::F32(value), label.clone(), comment.clone()).unwrap(),
                Token::String(value) => Value::new_string(value, label.clone(), comment.clone()).unwrap(),
                Token::MacroString(value) => Value::new_macro(value, true, label.clone(), comment.clone()).unwrap(),
                Token::MacroIdentifier(value) => Value::new_macro(value, false, label.clone(), comment.clone()).unwrap(),
                Token::ByteString(value) => Value::new_bytes(value, label.clone(), comment.clone()).unwrap(),
                Token::Version(value) => Value::new_version(value, label.clone(), comment.clone()).unwrap(),
                Token::Require(value) => Value::new_require(value, label.clone(), comment.clone()).unwrap(),
            }
        });
        let array = comment_parser()
            .then(label_parser())
            .then(
                ast.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket).ignored()),
            )
            .map(|((comment, label), entries)| {
                Value::new_array(entries, label.clone(), comment.clone()).unwrap()
            });

        let member = assignment_identifier()
            .then_ignore(just(Token::Assign))
            .then(ast);
        let object = comment_parser()
            .then(label_parser())
            .then(
                member
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map(|((comment, label), entries)| {
                Value::new_table(HashMap::from_iter(entries), label.clone(), comment.clone())
                    .unwrap()
            });
        object.or(array).or(atom)
    })
}

fn control_id() -> impl Parser<Token, String, Error = Simple<Token>> {
    recursive(|_| {
        select! {
            Token::ControlIdentifier(id) => id.clone()
        }
    })
}

fn type_id() -> impl Parser<Token, ValueType, Error = Simple<Token>> {
    recursive(|ast| {
        let atom = select! {
            Token::KeyString => ValueType::String,
            Token::KeyInt => ValueType::I64,
            Token::KeyInt8 => ValueType::I8,
            Token::KeyInt16 => ValueType::I16,
            Token::KeyInt32 => ValueType::I32,
            Token::KeyInt64 => ValueType::I64,
            Token::KeyUInt8 => ValueType::U8,
            Token::KeyUInt16 => ValueType::U16,
            Token::KeyUInt32 => ValueType::U32,
            Token::KeyUInt64 => ValueType::U64,
            Token::KeyBool => ValueType::Bool,
            Token::KeyFloat => ValueType::F64,
            Token::KeyFloat32 => ValueType::F32,
            Token::KeyFloat64 => ValueType::F64,
            Token::KeyBytes => ValueType::Bytes,
            Token::KeyVersion => ValueType::Version,
            Token::KeyRequire => ValueType::Require,
            Token::KeyLabel => ValueType::Label,
        };

        let list = just(Token::KeyArray)
            .ignore_then(
                ast.clone()
                    .separated_by(just(Token::Comma))
                    .delimited_by(just(Token::LBracket), just(Token::RBracket)),
            )
            .map(ValueType::Array);

        let member = assignment_identifier()
            .then_ignore(just(Token::Colon))
            .then(ast.clone());

        let table = just(Token::KeyTable)
            .ignore_then(
                member
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map(|x| ValueType::Table(HashMap::from_iter(x)));
        let member = assignment_identifier()
            .then_ignore(just(Token::Colon))
            .then(ast.clone());
        let block = just(Token::KeyBlock)
            .ignore_then(
                member
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map(|x| ValueType::Block(HashMap::from_iter(x)));
        let member = assignment_identifier()
            .then_ignore(just(Token::Colon))
            .then(ast.clone());
        let section = just(Token::KeySection)
            .ignore_then(
                member
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map(|x| ValueType::Section(HashMap::from_iter(x)));
        section.or(block).or(table).or(list).or(atom)
    })
}

fn statement_parser() -> impl Parser<Token, Value, Error = Simple<Token>> {
    recursive(|ast| {
        let control = comment_parser()
            .then(control_id())
            .then(just(Token::Colon).ignore_then(type_id()).or_not())
            .then_ignore(just(Token::Assign))
            .then(value_parser())
            .map(|(((comment, id), ty), value)| {
                Value::new_control(id, value, comment, ty).unwrap()
            });
        let assignment = comment_parser()
            .then(assignment_identifier())
            .then(just(Token::Colon).ignore_then(type_id()).or_not())
            .then_ignore(just(Token::Assign))
            .then(value_parser())
            .map(|(((comment, id), ty), value)| {
                Value::new_assignment(id, value, comment, ty).unwrap()
            });

        let block = comment_parser()
            .then(assignment_identifier())
            .then(labels())
            .then(
                ast.separated_by(just(Token::NewLine))
                    .allow_trailing()
                    .allow_leading()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map(|x| {
                        x.iter()
                            .map(|y: &Value| (y.inject_id().unwrap(), y.clone()))
                            .collect::<Vec<(String, Value)>>()
                    }),
            )
            .map(|(((comment, id), labels), stmts)| {
                Value::new_block(
                    id.clone(),
                    labels.clone(),
                    HashMap::from_iter(stmts),
                    comment,
                )
                .unwrap()
            });
        block.or(control).or(assignment)
    })
}

fn section_parser() -> impl Parser<Token, Value, Error = Simple<Token>> {
    recursive(|_| {
        comment_parser()
            .then(
                assignment_identifier().delimited_by(just(Token::LBracket), just(Token::RBracket)),
            )
            .then(
                statement_parser()
                    .separated_by(just(Token::NewLine))
                    .allow_leading()
                    .allow_trailing()
                    .map(|x| {
                        x.iter()
                            .map(|y| (y.inject_id().unwrap(), y.clone()))
                            .collect::<Vec<(String, Value)>>()
                    }),
            )
            .map(|((comment, id), stmts)| {
                Value::new_section(id.clone(), HashMap::from_iter(stmts), comment).unwrap()
            })
            .or(statement_parser())
    })
}

pub fn module_parser() -> impl Parser<Token, Value, Error = Simple<Token>> {
    recursive(|_| {
        comment_parser()
            .then(
                section_parser()
                    .separated_by(just(Token::NewLine))
                    .allow_leading()
                    .allow_trailing(),
            )
            .map(|(comment, stmts)| {
                let children = stmts.iter().map(|x| (x.inject_id().unwrap(), x.clone()));
                Value::new_module(HashMap::from_iter(children), comment).unwrap()
            })
    })
}

#[cfg(test)]
mod test {
    use crate::lang::parser::{comment_parser, label_parser};
    use crate::lang::Token;
    use crate::ValueType;
    use chumsky::{Parser, Span, Stream};
    use logos::Logos;
    use std::collections::HashMap;

    macro_rules! streamify {
        ($input: expr) => {{
            let token_iter = Token::lexer($input).spanned().map(|(tok, span)| match tok {
                // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
                // to work with
                Ok(tok) => {
                    println!("token: {:?}", tok);
                    (tok, span.into())
                },
                Err(()) => (Token::Error, span.into()),
            });
            Stream::from_iter(($input.len()..$input.len()).into(), token_iter)
        }};
    }

    #[test]
    fn comment() {
        let input = "# Line Comment\n";
        let stream = streamify!(input);
        assert_eq!(
            comment_parser().parse(stream).unwrap(),
            Some("Line Comment".into()),
            "verify line comment"
        );
        let input = "/*\n Multiline\n Comment \n*/";
        let stream = streamify!(input);
        assert_eq!(
            comment_parser().parse(stream).unwrap(),
            Some("Multiline\n Comment".into()),
            "verify multi-line comments"
        );
    }

    #[test]
    fn label() {
        let input = "!Label";
        let stream = streamify!(input);
        assert_eq!(
            label_parser().parse(stream).unwrap(),
            Some("Label".into()),
            "verify label"
        );
    }

    #[test]
    fn assignment_identifier() {
        let input = "foo";
        let stream = streamify!(input);
        assert_eq!(
            super::assignment_identifier().parse(stream).unwrap(),
            "foo",
            "verify assignment identifier"
        );
        let input = "\"foo\"";
        let stream = streamify!(input);
        assert_eq!(
            super::assignment_identifier().parse(stream).unwrap(),
            "foo",
            "verify assignment string identifier"
        );
    }

    #[test]
    fn statement_id() {
        let input = "foo";
        let stream = streamify!(input);
        assert_eq!(
            super::statement_id().parse(stream).unwrap(),
            "foo",
            "verify statement_id"
        );
    }

    #[test]
    fn labels() {
        let input = "foo, \"bar\", baz";
        let tokens = Token::lexer(input).collect::<Vec<_>>();
        let stream = streamify!(input);
        assert_eq!(
            super::labels().parse(stream).unwrap(),
            vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]
        );
    }

    #[test]
    fn identifier() {
        let input = ["foo", "$foo", "!foo"];
        for code in input {
            let stream = streamify!(code);
            assert_eq!(super::identifier().parse(stream).unwrap(), "foo");
        }
    }

    #[test]
    fn integers() {
        let input = "-3_14";
        let stream = streamify!(input);
        assert_eq!(
            super::value_parser()
                .parse(stream)
                .unwrap()
                .as_int()
                .unwrap(),
            -314
        );
        let input = "-3_14i64";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_i64()
                .unwrap(),
            -314
        );
        let input = "0xAAFF";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_i64()
                .unwrap(),
            0xAAFF
        );
        let input = "0o14";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_i64()
                .unwrap(),
            0o14
        );
        let input = "0b1011u32";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_u32()
                .unwrap(),
            0b1011u32
        );
    }

    #[test]
    fn floats() {
        let input = "3.14";
        let stream = streamify!(input);
        assert_eq!(
            super::value_parser()
                .parse(stream)
                .unwrap()
                .as_float()
                .unwrap(),
            3.14
        );
        let input = "3.14f32";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_f32()
                .unwrap(),
            3.14f32
        );
    }

    #[test]
    fn string() {
        let input = "'Hello World'";
        let stream = streamify!(input);
        assert_eq!(
            super::value_parser()
                .parse(stream)
                .unwrap()
                .as_string()
                .unwrap(),
            "Hello World"
        );
        let input = "\"Hello World\"";
        let stream = streamify!(input);
        assert_eq!(
            super::value_parser()
                .parse(stream)
                .unwrap()
                .as_string()
                .unwrap(),
            "Hello World"
        );
    }

    #[test]
    fn macros() {
        let input = "m'foo'";
        let stream = streamify!(input);
        assert_eq!(
            super::value_parser()
                .parse(stream)
                .unwrap()
                .as_macro()
                .unwrap(),
            "foo"
        );
        let input = "m!foo";
        let stream = streamify!(input);
        assert_eq!(
            super::value_parser()
                .parse(stream)
                .unwrap()
                .as_macro()
                .unwrap(),
            "foo"
        );
    }

    #[test]
    fn versions() {
        let input = "1.2.4";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_version()
                .unwrap(),
            semver::Version::new(1, 2, 4)
        );
        let input = ">3.1";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_require()
                .unwrap(),
            semver::VersionReq::parse(">3.1").unwrap()
        );
    }

    #[test]
    fn arrays() {
        let input = "[1, 2, 3]";
        let stream = streamify!(input);
        assert_eq!(
            *super::value_parser()
                .parse(stream)
                .unwrap()
                .as_array()
                .unwrap()
                .iter()
                .map(|x| x.as_int().unwrap())
                .collect::<Vec<_>>(),
            vec![1, 2, 3]
        );
    }

    #[test]
    fn tables() {
        let input = "{one = 1, 'two' = 2}";
        let stream = streamify!(input);
        let result = super::value_parser().parse(stream).unwrap();
        let table = result.as_table().unwrap();
        assert_eq!(table.get("one").unwrap().as_int().unwrap(), 1);
        assert_eq!(table.get("two").unwrap().as_int().unwrap(), 2);
    }

    #[test]
    fn types() {
        let cases = [
            ("string", ValueType::String),
            ("int", ValueType::I64),
            ("i64", ValueType::I64),
            ("i32", ValueType::I32),
            ("i16", ValueType::I16),
            ("i8", ValueType::I8),
            ("u64", ValueType::U64),
            ("u32", ValueType::U32),
            ("u16", ValueType::U16),
            ("u8", ValueType::U8),
            ("float", ValueType::F64),
            ("f64", ValueType::F64),
            ("f32", ValueType::F32),
            ("bytes", ValueType::Bytes),
            ("version", ValueType::Version),
            ("require", ValueType::Require),
            ("label", ValueType::Label),
            (
                "array[string, int, float]",
                ValueType::Array(vec![ValueType::String, ValueType::I64, ValueType::F64]),
            ),
            (
                "table{one: string, two: float}",
                ValueType::Table(HashMap::from_iter([
                    ("one".to_string(), ValueType::String),
                    ("two".to_string(), ValueType::F64),
                ])),
            ),
        ];
        for (input, ty) in cases {
            let stream = streamify!(input);
            assert_eq!(super::type_id().parse(stream).unwrap(), ty);
        }
    }

    #[test]
    fn control_statement() {
        let input = "$control = 3.14";
        let stream = streamify!(input);
        assert!(super::statement_parser().parse(stream).is_ok());
        let input = "$control: f32 = 3.14f32";
        let stream = streamify!(input);
        assert!(super::statement_parser().parse(stream).is_ok());
    }

    #[test]
    fn assign_statement() {
        let input = "assign = 3.14";
        let stream = streamify!(input);
        assert!(super::statement_parser().parse(stream).is_ok());
        let input = "assign: f32 = 3.14f32";
        let stream = streamify!(input);
        assert!(super::statement_parser().parse(stream).is_ok());
    }

    #[test]
    fn block_statement() {
        let input = r##"test "label", leb {
            assign = 3.14
        }"##;
        let stream = streamify!(input);
        super::statement_parser().parse(stream).unwrap();
    }

    #[test]
    fn section_statement() {
        let input = r##"[name]
        package = "hello"
        test "label" {
            assign = 3.14
        }
        "##;
        let stream = streamify!(input);
        super::section_parser().parse(stream).unwrap();
    }

    #[test]
    fn module_statement() {
        let input = r##"
        $control = !Schema 1.0.0
        [section-1]
        package = "hello"
        version = 1.2.3
        [section-2]
        scale = 2
        depends "build" {
            name = "fire"
            with = >3.4
        }
        "##;
        let stream = streamify!(input);
        super::module_parser().parse(stream).unwrap();
    }

    #[test]
    fn file_test() {
        let code = std::fs::read_to_string("examples/example.bml").unwrap();
        let stream = streamify!(code.as_str());
        super::module_parser().parse(stream).unwrap();
    }
}
