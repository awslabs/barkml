use base64::Engine;
use snafu::ResultExt;
use std::collections::VecDeque;

use crate::value::Value;
use crate::{Float, Int};

peg::parser! {
    grammar parser() for str {
        rule sp() = ['\u{20}' | '\u{0B}']+
        rule nl() = ['\u{0A}' | '\u{0D}' ]+
        rule nl_or_eof() = ['\u{0A}' | '\u{0D}' | '\u{03}' | '\u{04}' | '\u{019}' |'\u{017}']
        rule ws() = quiet! { (sp() / nl())+ }

        rule text() -> &'input str
            = s:$(['\0'..='\u{1A}' | '\u{1C}'..='\u{10ffff}']) { s }
        rule text_no_nl() -> &'input str
            = s:$(['\0'..='\u{09}' | '\u{0B}' | '\u{0C}' | '\u{0E}'..='\u{10ffff}']) { s }
        rule eof()
         = ['\u{03}' | '\u{04}' | '\u{019}' |'\u{017}' ]

        rule null() -> Value
            = l:prefix()? ("null" / "Null" / "NULL" / "nil" / "Nil" / "NIL" / "none" / "None" / "NONE") { Value::Null(l) }

        rule positive() -> bool
            = ("true" / "True" / "TRUE" / "yes" / "Yes" / "YES" / "On" / "on" / "ON" ) { true }
        rule negative() -> bool
            = ("false" / "False" / "FALSE" / "no" / "No" / "NO" / "Off" / "off" / "OFF" ) { false }
        rule boolean() -> bool
            = b:(positive() / negative()) { b }
        rule bool_value() -> Value
            = l:prefix()? b:boolean() { Value::Bool(b, l) }

        // Integer Types
        rule int() -> Int
            = n:$("-"? ['0'..='9']+) { Int::I64(n.parse().or(Err("i64")).unwrap()) }
        rule i8() -> Int
            = n:$("-"? ['0'..='9']+) "i8" { Int::I8(n.parse().or(Err("i8")).unwrap()) }
        rule i16() -> Int
            = n:$("-"? ['0'..='9']+) "i16" { Int::I16(n.parse().or(Err("i16")).unwrap()) }
        rule i32() -> Int
            = n:$("-"? ['0'..='9']+) "i32" { Int::I32(n.parse().or(Err("i32")).unwrap()) }
        rule i64() -> Int
            = n:$("-"? ['0'..='9']+) "i64" { Int::I64(n.parse().or(Err("i64")).unwrap()) }
        rule u8() -> Int
            = n:$(['0'..='9']+) "u8" { Int::U8(n.parse().or(Err("u8")).unwrap()) }
        rule u16() -> Int
            = n:$(['0'..='9']+) "u16" { Int::U16(n.parse().or(Err("u16")).unwrap()) }
        rule u32() -> Int
            = n:$(['0'..='9']+) "u32" { Int::U32(n.parse().or(Err("u32")).unwrap()) }
        rule u64() -> Int
            = n:$(['0'..='9']+) "u64" { Int::U64(n.parse().or(Err("u64")).unwrap()) }

        rule float() -> Float
            = n:$("-"? ['0'..='9']+ "." ['0'..='9']* (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?) { Float::F64(n.parse().or(Err("f64")).unwrap()) }
        rule f32() -> Float
            = n:$("-"? ['0'..='9']+ "." ['0'..='9']* (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?) "f32" { Float::F32(n.parse().or(Err("f32")).unwrap()) }
        rule f64() -> Float
            = n:$("-"? ['0'..='9']+ "." ['0'..='9']* (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?) "f64" { Float::F64(n.parse().or(Err("f64")).unwrap()) }

        rule int_value() -> Value
            = l:prefix()? n:(
            int() /
            i8() /
            i16() /
            i32() /
            i64() /
            u8() /
            u16() /
            u32() /
            u64()
        ) { Value::Int(n, l) }
        rule float_value() -> Value
            = l:prefix()? n:(float() / f32() / f64() ) { Value::Float(n, l) }

        rule macro_string() -> String
            = "m'" s:$(['\0'..='\u{0026}' | '\u{0028}'..='\u{10ffff}']+) "'" {
            s.to_string()
        }
        rule macro_label() -> String
            = "m!" s:$(text_no_nl()+) {
            s.to_string()
        }
        rule macro_literal() -> Value
            = l:prefix()? s:macro_label()  { Value::Macro(s, l, false) }
        rule macro_value() -> Value
            = l:prefix()? s:macro_string() { Value::Macro(s, l, true) }

        rule byte_string() -> Vec<u8>
            = "b'" s:$(['\0'..='\u{0026}' | '\u{0028}'..='\u{10ffff}']+) "'" {?
            let encoded = s.to_string();
            base64::engine::general_purpose::URL_SAFE_NO_PAD.decode(encoded).or(Err("not base64 encoded"))
        }
        rule byte_value() -> Value
            = l:prefix()? b:byte_string() { Value::Bytes(b, l) }

        rule line_string() -> String
            = "'" s:$(['\0'..='\u{0026}' | '\u{0028}'..='\u{10ffff}']*) "'" { s.to_string() }
        rule double_string() -> String
            = "\"" s:$(['\0'..='\u{0021}' | '\u{0023}'..='\u{10ffff}']*) "\"" { s.to_string() }
        rule ident_string() -> String
            = s:$(['a'..='z' | 'A'..='Z' ] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_']*) {
            s.to_string()
        }
        rule label() -> String
            = "!" s:$(['a'..='z' | 'A'..='Z' ] ['a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' | ':']*) { s.to_string() }
        rule prefix() -> String
            = s:label() sp() { s }
        rule label_value() -> Value
            = s:label() { Value::Label(s) }

        rule string_value() -> Value
            = l:prefix()? s:(line_string() / double_string()) { Value::String(s, l) }

        rule array() -> Value
            = l:prefix()? "[" ws()? a:(value() ** (ws()? "," ws()?)) ws()? "]" { Value::Array(a, l) }

        rule keypair() -> (String, Value)
            = k:(line_string() / double_string() / ident_string()) ws()? "=" ws()? v:(value()) {
            (k, v)
        }
        rule table() -> Value
            = l:prefix()? "{" ws()? pairs:(keypair() ** (ws()? "," ws()?)) ws()? "}" {
            Value::Table(pairs.into_iter().collect(), l)
        }

        rule number() -> u64
            = n:$(['0'..='9']+) { n.parse().or(Err("u64")).unwrap() }
        rule alphanumeric() -> &'input str
            = s:$(['a'..='z'|'A'..='Z'|'0'..='9'|'-'|'.']+) { s }

        rule version_core() -> VecDeque<u64>
            = c:(number() **<3> ".") {
            c.into()
        }
        rule base_version() -> semver::Version
            = c:version_core() {
            let mut vector = c.clone();
            semver::Version::new(vector.pop_front().unwrap(), vector.pop_front().unwrap_or(0), vector.pop_front().unwrap_or(0))
        }
        rule prerelease() -> semver::Prerelease
            = "-" p:alphanumeric() {?
            semver::Prerelease::new(p).or(Err("bad prerelease"))
        }
        rule build() -> semver::BuildMetadata
            = "+" p:alphanumeric() {?
            semver::BuildMetadata::new(p).or(Err("bad build id"))
        }
        rule version() -> Value
            = l:prefix()? v:base_version() pre:prerelease()? build:build()? {
            let mut version = v.clone();
            if let Some(pre) = pre {
                version.pre = pre.clone();
            }
            if let Some(build) = build {
                version.build = build.clone();
            }
            Value::Version(version, l)
        }

        rule operator() -> semver::Op
            = s:$("=" / ">" / ">=" / "<" / "<=" / "~" / "^" ) {?
            match s {
                "=" => Ok(semver::Op::Exact),
                ">" => Ok(semver::Op::Greater),
                ">=" => Ok(semver::Op::GreaterEq),
                "<" => Ok(semver::Op::Less),
                "<=" => Ok(semver::Op::LessEq),
                "~" => Ok(semver::Op::Tilde),
                "^" => Ok(semver::Op::Caret),
                _ => Err("invalid operator for version requirement")
            }
        }
        rule comparitor_core() -> VecDeque<u64>
            = c:(number() **<0,3> ".") {
            c.into()
        }

        rule base_comparator() -> semver::Comparator
            = o:operator() c:comparitor_core() {
            let mut vector = c.clone();
            semver::Comparator {
                op: o,
                major: vector.pop_front().unwrap(),
                minor: vector.pop_front(),
                patch: vector.pop_front(),
                pre: semver::Prerelease::EMPTY,
            }
        }
        rule comparator() -> semver::Comparator
            = c:base_comparator() pre:prerelease()? {
            let mut comparator = c.clone();
            if let Some(pre) = pre {
                comparator.pre = pre;
            }
            comparator
        }
        rule require() -> Value
            = l:prefix()? c:(comparator() ++ ",") {
            Value::Require(semver::VersionReq { comparators: c }, l)
        }

        pub(crate) rule value() -> Value
            = v:(
                table() /
                array() /
                macro_value() /
                macro_literal() /
                byte_value() /
                string_value() /
                version() /
                require() /
                float_value() /
                int_value() /
                bool_value() /
                null() /
                label_value()) {
            v
        }


        rule comment() -> Value
            = "#" sp() s:$(text_no_nl()*) { Value::Comment(s.to_string()) }

        rule control() -> Value
            = "$" l:ident_string() ws()? "=" ws()? v:value() {
            Value::Control {
                label: l,
                value: Box::new(v),
            }
        }

        rule assignment() -> Value
            = l:(double_string() / line_string() / ident_string()) ws()? "=" ws()? v:value() {
            Value::Assignment {
                label: l,
                value: Box::new(v),
            }
        }

        rule block() -> Value
            = id:(ident_string()) ws()? labels:((double_string() / line_string()) ** ws()) ws()? "{" ws()? s:(statement_no_section() ** ws()) ws()? "}" {
            Value::Block {
                id,
                labels,
                statements: s,
            }
        }

        rule section() -> Value
            = "[" id:(ident_string() / double_string() / line_string()) "]" nl() s:(statement_no_section() ** ws()) {
            Value::Section {
                id,
                statements: s,
            }
        }

        rule statement_no_section() -> Value
            = c:(comment() / control() / assignment() / block() / value() ) { c }

        pub(crate) rule statement() -> Value
            = c:(comment() / section() / control() / assignment() / block() / value() ) { c }

        pub rule idl() -> Vec<Value> = ws()? s:(statement() ** ws()) ws()? {?
            Ok(join_statements(s))
        }
    }
}

pub(crate) fn parse(input: &str) -> crate::error::Result<Value> {
    Ok(Value::Module(
        parser::idl(input).context(crate::error::ParseSnafu)?,
    ))
}

fn join_statements(input: Vec<Value>) -> Vec<Value> {
    let mut output = Vec::new();
    let mut last_comment: Option<Value> = None;
    for stmt in input.iter() {
        match stmt {
            Value::Comment(s) => {
                if let Some(Value::Comment(l)) = last_comment {
                    last_comment = Some(Value::Comment([l.clone(), s.clone()].join("\n")));
                } else {
                    last_comment = Some(stmt.clone());
                }
            }
            _ => {
                if let Some(c) = last_comment {
                    output.push(c.clone());
                    last_comment = None;
                }
                output.push(stmt.clone());
            }
        }
    }
    if let Some(c) = last_comment {
        output.push(c);
    }
    output
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use assert_matches::assert_matches;

    use crate::{Float, Int};

    use super::parser::idl;

    #[test]
    fn test_idl_parsing() {
        let _expected = vec![
            crate::Value::Control {
                label: "version".to_string(),
                value: Box::new(crate::Value::String("1.0".to_string(), None)),
            },
            crate::Value::Section {
                id: "section".to_string(),
                statements: vec![
                    crate::Value::Comment("Documentation".to_string()),
                    crate::Value::Control {
                        label: "foo".to_string(),
                        value: Box::new(crate::Value::Int(Int::I64(3), None)),
                    },
                    crate::Value::Comment("Documentation".to_string()),
                    crate::Value::Assignment {
                        label: "foo".to_string(),
                        value: Box::new(crate::Value::Int(Int::I64(3), None)),
                    },
                    crate::Value::Comment("Documentation".to_string()),
                ],
            },
            crate::Value::Section {
                id: "section2".to_string(),
                statements: vec![crate::Value::Block {
                    id: "foo".to_string(),
                    labels: vec!["bar".to_string(), "baz".to_string()],
                    statements: vec![
                        crate::Value::Comment("Documentation".to_string()),
                        crate::Value::Block {
                            id: "nested".to_string(),
                            labels: vec!["bar".to_string()],
                            statements: vec![crate::Value::Assignment {
                                label: "bizness".to_string(),
                                value: Box::new(crate::Value::Float(Float::F64(3.14), None)),
                            }],
                        },
                    ],
                }],
            },
        ];
        assert_matches!(
            super::parser::idl(
                r#"
        $version = "1.0"
        [section]
        # Documentation
        $foo = 3
        # Documentation
        foo = 3
        # Documentation
        [section2]
        foo 'bar' "baz" {
            # Documentation
            nested 'bar' {
                'bizness' = 3.14
            }
        }
        "#
            )
            .unwrap(),
            _expected
        );
    }

    #[test]
    fn test_block_with_macro() {
        assert!(idl(r#"
macros = "Hello"
http "test" "this" {
    value = m'{macros}/whee'
}
"#)
        .is_ok());
    }

    #[test]
    fn test_statement_parsing() {
        let _expected = crate::Value::Control {
            label: "foo".to_string(),
            value: Box::new(crate::Value::Int(Int::I64(3), None)),
        };
        assert_matches!(super::parser::statement("$foo = 3").unwrap(), _expected);
        let _expected = crate::Value::Comment("Documentation".to_string());
        assert_matches!(
            super::parser::statement("# Documentation").unwrap(),
            _expected
        );
        let _expected = crate::Value::Assignment {
            label: "foo".to_string(),
            value: Box::new(crate::Value::Int(Int::I64(3), None)),
        };
        assert_matches!(super::parser::statement("foo = 3").unwrap(), _expected);
        assert_matches!(super::parser::statement("'foo' = 3").unwrap(), _expected);
        assert_matches!(super::parser::statement("\"foo\" = 3").unwrap(), _expected);
        let _expected = crate::Value::Block {
            id: "foo".to_string(),
            labels: vec!["bar".to_string(), "baz".to_string()],
            statements: vec![crate::Value::Comment("Documentation".to_string())],
        };
        assert_matches!(
            super::parser::statement("foo 'bar' \"baz\" {\n # Documentation\n }").unwrap(),
            _expected
        );
        let _expected = crate::Value::Section {
            id: "foo".to_string(),
            statements: vec![crate::Value::Comment("Documentation".to_string())],
        };
        assert_matches!(
            super::parser::statement("[foo]\n# Documentation").unwrap(),
            _expected
        );
    }

    macro_rules! value_parse_test {
        ($code: literal, $value: expr, $extract: ident) => {
            let left = super::parser::value($code).unwrap();
            let left = left.$extract().unwrap();
            assert_eq!(*left, $value);
        };

        (* $code: literal, $value: expr, $extract: ident) => {
            let left = super::parser::value($code).unwrap();
            let left = left.$extract().unwrap();
            assert_eq!(*left, $value);
        };

        ([ $code: literal, $value: expr, $extract: ident ]) => {
            let left = super::parser::value($code).unwrap();
            let array = left.as_array().unwrap();
            for entry in array {
                let lvalue = entry.$extract().unwrap();
                assert_eq!(*lvalue, $value);
            }
        };
        ({ $code: literal, $value: expr, $extract: ident }) => {
            let left = super::parser::value($code).unwrap();
            let table = left.as_table().unwrap();
            for (_, value) in table {
                let lvalue = value.$extract().unwrap();
                assert_eq!(*lvalue, $value);
            }
        };
    }

    #[test]
    fn test_value_parsing() {
        value_parse_test!("3", 3, as_i64);
        value_parse_test!("-3", -3, as_i64);
        value_parse_test!("3.14", 3.14, as_f64);
        value_parse_test!("-3.14", -3.14, as_f64);
        value_parse_test!(*"'foo'", "foo".to_string(), as_string);
        value_parse_test!(*"\"foo\"", "foo".to_string(), as_string);
        value_parse_test!(*"'\nfoobar\n'", "\nfoobar\n".to_string(), as_string);
        value_parse_test!(["[5, 5, 5]", 5, as_i64]);
        value_parse_test!({"{ foo = 5, 'bar' = 5, \"baz\" = 5 }", 5, as_i64 });
    }

    #[test]
    fn test_full_file() {
        let _expected = vec![
            crate::Value::Control {
                label: "schema".to_string(),
                value: Box::new(crate::Value::String(
                    "1.0.0".to_string(),
                    Some("Test".to_string()),
                )),
            },
            crate::Value::Section {
                id: "section-a".to_string(),
                statements: vec![
                    crate::Value::Comment("Documentation".to_string()),
                    crate::Value::Assignment {
                        label: "number".to_string(),
                        value: Box::new(crate::Value::Int(Int::I64(4), None)),
                    },
                    crate::Value::Assignment {
                        label: "float".to_string(),
                        value: Box::new(crate::Value::Float(Float::F64(3.14), None)),
                    },
                    crate::Value::Assignment {
                        label: "string".to_string(),
                        value: Box::new(crate::Value::String("foobar".to_string(), None)),
                    },
                    crate::Value::Assignment {
                        label: "array".to_string(),
                        value: Box::new(crate::Value::Array(
                            vec![
                                crate::Value::String("hello".to_string(), None),
                                crate::Value::Int(Int::I64(5), None),
                                crate::Value::Float(Float::F64(3.14), None),
                                crate::Value::String("single".to_string(), None),
                            ],
                            None,
                        )),
                    },
                    crate::Value::Assignment {
                        label: "object".to_string(),
                        value: Box::new(crate::Value::Table(
                            HashMap::from([
                                (
                                    "foo".to_string(),
                                    crate::Value::Bytes(b"binarystring".to_vec(), None),
                                ),
                                ("bar".to_string(), crate::Value::Int(Int::I64(4), None)),
                            ]),
                            None,
                        )),
                    },
                    crate::Value::Block {
                        id: "block".to_string(),
                        labels: vec!["block-a".to_string(), "label-a".to_string()],
                        statements: vec![crate::Value::Assignment {
                            label: "simple".to_string(),
                            value: Box::new(crate::Value::String("me".to_string(), None)),
                        }],
                    },
                ],
            },
            crate::Value::Section {
                id: "section-b".to_string(),
                statements: vec![
                    crate::Value::Comment("Documentation".to_string()),
                    crate::Value::Assignment {
                        label: "number".to_string(),
                        value: Box::new(crate::Value::Int(Int::I64(4), None)),
                    },
                    crate::Value::Assignment {
                        label: "float".to_string(),
                        value: Box::new(crate::Value::Float(Float::F64(3.14), None)),
                    },
                    crate::Value::Assignment {
                        label: "string".to_string(),
                        value: Box::new(crate::Value::String("foobar".to_string(), None)),
                    },
                    crate::Value::Assignment {
                        label: "array".to_string(),
                        value: Box::new(crate::Value::Array(
                            vec![
                                crate::Value::String("hello".to_string(), None),
                                crate::Value::Int(Int::I64(5), None),
                                crate::Value::Float(Float::F64(3.14), None),
                                crate::Value::String("single".to_string(), None),
                            ],
                            None,
                        )),
                    },
                    crate::Value::Assignment {
                        label: "object".to_string(),
                        value: Box::new(crate::Value::Table(
                            HashMap::from([
                                (
                                    "foo".to_string(),
                                    crate::Value::Bytes(b"binarystring".to_vec(), None),
                                ),
                                ("bar".to_string(), crate::Value::Int(Int::I64(4), None)),
                            ]),
                            None,
                        )),
                    },
                    crate::Value::Block {
                        id: "block".to_string(),
                        labels: vec!["block-a".to_string(), "label-a".to_string()],
                        statements: vec![
                            crate::Value::Assignment {
                                label: "simple".to_string(),
                                value: Box::new(crate::Value::String(
                                    "foobar bar".to_string(),
                                    None,
                                )),
                            },
                            crate::Value::Assignment {
                                label: "replacement".to_string(),
                                value: Box::new(crate::Value::Float(Float::F64(3.14), None)),
                            },
                        ],
                    },
                ],
            },
        ];
        let config_str = std::fs::read_to_string("examples/example.bml").unwrap();
        let config = super::parser::idl(config_str.as_str()).unwrap();
        assert_matches!(config, _expected);
    }
}
