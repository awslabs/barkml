use std::collections::{HashMap, VecDeque};

use base64::Engine;
use snafu::ResultExt;

use crate::value::{Value, ValueType};
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
            = l:prefix()? ("null" / "Null" / "NULL" / "nil" / "Nil" / "NIL" / "none" / "None" / "NONE") {? Value::new_null(l, None).or(Err("value definition")) }

        rule positive() -> bool
            = ("true" / "True" / "TRUE" / "yes" / "Yes" / "YES" / "On" / "on" / "ON" ) { true }
        rule negative() -> bool
            = ("false" / "False" / "FALSE" / "no" / "No" / "NO" / "Off" / "off" / "OFF" ) { false }
        rule boolean() -> bool
            = b:(positive() / negative()) { b }
        rule bool_value() -> Value
            = l:prefix()? b:boolean() {? Value::new_bool(b, l, None).or(Err("value definition")) }

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

        rule floating() -> f64
            = n:$("-"? ['0'..='9']+ "." ['0'..='9']* (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?)  {? n.parse().or(Err("floating point")) }
        rule float() -> Float
            = n:floating() { Float::F64(n) }
        rule f32() -> Float
            = n:floating() "f32" { Float::F32(n as f32) }
        rule f64() -> Float
            = n:floating() "f64" { Float::F64(n) }

        rule int_value() -> Value
            = l:prefix()? n:(
            i8() /
            i16() /
            i32() /
            i64() /
            u8() /
            u16() /
            u32() /
            u64() /
            int()
        ) {? Value::new_precise_int(n, l, None).or(Err("value definition")) }
        rule float_value() -> Value
            = l:prefix()? n:(f32() / f64() / float() )  {?
            Value::new_precise_float(n, l, None).or(Err("value definition")) }

        rule macro_string() -> String
            = "m'" s:$(['\0'..='\u{0026}' | '\u{0028}'..='\u{10ffff}']+) "'" {
            s.to_string()
        }
        rule macro_label() -> String
            = "m!" s:$(text_no_nl()+) {
            s.to_string()
        }
        rule macro_literal() -> Value
            = l:prefix()? s:macro_label() {? Value::new_macro(s, false, l, None).or(Err("value definition")) }
        rule macro_value() -> Value
            = l:prefix()? s:macro_string() {? Value::new_macro(s, true, l, None).or(Err("value definition")) }

        rule byte_string() -> Vec<u8>
            = "b'" s:$(['\0'..='\u{0026}' | '\u{0028}'..='\u{10ffff}']+) "'" {?
            let encoded = s.to_string();
            base64::engine::general_purpose::URL_SAFE_NO_PAD.decode(encoded).or(Err("not base64 encoded"))
        }
        rule byte_value() -> Value
            = l:prefix()? b:byte_string() {? Value::new_bytes(b, l, None).or(Err("value definition")) }

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
            = s:label() {? Value::new_label(s, None).or(Err("Value declaration")) }

        rule string_value() -> Value
            = l:prefix()? s:(line_string() / double_string()) {? Value::new_string(s, l, None).or(Err("value definition")) }

        rule array() -> Value
            = l:prefix()? "[" ws()? a:(value() ** (ws()? "," ws()?)) ws()? "]" {? Value::new_array(a, l, None).or(Err("value definition")) }

        rule keypair() -> (String, Value)
            = k:(line_string() / double_string() / ident_string()) ws()? "=" ws()? v:(value()) {
            (k, v)
        }
        rule table() -> Value
            = l:prefix()? "{" ws()? pairs:(keypair() ** (ws()? "," ws()?)) ws()? "}" {?
            Value::new_table(pairs.into_iter().collect(), l, None).or(Err("value definition"))
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
            = l:prefix()? v:base_version() pre:prerelease()? build:build()? {?
            let mut version = v.clone();
            if let Some(pre) = pre {
                version.pre = pre.clone();
            }
            if let Some(build) = build {
                version.build = build.clone();
            }
            Value::new_version(version, l, None).or(Err("value definition"))
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
            = l:prefix()? c:(comparator() ++ ",") {?
            Value::new_require(semver::VersionReq { comparators: c }, l, None).or(Err("value definition"))
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

        // Type definitions!
        rule string_type() -> ValueType = "string" { ValueType::String }
        rule int_type() -> ValueType = "int" { ValueType::I64 }
        rule i8_type() -> ValueType = "i8" { ValueType::I8 }
        rule i16_type() -> ValueType = "i16" { ValueType::I16 }
        rule i32_type() -> ValueType = "i32" { ValueType::I32 }
        rule i64_type() -> ValueType = "i64" { ValueType::I64 }
        rule u8_type() -> ValueType = "u8" { ValueType::U8 }
        rule u16_type() -> ValueType = "u16" { ValueType::U16 }
        rule u32_type() -> ValueType = "u32" { ValueType::U32 }
        rule u64_type() -> ValueType = "u64" { ValueType::U64 }
        rule float_type() -> ValueType = "float" { ValueType::F64 }
        rule f32_type() -> ValueType = "f32" { ValueType::F32 }
        rule f64_type() -> ValueType = "f64" { ValueType::F64 }
        rule bytes_type() -> ValueType = "bytes" { ValueType::Bytes }
        rule bool_type() -> ValueType = "bool" { ValueType::Bool }
        rule version_type() -> ValueType = "version" { ValueType::Version }
        rule require_type() -> ValueType = "require" { ValueType::Require }
        rule label_type() -> ValueType = "label" { ValueType::Label }

        rule simple_type() -> ValueType = t:(
            string_type() /
            int_type() /
            i8_type() /
            i16_type() /
            i32_type() /
            i64_type() /
            u8_type() /
            u16_type() /
            u32_type() /
            u64_type() /
            float_type() /
            f32_type() /
            f64_type() /
            bytes_type() /
            bool_type() /
            version_type() /
            require_type() /
            label_type()
        ) { t }

        rule array_type() -> ValueType =
            "array" ws()? "[" ws()? st:(type_name() ** ",") ws()? "]" {
            ValueType::Array(st)
        }
        rule subtype_def() -> (String, ValueType) =
            key:ident_string() ws()? value:type_def()  {
            (key, value)
        }
        rule table_type() -> ValueType =
            "table" ws()? "{" ws()? st:(subtype_def() ** ("," ws()?)) "}" ws()? {
            ValueType::Table(st.iter().cloned().collect())
        }
        rule section_type() -> ValueType =
            "section" ws()? "{" ws()? st:(subtype_def() ** ("," ws()?)) "}" ws()? {
            ValueType::Section(st.iter().cloned().collect())
        }
        rule block_type() -> ValueType =
            "block" ws()? "{" ws()? st:(subtype_def() ** ("," ws()?)) "}" ws()? {
            ValueType::Block(st.iter().cloned().collect())
        }
        rule module_type() -> ValueType =
            "module" ws()? "{" ws()? st:(subtype_def() ** ("," ws()?)) "}" ws()? {
            ValueType::Module(st.iter().cloned().collect())
        }

        rule type_name() -> ValueType
            = t:(simple_type() / array_type() / table_type() / section_type() / block_type() / module_type()) { t }
        rule type_def() -> ValueType
            = ":" ws()? t:type_name() {
            t
        }

        rule line_comment() -> String
            = "#" sp() s:$(text_no_nl()*) nl() { s.trim().to_string() }
        rule comment() -> String
            = s:line_comment() { s }

        rule control() -> (String, Value)
            = "$" l:ident_string() ws()? t:type_def()? ws()? "=" ws()? v:value() {?
            Ok((l.clone(), Value::new_control(l.clone(), v, None, t).or(Err("statement declaration"))?))
        }

        rule assignment() -> (String, Value)
            = l:(double_string() / line_string() / ident_string()) ws()? t:type_def()? ws()? "=" ws()? v:value() {?
            Ok((l.clone(), Value::new_assignment(l.clone(), v, None, t).or(Err("statement declaration"))?))
        }

        rule block() -> (String, Value)
            = id:(ident_string()) ws()? labels:((double_string() / line_string()) ** ws()) ws()? "{" ws()? s:(statement_no_section() ** ws()) ws()? "}" {?
            Ok((id.clone(), Value::new_block(id.clone(), labels, s.iter().cloned().collect(), None).or(Err("statement declaration"))?))
        }

        rule section() -> (String, Value)
            = "[" id:(ident_string() / double_string() / line_string()) "]" nl() s:(statement_no_section() ** ws()) {?
            Ok((id.clone(), Value::new_section(id, s.iter().cloned().collect(), None).or(Err("statement declaration"))?))
        }

        rule statement_no_section() -> (String, Value)
            = com:comment()? ws()? c:(control() / assignment() / block() ) {
            let mut value = c.clone();
            value.1.comment = com;
            value
        }

        pub(crate) rule statement() -> (String, Value)
            = com:comment()? ws()? c:(section() / control() / assignment() / block() ) {
            let mut value = c.clone();
            value.1.comment = com;
            value
        }

        pub rule idl() -> HashMap<String, Value> = ws()? s:(statement() ** ws()) ws()? {
            s.iter().cloned().collect()
        }
    }
}

pub(crate) fn parse(input: &str) -> crate::error::Result<Value> {
    Value::new_module(parser::idl(input).context(crate::error::ParseSnafu)?, None)
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use assert_matches::assert_matches;

    use crate::{Float, Int, Value, ValueType};

    use super::parser::idl;

    #[test]
    fn test_idl_parsing() {
        let _expected = HashMap::from([
            (
                "version".to_string(),
                Value::new_control(
                    "version".into(),
                    Value::new_string("1.0".into(), None, None).unwrap(),
                    None,
                    None,
                )
                .unwrap(),
            ),
            (
                "section".to_string(),
                Value::new_section(
                    "section".into(),
                    HashMap::from([
                        (
                            "foo".to_string(),
                            Value::new_control(
                                "foo".into(),
                                Value::new_int(3, None, None).unwrap(),
                                Some("Documentation".to_string()),
                                Some(ValueType::I64),
                            )
                            .unwrap(),
                        ),
                        (
                            "bar".to_string(),
                            Value::new_assignment(
                                "bar".into(),
                                Value::new_precise_int(Int::I16(3), None, None).unwrap(),
                                Some("Documentation".to_string()),
                                None,
                            )
                            .unwrap(),
                        ),
                    ]),
                    None,
                )
                .unwrap(),
            ),
        ]);
        assert_matches!(
            super::parser::idl(
                r#"
        $version = "1.0"
        [section]
        # Documentation
        $foo: int = 3
        # Documentation
        bar = 3i16
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
        let _expected = Value::new_control(
            "foo".into(),
            Value::new_int(3, None, None).unwrap(),
            None,
            None,
        )
        .unwrap();
        assert_matches!(super::parser::statement("$foo = 3").unwrap(), _expected);
        let _expected = Value::new_assignment(
            "foo".into(),
            Value::new_precise_float(Float::F32(3.14), None, Some("PI".to_string())).unwrap(),
            Some("Documented".to_string()),
            Some(ValueType::F32),
        )
        .unwrap();
        assert_matches!(
            super::parser::statement("# Documented\n'foo' : f32 = 3.14f32").unwrap(),
            _expected
        );
        let _expected = Value::new_block(
            "foo".into(),
            vec!["bar".to_string(), "baz".to_string()],
            HashMap::new(),
            None,
        )
        .unwrap();

        assert_matches!(
            super::parser::statement("foo 'bar' \"baz\" {\n}").unwrap(),
            _expected
        );
        let _expected = Value::new_section(
            "foo".into(),
            HashMap::new(),
            Some("Documentation".to_string()),
        )
        .unwrap();
        assert_matches!(
            super::parser::statement("# Documentation\n[foo]\n").unwrap(),
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
        let config_str = std::fs::read_to_string("examples/example.bml").unwrap();
        let _ = super::parser::idl(config_str.as_str()).unwrap();
    }
}
