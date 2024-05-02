#[cfg(feature = "binary")]
use msgpack_simple::MsgPack;
#[cfg(feature = "binary")]
use snafu::ResultExt;

pub use loader::*;
pub use value::*;

pub mod error;
mod idl;
mod loader;
mod r#macro;
mod value;

/// Use standard loader settings to parse a BarkML file into a module
pub fn from_str(input: &str) -> error::Result<Value> {
    StandardLoader::default().source(input)?.load()
}

#[cfg(feature = "binary")]
/// Decode a barkml set of statements that are encoded in msgpack
pub fn decode(input: &[u8]) -> error::Result<Value> {
    let parent = MsgPack::parse(input).context(error::MsgPackEncodedSnafu)?;
    Value::from_binary(parent)
}

#[cfg(feature = "binary")]
/// Encode a barkml set of statements to msgpack
pub fn encode(input: &Value) -> Vec<u8> {
    let packed = input.to_binary();
    packed.encode()
}

#[cfg(test)]
mod test {
    use std::fs::read_to_string;

    #[cfg(feature = "binary")]
    use crate::{decode, encode, from_str};

    #[cfg(feature = "binary")]
    #[test]
    fn test_encode_decode() {
        let test_code = read_to_string("examples/example.bml").unwrap();
        let stmts = from_str(test_code.as_str()).expect("failed to parse code");
        let encoded = encode(&stmts);
        assert!(!encoded.is_empty());
        decode(encoded.as_slice()).unwrap();
    }
}
