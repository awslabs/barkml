#![allow(clippy::approx_constant)]
#![allow(clippy::from_str_radix_10)]

#[cfg(feature = "binary")]
use msgpack_simple::MsgPack;
#[cfg(feature = "binary")]
use snafu::ResultExt;

pub use loader::*;
pub use value::*;

pub mod error;
mod lang;
mod loader;
mod r#macro;
mod value;

/// Use standard loader settings to parse a BarkML file into a module
pub fn from_str(input: &str) -> error::Result<Value> {
    StandardLoader::default().source("unknown", input)?.load()
}

#[cfg(feature = "binary")]
/// Decode a barkml set of statements that are encoded in msgpack
pub fn decode(input: &[u8]) -> error::Result<Value> {
    let parent = MsgPack::parse(input).map_err(|e| error::Error::MsgPackEncoded {
        reason: e.to_string(),
    })?;
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

    #[cfg(feature = "binary")]
    use crate::{decode, encode, from_str};

    #[cfg(feature = "binary")]
    #[test]
    fn test_encode_decode() {
        let test_code = std::fs::read_to_string("examples/example.bml").unwrap();
        let stmts = from_str(test_code.as_str()).expect("failed to parse code");
        let encoded = encode(&stmts);
        assert!(!encoded.is_empty());
        decode(encoded.as_slice()).unwrap();
    }
}
