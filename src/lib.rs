pub mod error;
mod idl;
mod loader;
mod value;

pub use idl::from_str;
pub use idl::from_str_flat;
pub use value::*;

#[cfg(feature = "binary")]
use msgpack_simple::MsgPack;
#[cfg(feature = "binary")]
use snafu::ResultExt;

#[cfg(feature = "binary")]
/// Decode a barkml set of statements that are encoded in msgpack
pub fn decode(input: &[u8]) -> error::Result<Vec<Value>> {
    let parent = MsgPack::parse(input).context(error::MsgPackEncodedSnafu)?;
    let children = parent.as_array().context(error::MsgPackNotExpectedSnafu)?;
    let mut stmts = Vec::new();
    for entry in children.iter() {
        stmts.push(Value::from_binary(entry.clone())?);
    }
    Ok(stmts)
}

#[cfg(feature = "binary")]
/// Encode a barkml set of statements to msgpack
pub fn encode(input: &[Value]) -> Vec<u8> {
    let packed = MsgPack::Array(input.iter().map(|x| x.to_binary()).collect());
    packed.encode()
}

#[cfg(test)]
mod test {
    use crate::{decode, encode, from_str_flat};
    use std::fs::read_to_string;

    #[cfg(feature = "binary")]
    #[test]
    fn test_encode_decode() {
        let test_code = read_to_string("example.bml").unwrap();
        let stmts = from_str_flat(test_code.as_str()).unwrap();
        let encoded = encode(stmts.as_slice());
        assert!(!encoded.is_empty());
        let decoded = decode(encoded.as_slice()).unwrap();
    }
}
