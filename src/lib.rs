use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct CompactSize {
    pub value: u64,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BitcoinError {
    InsufficientBytes,
    InvalidFormat,
}

impl CompactSize {
    pub fn new(value: u64) -> Self {
        // TODO: Construct a CompactSize from a u64 value
        Self { value }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: Encode according to Bitcoin's CompactSize format:
        // [0x00–0xFC] => 1 byte
        // [0xFDxxxx] => 0xFD + u16 (2 bytes)
        // [0xFExxxxxxxx] => 0xFE + u32 (4 bytes)
        // [0xFFxxxxxxxxxxxxxxxx] => 0xFF + u64 (8 bytes)
        match self.value {
            0..=0xFC => vec![self.value as u8],
            0xFD..=0xFFFF => {
                let mut bytes = vec![0xFD];
                bytes.extend_from_slice(&(self.value as u16).to_le_bytes());
                bytes
    }
            0x10000..=0xFFFFFFFF => {
                let mut bytes = vec![0xFE];
                bytes.extend_from_slice(&(self.value as u32).to_le_bytes());
                bytes
            }
            _ => {
                let mut bytes = vec![0xFF];
                bytes.extend_from_slice(&self.value.to_le_bytes());
                bytes
            }
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        // TODO: Decode CompactSize, returning value and number of bytes consumed.
        // First check if bytes is empty.
        // Check that enough bytes are available based on prefix.
        match bytes {
            [] => Err(BitcoinError::InvalidFormat),
            [value @ 0x00..=0xFC, ..] => Ok((CompactSize::new(*value as u64), 1)),
            [0xFD, rest @ ..] => {
                let value_bytes = rest.get(0..2).ok_or(BitcoinError::InsufficientBytes)?;
                let value = u16::from_le_bytes(value_bytes.try_into().unwrap()) as u64;
                Ok((CompactSize::new(value), 1 + value_bytes.len()))
            }
            [0xFE, rest @ ..] => {
                let value_bytes = rest.get(0..4).ok_or(BitcoinError::InsufficientBytes)?;
                let value = u32::from_le_bytes(value_bytes.try_into().unwrap()) as u64;
                Ok((CompactSize::new(value), 1 + value_bytes.len()))
            }
            [0xFF, rest @ ..] => {
                let value_bytes = rest.get(0..8).ok_or(BitcoinError::InsufficientBytes)?;
                let value = u64::from_le_bytes(value_bytes.try_into().unwrap());
                Ok((CompactSize::new(value), 1 + value_bytes.len()))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Txid(pub [u8; 32]);

impl Serialize for Txid {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // TODO: Serialize as a hex-encoded string (32 bytes => 64 hex characters)
        let hex_string = hex::encode(self.0);
        serializer.serialize_str(&hex_string)
    }
}

impl<'de> Deserialize<'de> for Txid {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // TODO: Parse hex string into 32-byte array
        // Use `hex::decode`, validate length = 32
        let hex_string = String::deserialize(deserializer)?;
        let bytes = hex::decode(&hex_string).map_err(serde::de::Error::custom)?;
        if bytes.len() < 32 {
            return Err(serde::de::Error::custom("Txid must be 32 bytes"));
        }
        let mut arr = [0u8; 32];
        arr.copy_from_slice(&bytes);
        Ok(Txid(arr))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct OutPoint {
    pub txid: Txid,
    pub vout: u32,
}

impl OutPoint {
    pub fn new(txid: [u8; 32], vout: u32) -> Self {
        // TODO: Create an OutPoint from raw txid bytes and output index
        Self {
            txid: Txid(txid),
            vout,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: Serialize as: txid (32 bytes) + vout (4 bytes, little-endian)
        let mut bytes: Vec<u8> = Vec::with_capacity(36);
        bytes.extend_from_slice(&self.txid.0);
        bytes.extend_from_slice(&self.vout.to_le_bytes());
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        // TODO: Deserialize 36 bytes: txid[0..32], vout[32..36]
        // Return error if insufficient bytes
        if bytes.len() < 36 {
            return Err(BitcoinError::InsufficientBytes);
        }
        let mut txid = [0u8; 32];
        txid.copy_from_slice(&bytes[0..32]);

        let vout: u32 = u32::from_le_bytes(bytes[32..36].try_into().unwrap());

        Ok((OutPoint::new(txid, vout), 36))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Script {
    pub bytes: Vec<u8>,
}

impl Script {
    pub fn new(bytes: Vec<u8>) -> Self {
        // TODO: Simple constructor
        Self { bytes }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: Prefix with CompactSize (length), then raw bytes
        let mut bytes = Vec::new();
        let compactsize = CompactSize::new(self.bytes.len() as u64);
        bytes.extend_from_slice(&compactsize.to_bytes());
        bytes.extend_from_slice(&self.bytes);
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        // TODO: Parse CompactSize prefix, then read that many bytes
        // Return error if not enough bytes
        let (script_len, compact_size) = CompactSize::from_bytes(bytes)?;

        let total_bytes = script_len.value as usize + compact_size;
        if total_bytes > bytes.len() {
            return Err(BitcoinError::InsufficientBytes);
        }
        let script_bytes = bytes[compact_size..total_bytes].to_vec();
        let script = Self::new(script_bytes);
        Ok((script, total_bytes))
    }
}

impl Deref for Script {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        // TODO: Allow &Script to be used as &[u8]
        &self.bytes
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct TransactionInput {
    pub previous_output: OutPoint,
    pub script_sig: Script,
    pub sequence: u32,
}

impl TransactionInput {
    pub fn new(previous_output: OutPoint, script_sig: Script, sequence: u32) -> Self {
        // TODO: Basic constructor
        Self {
            previous_output,
            script_sig,
            sequence,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: Serialize: OutPoint + Script (with CompactSize) + sequence (4 bytes LE)
        let mut bytes = Vec::new();
        bytes.extend(self.previous_output.to_bytes());
        bytes.extend(self.script_sig.to_bytes());
        bytes.extend(self.sequence.to_le_bytes());
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        // TODO: Deserialize in order:
        // - OutPoint (36 bytes)
        // - Script (with CompactSize)
        // - Sequence (4 bytes)
        let (previous_output, outpoint_size) = OutPoint::from_bytes(&bytes[..36])?;
        let (script_sig, script_bytes_len) = Script::from_bytes(&bytes[36..])?;
        let sequence = u32::from_le_bytes(
            bytes[36 + script_bytes_len..36 + script_bytes_len + 4]
                .try_into()
                .map_err(|_| BitcoinError::InsufficientBytes)?,
        );
        Ok((
            Self::new(previous_output, script_sig, sequence),
            outpoint_size + script_bytes_len + 4,
        ))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct BitcoinTransaction {
    pub version: u32,
    pub inputs: Vec<TransactionInput>,
    pub lock_time: u32,
}

impl BitcoinTransaction {
    pub fn new(version: u32, inputs: Vec<TransactionInput>, lock_time: u32) -> Self {
        // TODO: Construct a transaction from parts
        Self {
            version,
            inputs,
            lock_time,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: Format:
        // - version (4 bytes LE)
        // - CompactSize (number of inputs)
        // - each input serialized
        // - lock_time (4 bytes LE)
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.version.to_le_bytes());
        let input_count = CompactSize::new(self.inputs.len() as u64);

        bytes.extend_from_slice(&input_count.to_bytes());
        for input in &self.inputs {
            bytes.extend_from_slice(&input.to_bytes());
        }
        bytes.extend_from_slice(&self.lock_time.to_le_bytes());
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        // TODO: Read version, CompactSize for input count
        // Parse inputs one by one
        // Read final 4 bytes for lock_time
        let mut offset = 0;
        if bytes.len() < 4 {
            return Err(BitcoinError::InsufficientBytes);
        }
        let version = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
        offset += 4;

        let (input_count, compact_bytes) = CompactSize::from_bytes(&bytes[offset..])?;
        offset += compact_bytes;

        let mut inputs = Vec::new();

        for _ in 0..input_count.value {
            let (input, consumed) = TransactionInput::from_bytes(&bytes[offset..])?;
            inputs.push(input);
            offset += consumed;
        }

        if offset + 4 > bytes.len() {
            return Err(BitcoinError::InsufficientBytes);
        }
        let lock_time = u32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap());
        offset += 4;

        Ok((BitcoinTransaction::new(version, inputs, lock_time), offset))
    }
}

impl fmt::Display for BitcoinTransaction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Format a user-friendly string showing version, inputs, lock_time
        // Display scriptSig length and bytes, and previous output info
        writeln!(f, "Bitcoin Transaction:")?;
        writeln!(f, "  Version: {}", self.version)?;
        writeln!(f, "  Input Count: {}", self.inputs.len())?;
        for (i, input) in self.inputs.iter().enumerate() {
            writeln!(f, "  Input {}:", i)?;
            writeln!(
                f,
                "    Previous Output Vout: {}",
                input.previous_output.vout
            )?;
            writeln!(f, "    Script Sig Length: {}", input.script_sig.bytes.len())?;
            writeln!(
                f,
                "    Script Sig: {}",
                hex::encode(&input.script_sig.bytes)
            )?;
            writeln!(f, "    Sequence: 0x{:08x}", input.sequence)?;
        }
        writeln!(f, "  Lock Time: {}", self.lock_time)?;
        Ok(())
    }
}
