
use std::fmt::*;


/// Helper class for writing binary data
pub struct DataWriter {
    data_: Vec<u8>
}

impl DataWriter {
    /// Creates new writer
    pub fn new() -> DataWriter {
        return DataWriter {
            data_: vec![]
        }
    }

    /// Returns data stored in writer
    pub fn data(&self) -> &Vec<u8> {
        return &self.data_;
    }

    /// Returns size of current data in writer
    pub fn data_size(&self) -> usize {
        return self.data_.len();
    }

    /// Returns data as hex string
    pub fn hex_data(&self) -> String {
        let mut res = String::new();
        for byte in &self.data_ {
            write!(&mut res, "\\{byte:02X}")
                .expect("error writing hex data");
        }

        return res;
    }

    /// Writes array of bytes
    pub fn write_data(&mut self, arr: &[u8]) {
        self.data_.extend_from_slice(arr);
    }

    /// Writes i32
    pub fn write_i32(&mut self, val: i32) {
        self.write_data(&val.to_le_bytes());
    }
}
