
use super::wasm::*;


/// Value type. Can be FR type of WASM type
pub enum ValueType {
    WASM(WASMType),
    FR
}

impl ValueType {
    pub fn is_wasm(&self) -> bool {
        return match self {
            ValueType::WASM(_) => true,
            ValueType::FR => false
        }
    }

    pub fn is_fr(&self) -> bool {
        return !self.is_wasm();
    }
}
