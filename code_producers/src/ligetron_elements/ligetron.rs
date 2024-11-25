
use super::wasm::*;


/// Stores references to Ligetron functions
pub struct LigetronContext {
    pub print: WASMFunctionRef,
    pub print_str: WASMFunctionRef,
    pub dump_memory: WASMFunctionRef,
    pub assert_one: WASMFunctionRef,
}
