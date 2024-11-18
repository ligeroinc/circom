
use super::types::*;


/// FR arithmetics context, stores references to FR functions
#[derive(Clone)]
pub struct FRContext {
    pub copy: CircomFunctionRef,
    pub raw_copy: CircomFunctionRef
}
