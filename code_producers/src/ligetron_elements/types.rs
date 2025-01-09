
use super::wasm::*;


/// Value type. Can be FR type of WASM type
#[derive(Clone)]
pub enum CircomValueType {
    WASM(WASMType),
    FR,
    Array(Box<CircomValueType>, usize),
    Struct(CircomStructType)
}

impl CircomValueType {
    pub fn is_wasm(&self) -> bool {
        return match self {
            CircomValueType::WASM(_) => true,
            CircomValueType::FR => false,
            CircomValueType::Array(tp, _size) => tp.is_wasm(),
            CircomValueType::Struct(_) => false
        }
    }

    pub fn is_fr_array(&self) -> bool {
        return match self {
            CircomValueType::Array(tp, _size) => tp.is_fr(),
            _ => false
        }
    }

    pub fn is_fr(&self) -> bool {
        return !self.is_wasm();
    }

    /// Returns type size in bytes
    pub fn size(&self) -> usize {
        return match self {
            CircomValueType::WASM(wasm_type) => wasm_type.size(),
            CircomValueType::FR => 4,
            CircomValueType::Array(tp, size) => tp.size() * size,
            CircomValueType::Struct(str) => str.size()
        }
    }

    /// Converts type to string
    pub fn to_string(&self) -> String {
        return match self {
            CircomValueType::WASM(wasm_type) => wasm_type.to_string(),
            CircomValueType::FR => "fr".to_string(),
            CircomValueType::Array(tp, sz) => format!("{}[{}]", tp.to_string(), sz),
            CircomValueType::Struct(str) => {
                let mut res = "struct { ".to_string();
                res += &str.fields.iter().map(|fld| { fld.to_string() })
                    .collect::<Vec<String>>()
                    .join(", ");
                res += " }";
                return res;
            }
        }
    }
}


/// Struct type
#[derive(Clone)]
pub struct CircomStructType {
    pub fields: Vec<CircomValueType>
}

impl CircomStructType {
    /// Returns size of struct ype
    pub fn size(&self) -> usize {
        let mut sz = 0;
        for field in &self.fields {
            sz += field.size();
        }

        return sz;
    }

    /// Returns offset of struct field with specified index
    pub fn field_offset(&self, index: usize) -> usize {
        let mut offset = 0;
        for idx in 0 .. index {
            offset += self.fields[idx].size();
        }

        return offset;
    }
}


/// Function type
#[derive(Clone)]
pub struct CircomFunctionType {
    params: Vec<CircomValueType>,
    ret_types: Vec<CircomValueType>
}

impl CircomFunctionType {
    /// Creates new function type with specified parameters and return types
    pub fn new(params: Vec<CircomValueType>, ret_types: Vec<CircomValueType>) -> CircomFunctionType {
        return CircomFunctionType {
            params: params,
            ret_types: ret_types
        };
    }

    /// Creates new function type from WASM function type
    pub fn from_wasm(wasm_func: &WASMFunctionType) -> CircomFunctionType {
        let params = wasm_func.params().iter()
            .map(|t| CircomValueType::WASM(*t))
            .collect::<Vec<_>>();
        let ret_types = wasm_func.ret_types().iter()
            .map(|t| CircomValueType::WASM(*t))
            .collect::<Vec<_>>();
        return CircomFunctionType::new(params, ret_types);
    }

    /// Returns reference to vector of parameters
    pub fn params(&self) -> &Vec<CircomValueType> {
        return &self.params;
    }

    /// Adds parameter to function type
    pub fn add_param(&mut self, type_: CircomValueType) {
        self.params.push(type_);
    }

    /// Returns reference to vector of return types
    pub fn ret_types(&self) -> &Vec<CircomValueType> {
        return &self.ret_types;
    }

    /// Adds return type to function type
    pub fn add_ret_type(&mut self, type_: CircomValueType) {
        self.ret_types.push(type_);
    }

    /// Creates new function type from this type replacing return value type
    pub fn with_ret_type(self, ret_type: CircomValueType) -> CircomFunctionType {
        return CircomFunctionType {
            params: self.params,
            ret_types: vec![ret_type]
        };
    }

    /// Creates new function type from this type replacing parameter types
    pub fn with_params(self, params: &[CircomValueType]) -> CircomFunctionType {
        return CircomFunctionType {
            params: Vec::from(params),
            ret_types: self.ret_types
        };
    }

    /// Converts Circom function type to WASM function type
    pub fn to_wasm(&self) -> WASMFunctionType {
        let mut wasm_func_type = WASMFunctionType::new();

        for ret_type in self.ret_types() {
            match ret_type {
                CircomValueType::WASM(wasm_type) => {
                    wasm_func_type.add_ret_type(wasm_type.clone());
                },
                CircomValueType::FR => {
                    wasm_func_type.add_param(WASMType::PTR);
                },
                CircomValueType::Array(_, _) => {
                    wasm_func_type.add_param(WASMType::PTR);
                }
                CircomValueType::Struct(_) => {
                    wasm_func_type.add_param(WASMType::PTR);
                }
            }
        }

        for param in self.params() {
            match param {
                CircomValueType::WASM(wasm_type) => {
                    wasm_func_type.add_param(wasm_type.clone());
                },
                CircomValueType::FR => {
                    wasm_func_type.add_param(WASMType::PTR);
                },
                CircomValueType::Array(..) => {
                    wasm_func_type.add_param(WASMType::PTR);
                },
                CircomValueType::Struct(_) => {
                    wasm_func_type.add_param(WASMType::PTR);
                }
            }
        }

        return wasm_func_type;
    }

    // /// Converts function return types to WASM string
    // pub fn ret_types_to_string(&self) -> String {
    //     if self.ret_types.is_empty() {
    //         return format!("");
    //     }

    //     let str = self.ret_types.iter()
    //         .map(|t| t.generate().to_string())
    //         .collect::<Vec<String>>()
    //         .join(" ");

    //     return format!(" (result {})", str);
    // }

    // /// Converts function parameter types to WASM string
    // pub fn params_to_string(&self) -> String {
    //     if self.params.is_empty() {
    //         "".to_string()
    //     } else {
    //         format!(" (param {})", self.params.iter().map(|t| t.generate()).collect::<Vec<_>>().join(" "))
    //     }
    // }

    // /// Converts function type to WASM string
    // pub fn to_string(&self) -> String {
    //     let params_str = self.params_to_string();
    //     let ret_type_str = self.ret_types_to_string();

    //     return format!("(func{}{})", params_str, ret_type_str);
    // }
}


/// Reference to Circom function
#[derive(Clone)]
pub struct CircomFunctionRef {
    /// Function name
    name_: String,

    /// Function type
    type_: CircomFunctionType
}

impl CircomFunctionRef {
    /// Creates new function reference
    pub fn new(name: String, tp: CircomFunctionType) -> CircomFunctionRef {
        return CircomFunctionRef {
            name_: name,
            type_: tp
        };
    }

    /// Creates function reference from WASM function reference
    pub fn from_wasm(wasm_func: &WASMFunctionRef) -> CircomFunctionRef {
        return CircomFunctionRef::new(wasm_func.name().clone(),
                                      CircomFunctionType::from_wasm(wasm_func.tp()));
    }

    /// Returns function name
    pub fn name(&self) -> &String {
        return &self.name_;
    }

    /// Returns function type
    pub fn tp(&self) -> &CircomFunctionType {
        return &self.type_;
    }

    /// Converts this function reference to WASM function reference
    pub fn to_wasm(&self) -> WASMFunctionRef {
        return WASMFunctionRef::new(self.name(), self.tp().to_wasm());
    }
}
