
use super::types::*;


/// Local variable in generated WASM code
pub struct WASMLocalVariable {
    pub name: Option<String>,
    pub type_: WASMType,
}

impl WASMLocalVariable {
    /// Creates new local variable
    pub fn new(name: Option<String>, type_: WASMType) -> WASMLocalVariable {
        return WASMLocalVariable {
            name: name,
            type_: type_
        };
    }

    /// Returns variable type
    pub fn var_type(&self) -> WASMType {
        return self.type_;
    }

    /// Generates local variable into WASM code
    pub fn generate(&self) -> String {
        return format!("(local {})", self.generate_name_and_type());
    }

    /// Generates local variable into WASM code as function parameter
    pub fn generate_param(&self) -> String {
        return format!("(param {})", self.generate_name_and_type());
    }

    /// Generates local variable name and type
    fn generate_name_and_type(&self) -> String {
        let name_str = match &self.name {
            Some(s) => format!("${} ", s),
            None => "".to_string()
        };

        return format!("{}{}", name_str, self.type_.generate());
    }
}


/// Reference to WASM local variable
#[derive(Clone)]
pub enum WASMLocalVariableRef {
    Parameter(usize),
    Local(usize)
}


/// Stores current logical state of WASM stack frame.
pub struct WASMStackFrame {
    /// Vector of function parameters parameters
    params: Vec<WASMLocalVariable>,

    /// Vector of local variables
    locals: Vec<WASMLocalVariable>,

    /// Does this stack frame have unnamed local variables
    has_unnamed_locals: bool
}

impl WASMStackFrame {
    /// Creates new emtpty WASM stack
    pub fn new() -> WASMStackFrame {
        return WASMStackFrame {
            params: vec![],
            locals: vec![],
            has_unnamed_locals: false
        };
    }

    /// Adds new WASM named function parameter. Returns refence to function parameter.
    pub fn new_param(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        if self.has_unnamed_locals {
            panic!("can't add function parameters after unnamed local variables");
        }

        let param = WASMLocalVariable::new(Some(name.to_string()), type_);
        let param_idx = self.params.len();

        self.params.push(param);
        return WASMLocalVariableRef::Parameter(param_idx);
    }

    /// Returns reference to variable for parameter with specified index.
    pub fn param(&self, idx: usize) -> WASMLocalVariableRef {
        assert!(idx < self.params.len());
        return WASMLocalVariableRef::Parameter(idx);
    }

    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_named_local(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        let local = WASMLocalVariable::new(Some(name.to_string()), type_);
        let local_idx = self.locals.len();

        self.locals.push(local);
        return WASMLocalVariableRef::Local(local_idx);
    }

    /// Adds new unnamed WASM local variable. Returns reference to variable.
    pub fn new_local(&mut self, type_: WASMType) -> WASMLocalVariableRef {
        let local = WASMLocalVariable::new(None, type_);
        let local_idx = self.locals.len();

        self.locals.push(local);
        self.has_unnamed_locals = true;
        return WASMLocalVariableRef::Local(local_idx);
    }

    /// Returns reference to WASM local variable corresponding to variable reference
    pub fn local(&self, lref: &WASMLocalVariableRef) -> &WASMLocalVariable {
        return match lref {
            WASMLocalVariableRef::Parameter(idx) => {
                &self.params[*idx]
            },
            WASMLocalVariableRef::Local(idx) => {
                &self.locals[*idx]
            }
        };
    }

    /// Generates WASM code for parameters
    pub fn generate_params(&self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        for param in &self.params {
            instructions.push(format!("    {}", param.generate_param()));
        }

        return instructions;
    }

    /// Generates WASM code for local variables
    pub fn generate_locals(&self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        for local in &self.locals {
            instructions.push(format!("    {}", local.generate()));
        }

        return instructions;
    }

    /// Generates reference to WASM local
    pub fn gen_local_ref(&self, lref: &WASMLocalVariableRef) -> String {
        let var = self.local(lref);

        match &var.name {
            Some(name) => { return format!("${}", name); },
            None => {
                let idx = match lref {
                    WASMLocalVariableRef::Parameter(idx) => *idx,
                    WASMLocalVariableRef::Local(idx) => self.params.len() + idx
                };
                return format!("{}", idx);
            }
        }
    }
}
