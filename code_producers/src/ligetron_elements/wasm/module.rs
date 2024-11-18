
use super::types::*;


/// WASM global variable
pub struct WASMGlobalVariable {
    name: String,
    type_: WASMType,
    init_expr: String,
}

impl WASMGlobalVariable {
    /// Creates new global variable
    pub fn new(name: String, type_: WASMType, init_expr: String) -> WASMGlobalVariable {
        return WASMGlobalVariable {
            name: name,
            type_: type_,
            init_expr: init_expr
        }
    }

    /// Generates WASM for global variable
    pub fn generate(&self) -> String {
        return format!("(global ${} {} {})", self.name, self.type_.generate(), self.init_expr);
    }
}


/// Referenct to global variable
#[derive(Copy, Clone)]
pub struct WASMGlobalVariableRef {
    idx: usize
}

impl WASMGlobalVariableRef {
    /// Creates new reference to global variable
    pub fn new(idx: usize) -> WASMGlobalVariableRef {
        return WASMGlobalVariableRef {
            idx: idx
        };
    }
}


/// WASM Module
pub struct WASMModule {
    /// Vector of generated module imports
    pub imports: Vec<FunctionImport>,

    /// Vector of global variables in module
    globals: Vec<WASMGlobalVariable>
}

impl WASMModule {
    /// Creates new module
    pub fn new() -> WASMModule {
        return WASMModule {
            imports: vec![],
            globals: vec![]
        };
    }

    /// Adds function import into generated module. Returns reference of imported function
    pub fn import_function(&mut self,
                           name: &str,
                           type_: WASMFunctionType,
                           module_name: &str,
                           function_name: &str) -> WASMFunctionRef {
        let import = FunctionImport::new(name, type_.clone(), module_name, function_name);
        self.imports.push(import);
        return WASMFunctionRef::new(name, type_);
    }

    /// Creates new global variable
    pub fn new_global(&mut self,
                      name: String,
                      tp: WASMType,
                      init_expr: String) -> WASMGlobalVariableRef {
        let var = WASMGlobalVariable::new(name, tp, init_expr);
        let var_idx = self.globals.len();
        self.globals.push(var);
        return WASMGlobalVariableRef::new(var_idx);
    }

    /// Generates WASM for reference to global variable
    pub fn generate_global_ref(&self, var_ref: WASMGlobalVariableRef) -> String {
        let var = &self.globals[var_ref.idx];
        return format!("${}", var.name);
    }

    /// Generates module imports
    pub fn generate_imports(&self) -> Vec<String> {
        return self.imports.iter().map(|imp| imp.generate()).collect();
    }

    /// Generates global variables
    pub fn generate_globals(&self) -> Vec<String> {
        return self.globals.iter().map(|g| g.generate()).collect();
    }

    /// Returns global variable reference string and type
    pub fn get_global_ref_and_type(&self, var_ref: &WASMGlobalVariableRef) -> (String, WASMType) {
        let var = &self.globals[var_ref.idx];
        return (format!("${}", var.name), var.type_);
    }
}
