
use super::types::*;


/// WASM global variable
pub struct GlobalVariable {
    name: String,
    type_: WASMType,
    init_expr: String,
}

impl GlobalVariable {
    /// Creates new global variable
    pub fn new(name: String, type_: WASMType, init_expr: String) -> GlobalVariable {
        return GlobalVariable {
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
pub struct GlobalVariableRef {
    idx: usize
}

impl GlobalVariableRef {
    /// Creates new reference to global variable
    pub fn new(idx: usize) -> GlobalVariableRef {
        return GlobalVariableRef {
            idx: idx
        };
    }
}


/// WASM Module
pub struct Module {
    /// Vector of generated module imports
    pub imports: Vec<FunctionImport>,

    /// Vector of global variables in module
    globals: Vec<GlobalVariable>
}

impl Module {
    /// Creates new module
    pub fn new() -> Module {
        return Module {
            imports: vec![],
            globals: vec![]
        };
    }

    /// Adds function import into generated module. Returns reference of imported function
    pub fn import_function(&mut self,
                           name: &str,
                           type_: WASMFunctionType,
                           module_name: &str,
                           function_name: &str) -> FunctionRef {
        let import = FunctionImport::new(name, type_.clone(), module_name, function_name);
        self.imports.push(import);
        return FunctionRef::new(name, type_);
    }

    /// Creates new global variable
    pub fn new_global(&mut self,
                      name: String,
                      tp: WASMType,
                      init_expr: String) -> GlobalVariableRef {
        let var = GlobalVariable::new(name, tp, init_expr);
        let var_idx = self.globals.len();
        self.globals.push(var);
        return GlobalVariableRef::new(var_idx);
    }

    /// Generates WASM for reference to global variable
    pub fn generate_global_ref(&self, var_ref: GlobalVariableRef) -> String {
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
    pub fn get_global_ref_and_type(&self, var_ref: &GlobalVariableRef) -> (String, WASMType) {
        let var = &self.globals[var_ref.idx];
        return (format!("${}", var.name), var.type_);
    }
}
