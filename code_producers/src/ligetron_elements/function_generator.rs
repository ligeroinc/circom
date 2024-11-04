
use super::core::*;


/// Struct for generating code for a single function
pub struct FunctionGenerator {
    name: String,
    ret_types: Vec<BuiltinType>,
    params: Vec<LocalVariable>,
    locals: Vec<LocalVariable>,
    has_unnamed_locals: bool,

    instructions: Vec<String>,
    export_name: Option<String>,
}

impl FunctionGenerator {
    /// Constructs new function generator
    pub fn new(name: &str) -> FunctionGenerator {
        return FunctionGenerator {
            name: name.to_string(),
            ret_types: Vec::<BuiltinType>::new(),
            params: Vec::<LocalVariable>::new(),
            locals: Vec::<LocalVariable>::new(),
            has_unnamed_locals: false,
            instructions: Vec::<String>::new(),
            export_name: None
        }
    }

    /// Adds return type for function
    pub fn add_ret_type(&mut self, t: BuiltinType) {
        self.ret_types.push(t);
    }

    /// Adds new named function parameter. Returns refence to function parameter.
    pub fn new_param(&mut self, name: &str, type_: BuiltinType) -> LocalVariableRef {
        if self.has_unnamed_locals {
            panic!("can't add function parameters after unnamed local variables");
        }

        let param = LocalVariable::new(Some(name.to_string()), type_);

        self.params.push(param);
        return LocalVariableRef::Name(name.to_string());
    }

    /// Adds new named local variable. Returns reference to variable.
    pub fn new_named_local(&mut self, name: &str, type_: BuiltinType) -> LocalVariableRef {
        let local = LocalVariable::new(Some(name.to_string()), type_);
        self.locals.push(local);
        return LocalVariableRef::Name(name.to_string());
    }

    /// Adds new unnamed local variable. Returns reference to variable.
    pub fn new_local(&mut self, type_: BuiltinType) -> LocalVariableRef {
        let local = LocalVariable::new(None, type_);
        let local_idx = self.locals.len() + self.params.len();

        self.locals.push(local);
        self.has_unnamed_locals = true;
        return LocalVariableRef::Index(local_idx);
    }

    /// Sets function export name
    pub fn set_export_name(&mut self, name: &str) {
        self.export_name = Some(name.to_string());
    }

    /// Appends instruction to function body
    pub fn add(&mut self, instruction: &str) {
        self.instructions.push(instruction.to_string());
    }

    /// Generates function code as list of instructions
    pub fn generate(&self) -> Vec<String> {
        let mut instructions = Vec::<String>::new();

        // generating function header
        instructions.push(format!("(func ${}", &self.name));

        // generating export name
        match &self.export_name {
            Some(name) => {
                instructions.push(format!("    (export \"{}\")", name))
            }
            None => {}
        }

        // generating parameters
        for param in &self.params {
            instructions.push(format!("    {}", param.generate_param()));
        }

        // generating return types
        if !self.ret_types.is_empty() {
            let rt_str = self.ret_types.iter().map(|t| t.generate()).collect::<Vec<_>>().join(" ");
            instructions.push(format!("    (result {})", rt_str));
        }

        // generating locals
        instructions.push(format!(""));
        for local in &self.locals {
            instructions.push(format!("    {}", local.generate()));
        }

        // adding function body
        instructions.push(format!(""));
        for inst in &self.instructions {
            instructions.push(format!("    {}", inst))
        }

        // adding terminating ) for function body
        instructions.push(format!(")"));

        return instructions;
    }
}
