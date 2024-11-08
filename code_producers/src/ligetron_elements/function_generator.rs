
use super::types::*;
use WASMType::*;


/// Logical stack. Stores current logical state of stack.
pub struct Stack {
    values: Vec<WASMType>
}

impl Stack {
    /// Creates new emtpty WASM stack
    pub fn new() -> Stack {
        return Stack {
            values: Vec::new()
        };
    }

    /// Pushes value on top of stack
    pub fn push(&mut self, type_: WASMType) {
        self.values.push(type_);
    }

    /// Returns type of value located on top of stack
    pub fn top(&self) -> WASMType {
        match self.values.last() {
            Some(t) => { return *t; },
            None => { panic!("Stack is empty, but expected value on stack"); }
        }
    }

    /// Checks value located on top of stack and pops it
    pub fn pop(&mut self, type_: WASMType) {
        // checkint type of value on top of stack
        match self.values.last() {
            Some(t) => {
                if *t != type_ {
                    panic!("Type of top stack value does not match expected type: expected: {}, found: {}",
                    type_.to_string(), t.to_string());
                }
            },
            None => { panic!("Stack is empty, but expected type: {}", type_.to_string()); }
        }

        // removing value from top of stack
        self.values.pop();
    }

    /// Checks that values located on top of stack is suitable for pointer arithmetics
    pub fn pop_ptr_ops(&mut self) {
        if self.values.len() < 2 {
            panic!("Expected stack values for pointer arithmetics, but stack size is less than 2");
        }

        let t1 = self.top();
        self.drop();

        let t2 = self.top();
        self.drop();

        if t1 == I32 && t2 == PTR || t1 == PTR && t2 == I32 {
            return;
        }

        panic!("Expected stack values for pointer arithmetics, but found: {}, {}",
               t1.to_string(), t2.to_string());
    }

    /// Drops value from top of stack without checking it type
    pub fn drop(&mut self) {
        if self.values.pop() == None {
            panic!("Can't drop value from empty stack");
        }
    }
}


/// Local variable in generated WASM code
pub struct WASMLocalVariable {
    name: Option<String>,
    type_: WASMType,
}

impl WASMLocalVariable {
    /// Creates new local variable
    pub fn new(name: Option<String>, type_: WASMType) -> WASMLocalVariable {
        return WASMLocalVariable {
            name: name,
            type_: type_
        };
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


/// Reference to local WASM variable
#[derive(Clone)]
pub struct WASMLocalVariableRef {
    /// Variable index
    idx: usize
}

impl WASMLocalVariableRef {
    /// Creates new local variable reference
    fn new(idx: usize) -> WASMLocalVariableRef {
        return WASMLocalVariableRef {
            idx: idx
        };
    }
}


/// Circom local variable
struct LocalVariable {
    /// Name of variable
    name: String,

    /// Vector of WASM local variables where value of this varible is stored
    locals: Vec<WASMLocalVariableRef>
}

impl LocalVariable {
    /// Creates new local variable
    fn new(name: &str, locals: Vec<WASMLocalVariableRef>) -> LocalVariable {
        return LocalVariable {
            name: name.to_string(),
            locals: locals
        };
    }
}


/// Reference to Circlom local variable
#[derive(Clone)]
pub struct LocalVariableRef {
    idx: usize
}

impl LocalVariableRef {
    /// Creates new local variable reference
    fn new(idx: usize) -> LocalVariableRef {
        return LocalVariableRef {
            idx: idx
        };
    }
}


/// Struct for generating code for a single function
pub struct FunctionGenerator {
    /// Size of Circom variables, in 32bit
    size_32_bit: usize,

    /// Function name
    name: String,

    /// Vector of function return types
    ret_types: Vec<WASMType>,

    /// WASM parameters
    params: Vec<WASMLocalVariable>,

    /// WASM local variables
    locals: Vec<WASMLocalVariable>,

    /// Circom local variables
    vars: Vec<LocalVariable>,

    instructions: Vec<String>,
    export_name: Option<String>,

    /// Current logical stack state
    stack: Stack
}

impl FunctionGenerator {
    /// Constructs new function generator
    pub fn new(size_32_bit: usize, name: &str) -> FunctionGenerator {
        return FunctionGenerator {
            size_32_bit: size_32_bit,
            name: name.to_string(),
            ret_types: Vec::new(),
            params: Vec::new(),
            locals: Vec::new(),
            vars: Vec::new(),
            instructions: Vec::new(),
            export_name: None,
            stack: Stack::new()
        }
    }

    /// Adds WASM return type for function
    pub fn add_wasm_ret_type(&mut self, t: WASMType) {
        self.ret_types.push(t);
    }

    /// Adds new WASM named function parameter. Returns refence to function parameter.
    pub fn new_wasm_param(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        if !self.locals.is_empty() {
            panic!("can't add function parameters after local variables");
        }

        let param = WASMLocalVariable::new(Some(name.to_string()), type_);
        let local_idx = self.params.len();

        self.params.push(param);
        return WASMLocalVariableRef::new(local_idx);
    }

    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_wasm_named_local(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        let local = WASMLocalVariable::new(Some(name.to_string()), type_);
        let local_idx = self.locals.len() + self.params.len();

        self.locals.push(local);
        return WASMLocalVariableRef::new(local_idx);
    }

    /// Adds new unnamed WASM local variable. Returns reference to variable.
    pub fn new_wasm_local(&mut self, type_: WASMType) -> WASMLocalVariableRef {
        let local = WASMLocalVariable::new(None, type_);
        let local_idx = self.locals.len() + self.params.len();

        self.locals.push(local);
        return WASMLocalVariableRef::new(local_idx);
    }

    /// Returns reference to WASM local variable corresponding to reference
    fn wasm_local(&self, lref: &WASMLocalVariableRef) -> &WASMLocalVariable {
        return if lref.idx < self.params.len() {
            &self.params[lref.idx]
        } else {
            &self.locals[lref.idx - self.params.len()]
        };
    }

    /// Generates reference to WASM local
    fn gen_wasm_local_ref(&self, local: &WASMLocalVariableRef) -> String {
        let var = self.wasm_local(local);

        match &var.name {
            Some(name) => { return format!("${}", name); },
            None => { return format!("{}", local.idx); }
        }
    }

    /// Sets function export name
    pub fn set_export_name(&mut self, name: &str) {
        self.export_name = Some(name.to_string());
    }

    /// Generates function code as list of instructions. Makes this instance invalid
    pub fn generate(&mut self) -> Vec<String> {
        // checking that current stack state corresponds to function return type
        for ret in self.ret_types.iter().rev() {
            self.stack.pop(*ret);
        }

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


    ////////////////////////////////////////////////////////////
    // Instructions generation

    /// Appends instruction to function body
    fn gen_inst(&mut self, instruction: &str) {
        self.instructions.push(instruction.to_string());
    }

    /// Generates empty code line
    pub fn gen_empty(&mut self) {
        self.gen_inst("");
    }

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.gen_empty();
        self.gen_inst(&format!(";; {}", str));
    }

    /// Generates call instruction
    pub fn gen_call(&mut self, func: FunctionRef) {
        // popping parameters from the stack
        for par in func.type_.params() {
            self.stack.pop(*par);
        }

        // pushing return values to the stack
        for ret in func.type_.ret_types() {
            self.stack.push(*ret);
        }

        // generating call instruction
        self.gen_inst(&format!("call {}", func.generate()));
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, local: &WASMLocalVariableRef) {
        self.stack.push(self.wasm_local(local).type_);
        self.gen_inst(&format!("local.get {}", self.gen_wasm_local_ref(local)));
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &WASMLocalVariableRef, expr: &str) {
        self.gen_inst(&format!("local.set {} {}", self.gen_wasm_local_ref(local), expr));
    }

    /// Generates setting valuf of a local to current value on top of stack
    pub fn gen_local_set(&mut self, local: &WASMLocalVariableRef) {
        self.stack.pop(self.wasm_local(local).type_);
        self.gen_inst(&format!("local.set {}", self.gen_wasm_local_ref(local)));
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.stack.drop();
        self.gen_inst(&format!("drop"));
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: WASMType, value: i64) {
        self.stack.push(type_);
        self.gen_inst(&format!("{}.const {}", type_.generate(), value));
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, type_: WASMType) {
        if type_ == PTR {
            self.stack.pop_ptr_ops();
        } else {
            self.stack.pop(type_);
            self.stack.pop(type_);
        }
        self.stack.push(type_);

        self.gen_inst(&format!("{}.mul", type_.generate()));
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, type_: WASMType) {
        self.stack.pop(PTR);
        self.stack.push(type_);
        self.gen_inst(&format!("{}.load", type_.generate()));
    }


    ////////////////////////////////////////////////////////////
    // Fr code generation

    /// Adds Circom return type to function
    pub fn add_ret_val(&mut self) {
        for _ in 0 .. self.size_32_bit {
            self.add_wasm_ret_type(I32);
        }
    }

    /// Creates new Circom function parameter with specified name
    pub fn new_param(&mut self, name: &str) -> LocalVariableRef {
        // creating WASM parameters variables for representing Circom parameter
        let mut vars = Vec::<WASMLocalVariableRef>::new();
        for i in 0 .. self.size_32_bit {
            vars.push(self.new_wasm_param(&format!("{}_{}", name, i), I32));
        }

        let var_idx = self.vars.len();

        // creating Circom variable
        let var = LocalVariable::new(name, vars);
        self.vars.push(var);

        return LocalVariableRef::new(var_idx);
    }

    /// Creates new Circom local variable with specified name
    pub fn new_var(&mut self, name: &str) -> LocalVariableRef {
        // creating WASM local variables for representing Circom variable
        let mut vars = Vec::<WASMLocalVariableRef>::new();
        for i in 0 .. self.size_32_bit {
            vars.push(self.new_wasm_named_local(&format!("{}_{}", name, i), I32));
        }

        let var_idx = self.vars.len();

        // creating Circom variable
        let var = LocalVariable::new(name, vars);
        self.vars.push(var);

        return LocalVariableRef::new(var_idx);
    }

    /// Retrns Circom variable corresponding to variable reference
    fn get_var(&self, var_ref: &LocalVariableRef) -> &LocalVariable {
        return &self.vars[var_ref.idx];
    }

    /// Generates loading of variable value to stack
    pub fn gen_load_var(&mut self, var_ref: &LocalVariableRef) {
        let var = &self.get_var(var_ref);
        for loc in var.locals.clone() {
            self.gen_local_get(&loc);
        }
    }

    /// Generates saving stack value to variable
    pub fn gen_store_var(&mut self, var_ref: &LocalVariableRef) {
        let var = &self.get_var(var_ref);
        for loc in var.locals.clone().iter().rev() {
            self.gen_local_set(&loc);
        }
    }
}
