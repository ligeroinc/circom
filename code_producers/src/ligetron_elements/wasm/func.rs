
use super::inst_gen::*;
use super::module::*;
use super::stack::*;
use super::types::*;

use std::cell::RefCell;
use std::cell::RefMut;
use std::rc::Rc;


/// Represens current WASM function being generated
pub struct WASMFunction {
    /// Reference to parent module being generaed
    module_: Rc<RefCell<Module>>,

    /// Reference to WASM stack frame for this function
    frame: Rc<RefCell<WASMStackFrame>>,

    /// Reference to instructions generator for this function
    inst_gen: Rc<RefCell<InstructionGenerator>>,

    /// Function name
    name: String,

    /// Vector of function return types
    ret_types: Vec<WASMType>,

    /// Function export name
    export_name: Option<String>
}

impl WASMFunction {
    /// Creates new WASM function
    pub fn new(module: Rc<RefCell<Module>>,
               name: String,
               params: &Vec<(String, WASMType)>,
               ret_types: Vec<WASMType>) -> WASMFunction {

        // creating new wasm stack frame for this function
        let mut frame = WASMStackFrame::new();
        for (name, tp) in params {
            frame.new_param(name, *tp);
        }

        let frame_rc = Rc::new(RefCell::new(frame));

        // creating instructions generator for this function
        let inst_gen = Rc::new(RefCell::new(InstructionGenerator::new(module.clone(),
                                                                      frame_rc.clone())));

        return WASMFunction {
            module_: module,
            frame: frame_rc,
            inst_gen: inst_gen,
            name: name,
            ret_types: ret_types,
            export_name: None
        };
    }

    /// Returns shared reference to instruction generator for this function
    pub fn inst_gen_rc(&self) -> &Rc<RefCell<InstructionGenerator>> {
        return &self.inst_gen;
    }

    /// Returns mutable reference to instruction generator for this function
    pub fn inst_gen_mut(&self) -> RefMut<InstructionGenerator> {
        return self.inst_gen.borrow_mut();
    }

    /// Returns shared reference to stack frame for this function
    pub fn frame_rc(&self) -> &Rc<RefCell<WASMStackFrame>> {
        return &self.frame;
    }

    /// Returns mutable reference to stack frame for this function
    pub fn frame_mut(&self) -> RefMut<WASMStackFrame> {
        return self.frame.borrow_mut();
    }

    /// Adds WASM return type for function
    pub fn add_ret_type(&mut self, t: WASMType) {
        self.ret_types.push(t);
    }

    /// Adds new WASM named function parameter. Returns refence to function parameter.
    pub fn new_param(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        return self.frame.borrow_mut().new_param(name, type_);
    }

    /// Returns reference to variable for parameter with specified index
    pub fn param(&self, idx: usize) -> WASMLocalVariableRef {
        return self.frame.borrow().param(idx);
    }

    /// Adds new WASM named local variable. Returns reference to variable.
    pub fn new_named_local(&mut self, name: &str, type_: WASMType) -> WASMLocalVariableRef {
        return self.frame.borrow_mut().new_named_local(name, type_);
    }

    /// Adds new unnamed WASM local variable. Returns reference to variable.
    pub fn new_wasm_local(&mut self, type_: WASMType) -> WASMLocalVariableRef {
        return self.frame.borrow_mut().new_local(type_);
    }

    /// Generates reference to WASM local
    pub fn gen_local_ref(&self, local: &WASMLocalVariableRef) -> String {
        let frame = self.frame.borrow_mut();
        let var = frame.local(local);

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
        // checking that current wasm_stack state corresponds to function return type
        for ret in self.ret_types.iter().rev() {
            self.frame.borrow_mut().pop(*ret);
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
        instructions.append(&mut self.frame.borrow_mut().generate_params());

        // generating return types
        if !self.ret_types.is_empty() {
            let rt_str = self.ret_types.iter().map(|t| t.generate()).collect::<Vec<_>>().join(" ");
            instructions.push(format!("    (result {})", rt_str));
        }

        // generating locals
        instructions.push(format!(""));
        instructions.append(&mut self.frame.borrow().generate_locals());

        // // generating function entry code for memory stack
        // instructions.append(&mut self.mem_frame().gen_func_entry(&self.stack_ptr));

        // adding function body
        instructions.push(format!(""));
        instructions.append(&mut self.inst_gen.borrow_mut().instructions());

        // // generating function exit code for memory stack
        // instructions.append(&mut self.mem_frame().gen_func_exit(&self.stack_ptr));

        // adding terminating ) for function body
        instructions.push(format!(")"));

        return instructions;
    }


    ////////////////////////////////////////////////////////////
    // Instructions generation

    /// Appends instruction to function body
    fn gen_inst(&mut self, inst: &str) {
        self.inst_gen.borrow_mut().gen_inst(inst);
    }

    /// Generates empty code line
    pub fn gen_empty(&mut self) {
        self.inst_gen.borrow_mut().gen_empty();
    }

    /// Generates comment with emtpy line before it
    pub fn gen_comment(&mut self, str: &str) {
        self.inst_gen.borrow_mut().gen_comment(str);
    }

    /// Generates call instruction
    pub fn gen_call(&mut self, func: FunctionRef) {
        self.inst_gen.borrow_mut().gen_call(func);
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, var_ref: &WASMLocalVariableRef) {
        self.inst_gen.borrow_mut().gen_local_get(var_ref);
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &WASMLocalVariableRef, expr: &str) {
        self.inst_gen.borrow_mut().gen_local_set_expr(local, expr);
    }

    /// Generates setting valuf of a local to current value on top of wasm stack
    pub fn gen_local_set(&mut self, var_ref: &WASMLocalVariableRef) {
        self.inst_gen.borrow_mut().gen_local_get(var_ref);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.inst_gen.borrow_mut().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: WASMType, value: i64) {
        self.inst_gen.borrow_mut().gen_const(type_, value);
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, type_: WASMType) {
        self.inst_gen.borrow_mut().gen_mul(type_);
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, tp: WASMType) {
        self.inst_gen.borrow_mut().gen_load(tp);
    }
}
