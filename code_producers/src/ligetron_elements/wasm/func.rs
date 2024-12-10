
use super::super::log::*;
use super::frame::*;
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
    module_: Rc<RefCell<WASMModule>>,

    /// Reference to WASM stack frame for this function
    frame: Rc<RefCell<WASMStackFrame>>,

    /// Reference to WASM stack state for this function
    stack: Rc<RefCell<WASMStackState>>,

    /// Reference to instructions generator for this function
    inst_gen: Rc<RefCell<InstructionGenerator>>,

    /// Function name
    name_: String,

    /// Vector of function return types
    ret_types: Vec<WASMType>,

    /// Function export name
    export_name: Option<String>
}

impl WASMFunction {
    /// Creates new WASM function
    pub fn new(module: Rc<RefCell<WASMModule>>,
               name: String) -> WASMFunction {

        debug_log!("");
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("WASM FUNCTION BEGIN: {}", name);
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("");

        // creating new WASM stack frame for this function
        let frame_rc = Rc::new(RefCell::new(WASMStackFrame::new()));

        // creating new WASM stack state for this function
        let stack_rc = Rc::new(RefCell::new(WASMStackState::new()));

        // creating instructions generator for this function
        let inst_gen = Rc::new(RefCell::new(InstructionGenerator::new(module.clone(),
                                                                      frame_rc.clone(),
                                                                      stack_rc.clone())));

        return WASMFunction {
            module_: module,
            frame: frame_rc,
            stack: stack_rc,
            inst_gen: inst_gen,
            name_: name,
            ret_types: vec![],
            export_name: None
        };
    }

    /// Returns function name
    pub fn name(&self) -> &String {
        return &self.name_;
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

    /// Returns name and type of local
    pub fn local(&self, loc_ref: &WASMLocalVariableRef) -> (Option<String>, WASMType) {
        let frame = self.frame.borrow();
        let loc = frame.local(loc_ref);
        return (loc.name.clone(), loc.type_);
    }

    /// Returns local name
    pub fn local_name(&self, loc_ref: &WASMLocalVariableRef) -> Option<String> {
        return self.frame.borrow_mut().local(loc_ref).name.clone();
    }

    /// Generates reference to WASM local
    pub fn gen_local_ref(&self, lref: &WASMLocalVariableRef) -> String {
        return self.frame.borrow().gen_local_ref(lref)
    }

    /// Sets function export name
    pub fn set_export_name(&mut self, name: &str) {
        self.export_name = Some(name.to_string());
    }

    /// Generates function code as list of instructions. Makes this instance invalid
    pub fn generate(&mut self) -> Vec<String> {
        debug_log!("");
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("WASM FUNCTION END: {}", self.name_);
        debug_log!("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
        debug_log!("");

        // checking that current wasm_stack state corresponds to function return type
        for ret in self.ret_types.iter().rev() {
            self.stack.borrow_mut().pop(ret);
        }

        if !self.stack.borrow().is_empty() {
            println!("WASM stack is not empty at the end of function {}:\n{}\n",
                     self.name_,
                     self.stack.borrow().dump());
        }

        let mut instructions = Vec::<String>::new();

        // generating function header
        instructions.push(format!("(func ${}", &self.name_));

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
        for inst in self.inst_gen.borrow_mut().instructions() {
            instructions.push(format!("    {}", inst));
        }

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
    pub fn gen_call(&mut self, func: &WASMFunctionRef) {
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
        self.inst_gen.borrow_mut().gen_local_set(var_ref);
    }

    /// Generates getting value of a global
    pub fn gen_global_get(&mut self, var_ref: &WASMGlobalVariableRef) {
        self.inst_gen.borrow_mut().gen_global_get(var_ref);
    }

    /// Generates setting value of a global
    pub fn gen_global_set(&mut self, var_ref: &WASMGlobalVariableRef) {
        self.inst_gen.borrow_mut().gen_global_set(var_ref);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.inst_gen.borrow_mut().gen_drop();
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: WASMType, value: i64) {
        self.inst_gen.borrow_mut().gen_const(type_, value);
    }

    /// Generates add instruction
    pub fn gen_add(&mut self, tp: WASMType) {
        self.inst_gen.borrow_mut().gen_add(tp);
    }

    /// Generates sub instruction
    pub fn gen_sub(&mut self, tp: WASMType) {
        self.inst_gen.borrow_mut().gen_sub(tp);
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, tp: WASMType) {
        self.inst_gen.borrow_mut().gen_mul(tp);
    }

    /// Generates eqz instruction
    pub fn gen_eqz(&mut self, tp: &WASMType) {
        self.inst_gen.borrow_mut().gen_eqz(tp);
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, tp: WASMType) {
        self.inst_gen.borrow_mut().gen_load(tp);
    }

    /// Generates store instruction
    pub fn gen_store(&mut self, tp: &WASMType) {
        self.inst_gen.borrow_mut().gen_store(tp);
    }

    /// Starts generating if-else block
    pub fn gen_if(&mut self) {
        self.inst_gen.borrow_mut().gen_if();
    }

    /// Starts generating else block
    pub fn gen_else(&mut self) {
        self.inst_gen.borrow_mut().gen_else();
    }

    /// Finishes generating if-else block
    pub fn gen_endif(&mut self) {
        self.inst_gen.borrow_mut().gen_endif();
    }

    /// Starts generating loop block
    pub fn gen_loop_start(&mut self) {
        self.inst_gen.borrow_mut().gen_loop_start();
    }

    /// Finishes generating loop block and adds branch to the beginning of loop
    pub fn gen_loop_end(&mut self) {
        self.inst_gen.borrow_mut().gen_loop_end();
    }

    /// Generates conditional exit from current loop
    pub fn gen_loop_exit(&mut self) {
        self.inst_gen.borrow_mut().gen_loop_exit();
    }


    ////////////////////////////////////////////////////////////
    // Debugging

    /// Dump contents of function stack frame to string
    pub fn dump_stack(&self) -> String {
        return self.stack.borrow().dump();
    }
}
