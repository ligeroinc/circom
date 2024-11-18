
use super::module::*;
use super::stack::*;
use super::types::*;

use std::rc::Rc;
use std::cell::{RefCell, RefMut};


/// WASM instruction generator
pub struct InstructionGenerator {
    /// Reference to module being generated
    module_: Rc<RefCell<WASMModule>>,

    /// Reference to WASM stack frame for current function
    wasm_frame_: Rc<RefCell<WASMStackFrame>>,

    /// List of generated instructions
    insts: Vec<String>
}

impl InstructionGenerator {
    /// Creates new instruction generator
    pub fn new(module: Rc<RefCell<WASMModule>>,
               wasm_frame: Rc<RefCell<WASMStackFrame>>) -> InstructionGenerator {
        return InstructionGenerator {
            module_: module,
            wasm_frame_: wasm_frame,
            insts: vec![]
        };
    }

    /// Returns reference to module being generated
    pub fn module(&self) -> RefMut<WASMModule> {
        return self.module_.as_ref().borrow_mut();
    }

    /// Returns reference to wasm stack frame
    pub fn wasm_frame(&self) -> RefMut<WASMStackFrame> {
        return self.wasm_frame_.as_ref().borrow_mut();
    }

    /// Returns list of generated instructions
    pub fn instructions(&self) -> Vec<String> {
        return self.insts.clone();
    }

    /// Appends instruction to list of instructions
    /// TODO: make private
    pub fn gen_inst(&mut self, instruction: &str) {
        self.insts.push(instruction.to_string());
    }

    /// Inserts instructions from another generator into beginning of list of instructions
    pub fn insert_insts_begin(&mut self, inst_gen: &InstructionGenerator) {
        let mut new_insts = inst_gen.instructions();
        new_insts.append(&mut self.insts);
        self.insts = new_insts;
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
    pub fn gen_call(&mut self, func: &WASMFunctionRef) {
        // popping parameters from the wasm stack
        for par in func.type_.params().iter().rev() {
            self.wasm_frame().pop(*par);
        }

        // pushing return values to the wasm stack
        for ret in func.type_.ret_types() {
            self.wasm_frame().push(*ret);
        }

        // generating call instruction
        self.gen_inst(&format!("call {}", func.generate()));
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, var_ref: &WASMLocalVariableRef) {
        {
            let mut frame = self.wasm_frame_.as_ref().borrow_mut();
            let var_type = frame.local(var_ref).var_type();
            frame.push(var_type);
        }
        self.gen_inst(&format!("local.get {}", self.wasm_frame().gen_local_ref(var_ref)));
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &WASMLocalVariableRef, expr: &str) {
        self.gen_inst(&format!("local.set {} {}", self.wasm_frame().gen_local_ref(local), expr));
    }

    /// Generates setting valuf of a local to current value on top of wasm stack
    pub fn gen_local_set(&mut self, var_ref: &WASMLocalVariableRef) {
        {
            let mut frame = self.wasm_frame_.as_ref().borrow_mut();
            let var_type = frame.local(var_ref).var_type();
            frame.pop(var_type);
        }
        self.gen_inst(&format!("local.set {}", self.wasm_frame().gen_local_ref(var_ref)));
    }

    /// Generates getting value of a global
    pub fn gen_global_get(&mut self, var_ref: &WASMGlobalVariableRef) {
        let (ref_str, var_type) = self.module().get_global_ref_and_type(var_ref);
        self.gen_inst(&format!("global.get {}", ref_str));
        self.wasm_frame().push(var_type);
    }

    /// Generates setting valuf of a local to current value on top of wasm stack
    pub fn gen_global_set(&mut self, var_ref: &WASMGlobalVariableRef) {
        let (ref_str, var_type) = self.module().get_global_ref_and_type(var_ref);
        self.gen_inst(&format!("global.set {}", ref_str));
        self.wasm_frame().pop(var_type);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.wasm_frame().drop();
        self.gen_inst(&format!("drop"));
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: WASMType, value: i64) {
        self.wasm_frame().push(type_);
        self.gen_inst(&format!("{}.const {}", type_.generate(), value));
    }

    /// Generates add instruction
    pub fn gen_add(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            self.wasm_frame().pop_ptr_ops();
        } else {
            self.wasm_frame().pop(type_);
            self.wasm_frame().pop(type_);
        }
        self.wasm_frame().push(type_);

        self.gen_inst(&format!("{}.add", type_.generate()));
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            self.wasm_frame().pop_ptr_ops();
        } else {
            self.wasm_frame().pop(type_);
            self.wasm_frame().pop(type_);
        }
        self.wasm_frame().push(type_);

        self.gen_inst(&format!("{}.mul", type_.generate()));
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, tp: WASMType) {
        self.wasm_frame().pop(WASMType::PTR);
        self.wasm_frame().push(tp);
        self.gen_inst(&format!("{}.load", tp.generate()));
    }

    /// Generates store instruction
    pub fn gen_store(&mut self, tp: WASMType) {
        self.wasm_frame().pop(tp);
        self.wasm_frame().pop(WASMType::PTR);
        self.gen_inst(&format!("{}.store", tp.generate()));
    }
}
