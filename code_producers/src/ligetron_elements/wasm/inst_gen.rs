
use super::super::log::*;
use super::frame::*;
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
    frame_: Rc<RefCell<WASMStackFrame>>,

    /// Reference to WASM stack state for generated code
    stack_: Rc<RefCell<WASMStackState>>,

    /// List of generated instructions
    insts: Vec<String>,

    /// Current indentation
    indent: usize,
}

impl InstructionGenerator {
    /// Creates new instruction generator
    pub fn new(module: Rc<RefCell<WASMModule>>,
               frame: Rc<RefCell<WASMStackFrame>>,
               stack: Rc<RefCell<WASMStackState>>) -> InstructionGenerator {
        return InstructionGenerator {
            module_: module,
            frame_: frame,
            stack_: stack,
            insts: vec![],
            indent: 0
        };
    }

    /// Returns reference to module being generated
    pub fn module(&self) -> RefMut<WASMModule> {
        return self.module_.as_ref().borrow_mut();
    }

    /// Returns reference to wasm stack frame
    pub fn frame(&self) -> RefMut<WASMStackFrame> {
        return self.frame_.as_ref().borrow_mut();
    }

    /// Returns reference to WASM stack state
    pub fn stack(&self) -> RefMut<WASMStackState> {
        return self.stack_.as_ref().borrow_mut();
    }

    /// Returns list of generated instructions
    pub fn instructions(&self) -> Vec<String> {
        return self.insts.clone();
    }

    /// Appends instruction to list of instructions
    /// TODO: make private
    pub fn gen_inst(&mut self, instruction: &str) {
        let indent_str = (0 .. self.indent * 4).map(|_| " ").collect::<String>();
        let isnt_with_indent = format!("{}{}", indent_str, instruction);
        debug_log!("~~~INST: {}", isnt_with_indent);
        self.insts.push(isnt_with_indent.to_string());
    }

    /// Increases current indentation by 1
    pub fn add_indent(&mut self) {
        self.indent += 1;
    }

    /// Decreases current indentation by 1
    pub fn remove_indent(&mut self) {
        self.indent -= 1;
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
            self.stack().pop(par);
        }

        // pushing return values to the wasm stack
        for ret in func.type_.ret_types() {
            self.stack().push(*ret);
        }

        // generating call instruction
        self.gen_inst(&format!("call {}", func.generate()));
    }

    /// Generates call indirect instruction
    pub fn gen_call_indirect(&mut self, tp: &WASMFunctionType, type_name: String) {
        // popping function index from wasm stack
        self.stack().pop(&WASMType::I32);

        // popping parameters from the wasm stack
        for par in tp.params().iter().rev() {
            self.stack().pop(par);
        }

        // pushing return values to the wasm stack
        for ret in tp.ret_types() {
            self.stack().push(*ret);
        }

        // generating call instruction
        self.gen_inst(&format!("(call_indirect (type ${}))", type_name));
    }

    /// Generates getting value of a local
    pub fn gen_local_get(&mut self, var_ref: &WASMLocalVariableRef) {
        {
            let var_type = self.frame().local(var_ref).var_type();
            self.stack().push(var_type);
        }
        self.gen_inst(&format!("local.get {}", self.frame().gen_local_ref(var_ref)));
    }

    /// Generates setting value of a local to specified string expression
    pub fn gen_local_set_expr(&mut self, local: &WASMLocalVariableRef, expr: &str) {
        self.gen_inst(&format!("local.set {} {}", self.frame().gen_local_ref(local), expr));
    }

    /// Generates setting valuf of a local to current value on top of wasm stack
    pub fn gen_local_set(&mut self, var_ref: &WASMLocalVariableRef) {
        {
            let var_type = self.frame().local(var_ref).var_type();
            self.stack().pop(&var_type);
        }
        self.gen_inst(&format!("local.set {}", self.frame().gen_local_ref(var_ref)));
    }

    /// Generates getting value of a global
    pub fn gen_global_get(&mut self, var_ref: &WASMGlobalVariableRef) {
        let (ref_str, var_type) = self.module().get_global_ref_and_type(var_ref);
        self.gen_inst(&format!("global.get {}", ref_str));
        self.stack().push(var_type);
    }

    /// Generates setting valuf of a local to current value on top of wasm stack
    pub fn gen_global_set(&mut self, var_ref: &WASMGlobalVariableRef) {
        let (ref_str, var_type) = self.module().get_global_ref_and_type(var_ref);
        self.gen_inst(&format!("global.set {}", ref_str));
        self.stack().pop(&var_type);
    }

    /// Generates drop instruction
    pub fn gen_drop(&mut self) {
        self.stack().drop();
        self.gen_inst(&format!("drop"));
    }

    /// Generates constant instruction
    pub fn gen_const(&mut self, type_: WASMType, value: i64) {
        self.stack().push(type_);
        self.gen_inst(&format!("{}.const {}", type_.generate(), value));
    }

    /// Generates add instruction
    pub fn gen_add(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            self.stack().pop_ptr_ops();
        } else {
            self.stack().pop(&type_);
            self.stack().pop(&type_);
        }

        self.gen_inst(&format!("{}.add", type_.generate()));

        self.stack().push(type_);
    }

    /// Generates sub instruction
    pub fn gen_sub(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            self.stack().pop_ptr_ops();
        } else {
            self.stack().pop(&type_);
            self.stack().pop(&type_);
        }

        self.gen_inst(&format!("{}.sub", type_.generate()));

        self.stack().push(type_);
    }

    /// Generates mul instruction
    pub fn gen_mul(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            self.stack().pop_ptr_ops();
        } else {
            self.stack().pop(&type_);
            self.stack().pop(&type_);
        }
        self.stack().push(type_);

        self.gen_inst(&format!("{}.mul", type_.generate()));
    }

    /// Generates div_u instruction
    pub fn gen_div_u(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            self.stack().pop_ptr_ops();
        } else {
            self.stack().pop(&type_);
            self.stack().pop(&type_);
        }
        self.stack().push(type_);

        self.gen_inst(&format!("{}.div_u", type_.generate()));
    }

    /// Generates rem_u instruction
    pub fn gen_rem_u(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            panic!("rem_u instruction is not supported for ptr types");
        }

        self.stack().pop(&type_);
        self.stack().pop(&type_);
        self.stack().push(type_);

        self.gen_inst(&format!("{}.rem_u", type_.generate()));
    }

    /// Generates i64.extend_i32_u instruction
    pub fn gen_i64_extend_i32_u(&mut self) {
        self.stack().pop(&WASMType::I32);
        self.stack().push(WASMType::I64);
        self.gen_inst(&format!("i64.extend_i32_u"));
    }

    /// Generates i64.extend_i32_s instruction
    pub fn gen_i64_extend_i32_s(&mut self) {
        self.stack().pop(&WASMType::I32);
        self.stack().push(WASMType::I64);
        self.gen_inst(&format!("i64.extend_i32_s"));
    }

    /// Generates i32.wrap_i64 instruction
    pub fn gen_i32_wrap_i64(&mut self) {
        self.stack().pop(&WASMType::I64);
        self.stack().push(WASMType::I32);
        self.gen_inst(&format!("i32.wrap_i64"));
    }

    /// Generates shl instruction
    pub fn gen_shl(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            panic!("shl instruction is not supported for ptr types");
        }

        self.stack().pop(&type_);
        self.stack().pop(&type_);
        self.stack().push(type_);

        self.gen_inst(&format!("{}.shl", type_.generate()));
    }

    /// Generates shr_u instruction
    pub fn gen_shr_u(&mut self, type_: WASMType) {
        if type_ == WASMType::PTR {
            panic!("shr_u instruction is not supported for ptr types");
        }

        self.stack().pop(&type_);
        self.stack().pop(&type_);
        self.stack().push(type_);

        self.gen_inst(&format!("{}.shr_u", type_.generate()));
    }

    /// Generates load instruction
    pub fn gen_load(&mut self, tp: WASMType) {
        self.stack().pop(&WASMType::PTR);
        self.stack().push(tp);
        self.gen_inst(&format!("{}.load", tp.generate()));
    }

    /// Generates store instruction
    pub fn gen_store(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.stack().pop(&WASMType::PTR);
        self.gen_inst(&format!("{}.store", tp.generate()));
    }

    /// Generates eqz instruction
    pub fn gen_eqz(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.gen_inst(&format!("{}.eqz", tp.generate()));
        self.stack().push(WASMType::I32);
    }

    /// Generates eq instruction
    pub fn gen_eq(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.stack().pop(tp);
        self.gen_inst(&format!("{}.eq", tp.generate()));
        self.stack().push(WASMType::I32);
    }

    /// Generates ne instruction
    pub fn gen_ne(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.stack().pop(tp);
        self.gen_inst(&format!("{}.ne", tp.generate()));
        self.stack().push(WASMType::I32);
    }

    /// Generates lt_u instruction
    pub fn gen_lt_u(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.stack().pop(tp);
        self.gen_inst(&format!("{}.lt_u", tp.generate()));
        self.stack().push(WASMType::I32);
    }

    /// Generates le_u instruction
    pub fn gen_le_u(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.stack().pop(tp);
        self.gen_inst(&format!("{}.le_u", tp.generate()));
        self.stack().push(WASMType::I32);
    }

    /// Generates gt_u instruction
    pub fn gen_gt_u(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.stack().pop(tp);
        self.gen_inst(&format!("{}.gt_u", tp.generate()));
        self.stack().push(WASMType::I32);
    }

    /// Generates ge_u instruction
    pub fn gen_ge_u(&mut self, tp: &WASMType) {
        self.stack().pop(tp);
        self.stack().pop(tp);
        self.gen_inst(&format!("{}.ge_u", tp.generate()));
        self.stack().push(WASMType::I32);
    }

    /// Starts generating if-else block
    pub fn gen_if(&mut self) {
        self.stack().pop(&WASMType::I32);
        self.gen_inst("(if");
        self.add_indent();
        self.gen_inst("(then");
        self.add_indent();

        self.stack().start_branch();
    }

    /// Starts generating else block
    pub fn gen_else(&mut self) {
        self.remove_indent();
        self.gen_inst(")");
        self.gen_inst("(else");
        self.add_indent();

        self.stack().end_branch();
        self.stack().start_branch();
    }

    /// Finishes generating if-else block
    pub fn gen_endif(&mut self) {
        self.remove_indent();
        self.gen_inst(")");
        self.remove_indent();
        self.gen_inst(")");

        self.stack().end_branch();
    }

    /// Starts generating loop block
    pub fn gen_loop_start(&mut self) {
        self.gen_inst("(block");
        self.add_indent();
        self.gen_inst("(loop");
        self.add_indent();

        self.stack().start_branch();
    }

    /// Finishes generating loop block and adds branch to the beginning of loop
    pub fn gen_loop_end(&mut self) {
        self.gen_inst("br 0");

        self.remove_indent();
        self.gen_inst(")");
        self.remove_indent();
        self.gen_inst(")");

        self.stack().end_branch();
    }

    /// Generates conditional exit from current loop
    pub fn gen_loop_exit(&mut self) {
        self.stack().pop(&WASMType::I32);
        self.stack().check_branch();
        self.gen_inst("br_if 1");
    }
}
