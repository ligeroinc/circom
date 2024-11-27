
use super::wasm::*;
use super::log::*;

use std::rc::Rc;
use std::cell::RefCell;


/// Local variable allocated in memory stack frame. Local variables are live across
/// the entire function call.
struct MemoryStackLocal {
    /// Offset of local in stack frame
    offset: usize,

    /// Size of local
    size: usize
}

impl MemoryStackLocal {
    /// Creates new local
    pub fn new(size: usize, offset: usize) -> MemoryStackLocal {
        return MemoryStackLocal {
            size: size,
            offset: offset
        };
    }
}

/// Reference to local in memory stack frame
#[derive(Clone)]
pub struct MemoryStackLocalRef {
    idx: usize
}

impl MemoryStackLocalRef {
    /// Creates new reference to local
    pub fn new(idx: usize) -> MemoryStackLocalRef {
        return MemoryStackLocalRef {
            idx: idx
        };
    }
}


/// Value allocated in memory stack frame
struct MemoryStackValue {
    /// Index of value in stack
    idx: usize,

    /// Offset of value from beginning of temporary memory for current frame
    offset: usize,

    /// Size of allocated value
    size: usize,

    /// Was this value deallocated and popped from stack?
    deallocated: bool
}

impl MemoryStackValue {
    /// Creates new memory stack value
    fn new(idx: usize, offset: usize, size: usize) -> MemoryStackValue {
        return MemoryStackValue {
            idx: idx,
            offset: offset,
            size: size,
            deallocated: false
        };
    }

    /// Dumps memory value to string
    fn dump(&self, dump_idx: bool) -> String {
        let idx_str = if dump_idx {
            format!("#{:<3}\t", self.idx)
        } else {
            "".to_string()
        };
        return format!("{}offset=+{}\tsize={}", idx_str, self.offset, self.size);
    }
}


/// Reference to memory stack value
#[derive(Clone)]
pub struct MemoryStackValueRef {
    value_rc: Rc<RefCell<MemoryStackValue>>
}

impl MemoryStackValueRef {
    /// Creates new memory stack value reference
    fn new(value_rc: Rc<RefCell<MemoryStackValue>>) -> MemoryStackValueRef {
        return MemoryStackValueRef {
            value_rc: value_rc
        };
    }

    /// Returns index of value
    pub fn idx(&self) -> usize {
        return self.value_rc.borrow().idx;
    }

    /// Returns offset of value
    pub fn offset(&self) -> usize {
        return self.value_rc.borrow().offset;
    }

    /// Returns value size
    pub fn size(&self) -> usize {
        return self.value_rc.borrow().size;
    }

    /// Dumps memory value to string
    pub fn dump(&self, dump_idx: bool) -> String {
        return self.value_rc.borrow().dump(dump_idx);
    }
}


/// Stack frame in global memory
pub struct MemoryStackFrame {
    /// Reference to module being generated
    module: Rc<RefCell<WASMModule>>,

    /// Reference to global variable for memory stack pointer
    stack_ptr: WASMGlobalVariableRef,

    /// Reference to instructions genererator for current function
    inst_gen: Rc<RefCell<InstructionGenerator>>,

    /// Reference to WASM stack frame for current function
    wasm_stack_frame: Rc<RefCell<WASMStackFrame>>,

    /// Reference to frame base WASM local variable
    frame_base: WASMLocalVariableRef,

    /// Vector of locals in allocated stack frame
    locals: Vec<MemoryStackLocal>,

    /// Current size of locals allocated in stack frame
    locals_size: usize,

    /// Current size of temporary stack in stack frame (allocated after locals)
    stack_size: usize,

    /// Stack values
    values: Vec<Rc<RefCell<MemoryStackValue>>>
}

impl MemoryStackFrame {
    /// Creates new stack frame
    pub fn new(module: Rc<RefCell<WASMModule>>,
               stack_ptr: WASMGlobalVariableRef,
               inst_gen: Rc<RefCell<InstructionGenerator>>,
               wasm_stack_frame: Rc<RefCell<WASMStackFrame>>) -> MemoryStackFrame {

        // creating WASM local for storing frame base
        let frame_base = inst_gen.borrow_mut().wasm_frame().new_named_local("frame_base_ptr",
                                                                                 WASMType::PTR);

        return MemoryStackFrame {
            module: module,
            stack_ptr: stack_ptr,
            inst_gen: inst_gen,
            wasm_stack_frame: wasm_stack_frame,
            frame_base: frame_base,
            locals: vec![],
            locals_size: 0,
            stack_size: 0,
            values: vec![]
        };
    }

    /// Allocates new local variable of specified size. Reurns reference to local varialbe
    pub fn new_local(&mut self, size: usize) -> MemoryStackLocalRef {
        let var = MemoryStackLocal::new(size, self.locals_size);
        let var_idx = self.locals.len();
        self.locals.push(var);
        self.locals_size += size;
        return MemoryStackLocalRef::new(var_idx);
    }

    /// Returns reference to local variable at specified index
    pub fn local(&self, idx: usize) -> MemoryStackLocalRef {
        return MemoryStackLocalRef::new(idx);
    }

    /// Dumps local to string
    pub fn dump_local(&self, loc_ref: &MemoryStackLocalRef) -> String {
        let loc = &self.locals[loc_ref.idx];
        return format!("#{:<3} offset=+{} size={}", loc_ref.idx, loc.offset, loc.size);
    }

    /// Allocates values of specified sizes on top of stack. Returns references to allocated values.
    pub fn alloc(&mut self, sizes: Vec<usize>) -> Vec<MemoryStackValueRef> {
        // creating new stack values
        let mut res = Vec::<MemoryStackValueRef>::new();
        let mut total_size: usize = 0;
        for size in sizes {
            let val = MemoryStackValue::new(self.values.len(), self.stack_size, size);
            let val_rc = Rc::new(RefCell::new(val));
            self.values.push(val_rc.clone());
            self.stack_size += size;
            total_size += size;
            res.push(MemoryStackValueRef::new(val_rc));
        }

        // increasing global stack pointer
        self.inst_gen.borrow_mut().gen_global_get(&self.stack_ptr);
        self.inst_gen.borrow_mut().gen_const(WASMType::I32, total_size as i64);
        self.inst_gen.borrow_mut().gen_add(WASMType::PTR);
        self.inst_gen.borrow_mut().gen_global_set(&self.stack_ptr);

        return res;
    }

    /// Pops specified number of values from top of stack
    pub fn pop(&mut self, count: usize) {
        if count == 0 {
            return;
        }

        // removing values from top of stack and marking them deallocated,
        // calculating total size of deallocated stack space
        let mut total_size = 0;
        for _ in 0 .. count {
            match self.values.pop() {
                Some(val_rc) => {
                    val_rc.borrow_mut().deallocated = true;
                    total_size += val_rc.borrow().size;
                },
                None => {
                    panic!("Can't pop value from empty stack");
                }
            }
        }

        self.stack_size -= total_size;

        // decreasing global stack pointer
        self.inst_gen.borrow_mut().gen_global_get(&self.stack_ptr);
        self.inst_gen.borrow_mut().gen_const(WASMType::I32, -1 * (total_size as i64));
        self.inst_gen.borrow_mut().gen_add(WASMType::PTR);
        self.inst_gen.borrow_mut().gen_global_set(&self.stack_ptr);
    }

    /// Generates loading address of memory stack local to WASM stack
    pub fn gen_load_local_addr(&self, loc_ref: &MemoryStackLocalRef) {
        let loc = &self.locals[loc_ref.idx];
        self.inst_gen.borrow_mut().gen_local_get(&self.frame_base);
        if loc.offset != 0 {
            self.inst_gen.borrow_mut().gen_const(WASMType::I32, loc.offset as i64);
            self.inst_gen.borrow_mut().gen_add(WASMType::PTR);
        }
    }

    /// Generates loading address of memory stack value to WASM stack
    pub fn gen_load_value_addr(&self, val_ref: &MemoryStackValueRef, offset: usize) {
        let val = val_ref.value_rc.borrow();

        if val.deallocated {
            panic!("Trying to load address of deallocated value");
        }

        if val.offset >= self.stack_size {
            panic!("Stack value offset is less than stack size");
        }

        if offset >= val.size {
            panic!("Invalid offset inside value on memory stack")
        }

        let val_stack_ptr_offset = self.stack_size - val.offset - offset;
        self.inst_gen.borrow_mut().gen_global_get(&self.stack_ptr);
        if val_stack_ptr_offset != 0 {
            self.inst_gen.borrow_mut().gen_const(WASMType::I32, -1 * val_stack_ptr_offset as i64);
            self.inst_gen.borrow_mut().gen_add(WASMType::PTR);
        }
    }

    /// Generates function entry code for allocating stack
    pub fn gen_func_entry(&self) {
        let mut gen = InstructionGenerator::new(self.module.clone(),
                                                self.wasm_stack_frame.clone());

        // saving current value of stack pointer into frame base variable
        gen.gen_comment("saving original value of memory stack pointer");
        gen.gen_global_get(&self.stack_ptr);
        gen.gen_local_set(&self.frame_base);

        // adjusting current stack pointer to preserve space for local variables
        gen.gen_comment("adjusting value of memory stack pointer for local variables");
        gen.gen_global_get(&self.stack_ptr);
        gen.gen_const(WASMType::I32, self.locals_size as i64);
        gen.gen_add(WASMType::PTR);
        gen.gen_global_set(&self.stack_ptr);

        // adding instructions into beginning of current function
        self.inst_gen.borrow_mut().insert_insts_begin(&gen);
    }

    /// Generates function exit code for deallocating stack
    pub fn gen_func_exit(&self) {
        // restoring original value of stack frame pointer
        self.inst_gen.borrow_mut().gen_comment("restoring memory stack state");
        self.inst_gen.borrow_mut().gen_local_get(&self.frame_base);
        self.inst_gen.borrow_mut().gen_global_set(&self.stack_ptr);
    }

    /// Checks that stack is empty
    pub fn check_empty(&self) {
        if !self.values.is_empty() {
            panic!("Memory stack is not empty");
        }
    }

    /// Dumps contents of stack to string
    pub fn dump(&self) -> String {
        return self.values.iter()
            .enumerate()
            .map(|(idx, val)| {
                format!("{:3}\t{}", idx, val.borrow().dump(false))
            })
            .collect::<Vec<String>>()
            .join("\n");
    }
}
