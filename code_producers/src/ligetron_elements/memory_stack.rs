
use super::wasm::*;

use std::rc::Rc;
use std::cell::RefCell;


/// Local variable in memory stack frame. Represents chunk of allocated memory
/// in stack frame in global memory.
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


/// Stack frame in global memory
pub struct MemoryStackFrame {
    /// Reference to module being generated
    module: Rc<RefCell<Module>>,

    /// Reference to instructions genererator for current function
    inst_gen: Rc<RefCell<InstructionGenerator>>,

    /// Reference to WASM stack frame for current function
    wasm_stack_frame: Rc<RefCell<WASMStackFrame>>,

    /// Reference to frame base WASM local variable
    frame_base: WASMLocalVariableRef,

    /// Vector of locals in allocated stack frame
    locals: Vec<MemoryStackLocal>,

    /// Size of locals in stack frame
    locals_size: usize,

    /// Stack values
    values: Vec<usize>
}

impl MemoryStackFrame {
    /// Creates new stack frame
    pub fn new(module: Rc<RefCell<Module>>,
               inst_gen: Rc<RefCell<InstructionGenerator>>,
               wasm_stack_frame: Rc<RefCell<WASMStackFrame>>) -> MemoryStackFrame {

        // creating WASM local for storing frame base
        let frame_base = inst_gen.borrow_mut().wasm_frame().new_named_local("frame_base_ptr",
                                                                                 WASMType::PTR);

        return MemoryStackFrame {
            module: module,
            inst_gen: inst_gen,
            wasm_stack_frame: wasm_stack_frame,
            frame_base: frame_base,
            locals: vec![],
            locals_size: 0,
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

    /// Pushes value of specified size on top of stack
    pub fn push(&mut self, size: usize) {
        self.values.push(size);
    }

    /// Pops value of specified size from top of stack
    pub fn pop(&mut self, size: usize) {
        let val = self.values.pop();
        match val {
            Some(sz) => {
                if sz != size {
                    panic!("invalid size of current stack value: expected: {}, found: {}",
                           size, sz);
                }
            },
            None => { panic!("can't pop value from empty stack"); }
        }
    }

    /// Generates function entry code for allocating stack
    pub fn gen_func_entry(&self, stack_ptr: &GlobalVariableRef) -> Vec<String> {
        let mut gen = InstructionGenerator::new(self.module.clone(), self.wasm_stack_frame.clone());

        // saving current value of stack pointer into frame base variable
        gen.gen_global_get(stack_ptr);
        gen.gen_local_set(&self.frame_base);

        // adjusting current stack pointer to preserve space for local variables
        gen.gen_global_get(stack_ptr);
        gen.gen_const(WASMType::I32, self.locals_size as i64);
        gen.gen_add(WASMType::PTR);
        gen.gen_global_set(stack_ptr);

        return gen.instructions();
    }

    /// Generates function exit code for deallocating stack
    pub fn gen_func_exit(&self, stack_ptr: &GlobalVariableRef) -> Vec<String> {
        // restoring original value of stack frame pointer
        let mut gen = InstructionGenerator::new(self.module.clone(), self.wasm_stack_frame.clone());
        gen.gen_local_get(&self.frame_base);
        gen.gen_global_set(stack_ptr);

        return gen.instructions();
    }
}


// /// Global memory stack
// pub struct MemoryStack {
//     /// Reference to module being generated
//     module: Rc<RefCell<Module>>,

//     /// Reference to global variable containing current stack pointer
//     stack_ptr: GlobalVariableRef,

//     /// Vector of stack frames
//     frames: Vec<MemoryStackFrame>
// }

// impl MemoryStack {
//     /// Creates new empty stack
//     pub fn new(module: Rc<RefCell<Module>>, stack_ptr_name: String) -> MemoryStack {
//         // creating new global variable for stack pointer
//         // TODO: pass correct initial address
//         let stack_ptr = module.borrow_mut().new_global(stack_ptr_name,
//                                                        WASMType::PTR,
//                                                        "(i32.const 0)".to_string());

//         return MemoryStack {
//             module: module,
//             stack_ptr: stack_ptr,
//             frames: vec![]
//         }
//     }

//     /// Pushes new frame on top of stack
//     pub fn push_frame(&mut self) {
//         self.frames.push(MemoryStackFrame::new());
//     }

//     /// Pops stack frame from top of stack
//     pub fn pop_frame(&mut self) {
//         let frame = self.frames.pop();
//         assert!(frame.is_some());
//     }

//     /// Reurns size of current stack frame
//     pub fn frame_size(&self) -> usize {
//         return self.frames.last().expect("stack is empty").size;
//     }
// }
