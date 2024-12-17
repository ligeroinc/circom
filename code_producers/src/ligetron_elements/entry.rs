
use super::func::*;
use super::module::*;
use super::types::*;
use super::wasm::*;


use std::rc::Rc;
use std::cell::RefCell;


/// Generates entry function for module
pub fn generate_entry(module: Rc<RefCell<CircomModule>>, func_name: String) {
    let mut func = CircomFunction::new(module.clone(), func_name.clone(), 0);

    let stack_ptr = module.borrow_mut().stack_ptr().clone();

    func.set_export_name(&func_name);

    // initializing memory stack pointer
    func.gen_wasm_comment("initializing memory stack pointer");
    let stack_start = module.borrow_mut().calc_mem_stack_start();
    func.gen_wasm_const(WASMType::PTR, stack_start as i64);
    func.gen_wasm_global_set(&stack_ptr);

    // we use beginning of memory stack to temporarly
    // store information about command line arguments

    // getting number of arguments and buffer size for arguments.
    let args_sizes_get = module.borrow_mut().wasm_module().import_function(
        "args_sizes_get",
        WASMFunctionType::new().with_ret_type(WASMType::I32).with_params(&[WASMType::PTR, WASMType::PTR]),
        "wasi_snapshot_preview1",
        "args_sizes_get");
    func.gen_wasm_comment("getting size of program arguments");
    func.gen_wasm_global_get(&stack_ptr);    // address to store number of args
    func.gen_wasm_global_get(&stack_ptr);
    func.gen_wasm_const(WASMType::I32, 4);
    func.gen_wasm_add(WASMType::PTR);                           // address to store required args buffer size
    func.gen_wasm_call(&args_sizes_get);

    // removing call return value with error code from stack
    // TODO: check error code
    func.gen_wasm_drop();

    // saving number of arguments into local
    func.gen_wasm_comment("saving number of arguments into argc local");
    let argc = func.new_wasm_named_local("argc", WASMType::I32);
    func.gen_wasm_global_get(&stack_ptr);
    func.gen_wasm_load(WASMType::I32);
    func.gen_wasm_local_set(&argc);

    // saving size of buffer for arguments into local
    func.gen_wasm_comment("saving size of buffer for arguments into argv_size local");
    let argv_size = func.new_wasm_named_local("argv_size", WASMType::I32);
    func.gen_wasm_global_get(&stack_ptr);
    func.gen_wasm_const(WASMType::I32, 4);
    func.gen_wasm_add(WASMType::PTR);
    func.gen_wasm_load(WASMType::I32);
    func.gen_wasm_local_set(&argv_size);

    // getting arguments
    let args_get = module.borrow_mut().wasm_module().import_function(
        "args_get",
        WASMFunctionType::new().with_ret_type(WASMType::I32).with_params(&[WASMType::PTR, WASMType::PTR]),
        "wasi_snapshot_preview1",
        "args_get");
    func.gen_wasm_comment("getting program arguments");
    func.gen_wasm_global_get(&stack_ptr);    // address to store pointers to arguments
    func.gen_wasm_global_get(&stack_ptr);
    func.gen_wasm_local_get(&argc);
    func.gen_wasm_const(WASMType::I32, 4);
    func.gen_wasm_mul(WASMType::I32);
    func.gen_wasm_add(WASMType::PTR);
    func.gen_wasm_call(&args_get);

    // removing call result with error code
    // TODO: check error code
    func.gen_wasm_drop();

    let main_comp_func = module.borrow_mut().main_comp_run_function();

    // saving arguments to local variables
    let mut args = Vec::<WASMLocalVariableRef>::new();
    let mut arg_idx = 1;
    for par_type in main_comp_func.tp().params() {
        let sz = match par_type {
            CircomValueType::FRArray(size) => *size,
            CircomValueType::FR => 1,
            CircomValueType::WASM(..) => {
                panic!("Component function can't have wasm parameters");
            }
        };

        for _ in 0 .. sz {
            let loc = func.new_wasm_named_local(&format!("arg_{}", arg_idx), WASMType::I64);
            func.gen_wasm_global_get(&stack_ptr);
            func.gen_wasm_const(WASMType::I32, (arg_idx * 4) as i64);
            func.gen_wasm_add(WASMType::PTR);
            func.gen_wasm_load(WASMType::PTR);
            func.gen_wasm_load(WASMType::I64);
            func.gen_wasm_local_set(&loc);
            args.push(loc);

            arg_idx += 1;
        }
    }

    func.debug_dump_state("BEFORE ENTRY RESULTS");

    // allocating FR array for results
    func.gen_wasm_comment("allocating Fr array for main component results");
    let ret_vals = func.alloc_temp_n(main_comp_func.tp().ret_types());

    func.debug_dump_state("AFTER ENTRY RESULTS");

    // creating FR values from program arguments with Fr_rawCopyS2L function

    func.gen_wasm_comment("creating Fr array for porgram arguments");
    let fr_args = func.alloc_temp_n(main_comp_func.tp().params());

    let mut arg_idx = 0;
    for (fr_arg_idx, par_type) in main_comp_func.tp().params().iter().enumerate() {
        match par_type {
            CircomValueType::FRArray(size) => {
                for i in 0 .. *size {
                    func.load_const_index(i as i32);
                    func.load_array_element_ref(&fr_args[fr_arg_idx]);
                    func.load_wasm_local(&args[arg_idx]);
                    let raw_copy = &module.borrow_mut().fr().raw_copy.clone();
                    func.gen_call(&raw_copy);
                    arg_idx += 1;
                }
            }
            CircomValueType::FR => {
                func.load_ref(&fr_args[fr_arg_idx]);
                func.load_wasm_local(&args[arg_idx]);

                func.debug_dump_state("BEFORE raw copy");
                let raw_copy = &module.borrow_mut().fr().raw_copy.clone();
                func.gen_call(&raw_copy);
                func.debug_dump_state("AFTER raw copy");

                arg_idx += 1;
            }
            CircomValueType::WASM(..) => {
                panic!("Component function can't have wasm parameters");
            }
        }
    }

    func.debug_dump_state("AFTER ENTRY ARGUMENTS");

    // executing main component
    func.gen_wasm_comment("executing main component");
    let main_func = module.borrow_mut().main_comp_run_function();
    func.gen_call(&main_func);

    // calling exit function at the end of entry function
    // TODO: pass correct exit code?
    let proc_exit = module.borrow_mut().wasm_module().import_function(
        "proc_exit",
        WASMFunctionType::new().with_params(&[WASMType::I32]),
        "wasi_snapshot_preview1",
        "proc_exit");
    func.gen_wasm_comment("calling exit function");
    func.gen_wasm_const(WASMType::I32, 0);
    func.gen_wasm_call(&proc_exit);

    func.generate(false);
}
