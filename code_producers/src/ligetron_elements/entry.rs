
use super::func::*;
use super::module::*;
use super::types::*;
use super::wasm::*;

use num_bigint_dig::BigInt;

use std::rc::Rc;
use std::cell::RefCell;


/// Generates entry function for module
pub fn generate_entry(module: Rc<RefCell<CircomModule>>, func_name: String) {
    let mut func = CircomFunction::new(module.clone(), func_name.clone(), vec![]);

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

    // saving address of arguments pointers to WASM local variable
    func.gen_wasm_comment("saving address to pointers of program arguments");
    let args_pointers = func.new_wasm_named_local("args_pointers", WASMType::PTR);
    func.gen_wasm_global_get(&stack_ptr);
    func.gen_wasm_local_set(&args_pointers);

    // adjusting stack pointer to preserve space for arguments
    func.gen_wasm_comment("adjusting stack pointer to store arguments");
    func.gen_wasm_global_get(&stack_ptr);
    func.gen_wasm_local_get(&argc);
    func.gen_wasm_const(WASMType::I32, 4);
    func.gen_wasm_mul(WASMType::I32);
    func.gen_wasm_add(WASMType::PTR);
    func.gen_wasm_local_get(&argv_size);
    func.gen_wasm_add(WASMType::PTR);
    func.gen_wasm_global_set(&stack_ptr);


    // getting arguments
    let args_get = module.borrow_mut().wasm_module().import_function(
        "args_get",
        WASMFunctionType::new().with_ret_type(WASMType::I32).with_params(&[WASMType::PTR, WASMType::PTR]),
        "wasi_snapshot_preview1",
        "args_get");
    func.gen_wasm_comment("getting program arguments");
    func.gen_wasm_local_get(&args_pointers);
    func.gen_wasm_local_get(&args_pointers);
    func.gen_wasm_local_get(&argc);
    func.gen_wasm_const(WASMType::I32, 4);
    func.gen_wasm_mul(WASMType::I32);
    func.gen_wasm_add(WASMType::PTR);
    func.gen_wasm_call(&args_get);

    // removing call result with error code
    // TODO: check error code
    func.gen_wasm_drop();

    // allocating main component
    func.gen_wasm_comment("allocating fp256 values for main component");
    let main_comp = func.alloc_temp_component(module.borrow().main_comp_template().clone());

    // initializing FR values from program arguments
    func.gen_wasm_comment("initializing fp256 values for porgram arguments");
    for arg_idx in 0 .. main_comp.main_parameters_count() {
        // pointer to FR value
        let sig_ref = main_comp.main_parameter(arg_idx);
        sig_ref.gen_load_ptr_to_wasm_stack(&mut func.wasm_func_rc().borrow().inst_gen_mut(),
                                           func.get_frame());

        // pointer to string with number
        func.gen_wasm_local_get(&args_pointers);
        func.gen_wasm_const(WASMType::I32, ((arg_idx + 1) * 4) as i64);
        func.gen_wasm_add(WASMType::PTR);
        func.gen_wasm_load(WASMType::PTR);

        // number base
        func.gen_wasm_const(WASMType::I32, 10);

        let fp256_set_str = &&module.borrow_mut().ligetron().fp256_set_str.clone();
        func.gen_wasm_call(&fp256_set_str.to_wasm());
        func.gen_wasm_drop();
    }

    func.debug_dump_state("AFTER ENTRY ARGUMENTS");

    // executing main component
    func.gen_wasm_comment("executing main component");
    func.gen_call(&main_comp.run_func());

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
