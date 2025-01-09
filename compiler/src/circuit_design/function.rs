use std::panic::Location;

use super::types::*;
use crate::hir::very_concrete_program::Param;
use crate::intermediate_representation::ir_interface::*;
use crate::translating_traits::*;
use code_producers::c_elements::*;
use code_producers::ligetron_elements;
use code_producers::wasm_elements::*;
use code_producers::ligetron_elements::*;
//use std::io::Write;

pub type FunctionCode = Box<FunctionCodeInfo>;
#[derive(Default)]
pub struct FunctionCodeInfo {
    pub header: String,
    pub name: String,
    pub params: Vec<Param>,
    pub returns: Vec<Dimension>,
    pub body: InstructionList,
    pub max_number_of_vars: usize,
    pub max_number_of_ops_in_expression: usize,
    pub variables: Vec<usize>,
}

impl ToString for FunctionCodeInfo {
    fn to_string(&self) -> String {
        let mut body = "".to_string();
        for i in &self.body {
            body = format!("{}{}\n", body, i.to_string());
        }
        format!("FUNCTION({})(\n{})", self.header, body)
    }
}

impl WriteWasm for FunctionCodeInfo {
    fn produce_wasm(&self, producer: &WASMProducer) -> Vec<String> {
        use code_producers::wasm_elements::wasm_code_generator::*;
        //to be revised
        let mut instructions = vec![];
        let funcdef = format!("(func ${} (type $_t_i32i32ri32)", self.header);
        instructions.push(funcdef);
        instructions.push(format!("(param {} i32)", producer.get_result_address_tag()));
        instructions.push(format!("(param {} i32)", producer.get_result_size_tag()));
	instructions.push("(result i32)".to_string()); //state 0 = OK; > 0 error
        instructions.push(format!("(local {} i32)", producer.get_cstack_tag()));
        instructions.push(format!("(local {} i32)", producer.get_lvar_tag()));
        instructions.push(format!("(local {} i32)", producer.get_expaux_tag()));
        instructions.push(format!("(local {} i32)", producer.get_temp_tag()));
        instructions.push(format!("(local {} i32)", producer.get_aux_0_tag()));
        instructions.push(format!("(local {} i32)", producer.get_aux_1_tag()));
        instructions.push(format!("(local {} i32)", producer.get_aux_2_tag()));
        instructions.push(format!("(local {} i32)", producer.get_counter_tag()));
        instructions.push(format!("(local {} i32)", producer.get_store_aux_1_tag()));
        instructions.push(format!("(local {} i32)", producer.get_store_aux_2_tag()));
        instructions.push(format!("(local {} i32)", producer.get_copy_counter_tag()));
        instructions.push(format!("(local {} i32)", producer.get_call_lvar_tag()));
        instructions.push(format!(" (local {} i32)", producer.get_merror_tag()));
        let local_info_size_u32 = producer.get_local_info_size_u32();
        //set lvar (start of auxiliar memory for vars)
        instructions.push(set_constant("0"));
        instructions.push(load32(None)); // current stack size
        let var_start = local_info_size_u32 * 4; // starts after local info
        if local_info_size_u32 != 0 {
            instructions.push(set_constant(&var_start.to_string()));
            instructions.push(add32());
        }
        instructions.push(set_local(producer.get_lvar_tag()));
        //set expaux (start of auxiliar memory for expressions)
        instructions.push(get_local(producer.get_lvar_tag()));
        let var_stack_size = self.max_number_of_vars * 4 * (producer.get_size_32_bits_in_memory()); // starts after vars
        instructions.push(set_constant(&var_stack_size.to_string()));
        instructions.push(add32());
        instructions.push(set_local(producer.get_expaux_tag()));
        //reserve stack and sets cstack (starts of local var memory)
        let needed_stack_bytes = var_start
            + var_stack_size
            + self.max_number_of_ops_in_expression * 4 * (producer.get_size_32_bits_in_memory());
        let mut reserve_stack_fr_code = reserve_stack_fr(producer, needed_stack_bytes);
        instructions.append(&mut reserve_stack_fr_code); //gives value to $cstack
        if producer.needs_comments() {
            instructions.push(";; start of the function code".to_string());
	}
        //generate code

        for t in &self.body {
            let mut instructions_body = t.produce_wasm(producer);
            instructions.append(&mut instructions_body);
        }
        instructions.push(set_constant("0"));	
        instructions.push(")".to_string());
        instructions
    }
}

impl GenerateLigetron for FunctionCodeInfo {
    fn generate_ligetron(&self, producer: &mut LigetronProducer) {
        let ret_val_size: usize =
        if self.returns.len() == 0 {
            1
        } else if self.returns.len() == 1 {
            *self.returns.first().unwrap()
        } else {
            panic!("NYI");
        };

        // building local variables info
        let local_vars = build_variables_info(&self.body, &self.variables, self.params.len());

        // starting new function
        producer.new_function(&self.header, local_vars, ret_val_size);

        // adding function parameters
        for par in &self.params {
            producer.func().new_circom_param(&par.name);
        }

        // generating function body
        for inst in &self.body {
            inst.generate_ligetron(producer);
        }

        // finishing function generation
        producer.end_function(true);
    }
}

impl WriteC for FunctionCodeInfo {
    fn produce_c(&self, producer: &CProducer, _parallel: Option<bool>) -> (Vec<String>, String) {
        use c_code_generator::*;
        let header = format!("void {}", self.header);
        let params = vec![
            declare_circom_calc_wit(),
            declare_lvar_pointer(),
            declare_component_father(),
            declare_dest_pointer(),
            declare_dest_size(),
        ];
        let mut body = vec![];
        body.push(format!("{};", declare_circuit_constants()));
        body.push(format!("{};", declare_expaux(self.max_number_of_ops_in_expression)));
        body.push(format!("{};", declare_my_template_name_function(&self.name)));
        body.push(format!("u64 {} = {};", my_id(), component_father()));
        for t in &self.body {
            let (mut instructions_body, _) = t.produce_c(producer, Some(false));
            body.append(&mut instructions_body);
        }
        let callable = build_callable(header, params, body);
        (vec![callable], "".to_string())
    }
}

impl FunctionCodeInfo {
    pub fn wrap(self) -> FunctionCode {
        FunctionCode::new(self)
    }
    pub fn is_linked(&self, name: &str, params: &Vec<Param>) -> bool {
        self.name.eq(name) && self.params.eq(params)
    }
}


/// Helper class for performing analysis and udentifying of variables that can be
/// represented as simple integer values instead of big integers
struct LocalVariablesAnalyzer {
    params_count: usize,
    variables: Vec<LocalVarInfo>
}

impl LocalVariablesAnalyzer {
    /// Creates new analyzer
    pub fn new(vars_sizes: &Vec<usize>, params_count: usize) -> LocalVariablesAnalyzer {
        // creating initial variables info with all variables represented as big integers
        let variables = vars_sizes.iter().map(|sz| {
            LocalVarInfo {
                size: *sz,
                is_32bit: false,
            }
        }).collect::<Vec<_>>();

        return LocalVariablesAnalyzer {
            params_count: params_count,
            variables: variables
        };
    }

    /// Performs analysis of local variables for specified list of instructions
    pub fn analyze(&mut self, insts: &InstructionList) {
        loop {
            if !self.process_inst_list(insts, false) {
                break;
            } 
        }
    }

    /// Returns result of analysis
    pub fn variables(self) -> Vec<LocalVarInfo> {
        return self.variables;
    }

    /// Processes list of instructions. Returns true if variables info was changed.
    fn process_inst_list(&mut self, insts: &InstructionList, is_dest_32bit: bool) -> bool {
        let mut changed = false;
        for inst in insts {
            changed |= self.process_inst(inst, is_dest_32bit);
        }

        return changed;
    }

    /// Processes single instruction. Returns true if variables info was changed.
    fn process_inst(&mut self, inst: &Instruction, dest_32bit: bool) -> bool {
        match inst {
            Instruction::Value(value_bucket) => {
                return self.process_value(value_bucket);
            }
            Instruction::Load(load_bucket) => {
                return self.process_load(load_bucket, dest_32bit);
            }
            Instruction::Store(store_bucket) => {
                return self.process_store(store_bucket);
            }
            Instruction::Compute(compute_bucket) => {
                return self.process_compute(compute_bucket, dest_32bit);
            }
            Instruction::Call(call_bucket) => {
                return self.process_call(call_bucket);
            }
            Instruction::Branch(branch_bucket) => {
                return self.process_branch(branch_bucket)
            }
            Instruction::Return(return_bucket) => {
                return self.process_return(return_bucket);
            }
            Instruction::Assert(assert_bucket) => {
                return self.process_inst(assert_bucket.evaluate.as_ref(), dest_32bit);
            }
            Instruction::Log(log_bucket) => {
                return self.process_log(&log_bucket);
            }
            Instruction::Loop(loop_bucket) => {
                return self.process_loop(&loop_bucket);
            }
            Instruction::CreateCmp(create_cmp_bucket) => {
                return self.process_create_cmp(create_cmp_bucket);
            }
        }
    }

    /// Processes value instruction. Returns true if variable info was changed.
    fn process_value(&mut self, _inst: &ValueBucket) -> bool {
        // nothing to analyze here?
        return false;
    }

    /// Processes load instruction. Returns true if variable info was changed.
    fn process_load(&mut self, inst: &LoadBucket, dest_32bit: bool) -> bool {
        let mut changed = false;

        // if we are propagating 32bit from destination then checking if load address
        // corresponds to a single variable at constant location and mark this variable
        // as 32bit
        if dest_32bit {
            match self.extract_var_index(&inst.src, &inst.address_type) {
                Some(idx) => {
                    changed |= self.set_var_32bit(idx);
                }
                _ => {}
            }
        }

        // processing source address
        changed |= self.process_location(&inst.src, &inst.address_type);

        return changed;
    }

    /// Processes store instruction. Returns true if variable info was changed.
    fn process_store(&mut self, inst: &StoreBucket) -> bool {
        let mut changed = false;

        // processing destination address
        changed |= self.process_location(&inst.dest, &inst.dest_address_type);

        // propagating 32bit property from store destination variables to source variables
        // throught compute operators, i.e. if we have "a = b + c" instruction, and 'a'
        // variable is 32bit then both variables 'b' and 'c' should also be 32bit
        let dest_var_32bit = match self.extract_var_index(&inst.dest, &inst.dest_address_type) {
            Some(idx) => self.is_var_32bit(idx),
            _ => false
        };

        changed |= self.process_inst(inst.src.as_ref(), dest_var_32bit);

        return changed;
    }

    /// Processes compute instruction. Returns true if variable info was changed.
    fn process_compute(&mut self, inst: &ComputeBucket, dest_32bit: bool) -> bool {
        let is_32bit = dest_32bit && compute_op_propagates_32bit_down(inst.op);
        return self.process_inst_list(&inst.stack, is_32bit);
    }

    /// Processes call instruction. Returns true if variable info was changed.
    fn process_call(&mut self, inst: &CallBucket) -> bool {
        return self.process_inst_list(&inst.arguments, false);
    }

    /// Processes branch instruction. Returns true if variable info was changed.
    fn process_branch(&mut self, inst: &BranchBucket) -> bool {
        let mut changed = false;

        changed |= self.process_inst(&inst.cond, false);
        changed |= self.process_inst_list(&inst.if_branch, false);
        changed |= self.process_inst_list(&inst.else_branch, false);

        return changed;
    }

    /// Processes return instruction. Returns true if variable info was changed.
    fn process_return(&mut self, inst: &ReturnBucket) -> bool {
        return self.process_inst(&inst.value, false);
    }

    /// Processes loop instruction. Returns true if variable info was changed.
    fn process_loop(&mut self, inst: &LoopBucket) -> bool {
        let mut changed = false;

        // processing loop condition
        changed |= self.process_inst(&inst.continue_condition, false);

        // processing loop body
        changed |= self.process_inst_list(&inst.body, false);

        return changed;
    }

    /// Processes log instructions. Returns true if variable info was changed.
    fn process_log(&mut self, inst: &LogBucket) -> bool {
        let mut changed = false;

        for arg in &inst.argsprint {
            match arg {
                LogBucketArg::LogExp(expr_inst) => {
                    changed |= self.process_inst(expr_inst.as_ref(), false);
                }
                LogBucketArg::LogStr(..) => {}
            }
        }

        return changed;
    }

    /// Processes create component instruction. Returns true if variable info was changed.
    fn process_create_cmp(&mut self, _inst: &CreateCmpBucket) -> bool {
        // nothing to do here for now?
        return false;
    }

    /// Extracts variable index from location. Returns None if location is not a variable
    /// or not supported to convert to i32.
    fn extract_var_index(&self, loc: &LocationRule, addr: &AddressType) -> Option<usize> {
        // skipping locations other than variables
        return match addr {
            AddressType::Variable => {
                match &loc {
                    LocationRule::Indexed { location, .. } => {
                        match location.as_ref() {
                            Instruction::Value(value_bucket) => { Some(value_bucket.value) },
                            _ => None
                        }
                    }
                    LocationRule::Mapped { .. } => None
                }
            }
            _ => None
        };
    }

    /// Processes location and converts all variables used in address calculation
    /// to simple 32bit variables. Returns true if variables info was changed.
    fn process_location(&mut self, loc: &LocationRule, addr: &AddressType) -> bool {
        let mut changed = false;

        match addr {
            AddressType::SubcmpSignal { cmp_address, .. } => {
                changed |= self.process_address(&cmp_address);
            }
            _ => {}
        }

        match &loc {
            LocationRule::Indexed { location, .. } => {
                changed |= self.process_address(location);
            }
            LocationRule::Mapped { .. } => {
            }
        }

        return changed;
    }

    /// Converts circom variable index to real variable index taking into
    /// account sizes of variables
    fn calc_real_var_index(&self, idx: usize) -> usize {
        let mut curr_idx: usize = 0;
        let mut real_var_idx: usize = 0;

        loop {
            let var_size = self.variables[real_var_idx].size;

            if curr_idx + var_size > idx {
                return real_var_idx;
            }

            real_var_idx += 1;
            curr_idx += var_size;
        }
    }

    /// Returns true if variable referenced by specified index is marked as 32bit
    fn is_var_32bit(&self, idx: usize) -> bool {
        if idx < self.params_count {
            // skipping parameters
            return false;
        }

        let var_idx = self.calc_real_var_index(idx - self.params_count);
        return self.variables[var_idx].is_32bit;
    }

    /// Marks variable referenced by number as 32bit. Returns true if variable info was changed.
    fn set_var_32bit(&mut self, idx: usize) -> bool {
        if idx < self.params_count {
            // skipping parameters
            return false;
        }

        let var_idx = self.calc_real_var_index(idx - self.params_count);

        if self.variables[var_idx].is_32bit {
            // already marked as 32bit
            return false;
        }

        self.variables[var_idx].is_32bit = true;
        return true;
    }

    /// Processes location address instruction. Returns true if variable info was changed.
    fn process_address(&mut self, inst: &Instruction) -> bool {
        match inst {
            Instruction::Value(..) => {
                return false;
            }
            Instruction::Load(load_bucket) => {
                return self.process_load(load_bucket, true);
            }
            Instruction::Compute(compute_bucket) => {
                match compute_bucket.op {
                    OperatorType::ToAddress => {
                        // processing operand as normal instruction with 32bit dest flag
                        return self.process_inst(compute_bucket.stack[0].as_ref(), true);
                    }
                    OperatorType::AddAddress => {
                        // handled below match
                    }
                    OperatorType::MulAddress => {
                        // handled below match
                    }
                    _ => {
                        panic!("unknown compute operation used in address computation");
                    }
                }

                // OperatorType::AddAddress or OperatorType::MulAddress,
                // processing address operation arguments

                let mut changed = false;

                for arg in &compute_bucket.stack {
                    changed |= self.process_address(arg);
                }

                return changed;
            }
            _ => {
                panic!("unknown instruction used in address computation");
            }
        }
    }
}


/// Builds variables information for all local variables for Ligetron target.
/// Perfrorms analysis for detecting array indexes to convert them into simple 32bit variables.
pub fn build_variables_info(insts: &InstructionList,
                            vars_sizes: &Vec<usize>,
                            params_count: usize) -> Vec<LocalVarInfo> {
    let mut analyzer = LocalVariablesAnalyzer::new(vars_sizes, params_count);
    analyzer.analyze(insts);
    return analyzer.variables();
}
