use crate::translating_traits::GenerateLigetron;

use super::ir_interface::*;
use code_producers::ligetron_elements::*;

#[derive(Clone)]
pub enum StatusInput {
    Last,
    NoLast,
    Unknown,
}

#[derive(Clone)]
pub enum InputInformation {
    NoInput,
    Input {status: StatusInput},
}

impl ToString for InputInformation {
    fn to_string(&self) -> String {
        use InputInformation::*;
        match self {
            NoInput => "NO_INPUT".to_string(),
            Input { status } => {
                match status {
                    StatusInput::Last => "LAST".to_string(),
                    StatusInput::NoLast => "NO_LAST".to_string(),
                    StatusInput::Unknown => "UNKNOWN".to_string(),
                }
            }
        }
    }
}

#[derive(Clone)]
pub enum AddressType {
    Variable,
    Signal,
    SubcmpSignal { cmp_address: InstructionPointer, uniform_parallel_value: Option<bool>, is_output: bool, input_information: InputInformation },
}

impl ToString for AddressType {
    fn to_string(&self) -> String {
        use AddressType::*;
        match self {
            Variable => "VARIABLE".to_string(),
            Signal => "SIGNAL".to_string(),
            SubcmpSignal { cmp_address, input_information, .. } => format!("SUBCOMPONENT:{}:{}", cmp_address.to_string(), input_information.to_string()),
        }
    }
}


/// Generates loading of value reference for Ligetron target
pub fn generate_ligetron_load_ref(producer: &mut LigetronProducer,
                                  loc: &LocationRule,
                                  addr_t: &AddressType,
                                  size: &SizeOption,
                                  for_store: bool) {

    let sz = match &size {
        SizeOption::Single(size) => *size,
        SizeOption::Multiple(_sizes) => {
            panic!("NYI");
        }
    };

    // loading subcomponent address
    match &addr_t {
        AddressType::SubcmpSignal { cmp_address, .. } => {
            // generating code for calculating subcomponent index
            let old_comp_type = producer.set_addr_computation_type();
            cmp_address.generate_ligetron(producer);
            producer.set_computation_type(old_comp_type);

            // loading subcomponent address
            producer.load_subcmp_address();

            producer.debug_dump_state("after subcomponent address load");
        }
        _ => {}
    }

    match &loc {
        LocationRule::Indexed { location, .. } => {
            // generating code for calculating value index
            let old_comp_type = producer.set_addr_computation_type();
            location.generate_ligetron(producer);
            producer.set_computation_type(old_comp_type);

            match &addr_t {
                AddressType::Variable => {
                    producer.load_local_var_ref(sz, for_store);
                }
                AddressType::Signal => {
                    producer.load_signal_ref(sz);
                }
                AddressType::SubcmpSignal { .. } => {
                    producer.load_subcmp_signal_ref(sz);
                }
            }
        }
        LocationRule::Mapped { signal_code, indexes } => {
            // generating code for calculating mapped indexes
            let indexes_count = if !indexes.is_empty() {
                if indexes.len() != 1 {
                    panic!("multiple indexes are not supported");
                }

                let idx_access = &indexes[0];
                match idx_access {
                    AccessType::Indexed(info) => {
                        let old_comp_type = producer.set_addr_computation_type();
                        for idx_inst in &info.indexes {
                            idx_inst.generate_ligetron(producer);
                        }

                        producer.set_computation_type(old_comp_type);

                        info.indexes.len()
                    }
                    AccessType::Qualified(..) => {
                        panic!("Qualified access type is not supported");
                    }
                }
            } else {
                0
            };

            producer.debug_dump_state("after mapped indexes calculation");

            producer.load_subcmp_mapped_signal_ref(*signal_code, indexes_count);

            producer.debug_dump_state("after load mapped signal");
        }
    }
}
