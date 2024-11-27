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
                                  size: &SizeOption) {

    let sz = match &size {
        SizeOption::Single(size) => *size,
        SizeOption::Multiple(_sizes) => {
            panic!("NYI");
        }
    };

    match &loc {
        LocationRule::Indexed { location, .. } => {
            match &addr_t {
                AddressType::Variable => {
                    // extracting variable number from location instruction
                    match location.as_ref() {
                        Instruction::Value(value) => {
                            if sz == 1 {
                                producer.load_local_var_ref(value.value);
                            } else {
                                producer.load_local_var_array_ref(value.value, sz);
                            }
                        },
                        _ => { panic!("indexed signal load location is not a constant value"); }
                    }
                }
                AddressType::Signal => {
                    // extracting signal number from location instruction
                    match location.as_ref() {
                        Instruction::Value(value) => {
                            producer.load_signal_ref(value.value, sz);
                        },
                        _ => { panic!("indexed signal load location is not a constant value"); }
                    }
                }
                AddressType::SubcmpSignal { .. } => {
                    panic!("NYI");
                }
            }
        }
        LocationRule::Mapped { .. } => {
            panic!("NYI");
        }
    }
}
