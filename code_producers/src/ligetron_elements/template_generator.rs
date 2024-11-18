
use super::func::*;
use super::types::*;
use super::value::*;
use super::wasm::*;
use super::FRContext;

use serde_json::Value;
use WASMType::*;

use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};


/// Signal kind
#[derive(PartialEq)]
#[derive(Clone)]
#[derive(Copy)]
pub enum SignalKind {
    Input,
    Output,
    Intermediate
}


/// Signal info, used to pass signal data from external code to generator
#[derive(Clone)]
pub struct SignalInfo {
    kind: SignalKind
}

impl SignalInfo {
    /// Creates new signal info
    pub fn new(kind: SignalKind) -> SignalInfo {
        return SignalInfo {
            kind: kind
        };
    }
}


/// Represents a signal in a template
struct Signal {
    kind: SignalKind,
    var: CircomValueRef
}

impl Signal {
    /// Creates new signal
    pub fn new(kind: SignalKind, var: CircomValueRef) -> Signal {
        return Signal {
            kind: kind,
            var: var
        };
    }
}


/// Type for generating code for a single template
pub struct TemplateGenerator {
    /// Name of template being generated
    name_: String,

    /// Vector of template signals
    signals: Vec<Signal>,

    /// Function generator for generating template run function
    func_: Rc<RefCell<CircomFunction>>
}

impl TemplateGenerator {
    /// Constructs new template generator
    pub fn new(size_32_bit: usize,
               fr: FRContext,
               module: Rc<RefCell<WASMModule>>,
               name: String,
               sig_info: &Vec<SignalInfo>,
               n_local_vars: usize,
               stack_ptr: WASMGlobalVariableRef) -> TemplateGenerator {

        // creating function generator for template run function
        let func_name = format!("{}_template", &name);
        let mut func = CircomFunction::new(size_32_bit,
                                           fr,
                                           module,
                                           stack_ptr,
                                           func_name,
                                           n_local_vars);

        // processing template signals
        let mut signals = Vec::<Signal>::new();
        for (idx, sig) in sig_info.iter().enumerate() {
            let sig_val_ref = match sig.kind {
                SignalKind::Input => {
                    // adding function parameter for input signal
                    func.new_circom_param(&format!("input_signal_{}", idx))
                },
                SignalKind::Output => {
                    // adding return value for output signal
                    func.add_circom_ret_val(&format!("output_signal_{}", idx))
                },
                SignalKind::Intermediate => {
                    // adding local variable for intermediate signal
                    func.new_circom_var(&format!("intermediate_signal_{}", idx))
                }
            };

            let sig = Signal::new(sig.kind, sig_val_ref);
            signals.push(sig);
        }

        let templ_gen = TemplateGenerator {
            name_: name.clone(),
            signals: signals,
            func_: Rc::new(RefCell::new(func))
        };

        return templ_gen;
    }

    /// Returns name of template being generated
    pub fn name(&self) -> &String {
        return &self.name_;
    }

    /// Returns Rc to function generator for generating template run function
    pub fn func_gen_rc(&mut self) -> Rc<RefCell<CircomFunction>> {
        return self.func_.clone();
    }

    /// Returns reference to function generator for generating template run function
    pub fn func_gen(&self) -> RefMut<CircomFunction> {
        return self.func_.as_ref().borrow_mut();
    }

    // /// Adds new signal
    // pub fn add_signal(&mut self, kind: SignalKind, var: CircomLocalVariableRef) {
    //     self.signals.push(Signal::new(kind, var));
    // }

    /// Returns reference to Circom value for signal with specified number
    pub fn signal(&self, idx: usize) -> CircomValueRef {
        assert!(idx < self.signals.len());
        return self.signals[idx].var.clone();
    }

    /// Returns reference to Circom value for variable with specified number
    pub fn circom_var(&self, idx: usize) -> CircomValueRef {
        return self.func_gen().circom_var(idx);
    }

    /// Builds and returns run function type for this template
    pub fn function_type(&self) -> CircomFunctionType {
        let mut params = Vec::<CircomValueType>::new();
        let mut ret_types = Vec::<CircomValueType>::new();

        for sig in &self.signals {
            match sig.kind {
                SignalKind::Input => {
                    params.push(CircomValueType::FR);
                },
                SignalKind::Output => {
                    ret_types.push(CircomValueType::FR);
                },
                _ => {}
            }
        }

        return CircomFunctionType::new(params, ret_types);
    }

    /// Generates final template code.
    pub fn end(&mut self) {
        // // loading output signals on stack before return
        // self.func_gen().gen_comment("returning output signals");
        // for sig in self.signals.iter().filter(|s| s.kind == SignalKind::Output) {
        //     self.func_gen().gen_load_var(&sig.var);
        // }
    }
}
