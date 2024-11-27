
use super::func::*;
use super::stack::CircomLocalVariableRef;
use super::stack::CircomParameterRef;
use super::stack::CircomReturnValueRef;
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
    kind: SignalKind,
    size: usize
}

impl SignalInfo {
    /// Creates new signal info
    pub fn new(kind: SignalKind, size: usize) -> SignalInfo {
        return SignalInfo {
            kind: kind,
            size: size
        };
    }
}


/// Represents a signal in a template
enum Signal {
    Input(CircomParameterRef),
    Output(CircomReturnValueRef),
    Intermediate(CircomLocalVariableRef),
}

impl Signal {
    /// Returns signal kind
    fn kind(&self) -> SignalKind {
        match self {
            Signal::Input(_) => SignalKind::Input,
            Signal::Output(_) => SignalKind::Output,
            Signal::Intermediate(_) => SignalKind::Intermediate
        }
    }
}


/// Reference to signal
pub struct SignalRef {
    idx: usize,
}

impl SignalRef {
    /// Creates reference to signal
    fn new(idx: usize) -> SignalRef {
        return SignalRef {
            idx: idx
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
            let sig_type = if sig.size == 1 {
                CircomValueType::FR
            } else {
                CircomValueType::FRArray(sig.size)
            };

            let signal = match sig.kind {
                SignalKind::Input => {
                    // adding function parameter for input signal
                    let par = func.new_param(format!("input_signal_{}", idx), sig_type);
                    Signal::Input(par)
                },
                SignalKind::Output => {
                    // adding return value for output signal
                    let ret = func.new_ret_val(sig_type, Some(format!("output_signal_{}", idx)));
                    Signal::Output(ret)
                },
                SignalKind::Intermediate => {
                    // adding local variable for intermediate signal
                    let var_name = format!("intermediate_signal_{}", idx);
                    let var = func.new_local_var(var_name, sig_type);
                    Signal::Intermediate(var)
                }
            };

            signals.push(signal);
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

    /// Returns reference to signal with specified index
    pub fn signal(&self, idx: usize) -> SignalRef {
        assert!(idx < self.signals.len());
        return SignalRef::new(idx);
    }

    /// Returns signal size
    pub fn signal_size(&self, sig: &SignalRef) -> usize {
        let sig_type = match &self.signals[sig.idx] {
            Signal::Input(par) => self.func_.borrow().param_type(par),
            Signal::Output(ret_val) => self.func_.borrow().ret_val_type(ret_val),
            Signal::Intermediate(var) => self.func_.borrow().local_var_type(var)
        };

        return match sig_type {
            CircomValueType::FR => 1,
            CircomValueType::FRArray(size) => size,
            CircomValueType::WASM(..) => {
                panic!("WASM types are not supported for signals");
            }
        };
    }

    /// Loads reference to signal on stack
    pub fn load_signal_ref(&mut self, sig: &SignalRef) {
        let sig = &self.signals[sig.idx];
        match sig {
            Signal::Input(par) => {
                self.func_gen().load_param_ref(par);
            }
            Signal::Output(ret_val) => {
                self.func_gen().load_ret_val_ref(ret_val);
            }
            Signal::Intermediate(loc_var) => {
                self.func_gen().load_local_var_ref(loc_var);
            }
        }
    }

    /// Loads reference to signal array on stack
    pub fn load_signal_array_ref(&mut self, sig: &SignalRef, offset: usize, size: usize) {
        let sig = &self.signals[sig.idx];
        match sig {
            Signal::Input(par) => {
                self.func_gen().load_param_array_ref(par, offset, size);
            }
            Signal::Output(ret_val) => {
                self.func_gen().load_ret_val_array_ref(ret_val, offset, size);
            }
            Signal::Intermediate(loc_var) => {
                self.func_gen().load_local_var_array_ref(loc_var, offset, size);
            }
        }
    }

    // /// Adds new signal
    // pub fn add_signal(&mut self, kind: SignalKind, var: CircomLocalVariableRef) {
    //     self.signals.push(Signal::new(kind, var));
    // }

    /// Builds and returns run function type for this template
    pub fn function_type(&self) -> CircomFunctionType {
        let mut params = Vec::<CircomValueType>::new();
        let mut ret_types = Vec::<CircomValueType>::new();

        for sig in &self.signals {
            match sig {
                Signal::Input(par) => {
                    params.push(self.func_.borrow().param_type(par));
                },
                Signal::Output(ret_val) => {
                    ret_types.push(self.func_.borrow().ret_val_type(ret_val));
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
