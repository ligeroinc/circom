
use super::func::*;
use super::stack::CircomLocalVariableRef;
use super::stack::CircomParameterRef;
use super::stack::CircomReturnValueRef;
use super::types::*;
use super::value::*;
use super::wasm::*;
use super::FRContext;
use super::LigetronProducerInfo;

use serde_json::Value;
use WASMType::*;

use std::collections::HashMap;
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


/// Template information
#[derive(Clone)]
pub struct TemplateInfo {
    /// Template name
    name_: String,

    /// Template signals
    signals: Vec<SignalInfo>,
}

impl TemplateInfo {
    /// Creates new template info
    pub fn new(name: String, signals: Vec<SignalInfo>) -> TemplateInfo {
        return TemplateInfo {
            name_: name,
            signals: signals
        }
    }

    /// Returns template name
    pub fn name(&self) -> &String {
        return &self.name_;
    }

    /// Builds and returns run function type for template
    pub fn function_type(&self) -> CircomFunctionType {
        let mut params = Vec::<CircomValueType>::new();
        let mut ret_types = Vec::<CircomValueType>::new();

        for sig in &self.signals {
            match sig.kind {
                SignalKind::Input => {
                    if sig.size == 1 {
                        params.push(CircomValueType::FR);
                    } else {
                        params.push(CircomValueType::FRArray(sig.size));
                    }
                },
                SignalKind::Output => {
                    if sig.size == 1 {
                        ret_types.push(CircomValueType::FR);
                    } else {
                        ret_types.push(CircomValueType::FRArray(sig.size));
                    }
                },
                _ => {}
            }
        }

        return CircomFunctionType::new(params, ret_types);
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


/// Subcomponent signal
pub enum SubcomponentSignal {
    Input(CircomLocalVariableRef),
    Output(CircomLocalVariableRef),
    Intermediate(usize)
}


/// Subcomponent in template
pub struct Subcomponent {
    /// ID of template
    template_id: usize,

    /// Local variables for component signals
    signals: Vec<SubcomponentSignal>,

    /// WASM local variable for storing number of input signals rest
    input_signals_count: WASMLocalVariableRef,
}

impl Subcomponent {
    /// Creates new subcomponent
    pub fn new(template_id: usize,
               signals: Vec<SubcomponentSignal>,
               input_signals_count: WASMLocalVariableRef) -> Subcomponent {
        return Subcomponent {
            template_id: template_id,
            signals: signals,
            input_signals_count: input_signals_count
        };
    }
}


/// Type for generating code for a single template
pub struct Template {
    /// Parent WASM module
    module: Rc<RefCell<WASMModule>>,

    /// Name of template being generated
    name_: String,

    /// Vector of template signals
    signals: Vec<Signal>,

    /// Vector of subcomponents
    subcomponents: Vec<Subcomponent>,

    /// Function generator for generating template run function
    func_: Rc<RefCell<CircomFunction>>
}

impl Template {
    /// Constructs new template generator
    pub fn new(size_32_bit: usize,
               fr: FRContext,
               module: Rc<RefCell<WASMModule>>,
               name: String,
               sig_info: &Vec<SignalInfo>,
               n_local_vars: usize,
               stack_ptr: WASMGlobalVariableRef) -> Template {

        // creating function generator for template run function
        let func_name = format!("{}_template", &name);
        let mut func = CircomFunction::new(size_32_bit,
                                           fr,
                                           module.clone(),
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

        let templ_gen = Template {
            module: module,
            name_: name.clone(),
            signals: signals,
            subcomponents: Vec::new(),
            func_: Rc::new(RefCell::new(func))
        };

        return templ_gen;
    }

    /// Returns name of template being generated
    pub fn name(&self) -> &String {
        return &self.name_;
    }

    /// Returns Rc to function generator for generating template run function
    pub fn func_rc(&mut self) -> Rc<RefCell<CircomFunction>> {
        return self.func_.clone();
    }

    /// Returns reference to function generator for generating template run function
    pub fn func(&self) -> RefMut<CircomFunction> {
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
                self.func().load_param_ref(par);
            }
            Signal::Output(ret_val) => {
                self.func().load_ret_val_ref(ret_val);
            }
            Signal::Intermediate(loc_var) => {
                self.func().load_local_var_ref(loc_var);
            }
        }
    }

    /// Loads reference to signal array on stack
    pub fn load_signal_array_ref(&mut self, sig: &SignalRef, offset: usize, size: usize) {
        let sig = &self.signals[sig.idx];
        match sig {
            Signal::Input(par) => {
                self.func().load_param_array_ref(par, offset, size);
            }
            Signal::Output(ret_val) => {
                self.func().load_ret_val_array_ref(ret_val, offset, size);
            }
            Signal::Intermediate(loc_var) => {
                self.func().load_local_var_array_ref(loc_var, offset, size);
            }
        }
    }

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
        // nothing to do here for now
    }

    /// Creates new subcomponent
    pub fn create_subcmp(&mut self,
                         subcmp_id: usize,
                         template_id: usize,
                         template: &TemplateInfo) {
        let mut signals = Vec::<SubcomponentSignal>::new();
        let mut input_signals_count: usize = 0;

        // allocating local variables for all subcomponent input and output signals
        let mut idx = 0;
        for sig_info in &template.signals {
            let sig = if sig_info.kind == SignalKind::Intermediate {
                SubcomponentSignal::Intermediate(sig_info.size)
            } else {
                let tp = if sig_info.size == 1 {
                    CircomValueType::FR
                } else {
                    CircomValueType::FRArray(sig_info.size)
                };

                let name = format!("subcmp_{}_signal_{}", subcmp_id, idx);
                let var = self.func().new_local_var(name, tp);
                match &sig_info.kind {
                    SignalKind::Input => SubcomponentSignal::Input(var),
                    SignalKind::Output => SubcomponentSignal::Output(var),
                    _ => { panic!("should not reach here"); }
                }
            };

            if sig_info.kind == SignalKind::Input {
                input_signals_count += sig_info.size;
            }

            signals.push(sig);
            idx += 1;
        }

        // allocating WASM local for input signals count
        let sig_count_name = format!("subcmp_{}_sig_count", subcmp_id);
        let sig_count = self.func_.borrow_mut().new_wasm_named_local(&sig_count_name,
                                                                     WASMType::I32);
        self.func().gen_const(WASMType::I32, input_signals_count as i64);
        self.func().gen_local_set(&sig_count);

        // adding component into list of components
        // checking that subcomponent ID is equal to current number of subcomponents
        assert!(self.subcomponents.len() == subcmp_id);
        self.subcomponents.push(Subcomponent::new(template_id, signals, sig_count));
    }

    /// Loads reference to subcomponent signal
    pub fn load_subcmp_signal_ref(&mut self,
                                  subcmp_idx: usize,
                                  circom_sig_idx: usize,
                                  size: usize) {
        let comp = &self.subcomponents[subcmp_idx];

        let mut curr_idx: usize = 0;
        let mut real_sig_idx: usize = 0;
        loop {
            let sig = &comp.signals[real_sig_idx];

            match &sig {
                SubcomponentSignal::Intermediate(sz) => {
                    // can't access intermediate signals of subcomponent
                    assert!(circom_sig_idx >= curr_idx + sz);
                    curr_idx += sz;
                    continue;
                }
                _ => {}
            }

            let sig_var = match &sig {
                SubcomponentSignal::Input(var) => var,
                SubcomponentSignal::Output(var) => var,
                SubcomponentSignal::Intermediate(..) => { panic!("should not reach here"); }
            };

            let sig_type = self.func_.borrow().local_var_type(&sig_var);
            let sig_size = match sig_type {
                CircomValueType::FR => 1,
                CircomValueType::FRArray(sz) => sz,
                _ => { panic!("WASM types are not allowed for signals"); }
            };

            if curr_idx + sig_size > circom_sig_idx {
                if curr_idx == circom_sig_idx && size == sig_size {
                    // reference to signal itself
                    self.func().load_local_var_ref(&sig_var);
                } else {
                    // reference to subarray inside array signal
                    self.func().load_local_var_array_ref(&sig_var,
                                                         circom_sig_idx - curr_idx,
                                                         size);
                }

                break;
            }

            real_sig_idx += 1;
            curr_idx += sig_size;
        }
    }

    /// Returns template ID for subcomponent
    pub fn subcmp_template_id(&self, subcmp_idx: usize) -> usize {
        return self.subcomponents[subcmp_idx].template_id;
    }

    /// Generates code for running subcomponent after store to signal
    pub fn gen_subcmp_run(&mut self,
                          subcmp_idx: usize,
                          sig_size: usize,
                          run_func: CircomFunctionRef) {

        let comp = &self.subcomponents[subcmp_idx];

        // decreasing number of input signals for component
        self.func().gen_local_get(&comp.input_signals_count);
        self.func().gen_const(WASMType::I32, sig_size as i64);
        self.func().gen_sub(WASMType::I32);
        self.func().gen_local_set(&comp.input_signals_count);

        // running subcomponent code if all input signals were set
        self.func().gen_local_get(&comp.input_signals_count);
        self.func().gen_if();
        self.func().gen_else();

        // loading subcomponent output signals
        for sig in &comp.signals {
            match sig {
                SubcomponentSignal::Output(var) => {
                    self.func().load_local_var_ref(var);
                }
                _ => {}
            }
        }

        // loading subcomponent input signals
        for sig in &comp.signals {
            match sig {
                SubcomponentSignal::Input(var) => {
                    self.func().load_local_var_ref(var);
                }
                _ => {}
            }
        }

        // calling template run function
        self.func().gen_call(&run_func);

        // dropping results of template run function
        let drop_count = run_func.tp().ret_types().iter().filter(|tp| tp.is_fr()).count();
        self.func().drop(drop_count);

        self.func().gen_endif();
    }
}
