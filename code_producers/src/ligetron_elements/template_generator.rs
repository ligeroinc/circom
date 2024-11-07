
use super::{types::*, function_generator::*};
use WASMType::*;

use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};


/// Signal kind
#[derive(PartialEq)]
#[derive(Clone)]
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
    var: LocalVariableRef
}

impl Signal {
    /// Creates new signal
    pub fn new(kind: SignalKind, var: LocalVariableRef) -> Signal {
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
    func_gen_: Rc<RefCell<FunctionGenerator>>
}

impl TemplateGenerator {
    /// Constructs new template generator
    pub fn new(name: String, signals: &Vec<SignalInfo>) -> TemplateGenerator {
        let func_name = format!("{}_template", &name);
        let mut templ_gen = TemplateGenerator {
            name_: name.clone(),
            signals: Vec::<Signal>::new(),
            func_gen_: Rc::new(RefCell::new(FunctionGenerator::new(&func_name)))
        };

        templ_gen.init(signals);

        return templ_gen;
    }

    /// Initializes generation of new function
    fn init(&mut self, signals: &Vec<SignalInfo>) {
        // NOTE: we have to process input signals before other signals because we
        // can't add local variables after function parameters

        // vector for storing variable references for all signals in their original order
        let mut sig_vars: Vec<Option<LocalVariableRef>> = vec![None; signals.len()];

        // creating function parameters for input signals
        for (idx, sig) in signals.iter()
                .enumerate()
                .filter(|(_, s)| s.kind == SignalKind::Input) {
            let par_name = format!("input_signal_{}", idx);
            let par = self.func_gen().new_param(&par_name, I64);
            sig_vars[idx] = Some(par);
        }

        // creating local variables for output signals and adding function return types
        for (idx, sig) in signals.iter()
                .enumerate().filter(|(_, s)| s.kind == SignalKind::Output) {
            // creating local variable
            let var_name = format!("output_signal_{}", idx);
            let var = self.func_gen().new_named_local(&var_name, I64);
            sig_vars[idx] = Some(var);

            // adding function return type
            self.func_gen().add_ret_type(I64);
        }
    
        // creating local variables for intermediate signals
        for (idx, sig) in
                signals.iter()
                .enumerate()
                .filter(|(_, s)| s.kind == SignalKind::Intermediate) {
            let var_name = format!("intermediate_signal_{}", idx);
            let var = self.func_gen().new_named_local(&var_name, I64);
            sig_vars[idx] = Some(var);
        }

        // adding signals
        for (idx, sig) in signals.iter().enumerate() {
            self.add_signal(sig.kind.clone(), sig_vars[idx].clone().unwrap());
        }
    }

    /// Returns name of template being generated
    pub fn name(&self) -> &String {
        return &self.name_;
    }

    /// Returns Rc to function generator for generating template run function
    pub fn func_gen_rc(&mut self) -> Rc<RefCell<FunctionGenerator>> {
        return self.func_gen_.clone();
    }

    /// Returns reference to function generator for generating template run function
    pub fn func_gen(&self) -> RefMut<FunctionGenerator> {
        return self.func_gen_.as_ref().borrow_mut();
    }

    /// Adds new signal
    pub fn add_signal(&mut self, kind: SignalKind, var: LocalVariableRef) {
        self.signals.push(Signal::new(kind, var));
    }

    /// Returns reference to local variable for signal with specified number
    pub fn signal(&self, idx: usize) -> LocalVariableRef {
        assert!(idx < self.signals.len());
        return self.signals[idx].var.clone();
    }

    /// Returns vector of variables for output signals
    pub fn output_signals(&self) -> Vec<LocalVariableRef> {
        return self.signals.iter()
            .filter(|s| s.kind == SignalKind::Output)
            .map(|s| s.var.clone()).collect();
    }

    /// Builds and returns run function type for this template
    pub fn function_type(&self) -> FunctionType {
        let mut ftype = FunctionType::new();

        for sig in &self.signals {
            match sig.kind {
                SignalKind::Input => {
                    ftype.add_param(WASMType::I64);
                },
                SignalKind::Output => {
                    ftype.add_ret_type(WASMType::I64);
                },
                _ => {}
            }
        }

        return ftype;
    }

    /// Generates final template code.
    pub fn end(&mut self) {
        // loading output signals on stack before return
        self.func_gen().gen_comment("returning output signals");
        for sig in self.signals.iter().filter(|s| s.kind == SignalKind::Output) {
            self.func_gen().gen_local_get(&sig.var);
        }
    }
}
