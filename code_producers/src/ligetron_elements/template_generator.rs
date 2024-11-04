
use super::core::*;


/// Type for generating code for a single template
pub struct TemplateGenerator {
    /// Vector of template signals
    signals: Vec<LocalVariableRef>,

    /// Vector of output signals
    pub output_signals: Vec<LocalVariableRef>
}

impl TemplateGenerator {
    /// Constructs new template generator
    pub fn new() -> TemplateGenerator {
        return TemplateGenerator {
            signals: Vec::<LocalVariableRef>::new(),
            output_signals: Vec::<LocalVariableRef>::new()
        };
    }

    /// Returns next signal number
    pub fn next_signal_number(&self) -> usize {
        return self.signals.len();
    }

    /// Adds new input signal
    pub fn add_input_signal(&mut self, var: LocalVariableRef) {
        self.signals.push(var);
    }

    /// Adds new output signal
    pub fn add_output_signal(&mut self, var: LocalVariableRef) {
        self.signals.push(var.clone());
        self.output_signals.push(var);
    }

    /// Adds new intermediate signal
    pub fn new_intermediate_signal(&mut self, var: LocalVariableRef) {
        self.signals.push(var);
    }

    /// Returns reference to local variable for signal with specified number
    pub fn signal(&self, idx: usize) -> LocalVariableRef {
        assert!(idx < self.signals.len());
        return self.signals[idx].clone();
    }

    /// Returns vector of output signals
    pub fn output_signals(&self) -> Vec<LocalVariableRef> {
        return self.output_signals.clone();
    }

    // /// Generates call to a component template with specified template name
    // pub fn gen_template_call(&self, name: &str, inputs: Vec<LocalVariableRef>) {
    //     // pushing 
    // }
}
