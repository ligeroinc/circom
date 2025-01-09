
use super::arrays::*;
use super::func::*;
use super::stack::*;
use super::structs::*;
use super::types::*;
use super::value::CircomValueRef;
use super::wasm::*;
use super::wasm_stack::WASMStackAddress;
use super::CircomModule;

use std::path::Component;
use std::rc::Rc;
use std::cell::{RefCell, RefMut};


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

    /// Returns name of component run function
    pub fn run_func_name(&self) -> String {
        return self.name().to_owned() + "_template";
    }

    /// Returns type of component run function
    pub fn run_func_type(&self) -> CircomFunctionType {
        let par = CircomValueType::Struct(CircomComponent::component_data_struct(&self));
        return CircomFunctionType::new(vec![par], vec![]);
    }

    /// Returns reference to component run function
    pub fn run_func(&self) -> CircomFunctionRef {
        return CircomFunctionRef::new(self.run_func_name(), self.run_func_type());
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
                        let fr_box = Box::new(CircomValueType::FR);
                        params.push(CircomValueType::Array(fr_box, sig.size));
                    }
                },
                SignalKind::Output => {
                    if sig.size == 1 {
                        ret_types.push(CircomValueType::FR);
                    } else {
                        let fr_box = Box::new(CircomValueType::FR);
                        ret_types.push(CircomValueType::Array(fr_box, sig.size));
                    }
                },
                _ => {}
            }
        }

        return CircomFunctionType::new(params, ret_types);
    }
}


/// Represents component in program
#[derive(Clone)]
pub struct CircomComponent {
    /// Template from which this component was created
    template: TemplateInfo,

    /// Reference to value containing component data
    data_: Box<dyn CircomValueRef>
}

impl CircomComponent {
    /// Creates new component with specified value for component data
    pub fn new(template: TemplateInfo, data: Box<dyn CircomValueRef>) -> CircomComponent {
        return CircomComponent {
            template: template,
            data_: data
        };
    }

    /// Allocates new component with specified allocator
    pub fn allocate<Allocator>(template: TemplateInfo,
                               allocator: Allocator) -> CircomComponent
            where Allocator: FnOnce(CircomValueType) -> Box<dyn CircomValueRef> {

        let data_struct = Self::component_data_struct(&template);
        let data = allocator(CircomValueType::Struct(data_struct));

        return CircomComponent::new(template, data);
    }

    /// Builds and returns struct type for storing component data for template
    pub fn component_data_struct(template: &TemplateInfo) -> CircomStructType {
        let mut fields = Vec::<CircomValueType>::new();

        // input signals counter
        fields.push(CircomValueType::WASM(WASMType::I32));

        fn add_signal(fields: &mut Vec::<CircomValueType>, size: usize) {
            if size == 1 {
                fields.push(CircomValueType::FR);
            } else {
                fields.push(CircomValueType::Array(Box::new(CircomValueType::FR), size));
            }
        }

        for sig in &template.signals {
            match sig.kind {
                SignalKind::Input => {
                    add_signal(&mut fields, sig.size);
                }
                SignalKind::Output => {
                    add_signal(&mut fields, sig.size);
                }
                SignalKind::Intermediate => {
                }
            }
        }

        return CircomStructType {
            fields: fields
        }
    }

    /// Returns struct for component data
    pub fn data_struct(&self) -> CircomStructType {
        return Self::component_data_struct(&self.template);
    }

    /// Returns reference to component data
    pub fn data(&self) -> &Box<dyn CircomValueRef> {
        return &self.data_;
    }

    /// Returns reference to input signal counter
    pub fn input_signal_counter(&self) -> Box<dyn CircomValueRef> {
        return field(self.data_.as_ref(), 0);
    }

    /// Returns reference to input or output signal with specified index
    pub fn io_signal(&self, index: usize) -> Box<dyn CircomValueRef> {
        return field(self.data_.as_ref(), index + 1);
    }

    /// Returns name of component run function
    pub fn run_func_name(&self) -> String {
        return self.template.run_func_name();
    }

    /// Returns type of component run function
    pub fn run_func_type(&self) -> CircomFunctionType {
        return self.template.run_func_type();
    }

    /// Returns reference to component run function
    pub fn run_func(&self) -> CircomFunctionRef {
        return self.template.run_func();
    }

    /// Returns number of input signals taking into account signal arrays
    pub fn input_signals_count(&self) -> usize {
        let mut count = 0;

        for sig in &self.template.signals {
            match sig.kind {
                SignalKind::Input => {
                    count += sig.size
                },
                _ => {}
            }
        }

        return count;
    }

    /// Returns number of arguments for this subcomponent used as main entry
    pub fn main_parameters_count(&self) -> usize {
        return self.input_signals_count();
    }

    /// Returns reference to value corresponding to parameter of main entry with specified index
    pub fn main_parameter(&self, index: usize) -> Box<dyn CircomValueRef> {
        let mut curr_idx: usize = 0;
        let mut real_sig_idx: usize = 0;
        let mut io_sig_idx: usize = 0;

        loop {
            let sig = &self.template.signals[real_sig_idx];

            match sig.kind {
                SignalKind::Input => {
                    if curr_idx + sig.size > index {
                        let sig_ref = self.io_signal(io_sig_idx);
                        if sig.size == 1 {
                            return sig_ref;
                        } else {
                            let array_idx = index - curr_idx;
                            return array_element(sig_ref, array_idx);
                        }
                    }
        
                    curr_idx += sig.size;
                    io_sig_idx += 1;
                }
                SignalKind::Output => {
                    io_sig_idx += 1;
                }
                SignalKind::Intermediate => {
                    // nothing to do here
                }
            }

            real_sig_idx += 1;
        }
    }

    /// Returns number of signals
    pub fn signals_count(&self) -> usize {
        return self.template.signals.len();
    }

    /// Returns infor for signal with specified index
    pub fn signal_info(&self, idx: usize) -> &SignalInfo {
        return &self.template.signals[idx];
    }

    /// Returns size of signal with specified index
    pub fn signal_size(&self, idx: usize) -> usize {
        return self.template.signals[idx].size;
    }

    /// Loads reference to subcomponent signal with signal index located on top of stack
    pub fn load_signal_ref(&self, frame: &mut CircomStackFrame, size: usize) {
        let circom_signal_offset = frame.top_index_offset(0) as usize;

        let mut curr_idx: usize = 0;
        let mut real_sig_idx: usize = 0;
        loop {
            let sig = self.signal_info(real_sig_idx);
            let sig_size = sig.size;

            match sig.kind {
                SignalKind::Intermediate => {
                    // can't access intermediate signals of subcomponent
                    assert!(circom_signal_offset >= curr_idx + sig.size);
                    curr_idx += sig.size;
                    continue;
                }
                _ => {}
            }

            let sig_val = self.io_signal(real_sig_idx);

            if curr_idx + sig_size > circom_signal_offset {
                // Substracting offset located on top of stack.
                // This should be done in compile time inside stack logic.
                frame.load_i32_const((curr_idx as i32) * -1);
                frame.index_add();

                if size == 1 {
                    if frame.top_index_is_const(0) &&
                       frame.top_index_offset(0) == 0 &&
                       sig_size == 1 {

                        frame.drop(1);
                        frame.load_ref(sig_val.clone_ref());
                    } else {
                        frame.load_array_element_ref(sig_val.clone_ref(), CircomValueType::FR);
                    }
                } else {
                    frame.load_array_slice_ref(sig_val.clone_ref(), size);
                }

                break;
            }

            real_sig_idx += 1;
            curr_idx += sig_size;
        }
    }
}

impl CircomValueRef for CircomComponent {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("COMPONENT ADDRESS ({})", self.data().dump_ref(frame));
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        return self.dump_ref(frame);
    }

    /// Returns value type
    fn xtype(&self, _frame: &CircomStackFrame) -> CircomValueType {
        return CircomValueType::Struct(self.data_struct());
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return self.data().is_wasm_stack();
    }

    /// Casts this value to compoment
    fn as_component(&self) -> Option<&CircomComponent> {
        return Some(self);
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        self.data().gen_load_ptr_to_wasm_stack(inst_gen, frame);
    }

    /// Generates loading of value onto WASM stack
    fn gen_load_to_wasm_stack(&self,
                              _inst_gen: &mut InstructionGenerator,
                              _frame: &CircomStackFrame) {
        panic!("component can't be loaded by value");
    }
}


/// Type for generating code for a single template
pub struct Template {
    /// Name of template being generated
    name_: String,

    /// This component
    self_component: CircomComponent,

    /// Vector of this template signals
    signals: Vec<Box<dyn CircomValueRef>>,

    /// Vector of subcomponents
    subcomponents: Vec<CircomComponent>,

    /// Local variable containing array of pointers to subcomponents
    subcomponents_ptrs: CircomLocalVariableRef,

    /// Underlying template run function
    func_: Rc<RefCell<CircomFunction>>
}

impl Template {
    /// Constructs new template generator
    pub fn new(module: Rc<RefCell<CircomModule>>,
               template: TemplateInfo,
               name: String,
               subcomponent_count: usize,
               locals_info: Vec<LocalVarInfo>) -> Template {
        // creating function generator for template run function
        let func_name = template.run_func_name();
        let mut func = CircomFunction::new(module.clone(), func_name, locals_info);

        // allocating self component with creating new function parameter
        let self_comp = CircomComponent::allocate(template, |tp| {
            return Box::new(func.new_param(format!("self"), tp));
        });

        // building vector of values for signals and allocating locals for intermediated signals
        let mut signals = Vec::<Box<dyn CircomValueRef>>::new();
        let mut io_sig_idx = 0;
        for sig_idx in 0 .. self_comp.signals_count() {
            let sig = &self_comp.signal_info(sig_idx);
            match &sig.kind {
                SignalKind::Input => {
                    signals.push(self_comp.io_signal(io_sig_idx));
                    io_sig_idx += 1;
                }
                SignalKind::Output => {
                    signals.push(self_comp.io_signal(io_sig_idx));
                    io_sig_idx += 1;
                }
                SignalKind::Intermediate => {
                    let sig_type = if sig.size == 1 {
                        CircomValueType::FR
                    } else {
                        CircomValueType::Array(Box::new(CircomValueType::FR), sig.size)
                    };

                    let var_name = format!("intermediate_signal_{}", sig_idx);
                    let var = func.new_local_var(var_name, sig_type);
                    signals.push(Box::new(var));
                }
            }
        }

        // allocating variable for array of pointers to subcomponents
        let wptr_type = CircomValueType::WASM(WASMType::PTR);
        let subcomponents_type = CircomValueType::Array(Box::new(wptr_type), subcomponent_count);
        let subcomponents_ptrs = func.new_local_var(format!("subcomponents"), subcomponents_type);

        let templ_gen = Template {
            name_: name.clone(),
            self_component: self_comp,
            signals: signals,
            subcomponents: Vec::new(),
            subcomponents_ptrs: subcomponents_ptrs,
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

    /// Returns signal size
    pub fn signal_size(&self, real_sig_idx: usize) -> usize {
        return self.self_component.signal_size(real_sig_idx);
    }

    /// Loads reference to signal on stack using offset located on top of stack
    pub fn load_signal_ref(&mut self, real_sig_idx: usize, size: usize) {
        let sig = &self.signals[real_sig_idx];

        if size == 1 {
            if self.func().top_index_is_const(0) &&
               self.func().top_index_offset(0) == 0 &&
               self.signal_size(real_sig_idx) == 1 {

                self.func().drop(1);
                self.func().load_ref(sig.clone_ref());
            } else {
                self.func().load_array_element_ref(sig.clone_ref());
            }
        } else {
            self.func().load_array_slice_ref(sig.clone_ref(), size);
        }
    }

    /// Generates final template code.
    pub fn end(&mut self) {
        // nothing to do here for now
    }

    /// Creates new subcomponent
    pub fn create_subcmp(&mut self, subcmp_id: usize, template: TemplateInfo) {
        // allocating subcomponent in local variable
        let subcmp = CircomComponent::allocate(template, |tp| {
            return Box::new(self.func().new_local_var(format!("subcmp_{}", subcmp_id), tp));
        });

        // initializing subcomponent input signals counter
        let inst_gen_rc = self.func().wasm_func_rc().borrow().inst_gen_rc().clone();
        let counter = subcmp.input_signal_counter();
        counter.gen_load_ptr_to_wasm_stack(&mut inst_gen_rc.borrow_mut(), &self.func().get_frame_mut());
        self.func().gen_wasm_const(WASMType::I32, subcmp.input_signals_count() as i64);
        self.func().gen_wasm_store(WASMType::I32);

        // saving pointer to subcomponent into subcomponents array
        let ptr = array_element(self.subcomponents_ptrs.clone_ref(), subcmp_id);
        ptr.gen_load_ptr_to_wasm_stack(&mut inst_gen_rc.borrow_mut(), &self.func().get_frame_mut());
        subcmp.data().gen_load_ptr_to_wasm_stack(&mut inst_gen_rc.borrow_mut(), &self.func().get_frame_mut());
        self.func().gen_wasm_store(WASMType::PTR);

        // adding component into list of components
        // checking that subcomponent ID is equal to current number of subcomponents
        assert!(self.subcomponents.len() == subcmp_id);
        self.subcomponents.push(subcmp);
    }

    /// Creates multiple subcomponents with specified start index and size
    pub fn create_subcmp_n(&mut self, first_index: usize, template: TemplateInfo, size: usize) {
        for i in 0 .. size {
            self.create_subcmp(first_index + i, template.clone());
        }
    }

    /// Loads subcomponent address
    pub fn load_subcmp_address(&mut self) {
        if self.func().top_index_is_const(0) {
            // accessing subcomponent with constant index
            let idx = self.func().top_index_offset(0) as usize;
            self.func().get_frame_mut().pop(1);
            self.func().get_frame_mut().load_ref(self.subcomponents[idx].clone_ref());
        } else {
            let inst_gen_rc = self.func().wasm_func_rc().borrow().inst_gen_rc().clone();

            let subcmp_offset = self.func().top_index_offset(0);

            // detecting subcomponent template from subcomponent index offset
            let template = self.subcomponents[subcmp_offset as usize].template.clone();

            // detecting subcomponent data type from subcomponent index offset
            let data_type = {
                let first_comp = &self.subcomponents[subcmp_offset as usize];
                CircomValueType::Struct(first_comp.data_struct())
            };

            // loading pointer to component data onto WASM stack
            gen_load_array_element_ptr_to_wasm_stack_dyn(self.func().get_frame_mut(),
                                                         self.subcomponents_ptrs.clone_ref().as_ref(),
                                                         subcmp_offset as usize,
                                                         &mut inst_gen_rc.borrow_mut());
            self.func().gen_wasm_load(WASMType::PTR);

            self.func().get_frame_mut().pop(1);
            let comp_addr = Box::new(WASMStackAddress::new(data_type));
            let comp = Box::new(CircomComponent::new(template, comp_addr));
            self.func().get_frame_mut().load_ref(comp);
        }
    }

    /// Loads reference to subcomponent signal
    pub fn load_subcmp_signal_ref(&mut self, size: usize) {
        let comp = match self.func().get_frame().top_component(1) {
            Some(comp) => comp,
            None => {
                panic!("top stack value is not a component");
            }
        };

        let circom_signal_offset = self.func().get_frame().top_index_offset(0) as usize;

        let mut curr_idx: usize = 0;
        let mut real_sig_idx: usize = 0;
        loop {
            let sig = comp.signal_info(real_sig_idx);
            let sig_size = sig.size;

            match sig.kind {
                SignalKind::Intermediate => {
                    // can't access intermediate signals of subcomponent
                    assert!(circom_signal_offset >= curr_idx + sig.size);
                    curr_idx += sig.size;
                    continue;
                }
                _ => {}
            }

            if curr_idx + sig_size > circom_signal_offset {
                // Substracting offset located on top of stack.
                // This should be done in compile time inside stack logic.
                self.func().get_frame_mut().load_i32_const((curr_idx as i32) * -1);
                self.func().get_frame_mut().index_add();

                // top(1) -- component address
                // top(0) -- array index for array signals

                // we need to calculate:
                // sig_loc(top(1))[top(0)]

                let sig_val = comp.io_signal(real_sig_idx);

                let signal_idx = self.func().get_frame().top(0);
                let signal_idx_val = self.func().get_frame().value_kind(&signal_idx);

                if !sig_val.is_wasm_stack() || self.func().get_frame().top_index_is_const(0) {
                    // at least one of two values is constant: subcomponent address
                    // or signal index. In that case we can load signal with standard
                    // stack functions performing deffered load

                    // removing component address from stack
                    self.func().get_frame_mut().pop(2);
                    self.func().get_frame_mut().load_ref(signal_idx_val);

                    if size == 1 {
                        if self.func().get_frame().top_index_is_const(0) &&
                           self.func().get_frame().top_index_offset(0) == 0 &&
                           sig_size == 1 {
    
                            // removing signal index from stack
                            self.func().get_frame_mut().pop(1);
    
                            // loading signal reference
                            self.func().get_frame_mut().load_ref(sig_val.clone_ref());
                        } else {
                            self.func().get_frame_mut().load_array_element_ref(sig_val.clone_ref(),
                                                                               CircomValueType::FR);
                        }
                    } else {
                        self.func().get_frame_mut().load_array_slice_ref(sig_val.clone_ref(), size);
                    }
                } else {
                    // both component address and signal index are variables, we need
                    // caluclate signal address manually

                    let inst_gen_rc = self.func().wasm_func_rc().borrow().inst_gen_rc().clone();

                    // calculating and saving signal array index into temporary local
                    let idx_loc = self.func().new_wasm_local(WASMType::I32);
                    signal_idx_val.gen_load_to_wasm_stack(&mut inst_gen_rc.borrow_mut(),
                                                          self.func().get_frame());
                    self.func().gen_wasm_local_set(&idx_loc);

                    // calculating component signal address
                    sig_val.gen_load_ptr_to_wasm_stack(&mut inst_gen_rc.borrow_mut(),
                                                       self.func().get_frame());

                    // calculating array element address
                    self.func().gen_wasm_local_get(&idx_loc);
                    self.func().gen_wasm_const(WASMType::I32, CircomValueType::FR.size() as i64);
                    self.func().gen_wasm_mul(WASMType::I32);
                    self.func().gen_wasm_add(WASMType::PTR);

                    // removing component address and array index from stack
                    self.func().get_frame_mut().pop(2);

                    // adding signal address to stack
                    let addr = Box::new(WASMStackAddress::new(CircomValueType::FR));
                    self.func().get_frame_mut().load_ref(addr);
                }

                break;
            }

            real_sig_idx += 1;
            curr_idx += sig_size;
        }
    }

    /// Generates code for running subcomponent after store to signal
    pub fn gen_subcmp_run(&mut self, sig_size: usize) {
        let comp = match self.func().get_frame().top_component(0) {
            Some(comp) => comp,
            None => {
                panic!("top stack value is not a component");
            }
        };

        let inst_gen_rc = self.func().wasm_func_rc().borrow().inst_gen_rc().clone();

        let comp_loc: Option<WASMLocalVariableRef> = if comp.is_wasm_stack() {
            // saving address of component to temporary local because we need to
            // use it several times
            let loc = self.func().new_wasm_local(WASMType::PTR);
            self.func().gen_wasm_local_set(&loc);
            self.func().gen_wasm_local_get(&loc);
            Some(loc)
        } else {
            None
        };

        // decreasing number of input signals for component
        let counter = comp.input_signal_counter();
        counter.gen_load_ptr_to_wasm_stack(&mut inst_gen_rc.borrow_mut(), &self.func().get_frame_mut());

        if let Some(loc) = &comp_loc {
            self.func().gen_wasm_local_get(loc);
        }
        counter.gen_load_to_wasm_stack(&mut inst_gen_rc.borrow_mut(), &self.func().get_frame_mut());
        self.func().gen_wasm_const(WASMType::I32, sig_size as i64);
        self.func().gen_wasm_sub(WASMType::I32);
        self.func().gen_wasm_store(WASMType::I32);

        // running subcomponent code if all input signals were set
        if let Some(loc) = &comp_loc {
            self.func().gen_wasm_local_get(loc);
        }
        counter.gen_load_to_wasm_stack(&mut inst_gen_rc.borrow_mut(), &self.func().get_frame_mut());
        self.func().gen_wasm_if();
        self.func().gen_wasm_else();

        if let Some(loc) = &comp_loc {
            self.func().gen_wasm_local_get(loc);
        }

        // loading pointer to subcomponent data
        self.func().load_ref(comp.data().clone_ref());

        self.func().debug_dump_state("BEFORE COMP RUN CALL");

        // calling subcomponent run function
        self.func().gen_call(&comp.run_func());

        self.func().debug_dump_state("AFTER COMP RUN CALL");

        self.func().gen_wasm_endif();

        // removing address of subcomponent from stack
        self.func().get_frame_mut().pop(1);
    }
}
