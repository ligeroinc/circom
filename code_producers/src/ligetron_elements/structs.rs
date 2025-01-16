
use super::stack::*;
use super::types::*;
use super::value::*;
use super::wasm::*;


/// Reference to struct field
#[derive(Clone)]
pub struct StructFieldRef {
    /// Reference to struct value
    str: Box<dyn CircomValueRef>,

    /// Field index
    index: usize,

    /// Optional field type instead of real field type
    tp: Option<CircomValueType>
}

impl StructFieldRef {
    /// Creates new array element reference
    pub fn new(str: Box<dyn CircomValueRef>, index: usize) -> StructFieldRef {
        return StructFieldRef {
            str: str,
            index: index,
            tp: None
        };
    }

    /// Creates new casted array element reference
    pub fn new_casted(str: Box<dyn CircomValueRef>,
                      index: usize,
                      tp: CircomValueType) -> StructFieldRef {
        return StructFieldRef {
            str: str,
            index: index,
            tp: Some(tp)
        };
    }


    /// Returns type of struct of field for this value
    pub fn struct_type(&self, frame: &CircomStackFrame) -> CircomStructType {
        let str_type = self.str.xtype(frame);
        match str_type {
            CircomValueType::Struct(stype) => {
                return stype;
            }
            _ => {
                panic!("struct field of non struct type is invalid");
            }
        }
    }
}

impl CircomValueRef for StructFieldRef {
    /// Clones value reference
    fn clone_ref(&self) -> Box<dyn CircomValueRef> {
        return Box::new(self.clone());
    }

    /// Dumps value reference to string
    fn dump_ref(&self, frame: &CircomStackFrame) -> String {
        return format!("({}).{}", self.str.dump_ref(frame), self.index);
    }

    /// Dumps value to string
    fn dump(&self, frame: &CircomStackFrame) -> String {
        return format!("{}, FR", self.dump_ref(frame));
    }

    /// Returns value type
    fn xtype(&self, frame: &CircomStackFrame) -> CircomValueType {
        if let Some(tp) = &self.tp {
            return tp.clone();
        }

        return self.struct_type(frame).fields[self.index].clone();
    }

    /// Returns true if value is located on WASM stack
    fn is_wasm_stack(&self) -> bool {
        return self.str.is_wasm_stack();
    }

    /// Generates loading of value ptr onto WASM stack
    fn gen_load_ptr_to_wasm_stack(&self,
                                  inst_gen: &mut InstructionGenerator,
                                  frame: &CircomStackFrame) {
        let offset = self.struct_type(frame).field_offset(self.index);
        self.str.gen_load_ptr_with_offset_to_wasm_stack(inst_gen, frame, offset);
    }
}


/// Returns field of struct value                 
pub fn field(str: &dyn CircomValueRef, index: usize) -> Box<dyn CircomValueRef> {
    return Box::new(StructFieldRef::new(str.clone_ref(), index));
}

/// Returns casted field of struct value                 
pub fn casted_field(str: &dyn CircomValueRef,
                    index: usize,
                    tp: CircomValueType) -> Box<dyn CircomValueRef> {
    return Box::new(StructFieldRef::new_casted(str.clone_ref(), index, tp));
}
