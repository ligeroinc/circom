
use super::types::*;

use WASMType::*;


/// Current state of WASM stack
#[derive(PartialEq)]
pub struct WASMStackState {
    /// Stack values
    values: Vec<WASMType>,

    /// Start indexes of current branches to verify stack
    branch_starts: Vec<usize>
}

impl WASMStackState {
    /// Creates new stack state
    pub fn new() -> WASMStackState {
        return WASMStackState {
            values: vec![],
            branch_starts: vec![]
        };
    }

    /// Pushes value on top of stack
    pub fn push(&mut self, type_: WASMType) {
        self.values.push(type_);
    }

    /// Returns type of value located on top of stack
    pub fn top(&self) -> WASMType {
        match self.values.last() {
            Some(t) => { return *t; },
            None => { panic!("Stack is empty, but expected value on stack"); }
        }
    }

    /// Checks value located on top of stack and pops it
    pub fn pop(&mut self, type_: &WASMType) {
        // checking type of value on top of stack
        match self.values.last() {
            Some(t) => {
                if *t != *type_ {
                    panic!("Type of top stack value does not match expected type: expected: {}, found: {}",
                    type_.to_string(), t.to_string());
                }
            },
            None => { panic!("Stack is empty, but expected type: {}", type_.to_string()); }
        }

        // removing value from top of stack
        self.drop();
    }

    /// Checks that values located on top of stack is suitable for pointer arithmetics
    pub fn pop_ptr_ops(&mut self) {
        if self.values.len() < 2 {
            panic!("Expected stack values for pointer arithmetics, but stack size is less than 2");
        }

        let t1 = self.top();
        self.drop();

        let t2 = self.top();
        self.drop();

        if t1 == I32 && t2 == PTR || t1 == PTR && t2 == I32 {
            return;
        }

        panic!("Expected stack values for pointer arithmetics, but found: {}, {}",
               t1.to_string(), t2.to_string());
    }

    /// Drops value from top of stack without checking it type
    pub fn drop(&mut self) {
        if self.values.is_empty() {
            panic!("Can't drop value from empty stack");
        }

        // checking branching
        match self.branch_starts.last() {
            Some(idx) => {
                if !idx == self.values.len() {
                    panic!("can't pop value located before current branch");
                }
            }
            None => {}
        }

        self.values.pop();
    }


    /// Dumps contents of stack frame to string
    pub fn dump(&self) -> String {
        return self.values.iter()
            .enumerate()
            .map(|(idx, t)| format!("{}\t{}", idx, t.to_string()))
            .collect::<Vec::<String>>()
            .join("\n");
    }

    /// Returns true if stack is empty
    pub fn is_empty(&self) -> bool {
        return self.values.is_empty();
    }

    /// Starts new branch in stack
    pub fn start_branch(&mut self) {
        self.branch_starts.push(self.values.len());
    }

    /// Checks that current stack state is equal to state before beginning
    /// of current branch
    pub fn check_branch(&self) {
        match self.branch_starts.last() {
            Some(idx) => {
                if *idx != self.values.len() {
                    panic!("additional values present on stack for current branch");
                }
            }
            None => {
                panic!("no started branch found");
            }
        }
    }

    /// Finishes branch in stack. Verifies that stack state after branching
    /// is equal to stack state before branching
    pub fn end_branch(&mut self) {
        self.check_branch();
        let res = self.branch_starts.pop();
        assert!(res.is_some());
    }
}
