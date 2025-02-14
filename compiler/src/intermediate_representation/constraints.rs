use program_structure::ast::*;
use program_structure::utils::environment::VarEnvironment;

use std::collections::{HashMap, HashSet};
use by_address::ByAddress;


/// AST Visitor
trait ASTVisitor<'a> {
    fn visit_expression(&mut self, expr: &'a Expression) {}
    fn visit_infix(&mut self, expr: &'a Expression) {}
    fn visit_prefix(&mut self, expr: &'a Expression) {}
    fn visit_variable(&mut self, expr: &'a Expression) {}
    fn visit_number(&mut self, expr: &'a Expression) {}
    fn visit_call(&mut self, expr: &'a Expression) {}

    fn visit_statement(&mut self, stmt: &'a Statement) {}
    fn visit_declaration(&mut self, stmt: &'a Statement) {}
    fn visit_substitution(&mut self, stmt: &'a Statement) {}
    fn visit_block(&mut self, stmt: &'a Statement) {}
    fn visit_if_else(&mut self, stmt: &'a Statement) {}
    fn visit_while(&mut self, w_stmt: &'a Statement) {}
    fn visit_assert(&mut self, stmt: &'a Statement) {}
    fn visit_constraint_equality(&mut self, stmt: &'a Statement) {}
    fn visit_return(&mut self, stmt: &'a Statement) {}
    fn visit_log_call(&mut self, stmt: &'a Statement) {}


    /// Traverses infix expression
    fn traverse_infix(&mut self, expr: &'a Expression) {
        self.visit_infix(expr);

        if let Expression::InfixOp { lhe, rhe, .. } = expr {
            self.traverse_expression(lhe);
            self.traverse_expression(rhe);
        } else {
            panic!("expression is not an infix expression");
        }
    }

    /// Traverses prefix expression
    fn traverse_prefix(&mut self, expr: &'a Expression) {
        self.visit_prefix(expr);

        if let Expression::PrefixOp { rhe, .. } = expr {
            self.traverse_expression(rhe);
        } else {
            panic!("expression is not an infix expression");
        }
    }

    /// Traverses variable expression
    fn traverse_variable(&mut self, expr: &'a Expression) {
        self.visit_variable(expr);

        // processing index expressions
        if let Expression::Variable { access, .. } = expr {
            for acc in access {
                if let Access::ArrayAccess(idx_expr) = acc {
                    self.traverse_expression(idx_expr);
                }
            }
        }
    }

    /// Traverses number expression
    fn traverse_number(&mut self, expr: &'a Expression) {
        self.visit_number(expr);
    }

    /// Traverses call expression
    fn traverse_call(&mut self, expr: &'a Expression) {
        self.visit_call(expr);

        if let Expression::Call { args, .. } = expr {
            for arg in args {
                self.traverse_expression(arg);
            }
        }
    }

    /// Traverses expression
    fn traverse_expression(&mut self, expr: &'a Expression) {
        self.visit_expression(expr);

        if expr.is_infix() {
            self.traverse_infix(expr);
        } else if expr.is_prefix() {
            self.traverse_prefix(expr)
        } else if expr.is_variable() {
            self.traverse_variable(expr)
        } else if expr.is_number() {
            self.traverse_number(expr)
        } else if expr.is_call() {
            self.traverse_call(expr)
        } else if expr.is_array() {
            unreachable!("This expression is syntactic sugar")
        } else if expr.is_switch() {
            unreachable!("This expression is syntactic sugar")
        } else {
            unreachable!("Unknown expression")
        }
    }

    /// Traverses declaration statement
    fn traverse_declaration(&mut self, stmt: &'a Statement) {
        self.visit_declaration(stmt);
    }

    /// Traverses substitution statement
    fn traverse_substitution(&mut self, stmt: &'a Statement) {
        self.visit_substitution(stmt);

        if let Statement::Substitution { rhe, .. } = stmt {
            self.traverse_expression(rhe);
        } else {
            panic!("statement is not a substitution");
        }
    }

    /// Traverses block statement
    fn traverse_block(&mut self, stmt: &'a Statement) {
        self.visit_block(stmt);

        if let Statement::Block { stmts, .. } = stmt {
            for stmt in stmts {
                self.traverse_statement(stmt);
            }
        } else {
            panic!("statement is not a block");
        }
    }

    /// Traverses if-else statement
    fn traverse_if_else(&mut self, stmt: &'a Statement) {
        self.visit_if_else(stmt);

        if let Statement::IfThenElse { cond, if_case, else_case, .. } = stmt {
            // processing condition expressions
            self.traverse_expression(cond);

            // processing if block
            self.traverse_statement(if_case);

            // processing else block
            if let Option::Some(else_block) = else_case {
                self.traverse_statement(else_block);
            }
        } else {
            panic!("statement is not an if-else");
        }
    }

    /// Traverses while statement
    fn traverse_while(&mut self, stmt: &'a Statement) {
        self.visit_while(stmt);

        if let Statement::While { cond, stmt, .. } = stmt {
            self.traverse_expression(cond);
            self.traverse_statement(stmt);
        } else {
            panic!("statement is not a while");
        }
    }

    /// Traverses assert statement
    fn traverse_assert(&mut self, stmt: &'a Statement) {
        self.visit_assert(stmt);

        if let Statement::Assert { arg, .. } = stmt {
            self.traverse_expression(arg);
        } else {
            panic!("statement is not an assert");
        }
    }

    /// Traverses constraint equality statement
    fn traverse_constraint_equality(&mut self, stmt: &'a Statement) {
        self.visit_constraint_equality(stmt);

        if let Statement::ConstraintEquality { lhe, rhe, .. } = stmt {
            self.traverse_expression(lhe);
            self.traverse_expression(rhe);
        } else {
            panic!("statement is not an constraint equality")
        }
    }

    /// Traverses return statement
    fn traverse_return(&mut self, stmt: &'a Statement) {
        self.visit_return(stmt);

        if let Statement::Return { value, .. } = stmt {
            self.traverse_expression(value);
        } else {
            panic!("Statement is not a return");
        }
    }

    /// Traverses log statement
    fn traverse_log_call(&mut self, stmt: &'a Statement) {
        self.visit_log_call(stmt);

        if let Statement::LogCall { args, .. } = stmt {
            for arg in args {
                match arg {
                    LogArgument::LogExp(arg) => {
                        self.traverse_expression(arg);
                    }
                    LogArgument::LogStr(_) => {}
                }
            }
        } else {
            panic!("statement is not a log call");
        }
    }

    /// Traverses single statement
    fn traverse_statement(&mut self, stmt: &'a Statement) {
        self.visit_statement(stmt);

        if stmt.is_declaration() {
            self.traverse_declaration(stmt);
        } else if stmt.is_substitution() {
            self.traverse_substitution(stmt);
        } else if stmt.is_block() {
            self.traverse_block(stmt);
        } else if stmt.is_if_then_else() {
            self.traverse_if_else(stmt);
        } else if stmt.is_while() {
            self.traverse_while(stmt);
        } else if stmt.is_assert() {
            self.traverse_assert(stmt);
        } else if stmt.is_constraint_equality() {
            self.traverse_constraint_equality(stmt);
        } else if stmt.is_return() {
            self.traverse_return(stmt);
        } else if stmt.is_log_call() {
            self.traverse_log_call(stmt);
        } else if stmt.is_initialization_block() {
            unreachable!("This statement is syntactic sugar");
        } else {
            unreachable!("Unknown statement");
        }
    }
}



/// Sotres use -> def information about variables in AST. Each variable is identified
/// by reference to Statement object defining variable. 
struct VariablesUseDefs<'a> {
    vars: HashSet<ByAddress<&'a Statement>>,
    expr_uses: HashMap<ByAddress<&'a Expression>, ByAddress<&'a Statement>>,
    stmt_uses: HashMap<ByAddress<&'a Statement>, ByAddress<&'a Statement>>,
}

impl<'a> VariablesUseDefs<'a> {
    /// Creates new instance
    pub fn new(_body: &'a Statement) -> VariablesUseDefs<'a> {
        return VariablesUseDefs {
            vars: HashSet::new(),
            expr_uses: HashMap::new(),
            stmt_uses: HashMap::new()
        }
    }

    /// Builds use-def chains for AST
    pub fn build(body: &'a Statement) -> VariablesUseDefs<'a> {
        let mut builder = VariablesUseDefsBuilder::new(body);
        builder.traverse_statement(body);
        return builder.get_use_defs();
    }

    /// Adds variable
    pub fn add_var(&mut self, def: &'a Statement) {
        if !self.vars.insert(ByAddress(def)) {
            panic!("variable already exists");
        }
    }

    /// Adds variable expression use
    pub fn add_expr_use(&mut self, def: &'a Statement, var_use: &'a Expression) {
        if self.expr_uses.insert(ByAddress(var_use), ByAddress(def)) != None {
            panic!("var is already associated with use expression");
        }
    }

    /// Returns variable def for specified use expression
    pub fn get_def_for_expr(&self, var_use: &'a Expression) -> Option<&'a Statement> {
        if let Some(def) = self.expr_uses.get(&ByAddress(var_use)) {
            return Some(def);
        } else {
            return None;
        }
    }

    /// Adds variable statement use
    pub fn add_stmt_use(&mut self, def: &'a Statement, var_use: &'a Statement) {
        if self.stmt_uses.insert(ByAddress(var_use), ByAddress(def)) != None {
            panic!("var is already associated with use expression");
        }
    }

    /// Returns variable def for specified use statement
    pub fn get_def_for_stmt(&self, var_use: &'a Statement) -> Option<&'a Statement> {
        if let Some(def) = self.stmt_uses.get(&ByAddress(var_use)) {
            return Some(def);
        } else {
            return None;
        }
    }
}


/// Builder for variables use -> def information
struct VariablesUseDefsBuilder<'a> {
    body: &'a Statement,
    vars: VariablesUseDefs<'a>,
    env: VarEnvironment<&'a Statement>
}

impl <'a> VariablesUseDefsBuilder<'a> {
    pub fn new(body: &'a Statement) -> VariablesUseDefsBuilder<'a> {
        return VariablesUseDefsBuilder {
            body: body,
            vars: VariablesUseDefs::new(body),
            env: VarEnvironment::new()
        }
    }

    pub fn get_use_defs(self) -> VariablesUseDefs<'a> {
        return self.vars;
    }
}

impl <'a> ASTVisitor<'a> for VariablesUseDefsBuilder<'a> {
    /// Traverses block statement. Handles adding/removing variables block
    fn traverse_block(&mut self, stmt: &'a Statement) {
        self.visit_block(stmt);

        if let Statement::Block { stmts, .. } = stmt {
            self.env.add_variable_block();

            for stmt in stmts {
                self.traverse_statement(stmt);
            }

            self.env.remove_variable_block();
        } else {
            panic!("statement is not a block");
        }
    }

    /// Visits declaration statement
    fn visit_declaration(&mut self, stmt: &'a Statement) {
        if let Statement::Declaration { name, .. } = stmt {
            self.vars.add_var(stmt);
            self.env.add_variable(name, stmt);
        } else {
            panic!("statement is not a declaration");
        }
    }

    /// Visits substitution statement
    fn visit_substitution(&mut self, stmt: &'a Statement) {
        if let Statement::Substitution { var, op, .. } = stmt {
            if let AssignOp::AssignVar = op {
                // adding varaible use
                let def = *self.env.get_variable(var).expect("can't find variable");
                self.vars.add_stmt_use(def, stmt);
            }
        } else {
            panic!("statement is not a substitution");
        }
    }

    /// Visits variable expression
    fn visit_variable(&mut self, expr: &'a Expression) {
        if let Expression::Variable { name, .. } = expr {
            if let Some(def) = self.env.get_variable(&name) {
                self.vars.add_expr_use(def, expr);
            }
        } else {
            panic!("expression is not a variable");
        }
    }
}


/// Constraitns analyzer that analyzes code and builds set of variables that
/// should be generated with constraints
struct ContraintsAnalyzer<'ast, 'ud> {
    body: &'ast Statement,
    use_defs: &'ud VariablesUseDefs<'ast>,
    constr_vars: HashSet<ByAddress<&'ast Statement>>,
    constr_stmts: HashSet<ByAddress<&'ast Statement>>,

    /// Changed flag for iterative processing
    changed: bool,

    /// Current constrained flag for code being processed
    constrained: bool,
}

impl <'ast, 'ud> ContraintsAnalyzer<'ast, 'ud> {
    pub fn new(body: &'ast Statement,
               use_defs: &'ud VariablesUseDefs<'ast>) -> ContraintsAnalyzer<'ast, 'ud> {
        return ContraintsAnalyzer {
            body: body,
            use_defs: use_defs,
            constr_vars: HashSet::new(),
            constr_stmts: HashSet::new(),
            changed: true,
            constrained: false
        };
    }

    /// Analyzes constraints
    pub fn analyze(&mut self) {
        while self.changed {
            self.changed = false;
            self.traverse_statement(self.body);
        }
    }
}

impl <'ast, 'ud> ASTVisitor<'ast> for ContraintsAnalyzer<'ast, 'ud> {
    /// Traverses substitution statement
    fn traverse_substitution(&mut self, stmt: &'ast Statement) {
        if let Statement::Substitution { var, op, rhe, .. } = stmt {
            // changing constrained flag depending on variable constrained status or
            // assignment operation
            let constrained = if let AssignOp::AssignVar = op {
                if let Some(var_def) = self.use_defs.get_def_for_stmt(stmt) {
                    self.constr_vars.contains(&ByAddress(var_def))
                } else {
                    false
                }
            } else if let AssignOp::AssignConstraintSignal = op {
                true
            } else {
                false
            };

            if constrained {
                self.constrained = true;
                self.changed |= self.constr_stmts.insert(ByAddress(stmt));
            }

            self.traverse_expression(rhe);

            // restoring constrained flag
            self.constrained = false;
        } else {
            panic!("statement is not a substitution");
        }
    }

    /// Visits variable expression
    fn visit_variable(&mut self, expr: &'ast Expression) {
        if let Expression::Variable { name, .. } = expr {
            if self.constrained {
                if let Some(var_def) = self.use_defs.get_def_for_expr(expr) {
                    self.changed |= self.constr_vars.insert(ByAddress(var_def));
                }
            }
        } else {
            panic!("expression is not a variable");
        }
    }

    /// Traverses constraint equality statement
    fn traverse_constraint_equality(&mut self, stmt: &'ast Statement) {
        self.visit_constraint_equality(stmt);

        if let Statement::ConstraintEquality { lhe, rhe, .. } = stmt {
            // constraint equality arguments should be calculated with constraints

            let old_constrained = self.constrained;
            self.constrained = true;

            self.traverse_expression(lhe);
            self.traverse_expression(rhe);

            self.constrained = old_constrained;
        } else {
            panic!("statement is not an constraint equality")
        }
    }
}


/// Analyzes constraints for variables. Returns set of substitution statements that
/// should be generated as constrained.
pub fn analyze_var_constraints<'a>(body: &'a Statement) -> HashSet<ByAddress<&'a Statement>> {
    let use_defs = VariablesUseDefs::build(body);
    let mut analyzer = ContraintsAnalyzer::new(body, &use_defs);
    analyzer.analyze();

    return analyzer.constr_stmts;
}
