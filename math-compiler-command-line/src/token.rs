use crate::complex::{Complex, ComplexHash};
use crate::equation::{Equation, EMPTY_STR, FAKE_FUNC_CSC, FAKE_FUNC_SEC, FAKE_FUNC_COT, FAKE_FUNC_ACSC, FAKE_FUNC_ASEC, FAKE_FUNC_ACOT, FAKE_FUNC_CSCH, FAKE_FUNC_SECH, FAKE_FUNC_COTH, FAKE_FUNC_ACSCH, FAKE_FUNC_ASECH, FAKE_FUNC_ACOTH, FUNC_ABS, FUNC_ACOS, FUNC_ACOSH, FUNC_ADD, FUNC_AND, FUNC_ARG, FUNC_ASIN, FUNC_ASINH, FUNC_ATAN, FUNC_ATANH, FUNC_CEIL, FUNC_CONJ, FUNC_COS, FUNC_COSH, FUNC_DIV, FUNC_EQUAL, FUNC_EXP, FUNC_FLOOR, FUNC_GREATER, FUNC_GREATER_OR_EQUAL, FUNC_IM, FUNC_LESS, FUNC_LESS_OR_EQUAL, FUNC_LN1, FUNC_LN2, FUNC_LOG2, FUNC_LOG3, FUNC_MAX, FUNC_MIN, FUNC_MUL, FUNC_NEG, FUNC_NOT, FUNC_NOT_EQUAL, FUNC_OR, FUNC_POW, FUNC_RE, FUNC_RECIP, FUNC_ROUND, FUNC_SIN, FUNC_SINH, FUNC_SQRT, FUNC_SUB, FUNC_TAN, FUNC_TANH, INSTR_ABS, INSTR_ACOS, INSTR_ACOSH, INSTR_ADD, INSTR_AND, INSTR_ARG, INSTR_ASIN, INSTR_ASINH, INSTR_ATAN, INSTR_ATANH, INSTR_CEIL, INSTR_CONJ, INSTR_COS, INSTR_COSH, INSTR_DIV, INSTR_EQUAL, INSTR_EXP, INSTR_FLOOR, INSTR_GREATER, INSTR_GREATER_OR_EQUAL, INSTR_IM, INSTR_ITER, INSTR_ITER_TEST, INSTR_JUMP, INSTR_JUMP_IF_NOT, INSTR_LESS, INSTR_LESS_OR_EQUAL, INSTR_LN1, INSTR_LN2, INSTR_LOG2, INSTR_LOG3, INSTR_MAX, INSTR_MIN, INSTR_MUL, INSTR_NEG, INSTR_NOT, INSTR_NOT_EQUAL, INSTR_OR, INSTR_POW, INSTR_RE, INSTR_READ, /*INSTR_READ_REL,*/ INSTR_RECIP, INSTR_ROUND, INSTR_SIN, INSTR_SINH, INSTR_SQRT, INSTR_STACK_SUB, INSTR_STORE, INSTR_SUB, INSTR_TAN, INSTR_TANH, INSTR_WRITE, PREC_ADD, PREC_CMP, PREC_DIV, PREC_LOG, PREC_MUL, PREC_NEG, PREC_POW, PREC_SUB};

use derivative::Derivative;
use std::mem::discriminant;
use std::collections::{HashMap, VecDeque};

// represents a token (a symbol, word, number, or other), for parsing and compiling purposes.
//#[derive(Debug, Clone, PartialEq, Eq)]
#[derive(Derivative)]
#[derivative(Debug, PartialEq, Eq)]
pub(crate) enum Token<'token> {
    // control
    Store, // store the current value on the stack.
    Iter,
    // TODO: DONT NEED THIS, USE NAME INSTEAD
    IterVar {
	name: &'token str,
    },
    IterTest {
	index_of_instr_iter_redef: usize,
    },
    Index {
	index: usize,
    },
    IterPlaceholder {
	index_of_token_iter_test: usize,
    },
    Jump { // jump, with the index of the token to jump to.
	index_of_token_placeholder: usize,
    },
    JumpIfNot { // jump if the current value is 0, wiht the index of the token to jump to.
	index_of_token_placeholder: usize,
    },
    If { // an if, with the index of another token to jump to and whether it's paired with an else.
	index_of_token_jump_if_not: usize,
	paired_with_else: bool,
    },
    Else { // an else, with the index of another token to jump to.
	index_of_token_jump: usize,
    },
    Placeholder { // a placeholder that contains the index of another instruction.
	index_of_instr: usize,
    },
    ReadRel { // read relative to the stack counter.
	rel: usize,
    },
    // func
    PrefixOp { // a prefix operator, such as negation: -z.
	id: usize,
	prec: usize,
    },
    InfixOp { // a infix operator, such as addition: z + z.
	id: usize,
	prec: usize,
	order: bool, // does what is the order of operations for x % x % x where % is the infix op? is it (x % x) % x? order should be true in this case. If it is x % (x % x), order should be false.
    },
    PostfixOp {	// a postfix operator, such as factorial: z!.
	id: usize,
	prec: usize,
    },
    FixedFunc { // a function that has a known number of arguments immediately when the function name has been parsed.
	id: usize,
	args: usize,
	name: &'token str, // name of the function,
	#[derivative(Debug="ignore")]
	#[derivative(PartialEq="ignore")]
	instructionize: Option<Box<dyn Instructionize>>,
    },
    UnfixedFunc { // a function that could have multiple different numbers of arguments, which will be determined after we finish parsing. 
	args: HashMap<usize, usize>,
	name: &'token str, // name of the function, for debugging purposes.
    },
    VarFunc { // a function where the number of arguments will supplied at runtime to the function.
	id: usize,
	args: usize,
    },
    // group
    LeftParen { // a left parenthesis.
	count: usize,
	defs: usize,
    },
    RightParen, // a right parenthesis.
    Comma, // a comma.
    LeftBracket { // a left bracket.
	count: usize,
    },
    RightBracket, // a right bracket.
    LeftCurly { // a left curly bracket.
	count: usize,
    },
    RightCurly, // a right curly bracket.
    // const
    Const { // a number, with the index to the number.
	index: usize,
    },
    // var
    Var { // a variable, with the index to the variable.
	index: usize,
    },
    // def
    /*NoArgDef { // a no argument definition.
	//name: &'token str,
	//index: usize,
    },*/
    NoArgDef,
    ArgDef { // an argument definition.
	index: usize,
    },
    Def, // a definition.
    Name { // an unknown definition.
	name: &'token str,
    },
    //Read {
    //
    //}
    NoArgVal { // a no argument val.
	index: usize,
    },
    ArgVal { // an argument val.
	index: usize,
    },
    Assign, // an assignment.
    End, // an end.
    FreeDef { // frees `count` defs off the stack.
	count: usize,
    },
    Redef,
    Range,
    Write {
	index: usize,
    },
    Block, // just a block
}

//#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) trait Instructionize {
    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, args: usize, const_tokens: &mut Vec<Complex>, const_counts: &mut Vec<usize>, const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String>;
}

impl<'t> Token<'t> {
    // returns a new store token.
    pub(crate) fn new_store() -> Self {
	return Token::Store;
    }
    // returns a new "+" infix operator.
    pub(crate) fn new_infix_op_add() -> Self {
	return Token::InfixOp{id: FUNC_ADD, prec: PREC_ADD, order: true};
    }
    // returns a new "+" unfixed function.
    pub(crate) fn new_unfixed_func_add(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(2, FUNC_ADD);
	    args
	}, name: name};
    }
    // returns a new "-" infix operator.
    pub(crate) fn new_infix_op_sub() -> Self {
	return Token::InfixOp{id: FUNC_SUB, prec: PREC_SUB, order: true};
    }
    // returns a new "-" unfixed function.
    pub(crate) fn new_unfixed_func_sub(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(2, FUNC_SUB);
	    args
	}, name: name};
    }
    // returns a new "*" infix operator.
    pub(crate) fn new_infix_op_mul() -> Self {
	return Token::InfixOp{id: FUNC_MUL, prec: PREC_MUL, order: true};
    }
    // returns a new "*" unfixed function.
    pub(crate) fn new_unfixed_func_mul(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(2, FUNC_MUL);
	    args
	}, name: name};
    }
    // returns a new "/" infix operator.
    pub(crate) fn new_infix_op_div() -> Self {
	return Token::InfixOp{id: FUNC_DIV, prec: PREC_DIV, order: true};
    }
    // returns a new "/" unfixed function.
    pub(crate) fn new_unfixed_func_div(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(2, FUNC_DIV);
	    args
	}, name: name};
    }
    // returns a new "-" prefix operator.
    pub(crate) fn new_prefix_op_neg() -> Self {
	return Token::PrefixOp{id: FUNC_NEG, prec: PREC_NEG};
    }
    // returns a new "-" unfixed function.
    pub(crate) fn new_unfixed_func_neg(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_NEG);
	    args
	}, name: name};
    }
    // returns a new "^" infix operator.
    pub(crate) fn new_infix_op_pow() -> Self {
	return Token::InfixOp{id: FUNC_POW, prec: PREC_POW, order: false};
    }
    // returns a new "^" unfixed function.
    pub(crate) fn new_fixed_func_pow(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_POW, args: 2, name, instructionize: None};
    }
    // returns a new "(" token.
    pub(crate) fn new_left_paren() -> Self {
	return Token::LeftParen{count: 0, defs: 0};
    }
    // returns a new ")" token.
    pub(crate) fn new_right_paren() -> Self {
	return Token::RightParen;
    }
    // returns a new "," token.
    pub(crate) fn new_comma() -> Self {
	return Token::Comma;
    }
    // returns a new number, referring to the number at the specific index.
    pub(crate) fn new_const(index: usize) -> Self {
	return Token::Const{index: index};
    }
    // returns a new "exp" unfixed function, with the given name.
    pub(crate) fn new_exp(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_EXP);
	    args
	}, name: name};
    }
    // returns a new "ln" unfixed function, with the given name.
    pub(crate) fn new_ln(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_LN1);
	    args.insert(2, FUNC_LN2);
	    args
	}, name: name};
    }
    // returns a new "sin" fixed function, with the given name.
    pub(crate) fn new_sin(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_SIN, args: 1, name, instructionize: None};
    }
    // returns a new "cos" fixed function, with the given name.
    pub(crate) fn new_cos(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_COS, args: 1, name, instructionize: None};
    }
    // returns a new "tan" fixed function, with the given name.
    pub(crate) fn new_tan(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_TAN, args: 1, name, instructionize: None};
    }
    // returns a new "sqrt" unfixed function, with the given name.
    pub(crate) fn new_sqrt(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_SQRT);
	    args
	}, name: name};
    }
    // returns a new "asin" unfixed function, with the given name.
    pub(crate) fn new_asin(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_ASIN);
	    args
	}, name: name};
    }
    // returns a new "acos" unfixed function, with the given name.
    pub(crate) fn new_acos(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_ACOS);
	    args
	}, name: name};
    }
    // returns a new "atan" unfixed function, with the given name.
    pub(crate) fn new_atan(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_ATAN);
	    args
	}, name: name};
    }
    // returns a new "sinh" fixed function, with the given name.
    pub(crate) fn new_sinh(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_SINH, args: 1, name, instructionize: None};
    }
    // returns a new "cosh" fixed function, with the given name.
    pub(crate) fn new_cosh(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_COSH, args: 1, name, instructionize: None};
    }
    // returns a new "tanh" fixed function, with the given name.
    pub(crate) fn new_tanh(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_TANH, args: 1, name, instructionize: None};
    }
    // returns a new "asinh" unfixed function, with the given name.
    pub(crate) fn new_asinh(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_ASINH);
	    args
	}, name: name};
    }
    // returns a new "acosh" unfixed function, with the given name.
    pub(crate) fn new_acosh(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_ACOSH);
	    args
	}, name: name};
    }
    // returns a new "atanh" unfixed function, with the given name.
    pub(crate) fn new_atanh(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_ATANH);
	    args
	}, name: name};
    }
    // returns a new "arg" fixed function, with the given name.
    pub(crate) fn new_arg(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_ARG, args: 1, name, instructionize: None};
    }
    // returns a new "abs" fixed function, with the given name.
    pub(crate) fn new_abs(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_ABS, args: 1, name, instructionize: None};
    }
    // returns a new "re" fixed function, with the given name.
    pub(crate) fn new_re(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_RE, args: 1, name, instructionize: None};
    }
    // returns a new "im" fixed function, with the given name.
    pub(crate) fn new_im(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_IM, args: 1, name, instructionize: None};
    }
    // returns a new "conj" fixed function, with the given name.
    pub(crate) fn new_conj(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_CONJ, args: 1, name, instructionize: None};
    }
    // returns a new "ceil" fixed function, with the given name.
    pub(crate) fn new_ceil(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_CEIL, args: 1, name, instructionize: None};
    }
    // returns a new "floor" fixed function, with the given name.
    pub(crate) fn new_floor(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_FLOOR, args: 1, name, instructionize: None};
    }
    // returns a new "round" fixed function, with the given name.
    pub(crate) fn new_round(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_ROUND, args: 1, name, instructionize: None};
    }
    // returns a new "log" unfixed function, with the given name.
    pub(crate) fn new_log(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(1, FUNC_LN1);
	    args.insert(2, FUNC_LOG2);
	    args.insert(3, FUNC_LOG3);
	    args
	}, name: name};
    }
    // returns a new "max" unfixed function, with the given name.
    pub(crate) fn new_max(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(2, FUNC_MAX);
	    args
	}, name: name};
    }
    // returns a new "min" unfixed function, with the given name.
    pub(crate) fn new_min(name: &'t str) -> Self {
	return Token::UnfixedFunc{args: {
	    let mut args: HashMap<usize, usize> = HashMap::new();
	    args.insert(2, FUNC_MIN);
	    args
	}, name: name};
    }
    // returns a new "==" infix operator.
    pub(crate) fn new_infix_op_equal() -> Self {
	return Token::InfixOp{id: FUNC_EQUAL, prec: PREC_CMP, order: true};
    }
    // returns a new "!=" infix operator.
    pub(crate) fn new_infix_op_not_equal() -> Self {
	return Token::InfixOp{id: FUNC_NOT_EQUAL, prec: PREC_CMP, order: true};
    }
    // returns a new ">" infix operator.
    pub(crate) fn new_infix_op_greater() -> Self {
	return Token::InfixOp{id: FUNC_GREATER, prec: PREC_CMP, order: true};
    }
    // returns a new "<" infix operator.
    pub(crate) fn new_infix_op_less() -> Self {
	return Token::InfixOp{id: FUNC_LESS, prec: PREC_CMP, order: true};
    }
    // returns a new ">=" infix operator.
    pub(crate) fn new_infix_op_greater_or_equal() -> Self {
	return Token::InfixOp{id: FUNC_GREATER_OR_EQUAL, prec: PREC_CMP, order: true};
    }
    // returns a new "<=" infix operator.
    pub(crate) fn new_infix_op_less_or_equal() -> Self {
	return Token::InfixOp{id: FUNC_LESS_OR_EQUAL, prec: PREC_CMP, order: true};
    }
    // returns a new "&" infix operator.
    pub(crate) fn new_infix_op_and() -> Self {
	return Token::InfixOp{id: FUNC_AND, prec: PREC_LOG, order: true};
    }
    // returns a new "|" infix operator.
    pub(crate) fn new_infix_op_or() -> Self {
	return Token::InfixOp{id: FUNC_OR, prec: PREC_LOG, order: true};
    }
    // returns a new "~" prefix operator.
    pub(crate) fn new_prefix_op_not() -> Self {
	return Token::PrefixOp{id: FUNC_NOT, prec: PREC_LOG};
    }
    // returns true if these two tokens have the same enum variant, otherwise false.
    pub(crate) fn variant_eq(v0: &Token, v1: &Token) -> bool {
	return discriminant(v0) == discriminant(v1);
    }
    // returns a new fixed function, with the given id, args, and name.
    pub(crate) fn new_fixed_func(id: usize, args: usize, name: &'t str) -> Self {
	return Token::FixedFunc{id, args, name, instructionize: None};
    }
    // returns a new "jump if not" token.
    pub(crate) fn new_jump_if_not() -> Self {
	return Token::JumpIfNot{index_of_token_placeholder: 0}; // 0 is a dummy value
    }
    // returns a new "jump" token.
    pub(crate) fn new_jump() -> Self {
	return Token::Jump{index_of_token_placeholder: 0}; // 0 is a dummy value.
    }
    // returns a new "if" token.
    pub(crate) fn new_if() -> Self {
	return Token::If{index_of_token_jump_if_not: 0, paired_with_else: false}; // 0 is dummy value.
    }
    // returns a new "else" token.
    pub(crate) fn new_else() -> Self {
	return Token::Else{index_of_token_jump: 0}; // 0 is dummy value.
    }
    // returns a new "placeholder" token.
    pub(crate) fn new_placeholder() -> Self {
	return Token::Placeholder{index_of_instr: 0}; // 0 is dummy value.
    }
    // returns a new "[" token.
    pub(crate) fn new_left_bracket() -> Self {
	return Token::LeftBracket{count: 0};
    }
    // returns a new "]" token.
    pub(crate) fn new_right_bracket() -> Self {
	return Token::RightBracket;
    }
    // returns a new "{" token.
    pub(crate) fn new_left_curly() -> Self {
	return Token::LeftCurly{count: 0};
    }
    // returns a new "}" token.
    pub(crate) fn new_right_curly() -> Self {
	return Token::RightCurly;
    }
    // returns true if this token is a "(", otherwise false.
    pub(crate) fn is_left_paren(&self) -> bool {
	if let Token::LeftParen{..} = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a ")", otherwise false.
    pub(crate) fn is_right_paren(&self) -> bool {
	if let Token::RightParen = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a ",", otherwise false.
    pub(crate) fn is_comma(&self) -> bool {
	if let Token::Comma = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a "[", otherwise false.
    pub(crate) fn is_left_bracket(&self) -> bool {
	if let Token::LeftBracket{..} = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a "]", otherwise false.
    pub(crate) fn is_right_bracket(&self) -> bool {
	if let Token::RightBracket = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a "{", otherwise false.
    pub(crate) fn is_left_curly(&self) -> bool {
	if let Token::LeftCurly{..} = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a "}", otherwise false.
    pub(crate) fn is_right_curly(&self) -> bool {
	if let Token::RightCurly{..} = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a "(", "[", or "{", otherwise false.
    pub(crate) fn is_left_group(&self) -> bool {
	match self {
	    Token::LeftParen{..} | Token::LeftBracket{..} | Token::LeftCurly{..} => {
		return true;
	    }
	    _ => {}
	}
	return false;
    }
    // returns true if this token is a ")", "]", or "}", otherwise false.
    pub(crate) fn is_right_group(&self) -> bool {
	match self {
	    Token::RightParen | Token::RightBracket | Token::RightCurly => {
		return true;
	    }
	    _ => {}
	}
	return false;
    }
    // returns true if this token is a fixed function, unfixed function, or variable function, otherwise false.
    pub(crate) fn is_func(&self) -> bool {
	match self {
	    Token::FixedFunc{..} | Token::UnfixedFunc{..} | Token::VarFunc{..} => {
		return true;
	    }
	    _ => {}
	}
	return false;
    }
    // returns true if this token is a prefix operator, infix operator, or postfix operator, otherwise false.
    pub(crate) fn is_op(&self) -> bool {
	match self {
	    Token::PrefixOp{..} | Token::InfixOp{..} | Token::PostfixOp{..} => {
		return true;
	    },
	    _ => {},
	}
	return false;
    }
    // if this token is an operator, returns Some(operator precedence), otherwise None.
    pub(crate) fn op_prec(&self) -> Option<usize> {
	match self {
	    Token::PrefixOp{prec, ..} | Token::InfixOp{prec, ..} | Token::PostfixOp{prec, ..} => {
		return Some(*prec);
	    },
	    _ => {
		return None;
	    },
	}
    }
    // returns true if this token is an infix operator, otherwise false.
    pub(crate) fn is_infix_op(&self) -> bool {
	if let Token::InfixOp{..} = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is an "if", otherwise false.
    pub(crate) fn is_if(&self) -> bool {
	if let Token::If{..} = self {
	    return true;
	}
	return false;
    }
    // encodes the current token into bytes and pushes them onto instrs; if this token causes the store count to change, store_count and max_store_count should be updated accordingly;
    // const_instrs, instr_tokens, offset, max_instrs_len, and def_store_indices are provided if needed.
    pub(crate) fn encode(&self,
			 instrs: &mut Vec<u8>,
			 const_instrs: &Vec<usize>,
			 store_count: &mut usize,
			 max_store_count: &mut usize,
			 instr_tokens: &mut VecDeque<Token>,
			 offset: usize,
			 max_instrs_len: usize,
			 def_store_indices: &mut Vec<usize>) {
			 //iter_stack_indices: &mut Vec<usize>) {
	match self {
	    // control
	    Token::Store => {
		instrs.push(INSTR_STORE);
		*store_count += 1;
		if *store_count > *max_store_count {
		    *max_store_count = *store_count;
		}
	    }
	    Token::Iter => {
		instrs.push(INSTR_ITER);
		//iter_stack_indices.push(*store_count);
		*store_count += 2;
		if *store_count > *max_store_count {
		    *max_store_count = *store_count;
		}
	    }
	    Token::IterPlaceholder{index_of_token_iter_test} => {
		let iter_test: &mut Token = &mut instr_tokens[*index_of_token_iter_test - offset];
		if let Token::IterTest{index_of_instr_iter_redef} = iter_test {
		    // always the case.
		    *index_of_instr_iter_redef = instrs.len();
		}
	    }
	    Token::IterTest{index_of_instr_iter_redef} => {
		instrs.push(INSTR_ITER_TEST);
		Equation::encode_value(*index_of_instr_iter_redef, instrs);
		*store_count -= 3;
		def_store_indices.pop();
		def_store_indices.pop();
	    }
	    Token::JumpIfNot{index_of_token_placeholder} => {
		instrs.push(INSTR_JUMP_IF_NOT);
		let placeholder: &mut Token = &mut instr_tokens[*index_of_token_placeholder - offset];
		if let Token::Placeholder{index_of_instr} = placeholder {
		    // always the case.
		    *index_of_instr = instrs.len();
		}
		Equation::encode_value_sized(0, max_instrs_len - 1, instrs); // 0 is dummy value.
	    }
	    Token::Jump{index_of_token_placeholder} => {
		instrs.push(INSTR_JUMP);
		let placeholder: &mut Token = &mut instr_tokens[*index_of_token_placeholder - offset];
		if let Token::Placeholder{index_of_instr} = placeholder {
		    // always the case.
		    *index_of_instr = instrs.len();
		}
		Equation::encode_value_sized(0, max_instrs_len - 1, instrs); // 0 is dummy value.
	    }
	    Token::Placeholder{index_of_instr} => {
		Equation::encode_value_sized_overwrite(instrs.len(), max_instrs_len - 1, instrs, *index_of_instr);
	    }
	    // func
	    Token::PrefixOp{id, ..} => {
		Equation::encode_func(*id, instrs);
	    }
	    Token::InfixOp{id, ..} => {
		Equation::encode_func(*id, instrs);
		*store_count -= 1;
	    }
	    Token::PostfixOp{id, ..} => {
		Equation::encode_func(*id, instrs);
	    }
	    Token::FixedFunc{id, args, ..} => {
		Equation::encode_func(*id, instrs);
		*store_count -= if *args == 0 {
		    0
		} else {
		    *args - 1
		};
	    }
	    Token::VarFunc {id, args} => {
		// TODO: implement
	    }
	    // const
	    Token::Const{index} => {
		Equation::encode_const(const_instrs[*index], instrs);
	    }
	    // var
	    Token::Var{index} => {
		// TODO: implement
	    }
	    // defs
	    Token::NoArgDef => {
		def_store_indices.push(*store_count);
	    }
	    Token::NoArgVal{index} => {
		instrs.push(INSTR_READ);
		Equation::encode_value(def_store_indices[*index], instrs);
	    }
	    Token::FreeDef{count} => {
		instrs.push(INSTR_STACK_SUB);
		Equation::encode_value(*count, instrs);
		*store_count -= *count;
		for _i in 0..*count {
		    def_store_indices.pop();
		}
	    }
	    Token::Write{index} => {
		instrs.push(INSTR_WRITE);
		Equation::encode_value(def_store_indices[*index], instrs);
	    }
	    _ => {
		// other tokens are for the compiling process only and do not go in the final instructions.
	    }
	}
    }
    // returns true if this token results in a value (from the point of view of the previous token), otherwise false.
    // for example, "(" results in a value, as seen in "(2)", which results in the value "2".
    pub(crate) fn is_result_value_next(&self) -> bool {
	match self {
	    Token::PrefixOp{..} | Token::FixedFunc{..} | Token::UnfixedFunc{..} | Token::VarFunc{..} | // func
	    Token::LeftParen{..} | Token::LeftBracket{..} | Token::LeftCurly{..} | // group
	    Token::Const{..} | // const
	    Token::Var{..} | // var
	    Token::Name{..} | Token::NoArgVal{..} | Token::ArgVal{..} => { // def
		return true;
	    },
	    Token::InfixOp{..} | Token::PostfixOp{..} | // func
	    Token::RightParen | Token::RightBracket | Token::RightCurly | Token::Comma | // group
	    _ => {
		return false;
	    },
	}
    }
    // returns a new "@" token.
    pub(crate) fn new_def() -> Self {
	return Token::Def;
    }
    pub(crate) fn is_def(&self) -> bool {
	if let Token::Def = self {
	    return true;
	} else {
	    return false;
	}
    }
    // returns a new unknown definition, with the given name.
    pub(crate) fn new_name(name: &'t str) -> Self {
	return Token::Name{name};
    }
    // returns true if this token is an unknown definition, otherwise false.
    pub(crate) fn is_name(&self) -> bool {
	if let Token::Name{..} = self {
	    return true;
	} else {
	    return false;
	}
    }
    // returns a new no argument definition, with the given name.
    pub(crate) fn new_no_arg_def() -> Self {
	return Token::NoArgDef;
    }
    // returns a new no argument value, with the given name.
    pub(crate) fn new_no_arg_val(index: usize) -> Self {
	return Token::NoArgVal{index};
    }
    // returns a new "=", with the given name.
    pub(crate) fn new_assign() -> Self {
	return Token::Assign;
    }
    // returns a new ";", with the given name.
    pub(crate) fn new_end() -> Self {
	return Token::End;
    }
    // returns true if this token is an "=", otherwise false.
    pub(crate) fn is_assign(&self) -> bool {
	if let Token::Assign = self {
	    return true;
	}
	return false;
    }
    // returns true if this token is a definition, otherwise false.
    pub(crate) fn is_no_arg_or_arg_def(&self) -> bool {
	match self {
	    Token::NoArgDef{..} | Token::ArgDef{..} => {
		return true;
	    }
	    _ => {}
	}
	return false;
    }
    // returns true if this token is a free definition, otherwise false.
    pub(crate) fn new_free_def(count: usize) -> Self {
	return Token::FreeDef{count};
    }
    pub(crate) fn new_iter() -> Self {
	return Token::Iter;
    }
    pub(crate) fn new_redef() -> Self {
	return Token::Redef;
    }
    pub(crate) fn new_range() -> Self {
	return Token::Range;
    }
    pub(crate) fn is_range(&self) -> bool {
	if let Token::Range = self {
	    return true;
	} else {
	    return false;
	}
    }
    pub(crate) fn new_write(index: usize) -> Self {
	return Token::Write{index};
    }
    pub(crate) fn new_block() -> Self {
	return Token::Block;
    }
    pub(crate) fn is_redef(&self) -> bool {
	if let Token::Redef = self {
	    return true;
	} else {
	    return false;
	}
    }
    pub(crate) fn is_block(&self) -> bool {
	if let Token::Block = self {
	    return true;
	} else {
	    return false;
	}
    }
    pub(crate) fn new_iter_var(name: &'t str) -> Self {
	return Token::IterVar{name};
    }
    pub(crate) fn new_iter_placeholder() -> Self {
	return Token::IterPlaceholder{index_of_token_iter_test: 0}; // 0 is dummy value
    }
    pub(crate) fn new_index(index: usize) -> Self {
	return Token::Index{index};
    }
    pub(crate) fn new_iter_test() -> Self {
	return Token::IterTest{index_of_instr_iter_redef: 0}; // 0 is dummy value
    }
    pub(crate) fn new_recip(name: &'t str) -> Self {
	return Token::FixedFunc{id: FUNC_RECIP, args: 1, name, instructionize: None};
    }
    pub(crate) fn new_csc(name: &'t str) -> Self {
	struct Csc;
	impl Instructionize for Csc {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_sin(EMPTY_STR));
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_CSC, args: 1, name, instructionize: Some(Box::new(Csc))};
    }
    pub(crate) fn new_sec(name: &'t str) -> Self {
	struct Sec;
	impl Instructionize for Sec {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_cos(EMPTY_STR));
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_SEC, args: 1, name, instructionize: Some(Box::new(Sec))};
    }
    pub(crate) fn new_cot(name: &'t str) -> Self {
	struct Cot;
	impl Instructionize for Cot {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_tan(EMPTY_STR));
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_COT, args: 1, name, instructionize: Some(Box::new(Cot))};
    }
    pub(crate) fn new_acsc(name: &'t str) -> Self {
	struct Acsc;
	impl Instructionize for Acsc {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		instr_tokens.push_back(Token::FixedFunc{id: FUNC_ASIN, args: 1, name: EMPTY_STR, instructionize: None});
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_ACSC, args: 1, name, instructionize: Some(Box::new(Acsc))};
    }
    pub(crate) fn new_asec(name: &'t str) -> Self {
	struct Asec;
	impl Instructionize for Asec {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		instr_tokens.push_back(Token::FixedFunc{id: FUNC_ACOS, args: 1, name: EMPTY_STR, instructionize: None});
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_ASEC, args: 1, name, instructionize: Some(Box::new(Asec))};
    }
    pub(crate) fn new_acot(name: &'t str) -> Self {
	struct Acot;
	impl Instructionize for Acot {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		instr_tokens.push_back(Token::FixedFunc{id: FUNC_ATAN, args: 1, name: EMPTY_STR, instructionize: None});
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_ACOT, args: 1, name, instructionize: Some(Box::new(Acot))};
    }
    pub(crate) fn new_csch(name: &'t str) -> Self {
	struct Csch;
	impl Instructionize for Csch {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_sinh(EMPTY_STR));
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_CSCH, args: 1, name, instructionize: Some(Box::new(Csch))};
    }
    pub(crate) fn new_sech(name: &'t str) -> Self {
	struct Sech;
	impl Instructionize for Sech {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_cosh(EMPTY_STR));
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_SECH, args: 1, name, instructionize: Some(Box::new(Sech))};
    }
    pub(crate) fn new_coth(name: &'t str) -> Self {
	struct Coth;
	impl Instructionize for Coth {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_tanh(EMPTY_STR));
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_COTH, args: 1, name, instructionize: Some(Box::new(Coth))};
    }
    pub(crate) fn new_acsch(name: &'t str) -> Self {
	struct Acsch;
	impl Instructionize for Acsch {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		instr_tokens.push_back(Token::FixedFunc{id: FUNC_ASINH, args: 1, name: EMPTY_STR, instructionize: None});
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_ACSCH, args: 1, name, instructionize: Some(Box::new(Acsch))};
    }
    pub(crate) fn new_asech(name: &'t str) -> Self {
	struct Asech;
	impl Instructionize for Asech {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		instr_tokens.push_back(Token::FixedFunc{id: FUNC_ACOSH, args: 1, name: EMPTY_STR, instructionize: None});
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_ASECH, args: 1, name, instructionize: Some(Box::new(Asech))};
    }
    pub(crate) fn new_acoth(name: &'t str) -> Self {
	struct Acoth;
	impl Instructionize for Acoth {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		instr_tokens.push_back(Token::new_recip(EMPTY_STR));
		instr_tokens.push_back(Token::FixedFunc{id: FUNC_ATANH, args: 1, name: EMPTY_STR, instructionize: None});
		return None;
	    }
	}
	return Token::FixedFunc{id: FAKE_FUNC_ACOTH, args: 1, name, instructionize: Some(Box::new(Acoth))};
    }
    /*pub(crate) fn new_pos(symbol: &'t str) -> Self {
	struct Pos;
	impl Instructionize for Pos {
	    fn instructionize(&self, instr_tokens: &mut VecDeque<Token>, _args: usize, _const_tokens: &mut Vec<Complex>, _const_counts: &mut Vec<usize>, _const_indices: &mut HashMap<ComplexHash, usize>) -> Option<String> {
		return None;
	    }
	}
	return Token::PrefixOp{id: FAKE_FUNC_POS, args: 1, name, instructionize: Some(Box::new(Pos));
    }*/
}
