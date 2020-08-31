extern crate unicode_segmentation;

use crate::complex::{Complex, ComplexHash, ONE, TAU, ZERO};
use crate::token::Token;

use std::collections::{HashMap, HashSet, VecDeque};
use std::f64::{consts::{E, PI}, INFINITY, NAN};
use std::mem::{discriminant, size_of};
use unicode_segmentation::UnicodeSegmentation;

pub(crate) const INSTR_STORE: u8 = 0;
pub(crate) const INSTR_ADD: u8 = 1;
pub(crate) const INSTR_SUB: u8 = 2;
pub(crate) const INSTR_MUL: u8 = 3;
pub(crate) const INSTR_DIV: u8 = 4;
pub(crate) const INSTR_NEG: u8 = 5;
pub(crate) const INSTR_POW: u8 = 6;
pub(crate) const INSTR_POWN: u8 = 7;
pub(crate) const INSTR_EXP: u8 = 8;
pub(crate) const INSTR_LN1: u8 = 9;
pub(crate) const INSTR_SIN: u8 = 10;
pub(crate) const INSTR_COS: u8 = 11;
pub(crate) const INSTR_TAN: u8 = 12;
pub(crate) const INSTR_SQRT: u8 = 13;
pub(crate) const INSTR_ASIN: u8 = 14;
pub(crate) const INSTR_ACOS: u8 = 15;
pub(crate) const INSTR_ATAN: u8 = 16;
pub(crate) const INSTR_SINH: u8 = 17;
pub(crate) const INSTR_COSH: u8 = 18;
pub(crate) const INSTR_TANH: u8 = 19;
pub(crate) const INSTR_ASINH: u8 = 20;
pub(crate) const INSTR_ACOSH: u8 = 21;
pub(crate) const INSTR_ATANH: u8 = 22;
pub(crate) const INSTR_ARG: u8 = 23;
pub(crate) const INSTR_ABS: u8 = 24;
pub(crate) const INSTR_RE: u8 = 25;
pub(crate) const INSTR_IM: u8 = 26;
pub(crate) const INSTR_CONJ: u8 = 27;
pub(crate) const INSTR_CEIL: u8 = 28;
pub(crate) const INSTR_FLOOR: u8 = 29;
pub(crate) const INSTR_ROUND: u8 = 30;
pub(crate) const INSTR_RECIP: u8 = 31;
pub(crate) const INSTR_LOG2: u8 = 32;
pub(crate) const INSTR_LOG3: u8 = 33;
pub(crate) const INSTR_LN2: u8 = 34;
pub(crate) const INSTR_ITER: u8 = 35;
pub(crate) const INSTR_ITER_STORE: u8 = 36;
pub(crate) const INSTR_READ: u8 = 37;
pub(crate) const INSTR_ITER_COUNT: u8 = 38;
pub(crate) const INSTR_ITER_TEST: u8 = 39;
pub(crate) const INSTR_MAX: u8 = 40;
pub(crate) const INSTR_MIN: u8 = 41;
pub(crate) const INSTR_JUMP: u8 = 42;
pub(crate) const INSTR_JUMP_IF_NOT: u8 = 43;
pub(crate) const INSTR_EQUAL: u8 = 44;
pub(crate) const INSTR_NOT_EQUAL: u8 = 45;
pub(crate) const INSTR_GREATER: u8 = 46;
pub(crate) const INSTR_LESS: u8 = 47;
pub(crate) const INSTR_GREATER_OR_EQUAL: u8 = 48;
pub(crate) const INSTR_LESS_OR_EQUAL: u8 = 49;
pub(crate) const INSTR_AND: u8 = 50;
pub(crate) const INSTR_OR: u8 = 51;
pub(crate) const INSTR_NOT: u8 = 52;
pub(crate) const INSTR_READ_REL: u8 = 53;
pub(crate) const INSTR_STACK_SUB: u8 = 54;

pub(crate) const FUNC_ADD: usize = 1;
pub(crate) const FUNC_SUB: usize = 2;
pub(crate) const FUNC_MUL: usize = 3;
pub(crate) const FUNC_DIV: usize = 4;
pub(crate) const FUNC_NEG: usize = 5;
pub(crate) const FUNC_POW: usize = 6;
pub(crate) const FUNC_POWN: usize = 7;
pub(crate) const FUNC_EXP: usize = 8;
pub(crate) const FUNC_LN1: usize = 9;
pub(crate) const FUNC_SIN: usize = 10;
pub(crate) const FUNC_COS: usize = 11;
pub(crate) const FUNC_TAN: usize = 12;
pub(crate) const FUNC_SQRT: usize = 13;
pub(crate) const FUNC_ASIN: usize = 14;
pub(crate) const FUNC_ACOS: usize = 15;
pub(crate) const FUNC_ATAN: usize = 16;
pub(crate) const FUNC_SINH: usize = 17;
pub(crate) const FUNC_COSH: usize = 18;
pub(crate) const FUNC_TANH: usize = 19;
pub(crate) const FUNC_ASINH: usize = 20;
pub(crate) const FUNC_ACOSH: usize = 21;
pub(crate) const FUNC_ATANH: usize = 22;
pub(crate) const FUNC_ARG: usize = 23;
pub(crate) const FUNC_ABS: usize = 24;
pub(crate) const FUNC_RE: usize = 25;
pub(crate) const FUNC_IM: usize = 26;
pub(crate) const FUNC_CONJ: usize = 27;
pub(crate) const FUNC_CEIL: usize = 28;
pub(crate) const FUNC_FLOOR: usize = 29;
pub(crate) const FUNC_ROUND: usize = 30;
pub(crate) const FUNC_RECIP: usize = 31;
pub(crate) const FUNC_LOG2: usize = 32;
pub(crate) const FUNC_LOG3: usize = 33;
pub(crate) const FUNC_LN2: usize = 34;
pub(crate) const FUNC_MAX: usize = 40;
pub(crate) const FUNC_MIN: usize = 41;
pub(crate) const FUNC_EQUAL: usize = 44;
pub(crate) const FUNC_NOT_EQUAL: usize = 45;
pub(crate) const FUNC_GREATER: usize = 46;
pub(crate) const FUNC_LESS: usize = 47;
pub(crate) const FUNC_GREATER_OR_EQUAL: usize = 48;
pub(crate) const FUNC_LESS_OR_EQUAL: usize = 49;
pub(crate) const FUNC_AND: usize = 50;
pub(crate) const FUNC_OR: usize = 51;
pub(crate) const FUNC_NOT: usize = 52;

pub(crate) const PREC_LOG: usize = 0;
pub(crate) const PREC_CMP: usize = 1;
pub(crate) const PREC_ADD: usize = 2;
pub(crate) const PREC_SUB: usize = 2;
pub(crate) const PREC_MUL: usize = 3;
pub(crate) const PREC_DIV: usize = 3;
pub(crate) const PREC_NEG: usize = 4;
pub(crate) const PREC_POW: usize = 5;

const MAX_ITERATIONS: u64 = 9007199254740992; // 2**53 = 2**(f64 mantissa bits + 1); integers larger than this cannot necessarily be exactly represented as an f64.

const ERROR_NO_EQUATION: &str = "no equation";
const ERROR_UNKNOWN_TOKEN: &str = "unknown token \"{}\"";
const ERROR_INVALID_NUMBER: &str = "invalid number \"{}\"";
const ERROR_MISMATCHED_PARENTHESES: &str = "mismatched parentheses";
const ERROR_EMPTY_ARGUMENT: &str = "empty argument";
const ERROR_PARENTHESES_ARGUMENT: &str = "parentheses must have 1 argument";
const ERROR_FUNC_NO_LEFT_PARENTHESIS: &str = "a left parenthesis must follow \"{}\"";
const ERROR_NONE: &str = "";
const ERROR_NO_PREFIX_VALUE: &str = "a value must succeed a prefix operator";
const ERROR_NO_INFIX_VALUE: &str = "a value must preceed and succeed an infix operator";
const ERROR_NO_POSTFIX_VALUE: &str = "a value must preceed a postfix operator";
const ERROR_MISMATCHED_CURLIES: &str = "mismatched curlies";
const ERROR_CURLIES_ARGUMENT: &str = "curlies must have 2 or 3 arguments";
const ERROR_IF_NO_CURLY: &str = "an if statement must be within curlies";
const ERROR_ELSE_NO_IF: &str = "an else statement must correspond to an if statement";
const ERROR_DEF_AT_NO_NAME: &str = "a unique and original definition name must follow an \"@\"";
const ERROR_DEF_OUT_OF_SCOPE: &str = "definition \"{}\" is out of scope";
const ERROR_DUPLICATE_DEF: &str = "duplicate definition \"{}\"";
const ERROR_DEF_PARENTHESES: &str = "definitions must be directly inside parentheses or the equation";
const ERROR_DEF_NO_ASSIGN: &str = "an assignment operator ['='] must follow the definition {}";
const ERROR_ADJACENT_VALUES: &str = "values cannot be adjacent";
const ERROR_LONE_ASSIGN: &str = "lone assignment not to a definition";
const ERROR_END_CAN_ONLY_FOLLOW_ASSIGN_TO_DEF: &str = "\";\" can only be placed after assignment to a definition";
const ERROR_EMPTY_ASSIGNMENT: &str = "empty assignment";

const SYMBOL_TYPE_NONE: u8 = 0;
const SYMBOL_TYPE_NUMBER: u8 = 1;
const SYMBOL_TYPE_LETTER: u8 = 2;
const SYMBOL_TYPE_SYMBOL: u8 = 3;
const SYMBOL_MAX_LENGTH: usize = 2;

enum Symbol<'sym> {
    Number(&'sym str),
    Letter(&'sym str),
    Symbol(&'sym str),
    None,
}

// a struct to represent an equation, which can parse, compile, and evaluate mathematical expressions from string.
pub struct Equation {
    valid: bool,
    equ: String,
    message: String,
    instructions: Box<[u8]>,
    constants: Box<[Complex]>,
    store_length: usize,
}

impl Equation {
    // parse and compile the given equation, and return whether it was successful or not.
    pub fn parse(&mut self, equ: String) -> bool {
	self.equ = equ;

	let mut tokens: VecDeque<Token> = VecDeque::new();
	let mut const_tokens: Vec<Complex> = Vec::new();
	let mut const_counts: Vec<usize> = Vec::new();
	let mut const_indices: HashMap<ComplexHash, usize> = HashMap::new();
	let mut defs: HashSet<&str> = HashSet::new(); // so we can see if def token is actually a def (so could possibly be out of scope), or if it is just an unknown token.

	let graphemes: Vec<(usize, &str)> = UnicodeSegmentation::grapheme_indices(&self.equ[..], true).collect::<Vec<(usize, &str)>>();

	let mut i: usize = 0;
	let mut flag_prev_token_def_at: bool = false;
	tokens.push_back(Token::new_left_paren()); // make defs work on base level (without parenthesis).
	while i < self.equ.len() {
	    let symbol: Symbol = Equation::symbolize(&self.equ[..], &graphemes, &mut i);
	    match symbol {
		Symbol::Number(s) => {
		    let result: Result<f64, std::num::ParseFloatError> = s.parse::<f64>();
		    match result {
			Ok(n) => {
			    Equation::push_const(Complex{re: n, im: 0.0}, &mut tokens, &mut const_tokens, &mut const_counts, &mut const_indices);
			}
			_ => {
			    self.message = format!("invalid number \"{}\"", s);
			    self.valid = false;
			    return false;
			}
		    }
		    flag_prev_token_def_at = false;
		},
		Symbol::Letter(s) => {
		    match s {
			"add" => {
			    tokens.push_back(Token::new_unfixed_func_add(s));
			}
			"sub" => {
			    tokens.push_back(Token::new_unfixed_func_sub(s));
			}
			"mul" => {
			    tokens.push_back(Token::new_unfixed_func_mul(s));
			}
			"div" => {
			    tokens.push_back(Token::new_unfixed_func_div(s));
			}
			"neg" => {
			    tokens.push_back(Token::new_unfixed_func_neg(s));
			}
			"pow" => {
			    tokens.push_back(Token::new_fixed_func_pow(s));
			}
			"exp" => {
			    tokens.push_back(Token::new_exp(s));
			}
			"ln" => {
			    tokens.push_back(Token::new_ln(s));
			}
			"sin" => {
			    tokens.push_back(Token::new_sin(s));
			}
			"cos" => {
			    tokens.push_back(Token::new_cos(s));
			}
			"tan" => {
			    tokens.push_back(Token::new_tan(s));
			}
			"sqrt" => {
			    tokens.push_back(Token::new_sqrt(s));
			}
			"asin" => {
			    tokens.push_back(Token::new_asin(s));
			}
			"acos" => {
			    tokens.push_back(Token::new_acos(s));
			}
			"atan" => {
			    tokens.push_back(Token::new_atan(s));
			}
			"sinh" => {
			    tokens.push_back(Token::new_sinh(s));
			}
			"cosh" => {
			    tokens.push_back(Token::new_cosh(s));
			}
			"tanh" => {
			    tokens.push_back(Token::new_tanh(s));
			}
			"asinh" => {
			    tokens.push_back(Token::new_asinh(s));
			}
			"acosh" => {
			    tokens.push_back(Token::new_acosh(s));
			}
			"atanh" => {
			    tokens.push_back(Token::new_atanh(s));
			}
			"arg" => {
			    tokens.push_back(Token::new_arg(s));
			}
			"abs" => {
			    tokens.push_back(Token::new_abs(s));
			}
			"re" => {
			    tokens.push_back(Token::new_re(s));
			}
			"im" => {
			    tokens.push_back(Token::new_im(s));
			}
			"conj" => {
			    tokens.push_back(Token::new_conj(s));
			}
			"ceil" => {
			    tokens.push_back(Token::new_ceil(s));
			}
			"floor" => {
			    tokens.push_back(Token::new_floor(s));
			}
			"round" => {
			    tokens.push_back(Token::new_round(s));
			}
			"log" => {
			    tokens.push_back(Token::new_log(s));
			}
			"max" => {
			    tokens.push_back(Token::new_max(s));
			}
			"min" => {
			    tokens.push_back(Token::new_min(s));
			}
			"i" => {
			    Equation::push_const(Complex{re: 0.0, im: 1.0}, &mut tokens, &mut const_tokens, &mut const_counts, &mut const_indices);
			}
			"pi" => {
			    Equation::push_const(Complex{re: PI, im: 0.0}, &mut tokens, &mut const_tokens, &mut const_counts, &mut const_indices);
			}
			"e" => {
			    Equation::push_const(Complex{re: E, im: 0.0}, &mut tokens, &mut const_tokens, &mut const_counts, &mut const_indices);
			}
			"inf" => {
			    Equation::push_const(Complex{re: INFINITY, im: 0.0}, &mut tokens, &mut const_tokens, &mut const_counts, &mut const_indices);
			}
			"nan" => {
			    Equation::push_const(Complex{re: NAN, im: 0.0}, &mut tokens, &mut const_tokens, &mut const_counts, &mut const_indices);
			}
			"tau" => {
			    Equation::push_const(Complex{re: TAU, im: 0.0}, &mut tokens, &mut const_tokens, &mut const_counts, &mut const_indices);
			}
			_ => {
			    tokens.push_back(Token::new_unknown_def(s));
			    if flag_prev_token_def_at {
				defs.insert(s);
			    }
			}
		    }
		    flag_prev_token_def_at = false;
		},
		Symbol::Symbol(s) => {
		    flag_prev_token_def_at = false;
		    match s {
			"+" => {
			    if Equation::prefix_or_infix_op(&tokens) {
				// context determines this "+" to be a prefix op.
				// do nothing, since we don't need to do anything to make something positive.
			    } else {
				tokens.push_back(Token::new_infix_op_add());
			    }
			}
			"-" => {
			    if Equation::prefix_or_infix_op(&tokens) {
				// context determines this "-" to be a prefix op.
				tokens.push_back(Token::new_prefix_op_neg());
			    } else {
				// context determines this "-" to be an infix op.
				tokens.push_back(Token::new_infix_op_sub());
			    }
			}
			"*" => {
			    tokens.push_back(Token::new_infix_op_mul());
			}
			"/" => {
			    tokens.push_back(Token::new_infix_op_div());
			}
			"**" | "^" => {
			    tokens.push_back(Token::new_infix_op_pow());
			}
			"(" => {
			    tokens.push_back(Token::new_left_paren());
			}
			")" => {
			    tokens.push_back(Token::new_right_paren());
			}
			"," => {
			    tokens.push_back(Token::new_comma());
			}
			"#" => {
			    break;
			}
			"==" => {
			    tokens.push_back(Token::new_infix_op_equal());
			}
			"!=" => {
			    tokens.push_back(Token::new_infix_op_not_equal());
			}
			">" => {
			    tokens.push_back(Token::new_infix_op_greater());
			}
			"<" => {
			    tokens.push_back(Token::new_infix_op_less());
			}
			">=" => {
			    tokens.push_back(Token::new_infix_op_greater_or_equal());
			}
			"<=" => {
			    tokens.push_back(Token::new_infix_op_less_or_equal());
			}
			"&" => {
			    tokens.push_back(Token::new_infix_op_and());
			}
			"|" => {
			    tokens.push_back(Token::new_infix_op_or());
			}
			"~" => {
			    tokens.push_back(Token::new_prefix_op_not());
			}
			"@" => {
			    tokens.push_back(Token::new_at());
			    flag_prev_token_def_at = true;
			}
			"=" => {
			    tokens.push_back(Token::new_assign());
			}
			";" => {
			    tokens.push_back(Token::new_end());
			}
			"->" => {
			    tokens.push_back(Token::new_if());
			}
			":" => {
			    tokens.push_back(Token::new_else());
			}
			"{" => {
			    tokens.push_back(Token::new_left_curly());
			}
			"}" => {
			    tokens.push_back(Token::new_right_curly());
			}
			"[" => {
			    tokens.push_back(Token::new_left_bracket());
			}
			"]" => {
			    tokens.push_back(Token::new_right_bracket());
			}
			_ => {
			    self.message = format!("unknown token \"{}\"", s);
			    self.valid = false;
			    return false;
			}
		    }
		},
		Symbol::None => (),
	    }
	}
	tokens.push_back(Token::new_right_paren());

	if tokens.len() == 2 { // we put a put the whole expression in a () to make base-level defs work.
	    self.valid = false;
	    self.message = format!("no equation");
	    return false;
	}

	let mut instr_tokens: VecDeque<Token> = VecDeque::new();
	let mut op_stack: Vec<Token> = Vec::new();
	let mut op_stack_infix_op_prec: Vec<usize> = Vec::new();
	let mut flag_prev_token_value: bool = false; // since no token precedes the first token, the previous token (which is nothing) could not have been a value).
	let mut def_indices: HashMap<&str, usize> = HashMap::new(); // &str is definition name, usize is what # definition this is (when definitions go out of scope the # that the next def will get is decreased.).
	let mut def_list: Vec<&str> = Vec::new(); // a list of what definitions are valid right now. left parenthesis keep track of how many defs they have in them, so when we hit the right parenthesis, the last x # of defs in this list can be removed from the list and def_indices (they go out of scope).

	let mut tokens_iter = tokens.into_iter().peekable();
	while tokens_iter.peek().is_some() {
	    let mut token: Token = tokens_iter.next().unwrap();
	    match &mut token {
		// control
		Token::If{index_of_token_jump_if_not, ..} => {
		    // there must be a value before and after that this infix op can apply to
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			!flag_prev_token_value || next_token.is_none() || !next_token.unwrap().is_result_value()
		    } {
			self.valid = false;
			self.message = ERROR_NO_INFIX_VALUE.to_string();
			return false;
		    }
		    // pop ops until we find a "{", and if not this is an error.
		    Equation::pop_ops_and_control(&mut op_stack, &mut instr_tokens, &mut op_stack_infix_op_prec);
		    if op_stack.len() == 0 || !op_stack.last().unwrap().is_left_curly() {
			self.valid = false;
			self.message = ERROR_IF_NO_CURLY.to_string();
			return false;
		    }
		    // update the number of arguments for this curly.
		    let left_curly: &mut Token = op_stack.last_mut().unwrap();
		    if let Token::LeftCurly{count} = left_curly {
			// always the case.
			*count += 1;
		    }
		    // record the index of the jump if instruction in this token, and then add it to the op_stack.
		    *index_of_token_jump_if_not = instr_tokens.len();
		    instr_tokens.push_back(Token::new_jump_if_not());
		    op_stack.push(token);
		    flag_prev_token_value = false; // this token is not a value
		},
		Token::Else{index_of_token_jump} => {
		    // there must be a value before and after that this infix op can apply to
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			!flag_prev_token_value || next_token.is_none() || !next_token.unwrap().is_result_value()
		    } {
			self.valid = false;
			self.message = ERROR_NO_INFIX_VALUE.to_string();
			return false;
		    }
		    // there must be a corresponding "if" on the operator stack, so pop until we find it.
		    Equation::pop_ops(&mut op_stack, &mut instr_tokens, &mut op_stack_infix_op_prec);
		    if op_stack.len() == 0 || !op_stack.last().unwrap().is_if() {
			self.valid = false;
			self.message = ERROR_ELSE_NO_IF.to_string();
			return false;
		    }
		    // update the number of arguments for this curly.
		    let left_curly: &mut Token = op_stack.last_mut().unwrap();
		    if let Token::LeftCurly{count} = left_curly {
			// always the case.
			*count += 1;
		    }
		    //
		    *index_of_token_jump = instr_tokens.len();
		    instr_tokens.push_back(Token::new_jump());
		    let if_token: &mut Token = op_stack.last_mut().unwrap();
		    if let Token::If{index_of_token_jump_if_not, paired_with_else} = if_token {
			// always the case
			let instr_tokens_len: usize = instr_tokens.len();
			let jump_if_not: &mut Token = &mut instr_tokens[*index_of_token_jump_if_not];
			if let Token::JumpIfNot{index_of_token_placeholder} = jump_if_not {
			    // always the case
			    *index_of_token_placeholder = instr_tokens_len;
			}
			*paired_with_else = true;
		    }
		    instr_tokens.push_back(Token::new_placeholder());
		    op_stack.push(token);
		    flag_prev_token_value = false; // this token is not a value
		},
		// func
		Token::PrefixOp{id: _, prec: _} => {
		    // there must be a value after that this prefix op can apply to.
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			next_token.is_none() || !next_token.unwrap().is_result_value()
		    } {
			self.valid = false;
			self.message = ERROR_NO_PREFIX_VALUE.to_string();
			return false;
		    }
		    op_stack.push(token);
		    flag_prev_token_value = false;
		}
		Token::InfixOp{id: _, prec, order} => {
		    // there must be a value before and after that this infix op can apply to.
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			!flag_prev_token_value || next_token.is_none() || !next_token.unwrap().is_result_value()
		    } {
			self.valid = false;
			self.message = ERROR_NO_INFIX_VALUE.to_string();
			return false;
		    }
		    let prec: usize = *prec;
		    let order: bool = *order;
		    while op_stack.len() > 0 {
			let op: &Token = op_stack.last().unwrap();
			if op.is_op()
			    && (prec < op.op_prec().unwrap()
				|| (op_stack_infix_op_prec.len() > 0
				    && prec < (&op_stack[*op_stack_infix_op_prec.last().unwrap()]).op_prec().unwrap())
				|| (prec == op.op_prec().unwrap()
				    && (token != *op || order))
				|| (op_stack_infix_op_prec.len() > 0
				    && prec == (&op_stack[*op_stack_infix_op_prec.last().unwrap()]).op_prec().unwrap()
				    && (token != op_stack[*op_stack_infix_op_prec.last().unwrap()] || order))) {
			    let op: Token = op_stack.pop().unwrap();
			    if op.is_infix_op() {
				op_stack_infix_op_prec.pop();
			    }
			    instr_tokens.push_back(op);
			} else {
			    break;
			}
		    }
		    op_stack_infix_op_prec.push(op_stack.len());
		    op_stack.push(token);
		    instr_tokens.push_back(Token::new_store());
		    flag_prev_token_value = false;
		}
		Token::PostfixOp{id: _, prec} => {
		    // there must be a value before that this postfix op can apply to.
		    if !flag_prev_token_value {
			self.valid = false;
			self.message = ERROR_NO_POSTFIX_VALUE.to_string();
			return false;
		    }
		    let prec: usize = *prec;
		    while op_stack.len() > 0 {
			let op: &Token = op_stack.last().unwrap();
			if op.is_op()
			    && (prec <= op.op_prec().unwrap()
				|| (op_stack_infix_op_prec.len() > 0
				    && prec <= (&op_stack[*op_stack_infix_op_prec.last().unwrap()]).op_prec().unwrap())) {
				let op: Token = op_stack.pop().unwrap();
				if op.is_infix_op() {
				    op_stack_infix_op_prec.pop();
				}
				instr_tokens.push_back(op);
			    } else {
				break;
			    }
		    }
		    instr_tokens.push_back(token);
		    flag_prev_token_value = true;
		}
		Token::FixedFunc{id, args, name} => {
		    let next_token = tokens_iter.peek();
		    if !(next_token.is_some() && next_token.unwrap().is_left_paren()) {
			// there is either not another token or it is not a left parenthesis, which is an error.
			self.valid = false;
			self.message = format!("a left parenthesis must follow {}", name);
			return false;
		    }
		    op_stack.push(token);
		    flag_prev_token_value = false;
		    // TODO: implement
		}
		Token::UnfixedFunc{args: _, name} => {
		    let next_token = tokens_iter.peek();
		    if !(next_token.is_some() && next_token.unwrap().is_left_paren()) {
			// there is either not another token or it is not a left parenthesis, which is an error.
			self.valid = false;
			self.message = format!("a left parenthesis must follow \"{}\"", name);
			return false;
		    }
		    op_stack.push(token);
		    flag_prev_token_value = false;
		}
		Token::VarFunc{id, args} => {
		    // TODO: implement
		}
		// group
		Token::LeftParen{count, ..} => {
		    let next_token = tokens_iter.peek();
		    if next_token.is_some() && next_token.unwrap().is_comma() {
			// empty argument: "(,"
			// the equation contained a left parenthesis followed by a comma, which is an error.
			self.valid = false;
			self.message = ERROR_EMPTY_ARGUMENT.to_string();
			return false;
		    }
		    if next_token.is_some() && next_token.unwrap().is_right_paren() {
			// a right parenthesis follows this left paren, so these parentheses have 0 arguments inside.
			*count = 0;
		    } else {
			// at least one argument inside.
			*count = 1;
		    }
		    op_stack.push(token);
		    flag_prev_token_value = false;
		}
		Token::RightParen => {
		    // pop operators off the operator stack until we find a left parenthesis to go with the right parenthesis.
		    Equation::pop_ops_and_control(&mut op_stack, &mut instr_tokens, &mut op_stack_infix_op_prec);
		    if op_stack.len() == 0 || !op_stack.last().unwrap().is_left_paren() {
			// mismatched parenthesis: every right parenthesis must have a matching left parenthesis, yet there were none on the operator stack
			self.valid = false;
			self.message = ERROR_MISMATCHED_PARENTHESES.to_string();
			return false;
		    } else if {
			let next_token: Option<&Token> = tokens_iter.peek();
			next_token.is_some() && next_token.unwrap().is_result_value()
		    } {
			// a value preceeds this ")" (e.g. ")2" or ")(") which is not valid.
			self.valid = false;
			self.message = ERROR_ADJACENT_VALUES.to_string();
			return false;
		    }
		    let left_paren: Token = op_stack.pop().unwrap();
		    if let Token::LeftParen{count, defs} = &left_paren {
			// always the case.
			if op_stack.len() > 0 && op_stack.last().unwrap().is_func() {
			    let func: Token = op_stack.pop().unwrap();
			    match &func {
				Token::FixedFunc{id, args, name} => {
				    if *args == *count {
					instr_tokens.push_back(func);
				    } else {
					self.valid = false;
					self.message = format!("{} is not a valid number of arguments for {}", *count, name);
					return false;
				    }
				},
				Token::UnfixedFunc{args, name} => {
				    //let unfixed_func: Token = op_stack.pop().unwrap();
				    match args.get(count) {
					Some(fixed_func) => {
					    instr_tokens.push_back(Token::new_fixed_func(*fixed_func, *count, name));
					},
					None => {
					    self.valid = false;
					    self.message = format!("{} is not a valid number of arguments for {}", *count, name);
					    return false;
					},
				    }
				},
				Token::VarFunc{..} => {
				    // TODO: implement.
				},
				_ => {}, // never the case
			    }
			} else {
			    if *count != 1 {
				// parenthesis either were empty or had more than 1 argument.
				self.valid = false;
				self.message = ERROR_PARENTHESES_ARGUMENT.to_string();
				return false;
			    }
			}
			// release defs for this left parenthesis.
			if *defs != 0 {
			    for i in 0..*defs {
				def_indices.remove(&def_list.pop().unwrap());
			    }
			    instr_tokens.push_back(Token::new_free_def(*defs));
			}
		    }
		    flag_prev_token_value = true;
		}
		Token::Comma => {
		    // pop operators off the operator stack until we find a left parenthesis; if we don't that's an error.
		    Equation::pop_ops_and_control(&mut op_stack, &mut instr_tokens, &mut op_stack_infix_op_prec);
		    if op_stack.len() == 0 || !op_stack.last().unwrap().is_left_paren() {
			// mismatched parenthesis: comma must always be inside parenthesis, yet there were no parenthesis on the operator stack.
			self.valid = false;
			self.message = ERROR_MISMATCHED_PARENTHESES.to_string();
			return false;
		    }
		    let next_token = tokens_iter.peek();
		    if next_token.is_some() && (next_token.unwrap().is_comma() || next_token.unwrap().is_right_paren()) {
			// empty argument, either ",," or ",)", which is an error.
			self.valid = false;
			self.message = ERROR_EMPTY_ARGUMENT.to_string();
			return false;
		    }
		    // the comma is converted into a store instruction
		    instr_tokens.push_back(Token::new_store());
		    let func: &mut Token = op_stack.last_mut().unwrap();
		    if let Token::LeftParen{count, ..} = func {
			// always the case, increment the argument count associated with this parenthesis.
			*count += 1;
		    }
		    flag_prev_token_value = false;
		}
		Token::LeftBracket{count} => {
		    
		}
		Token::RightBracket => {
		    
		}
		Token::LeftCurly{count} => {
		    let next_token = tokens_iter.peek();
		    if next_token.is_some() && next_token.unwrap().is_comma() {
			// empty argument: "{,"
			// the equation contained a left parenthesis followed by a comma, which is an error.
			self.valid = false;
			self.message = ERROR_EMPTY_ARGUMENT.to_string();
			return false;
		    }
		    if next_token.is_some() && next_token.unwrap().is_right_curly() {
			// a right curly follows this left curly, so these curlies have 0 arguments inside.
			*count = 0;
		    } else {
			// at least one argument inside.
			*count = 1;
		    }
		    op_stack.push(token);
		    flag_prev_token_value = false;
		}
		Token::RightCurly => {
		    // pop operators off the operator stack until we find a left curly to go with the right curly.
		    Equation::pop_ops_and_control(&mut op_stack, &mut instr_tokens, &mut op_stack_infix_op_prec);
		    if op_stack.len() == 0 || !op_stack.last().unwrap().is_left_curly() {
			// mismatched curlies: every right curly must have a matching left curly, yet there were none on the operator stack
			self.valid = false;
			self.message = ERROR_MISMATCHED_CURLIES.to_string();
			return false;
		    } else if {
			let next_token: Option<&Token> = tokens_iter.peek();
			next_token.is_some() && next_token.unwrap().is_result_value()
		    } {
			// a value preceeds this ")" (e.g. ")2" or ")(") which is not valid.
			self.valid = false;
			self.message = ERROR_ADJACENT_VALUES.to_string();
			return false;
		    }
		    let left_curly: Token = op_stack.pop().unwrap();
		    if let Token::LeftCurly{count} = &left_curly {
			// always the case.
			if *count != 2 && *count != 3 {
			    // curlies didn't have either 2 or 3 arguments
			    self.valid = false;
			    self.message = ERROR_CURLIES_ARGUMENT.to_string();
			    return false;
			}
		    }
		    flag_prev_token_value = true;
		}
		// const
		Token::Const{..} => {
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			next_token.is_some() && next_token.unwrap().is_result_value()
		    } {
			// a value preceeds this ")" (e.g. ")2" or ")(") which is not valid.
			self.valid = false;
			self.message = ERROR_ADJACENT_VALUES.to_string();
			return false;
		    }
		    instr_tokens.push_back(token);
		    flag_prev_token_value = true;
		}
		// var
		Token::Var{index} => {
		    // TODO: implement
		}
		Token::At => {
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			next_token.is_none() || !next_token.unwrap().is_unknown_def()
		    } {
			self.valid = false;
			self.message = ERROR_DEF_AT_NO_NAME.to_string();
			return false;
		    }
		    let mut next_token: Token = tokens_iter.next().unwrap();
		    if let Token::UnknownDef{name} = next_token {
			// always the case.
			if def_indices.contains_key(name) {
			    self.valid = false;
			    self.message = format!("duplicate definition \"{}\"", name);
			    return false;
			} else if op_stack.len() == 0 || !op_stack.last().unwrap().is_left_paren() {// || (op_stack.len() > 1 && op_stack[op_stack.len() - 2].is_func()) {
			    // TODO: check that previous token is a "(" or a ";".
			    self.valid = false;
			    self.message = ERROR_DEF_PARENTHESES.to_string();
			    return false;
			}
			let left_paren: &mut Token = op_stack.last_mut().unwrap();
			if let Token::LeftParen{count, defs} = left_paren {
			    // always happens.
			    *defs += 1;
			}
			op_stack.push(Token::new_no_arg_def(name));
			if {
			    let next_token: Option<&Token> = tokens_iter.peek();
			    next_token.is_none() || !next_token.unwrap().is_assign()
			} {
			    self.valid = false;
			    self.message = format!("an assignment operator ['='] must follow the definition {}", name);
			    return false;
			}
		    }
		    next_token = tokens_iter.next().unwrap();
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			next_token.is_none() || !next_token.unwrap().is_result_value()
		    } {
			self.valid = false;
			self.message = ERROR_EMPTY_ASSIGNMENT.to_string();
			return false;
		    }
		    flag_prev_token_value = false;
		},
		Token::UnknownDef{name} => {
		    if def_indices.contains_key(name) {
			instr_tokens.push_back(Token::new_no_arg_val(*def_indices.get(name).unwrap()));
		    } else if defs.contains(name) {
			self.valid = false;
			self.message = format!("definition \"{}\" is out of scope", name);
			return false;
		    } else {
			self.valid = false;
			self.message = format!("unknown token \"{}\"", name);
			return false;
		    }
		    flag_prev_token_value = true;
		},
		Token::Assign => {
		    self.valid = false;
		    self.message = ERROR_LONE_ASSIGN.to_string();
		    return false;
		    //flag_prev_token_def_at = false;
		},
		Token::End => {
		    if {
			let next_token: Option<&Token> = tokens_iter.peek();
			next_token.is_some() && {
			    let next_token: &Token = next_token.unwrap();
			    next_token.is_comma() || next_token.is_right_paren()
			}
		    } {
			self.valid = false;
			self.message = ERROR_EMPTY_ARGUMENT.to_string();
			return false;
		    }
		    Equation::pop_ops(&mut op_stack, &mut instr_tokens, &mut op_stack_infix_op_prec);
		    if {
			let top_op: Option<&Token> = op_stack.last();
			top_op.is_none() || !top_op.unwrap().is_no_arg_def()
		    } {
			self.valid = false;
			self.message = ERROR_END_CAN_ONLY_FOLLOW_ASSIGN_TO_DEF.to_string();
			return false;
		    }
		    let mut no_arg_def: Token = op_stack.pop().unwrap();
		    if let Token::NoArgDef{index, name} = &mut no_arg_def {
			let def_index: usize = def_list.len();
			def_indices.insert(name, def_index);
			def_list.push(name);
			*index = def_index;
		    }
		    instr_tokens.push_back(no_arg_def);
		    instr_tokens.push_back(Token::new_store());
		    flag_prev_token_value = false;
		},
		_ => {
		    //flag_prev_token_def_at = false;
		},
	    }
	    i += 1;
	}
	Equation::pop_ops_and_control(&mut op_stack, &mut instr_tokens, &mut op_stack_infix_op_prec);
	if op_stack.len() != 0 {
	    self.valid = false;
	    self.message = ERROR_MISMATCHED_PARENTHESES.to_string();
	    return false;
	}

	let mut instrs: Vec<u8> = Vec::with_capacity(instr_tokens.len()); // lenght is at least instr_tokens.len(), this would be if each token in instr_tokens encoded to only 1 byte, but sometimes tokens encode to more bytes.
	let mut consts: Vec<Complex> = Vec::new();
	let mut store_count: usize = 0;
	let mut max_store_count: usize = 0;
	let mut const_index: usize = 0;
	let mut i = 0;
	let mut const_instrs: Vec<usize> = vec![0; const_tokens.len()];

	for const_token in const_tokens.into_iter() {
	    if const_counts[i] != 0 {
		const_instrs[i] = const_index;
		const_index += 1;
		consts.push(const_token);
	    }
	    i += 1;
	}
	
	let mut offset: usize = 0;
	let instr_tokens_len = instr_tokens.len();
	let max_instrs_len = Equation::get_max_instrs_len(instr_tokens.len());
	let mut def_store_indices: Vec<usize> = Vec::new();
	while offset < instr_tokens_len {
	    let instr_token: Token = instr_tokens.pop_front().unwrap();
	    offset += 1;
	    instr_token.encode(&mut instrs, &const_instrs, &mut store_count, &mut max_store_count, &mut instr_tokens, offset, max_instrs_len, &mut def_store_indices);
	}

	self.instructions = instrs.into();
	self.constants = consts.into();
	self.store_length = max_store_count;

	self.valid = true;
	self.message = ERROR_NONE.to_string();
	return true;
    }
    // if the last expression this equation parsed was valid, evaluate that expression and return Some(result), otherwise None.
    pub fn eval(&self) -> Option<Complex> {
	if self.valid {
	    let instrs: &[u8] = &self.instructions[..];
	    let consts: &[Complex] = &self.constants[..];
	    let mut store: Vec<Complex> = vec![Complex::new(); self.store_length];
	    let mut store_count: usize = 0;
	    let mut w: Complex = Complex::new();
	    let mut i: usize = 0;
	    while i < instrs.len() {
		let instr: u8 = instrs[i];
		i += 1;
		match instr {
		    INSTR_STORE => {
			store[store_count].set(&w);
			store_count += 1;
		    },
		    INSTR_ADD => {
			store_count -= 1;
			w = w.add(&store[store_count]);
		    },
		    INSTR_SUB => {
			store_count -= 1;
			w = store[store_count].sub(&w);
		    },
		    INSTR_MUL => {
			store_count -= 1;
			w = w.mul(&store[store_count]);
		    },
		    INSTR_DIV => {
			store_count -= 1;
			w = store[store_count].div(&w);
		    },
		    INSTR_NEG => {
			w = w.neg();
		    },
		    INSTR_POW => {
			store_count -= 1;
			w = store[store_count].pow(&w);
		    },
		    INSTR_POWN => {
			store_count -= 1;
			w = store[store_count].pow(&w);
		    },
		    INSTR_EXP => {
			w = w.exp();
		    },
		    INSTR_LN1 => {
			w = w.ln();
		    },
		    INSTR_SIN => {
			w = w.sin();
		    },
		    INSTR_COS => {
			w = w.cos();
		    },
		    INSTR_TAN => {
			w = w.tan();
		    },
		    INSTR_SQRT => {
			w = w.sqrt();
		    },
		    INSTR_ASIN => {
			w = w.asin();
		    },
		    INSTR_ACOS => {
			w = w.acos();
		    },
		    INSTR_ATAN => {
			w = w.atan();
		    },
		    INSTR_SINH => {
			w = w.sinh();
		    },
		    INSTR_COSH => {
			w = w.cosh();
		    },
		    INSTR_TANH => {
			w = w.tanh();
		    },
		    INSTR_ASINH => {
			w = w.asinh();
		    },
		    INSTR_ACOSH => {
			w = w.acosh();
		    },
		    INSTR_ATANH => {
			w = w.atanh();
		    },
		    INSTR_ARG => {
			w = w.arg();
		    },
		    INSTR_ABS => {
			w = w.abs();
		    },
		    INSTR_RE => {
			w = w.re();
		    },
		    INSTR_IM => {
			w = w.im();
		    },
		    INSTR_CONJ => {
			w = w.conj();
		    },
		    INSTR_CEIL => {
			w = w.ceil();
		    },
		    INSTR_FLOOR => {
			w = w.floor();
		    },
		    INSTR_ROUND => {
			w = w.round();
		    },
		    INSTR_RECIP => {
			w = w.recip();
		    },
		    INSTR_LOG2 => {
			store_count -= 1;
			w = store[store_count].log2(&w);
		    },
		    INSTR_LOG3 => {
			store_count -= 2;
			w = store[store_count].log3(&store[store_count + 1], w.re);
		    },
		    INSTR_LN2 => {
			store_count -= 1;
			w = store[store_count].ln2(w.re);
		    },
		    INSTR_ITER => {
			//store[store_count] = Complex{re: f64::from_bits(0_u64), im: f64::from_bits((w.re as u64).clamp(0, MAX_ITERATIONS))};
			store[store_count] = Complex{re: f64::from_bits(0_u64), im: f64::from_bits(Equation::clamp::<u64>(w.re as u64, 0, MAX_ITERATIONS))};
			store_count += 2;
		    },
		    INSTR_ITER_STORE => {
			store[store_count - 1].set(&w);
		    },
		    INSTR_READ => {
			let index: usize = Equation::decode_value(instrs, &mut i);
			w.set(&store[index]);
		    },
		    INSTR_ITER_COUNT => {
			let index: usize = Equation::decode_value(instrs, &mut i);
			w = Complex{re: (store[index].re.to_bits()) as f64, im: 0.0};
		    },
		    INSTR_ITER_TEST => {
			let iter_count_and_max: &Complex = &store[store_count - 2];
			let iter_count: u64 = 1 + iter_count_and_max.re.to_bits();
			let iter_max: u64 = iter_count_and_max.im.to_bits();
			if iter_count < iter_max {
			    // keep iterating.
			    let index: usize = Equation::decode_value(instrs, &mut i);
			    i = index;
			    store[store_count - 2] = Complex{re: f64::from_bits(iter_count), im: f64::from_bits(iter_max)};
			} else {
			    // stop iterating.
			    if iter_max == 0 {
				// iter was supposed to do 0 iterations, but actually did 1, so set w = w = init.
				w.set(&store[store_count - 1]);
			    }
			    store_count -= 2;
			}
		    },
		    INSTR_MAX => {
			store_count -= 1;
			w = w.max(&store[store_count]);
		    },
		    INSTR_MIN => {
			store_count -= 1;
			w = w.min(&store[store_count]);
		    },
		    INSTR_JUMP => {
			let index: usize = Equation::decode_value(instrs, &mut i);
			i = index;
		    },
		    INSTR_JUMP_IF_NOT => {
			let index: usize = Equation::decode_value(instrs, &mut i);
			if w == ZERO {
			    i = index;
			}
		    },
		    INSTR_EQUAL => {
			store_count -= 1;
			w.set(if w == store[store_count] {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_NOT_EQUAL => {
			store_count -= 1;
			w.set(if w != store[store_count] {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_GREATER => {
			store_count -= 1;
			w.set(if store[store_count].re > w.re {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_LESS => {
			store_count -= 1;
			w.set(if store[store_count].re < w.re {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_GREATER_OR_EQUAL => {
			store_count -= 1;
			w.set(if store[store_count].re >= w.re {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_LESS_OR_EQUAL => {
			store_count -= 1;
			w.set(if store[store_count].re <= w.re {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_AND => {
			store_count -= 1;
			w.set(if w != ZERO && store[store_count] != ZERO {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_OR => {
			store_count -= 1;
			w.set(if w != ZERO || store[store_count] != ZERO {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_NOT => {
			w.set(if w == (Complex{re: 0.0, im: 0.0}) {
			    &ONE
			} else {
			    &ZERO
			});
		    },
		    INSTR_READ_REL => {
			let count: usize = Equation::decode_value(instrs, &mut i);
			w.set(&store[store_count - count]);
		    },
		    INSTR_STACK_SUB => {
			let count: usize = Equation::decode_value(instrs, &mut i);
			store_count -= count;
		    },
		    _ => {
			if (instr & 0b00100000) == 0b00000000 {
			    // constant
			    let index: usize = Equation::decode_const_or_var(instr, instrs, &mut i);
			    w.set(&consts[index]);
			} else {
			    // variable
			    //let index: usize = decode_var(instr, self.instructions, &mut i);
			}
		    },
		}
	    }
	    return Some(w);
	} else {
	    return None;
	}
    }
    // returns a new equation.
    pub fn new() -> Self {
	return Equation{valid: false, equ: "".to_string(), message: "".to_string(), instructions: Box::new([]), constants: Box::new([]), store_length: 0};
    }
    // gets the current expression fro this equation.
    pub fn get_equ<'equ>(&'equ self) -> &'equ String {
	return &self.equ;
    }
    // if the last expression parsed was invalid, returns Some(error), otherwise None.
    pub fn get_error<'equ>(&'equ self) -> Option<&'equ String> {
	if self.valid {
	    return None;
	} else {
	    return Some(&self.message);
	}
    }
    fn decode_value(instrs: &[u8], i: &mut usize) -> usize {
	let mut j: usize = 0;
	let mut value: usize = 0;
	while {
	    let instr: u8 = instrs[*i];
	    *i += 1;
	    value |= ((instr & 0b01111111) as usize) << (7*j);
	    j += 1;
	    instr >= 128_u8
	} {}
	return value;
    }
    fn decode_const_or_var(mut instr: u8, instrs: &[u8], i: &mut usize) -> usize {
	let mut j: usize = 5;
	let mut index: usize = (instr & 0b00011111) as usize;
	while instr >= 128_u8 {
	    instr = instrs[*i];
	    *i += 1;
	    index |= ((instr & 0b01111111) as usize) << j;
	    j += 7;
	}
	return index;
    }
    pub(crate) fn encode_const(mut index: usize, instrs: &mut Vec<u8>) {
	let mut instr: u8 = ((index as u8) & 0b0001_1111) | 0b0100_0000;
	index >>= 5;
	while index != 0 {
	    instr |= 0b1000_0000;
	    instrs.push(instr);
	    instr = (index as u8) & 0b0111_1111;
	    index >>= 7;
	}
	instrs.push(instr);
    }
    pub(crate) fn encode_var(mut index: usize, instrs: &mut Vec<u8>) {
	let mut instr: u8 = ((index as u8) & 0b0001_1111) | 0b0110_0000;
	index >>= 5;
	while index != 0 {
	    instr |= 0b1000_0000;
	    instrs.push(instr);
	    instr = (index as u8) & 0b0111_1111;
	    index >>= 7;
	}
	instrs.push(instr);
    }
    pub(crate) fn encode_func(mut func: usize, instrs: &mut Vec<u8>) {
	let mut instr: u8 = (func as u8) & 0b0011_1111;
	func >>= 6;
	while func != 0 {
	    instr |= 0b1000_0000;
	    instrs.push(instr);
	    instr = (func as u8) & 0b0111_1111;
	    func >>= 7;
	}
	instrs.push(instr);
    }
    pub(crate) fn encode_value_sized(mut val: usize, mut max_val: usize, instrs: &mut Vec<u8>) {
	let mut instr: u8 = (val as u8) & 0b0111_1111;
	val >>= 7;
	max_val >>= 7;
	while max_val != 0 {  
	    instr |= 0b1000_0000;
	    instrs.push(instr);
	    instr = (val as u8) & 0b0111_1111;
	    val >>= 7;
	    max_val >>= 7;
	}
	instrs.push(instr);
    }
    pub(crate) fn encode_value_sized_overwrite(mut val: usize, mut max_val: usize, instrs: &mut Vec<u8>, mut index: usize) {
	let mut instr: u8 = (val as u8) & 0b0111_1111;
	val >>= 7;
	max_val >>= 7;
	while max_val != 0 {  
	    instr |= 0b1000_0000;
	    instrs[index] = instr;
	    index += 1;
	    instr = (val as u8) & 0b0111_1111;
	    val >>= 7;
	    max_val >>= 7;
	}
	instrs[index] = instr;
    }
    pub(crate) fn encode_value(mut val: usize, instrs: &mut Vec<u8>) {
	let mut instr: u8 = (val as u8) & 0b0111_1111;
	val >>= 7;
	while val != 0 {
	    instr |= 0b1000_0000;
	    instrs.push(instr);
	    instr = (val as u8) & 0b0111_1111;
	    val >>= 7;
	}
	instrs.push(instr);
    }
    fn symbolize<'a>(equ: &'a str, graphemes: &Vec<(usize, &str)>, i: &mut usize) -> Symbol<'a> {
	let mut symbol_type: u8 = SYMBOL_TYPE_NONE;
	let mut start_index: usize = 0;
	let mut end_index: usize = 0;
	while *i < graphemes.len() {
	    let character: &str = graphemes[*i].1;
	    match symbol_type {
		SYMBOL_TYPE_NONE => {
		    if character != " " {
			start_index = *i;
			if Equation::is_number(character) {
			    symbol_type = SYMBOL_TYPE_NUMBER;
			} else if Equation::is_letter(character) {
			    symbol_type = SYMBOL_TYPE_LETTER;
			} else {
			    symbol_type = SYMBOL_TYPE_SYMBOL;
			}
		    }
		}
		SYMBOL_TYPE_NUMBER => {
		    if !Equation::is_number(character) {
			end_index = *i;
			break;
		    }
		}
		SYMBOL_TYPE_LETTER => {
		    if !Equation::is_letter(character) {
			end_index = *i;
			break;
		    }
		}
		SYMBOL_TYPE_SYMBOL => {
		    if *i - start_index == SYMBOL_MAX_LENGTH || !Equation::is_symbol(character) {
			end_index = *i;
			break;
		    }
		}
		_ => (),
	    }
	    *i += 1;
	}
	if end_index == 0 {
	    end_index = *i;
	}
	// refine symbols
	match symbol_type {
	    SYMBOL_TYPE_SYMBOL => {
		let start: usize = graphemes[start_index].0;
		let mut end: usize = if end_index < graphemes.len() {
		    graphemes[end_index].0
		} else {
		    equ.len()
		};
		while end_index - start_index > 1 && !Equation::is_symbol_str(&equ[start..end]) {
		    end_index -= 1;
		    end = graphemes[end_index].0;
		}
		*i = end_index;
	    },
	    _ => {},
	}
	//println!("{} {}", start_index, end_index);
	return match symbol_type {
	    SYMBOL_TYPE_NUMBER | SYMBOL_TYPE_LETTER | SYMBOL_TYPE_SYMBOL => {
		let start: usize = graphemes[start_index].0;
		let end: usize = if end_index < graphemes.len() {
		    graphemes[end_index].0
		} else {
		    equ.len()
		};
		let s: &str = &equ[start..end];
		match symbol_type {
		    SYMBOL_TYPE_NUMBER => Symbol::Number(s),
		    SYMBOL_TYPE_LETTER => Symbol::Letter(s),
		    SYMBOL_TYPE_SYMBOL => Symbol::Symbol(s),
		    _ => Symbol::None, // should never happen here.
		}
	    }
	    SYMBOL_TYPE_NONE | _ => Symbol::None,
	}
    }
    fn is_number(character: &str) -> bool {
	return match character {
	    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "." | "_" => true,
	    _ => false,
	}
    }
    fn is_letter(character: &str) -> bool {
	return match character {
	    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" => true,
	    _ => false,
	}
    }
    fn is_symbol(character: &str) -> bool {
	return !(Equation::is_number(character) || Equation::is_letter(character) || character == " ");
    }
    fn is_symbol_str(string: &str) -> bool {
	return match string {
	    "+" | "-" | "*" | "/" | "**" | "^" | "(" | ")" | "," | "#" | "==" | "!=" | ">" | "<" | ">=" | "<=" | "&" | "|" | "~" | "@" | "=" | ";" | "->" | ":" => {
		true
	    }
	    _ => {
		false
	    }
	}
    }
    fn push_const(c: Complex, tokens: &mut VecDeque<Token>, const_tokens: &mut Vec<Complex>, const_counts: &mut Vec<usize>, const_indices: &mut HashMap<ComplexHash, usize>) -> bool {
	let c_hash: ComplexHash = (&c).into();
	if const_indices.contains_key(&c_hash) {
	    let index: usize = match const_indices.get(&c_hash) {
		Some(i) => *i,
		None => {
		    return false;
		}
	    };
	    const_counts[index] += 1;
	    tokens.push_back(Token::new_const(index));
	} else {
	    let index: usize = const_tokens.len();
	    const_tokens.push(c);
	    const_counts.push(1);
	    const_indices.insert(c_hash, index);
	    tokens.push_back(Token::new_const(index));
	}
	return true;
    }
    fn prefix_or_infix_op(tokens: &VecDeque<Token>) -> bool {
	if tokens.len() == 0 {
	    return true;
	} else {
	    let token: &Token = tokens.back().unwrap();
	    return match token {
		Token::RightParen | Token::RightBracket | Token::RightCurly | Token::Const{..} | Token::Var{..} => false,
		_ => true,
	    };
	}
    }
    fn pop_ops_and_control<'a>(op_stack: &mut Vec<Token<'a>>, instr_tokens: &mut VecDeque<Token<'a>>, op_stack_infix_op_prec: &mut Vec<usize>) {
	while op_stack.len() > 0 && {
	    let top_token: &Token = op_stack.last().unwrap();
	    !top_token.is_left_group() && !top_token.is_def()
	} {
	    let op: Token = op_stack.pop().unwrap();
	    match &op {
		Token::If{index_of_token_jump_if_not, paired_with_else} => {
		    if !*paired_with_else {
			let instr_tokens_len: usize = instr_tokens.len();
			let jump_if_not: &mut Token = &mut instr_tokens[*index_of_token_jump_if_not];
			if let Token::JumpIfNot{index_of_token_placeholder} = jump_if_not {
			    // always the case.
			    *index_of_token_placeholder = instr_tokens_len;
			}
			//instr_tokens[*index_of_token_jump_if_not].index_of_token_placeholder = instr_tokens.len();
			instr_tokens.push_back(Token::new_placeholder());
		    }
		},
		Token::Else{index_of_token_jump} => {
		    let instr_tokens_len: usize = instr_tokens.len();
		    let jump: &mut Token = &mut instr_tokens[*index_of_token_jump];
		    if let Token::Jump{index_of_token_placeholder} = jump {
			// always the case.
			*index_of_token_placeholder = instr_tokens_len;
		    }
		    //instr_tokens[*index_of_token_jump].index_of_token_placeholder = instr_tokens.len();
		    instr_tokens.push_back(Token::new_placeholder());
		},
		Token::PrefixOp{..} | Token::InfixOp{..} | Token::PostfixOp{..} => {
		    if op.is_infix_op() {
			op_stack_infix_op_prec.pop();
		    }
		    instr_tokens.push_back(op);
		},
		_ => {}, // never should happen.
	    }
	}
    }
    // pop operators off the operator stack until we hit parenthesis, if/else, or definitions.
    fn pop_ops<'a>(op_stack: &mut Vec<Token<'a>>, instr_tokens: &mut VecDeque<Token<'a>>, op_stack_infix_op_prec: &mut Vec<usize>) {
	while op_stack.len() > 0 && {
	    let op: &Token = op_stack.last().unwrap();
	    op.is_op()
	} {
	    let op: Token = op_stack.pop().unwrap();
	    if op.is_infix_op() {
		op_stack_infix_op_prec.pop();
	    }
	    instr_tokens.push_back(op);
	}
    }
    fn get_max_instrs_len(num_tokens: usize) -> usize {
	return num_tokens*(1 + size_of::<usize>());
    }
    fn clamp<T>(val: T, min: T, max: T) -> T where T: std::cmp::Ord {
	if val < min {
	    return min;
	} else if max < val {
	    return max;
	} else {
	    return val;
	}
    }
}
