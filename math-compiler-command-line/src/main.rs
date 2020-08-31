// a command-line calculator that parses, compiles, and evaluates complex mathematical expressions, including complex numbers.

mod complex;
mod equation;
mod token;

use crate::complex::Complex;
use crate::equation::Equation;

use std::io;
use std::io::prelude::*;
use std::time::SystemTime;

/*
TODO:
- make instrs a vecdeque not a vec
- add defs or funcs to be able to have numbers, but not as char 0.
*/

const DEBUG: bool = false;

fn main() {
    let mut equ: Equation = Equation::new();
    loop {
	print!("> ");
	io::stdout().flush().ok();
	let mut input = String::new();
	match io::stdin().read_line(&mut input) {
	    Ok(..) => {
		if let Some('\n') = input.chars().next_back() {
		    input.pop();
		}
		if let Some('\r') = input.chars().next_back() {
		    input.pop();
		}
		if input == "q" {
		    break;
		} else if equ.parse(input) {
		    let result: Complex = equ.eval().unwrap();
		    if DEBUG {
			let now = SystemTime::now();
			for i in 0..65536 {
			    equ.eval().unwrap();
			}
			let time = now.elapsed().unwrap().as_micros();
			println!("  = {} ({}*us)", result, time);
		    } else {
			println!("  = {}", result);
		    }
		} else {
		    println!("  error: {}", equ.get_error().unwrap());
		}
	    }
	    Err(error) => {
		println!("  error: {}", error);
	    }
	}
    }
}
