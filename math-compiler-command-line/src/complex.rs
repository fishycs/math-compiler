#[allow(unused_imports)]
use std::convert::From;
use std::f64::{consts::PI, NAN}; // TAU is experimental.
use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::Wrapping;
use std::mem::size_of;

pub(crate) const TAU: f64 = 6.28318530717958647692528676655900;

pub(crate) const ZERO: Complex = Complex{re: 0.0, im: 0.0};
pub(crate) const ONE: Complex = Complex{re: 1.0, im: 0.0};
pub(crate) const NEG_ONE: Complex = Complex{re: -1.0, im: 0.0};
pub(crate) const I: Complex = Complex{re: 0.0, im: 1.0};
pub(crate) const NEG_I: Complex = Complex{re: 0.0, im: -1.0};
pub(crate) const HALF_I: Complex = Complex{re: 0.0, im: 0.5};

const MAX_POWN_EXPONENT: f64 = 64.0;
const LOG2_OF_MAX_POWN_EXPONENT: u64 = 6;
const U64_EXP_BITS: u64 = 11;
const U64_MANTISSA_BITS: u64 = 52;
const U64_SHIFT_MASK: u64 = 0b11_1111;

// a struct to represent complex numbers with methods and associated functions for common operations.
// although rust already has a complex struct, I made my own simply to familiarize myself with rust.
#[derive(Debug, Clone, PartialEq)]
pub struct Complex {
    pub re: f64,
    pub im: f64,
}

#[allow(dead_code)]
impl Complex {
    pub fn add<'add>(&'add self, z1: &'add Complex) -> Complex {
	return Complex{re: self.re + z1.re, im: self.im + z1.im};
    }
    pub fn sub(&self, z1: &Complex) -> Complex {
	return Complex{re: self.re - z1.re, im: self.im - z1.im};
    }
    pub fn mul(&self, z1: &Complex) -> Complex {
	return Complex{re: self.re*z1.re - self.im*z1.im, im: self.im*z1.re + self.re*z1.im};
    }
    pub fn div(&self, z1: &Complex) -> Complex {
	let d: f64 = z1.re*z1.re + z1.im*z1.im;
	return Complex{re: (self.re*z1.re + self.im*z1.im)/d, im: (self.im*z1.re - self.re*z1.im)/d};
    }
    pub fn neg(&self) -> Complex {
	return Complex{re: -self.re, im: -self.im};
    }
    pub fn exp(&self) -> Complex {
	let c: f64 = self.re.exp();
	let a: (f64, f64) = self.im.sin_cos();
	return Complex{re: c*a.1, im: c*a.0};
    }
    pub fn ln(&self) -> Complex {
	return Complex{re: (self.re.hypot(self.im)).ln(), im: self.im.atan2(self.re)};
    }
    pub fn pow(&self, z1: &Complex) -> Complex {
	if z1.im == 0.0 && {
	    let bits: u64 = z1.re.to_bits();
	    let exp: u64 = (0x7ff0_0000_0000_0000 & bits) >> U64_MANTISSA_BITS;
	    let pow: u64 = (Wrapping(exp) - Wrapping((1 << (U64_EXP_BITS - 1)) - 1)).0;
	    let mantissa: u64 = 0x000f_ffff_ffff_ffff & bits;
	    (pow < LOG2_OF_MAX_POWN_EXPONENT || exp == 0) && (mantissa << (pow & U64_SHIFT_MASK)) & 0x000f_ffff_ffff_ffff == 0
	} {
	    return self.pown(z1);
	} else {
	    return self.ln().mul(z1).exp();
	}
    }
    pub fn pown(&self, z1: &Complex) -> Complex {
	let mut c0: u64 = z1.re.abs() as u64;
	if c0 != 0 {
	    let recip: bool = z1.re < 0.0;
	    let mut t0: Complex =
		if (c0 & 1) == 1 {
		    self.clone()
		} else {
		    ONE.clone()
		};
	    let mut t1: Complex = self.clone();
	    c0 >>= 1;
	    while c0 != 0 {
		t1 = t1.sq();
		if (c0 & 1) == 1 {
		    t0 = t0.mul(&t1);
		}
		c0 >>= 1;
	    }
	    if recip {
		t0 = t0.recip();
	    }
	    return t0;//.clone();
	} else {
	    if self == &ZERO {
		return Complex{re: NAN, im: NAN};
	    } else {
		return ONE.clone();
	    }
	}
    }
    pub fn sin(&self) -> Complex {
	let a: (f64, f64) = self.re.sin_cos();
	return Complex{re: a.0*self.im.cosh(), im: a.1*self.im.sinh()};
    }
    pub fn cos(&self) -> Complex {
	let a: (f64, f64) = self.re.sin_cos();
	return Complex{re: a.1*self.im.cosh(), im: -a.0*self.im.sinh()};
    }
    pub fn tan(&self) -> Complex {
	let a: (f64, f64)  = (2.0*self.re).sin_cos();
	let d: f64 = a.1 + (2.0*self.im).cosh();
	return Complex{re: a.0/d, im: (2.0*self.im).sinh()/d};
    }
    pub fn sqrt(&self) -> Complex {
	return self.ln().scale(0.5).exp();
    }
    pub fn asin(&self) -> Complex {
	if self.im <= 0.0 {
	    return ONE.sub(&self.sq()).sqrt().add(&self.mul(&I)).ln().mul(&NEG_I);
	} else {
	    let t: Complex = self.conj();
	    return ONE.sub(&t.sq()).sqrt().add(&t.mul(&I)).ln().mul(&NEG_I).conj();
	}
    }
    pub fn acos(&self) -> Complex {
	if self.im >= 0.0 {
	    return self.add(&ONE.sub(&self.sq()).sqrt().mul(&I)).ln().mul(&NEG_I);
	} else {
	    let t: Complex = self.conj();
	    return t.add(&ONE.sub(&t.sq()).sqrt().mul(&I)).ln().mul(&NEG_I).conj();
	}
    }
    pub fn atan(&self) -> Complex {
	if self.im >= 0.0 {
	    return I.add(self).div(&I.sub(self)).ln().mul(&HALF_I);
	} else {
	    let t: Complex = self.conj();
	    return I.add(&t).div(&I.sub(&t)).ln().mul(&HALF_I).conj();
	}
    }
    pub fn sinh(&self) -> Complex {
	return Complex{re: (self.im.cos())*(self.re.sinh()), im: (self.im.sin())*(self.re.cosh())};
    }
    pub fn cosh(&self) -> Complex {
	return Complex{re: (self.im.cos())*(self.re.cosh()), im: (self.im.sin())*(self.re.sinh())};
    }
    pub fn tanh(&self) -> Complex {
	let c0: f64 = self.im.cos();
	let c1: f64 = self.re.sinh();
	let d: f64 = c0*c0 + c1*c1;
	return Complex{re: (self.re.sinh())*(self.re.cosh())/d, im: (self.im.sin())*(self.im.cos())/d};
    }
    pub fn asinh(&self) -> Complex {
	if self.re >= 0.0 {
	    return self.sq().add(&ONE).sqrt().add(self).ln();
	} else {
	    let t: Complex = self.conj();
	    return t.sq().add(&ONE).sqrt().add(&t).ln().conj();
	}
    }
    pub fn acosh(&self) -> Complex {
	return self.add(&ONE).sqrt().mul(&self.sub(&ONE).sqrt()).add(self).ln();
    }
    pub fn atanh(&self) -> Complex {
	return ONE.add(self).div(&ONE.sub(self)).ln().scale(0.5);
    }
    pub fn arg(&self) -> Complex {
	return Complex{re: self.im.atan2(self.re), im: 0.0};
    }
    pub fn abs(&self) -> Complex {
	return Complex{re: self.re.hypot(self.im), im: 0.0};
    }
    pub fn re(&self) -> Complex {
	return Complex{re: self.re, im: 0.0};
    }
    pub fn im(&self) -> Complex {
	return Complex{re: self.im, im: 0.0};
    }
    pub fn conj(&self) -> Complex {
	return Complex{re: self.re, im: -self.im};
    }
    pub fn ceil(&self) -> Complex {
	return Complex{re: self.re.ceil(), im: self.im.ceil()};
    }
    pub fn floor(&self) -> Complex {
	return Complex{re: self.re.floor(), im: self.im.floor()};
    }
    pub fn round(&self) -> Complex {
	return Complex{re: self.re.round(), im: self.im.round()};
    }
    pub fn recip(&self) -> Complex {
	let d: f64 = self.re*self.re + self.im*self.im;
	return Complex{re: self.re/d, im: -self.im/d};
    }
    pub fn log2(&self, z1: &Complex) -> Complex {
	return self.ln().div(&z1.ln());
    }
    pub fn log3(&self, z1: &Complex, b0: f64) -> Complex {
	let mut t: Complex = self.ln();
	t.im += TAU*(b0.floor());
	return t.div(&z1.ln());
    }
    pub fn ln2(&self, b0: f64) -> Complex {
	let mut t: Complex = self.ln();
	t.im += TAU*(b0.floor());
	return t;
    }
    pub fn scale(&self, c: f64) -> Complex {
        return Complex{re: self.re*c, im: self.im*c};
    }
    pub fn new() -> Self {
	return Complex{re: 0.0, im: 0.0};
    }
    pub fn set(&mut self, z: &Complex){
	self.re = z.re;
	self.im = z.im;
    }
    pub fn max(&self, z1: &Complex) -> Complex {
	return Complex{re: self.re.max(z1.re), im: self.im.max(z1.im)};
    }
    pub fn min(&self, z1: &Complex) -> Complex {
	return Complex{re: self.re.min(z1.re), im: self.im.min(z1.im)};
    }
    pub fn sq(&self) -> Complex {
	return Complex{re: self.re*self.re - self.im*self.im, im: 2.0*self.im*self.re};
    }
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	if self.re.is_nan() || self.im.is_nan() {
	    write!(f, "nan")
	} else if self.im == 0.0 {
	    write!(f, "{}", self.re)
	} else if self.re == 0.0 {
	    if self.im == 1.0 {
		write!(f, "i")
	    } else {
		write!(f, "{}*i", self.im)
	    }
	} else if self.im < 0.0 {
	    if self.im == -1.0 {
		write!(f, "{} - i", self.re)
	    } else {
		write!(f, "{} - {}*i", self.re, -self.im)
	    }
	} else {
	    if self.im == 1.0 {
		write!(f, "{} + i", self.re)
	    } else {
		write!(f, "{} + {}*i", self.re, self.im)
	    }
	}
    }
}

// Complex does not implement Eq trait, so it is not hashable, so create a seperate class to hash Complex, where NaN == NaN.
#[derive(Debug, Clone)]
pub struct ComplexHash {
    pub re: f64,
    pub im: f64,
}

impl ComplexHash {
    fn hash_val(val: f64) -> u64 {
	if val.is_nan() {
	    // return a standard nan bit pattern for nan.
	    return 0x7ff0000000000000;
	} else {
	    // note: -0 should hash differently than 0, because they are different values.
	    return val.to_bits();
	}
    }
}

impl PartialEq for ComplexHash {
    fn eq(&self, o: &Self) -> bool {
	return ComplexHash::hash_val(self.re) == ComplexHash::hash_val(o.re) && ComplexHash::hash_val(self.im) == ComplexHash::hash_val(o.im);
    }
}

impl Eq for ComplexHash {}

impl Hash for ComplexHash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ComplexHash::hash_val(self.re).hash(state);
        ComplexHash::hash_val(self.im).hash(state);
    }
}

impl From<&Complex> for ComplexHash {
    fn from(c: &Complex) -> Self {
	return ComplexHash{re: c.re, im: c.im};
    }
}
