# math-compiler

Command line tool to compile and evaluate complex math expressions

To run this, you will need Rust.

Run `cargo build --release` in the `math-compiler-command-line/` folder to compile the program.

Run `math-compiler-command-line/target/release/math_compiler_command_line.exe` to run the program.

# math notation, operations, constants, and variables

**Numbers:**

\> Integer: 123456789

\> Decimal: 0.123456789

\> Exponent: 98765432*10^10

(u, v, and w represent arbitrary functions of z)

**Arithmetic:**

\> Addition: u + v

\> Subtraction: u - v

\> Multiplication: u\*v

\> Division: u/v

\> Negation: -u

**Exponentials and logarithms:**

\> Exponentiation: u^v or u\*\*v or pow(u, v)

\> Square root: sqrt(u)

\> Natural exponentiation (of base e): exp(u)

\> Natural logarithm (of base e): ln(u) or log(u)

\> Based logarithm: log(u, base)

\> Branched and based logarithm: log(u, base, branch)

**Trigonometric functions:**

\> Sine: sin(u)

\> Cosine: cos(u)

\> Tangent: tan(u)

\> Cosecant: csc(u)

\> Secant: sec(u)

\> Cotangent: cot(u)

**Inverse trigonometric functions**:

\> Inverse sine: asin(u)

\> Inverse cosine: acos(u)

\> Inverse tangent: atan(u)

\> Inverse cosecant: acsc(u)

\> Inverse secant: asec(u)

\> Inverse cotangent: acot(u)

**Hyperbolic trig functions**:

\> Hyperbolic sine: sinh(u)

\> Hyperbolic cosine: cosh(u)

\> Hyperbolic tangent: tanh(u)

\> Hyperbolic cosecant: csch(u)

\> Hyperbolic secant: sech(u)

\> Hyperbolic cotangent: coth(u)

**Inverse hyperbolic trig functions**:

\> Inverse hyperbolic sine: asinh(u)

\> Inverse hyperbolic cosine: acosh(u)

\> Inverse hyperbolic tangent: atanh(u)

\> Inverse hyperbolic cosecant: acsch(u)

\> Inverse hyperbolic secant: asech(u)

\> Inverse hyperbolic cotangent: acoth(u)

**Comparisons**:

\> Equal: u == v (if u equals v, returns 1, otherwise returns 0)

\> Not equal: u != v (if u does not equal v, returns 1, otherwise returns 0)

\> Less: u < v (if re(u) < re(v), returns 1, otherwise returns 0)

\> Greater: u > v (if re(u) > re(v), returns 1, otherwise returns 0)

\> Less or equal: u <= v (if re(u) <= re(v), returns 1, otherwise returns 0)

\> Greater or equal: u >= v (if re(u) >= re(v), returns 1, otherwise returns 0)

**Logical operators**:

\> And: u & v (if both u and v are not 0, returns 1, otherwise returns 0)

\> Or: u | v (if either or both u and v are not 0, returns 1, otherwise returns 0)

\> Not: ~u (if u is 0, returns 1, otherwise returns 0)

**Auxiliary functions**:

\> Absolute value: abs(u)

\> Complex argument: arg(u)

\> Real part: re(u)

\> Imaginary part: im(u)

\> Complex conjugate: conj(u)

\> Ceiling: ceil(u)

\> Floor: floor(u)

\> Round: round(u)

\> Minimum: min(u, v)

\> Maximum: max(u, v)

**Constants**:

\> Imaginary unit: i and j = sqrt(-1)

\> Euler's Number: e = 2.718281828…

\> Pi: pi = 3.1415926535…

**Branch functions**:

\> If: *{condition => u}* (if condition is not 0, then u is returned, otherwise 0 is returned)

\> If-else: *{condition => u : v}* (if condition is not 0, then u is returned, otherwise v is returned)

**Definitions**:

\> Definitions: *@name = u ; v(name)* (the variable name can be any unique name; the variable name is defined and given value u, and then name the variable name can be used until it goes out of scope; if the variable is defined directly inside parentheses, then the scope is inside those parentheses; if the variable is defined directly inside the entire equation, then the scope is the whole equation; variables must be defined either directly at the beginning of the equation or directly inside parentheses; multiple variables can be defined in succession, without the need for nested parentheses)

\> Redefinitions: *$name = u; v(name)* (the variable name must already be defined and must be in scope; the variable name is redefined and assigned the value u, and the variable name can continue to be used as a defined variable)

**Iterated functions**:

\> Iterated function: *[@count -> u ; @iter = v ; $iter = w(count, iter)]* (the variable count will start and 0 and increment by 1 until it reaches u; the variable iter will initially have the value v; every iteration, the variable iter will be redefined as w(count, iter), and count will be incremented by 1; the loops stops when count reaches the value u, but note that the loop will not run for the time when count is u)

\> Summation: *[@count -> u ; @iter = 0; $iter = iter + w(count)]* (summation is a special case of iterated functions)

\> Product: *[@count -> u ; @iter = 1; $iter = iter*w(count)]* (product is a special case of iterated functions)
