// Declaration of a constant.
// Must have an initializing expression.
constant PI = 3.14;

// Declaration of an immutable storage element.
// Must have an initializing expression.
let K: integer{0..1000} = 500;

// Declarations of mutable storage elements.
var PC: bits (32) = Zeros{32};
var Regs: array[[16]] of bits(32);

// A mutable storage element without an initializing expression.
var c : collection { data: bits(16), status: bits(8) };

// Declaration of a configurable storage element.
// Must have an initializing expression.
config MaxIrq: integer = 480;
