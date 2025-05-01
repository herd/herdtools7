impdef func Foo(bv : bits(2) { [0] lsb, [1] msb }) begin pass; end;

// Illegal: signature does not match impdef, since the order
// of bitfields for `bv` is different than the one in the
// signature above.
implementation func Foo(bv : bits(64) { [1] msb, [0] lsb }) begin pass; end;
