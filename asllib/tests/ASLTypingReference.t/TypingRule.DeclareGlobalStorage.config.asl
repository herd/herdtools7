constant y = 1;
constant x = 5;
config c : integer{1..x} = (x - 1) + y;
config c_unconstrained_int : integer = 5;

// The next declaration in comment is illegal,
// since pending constraints are not allowed
// for configuration storage elements.
// config c_inherited_constrained : integer{} = 5;
