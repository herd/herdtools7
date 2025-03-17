constant y = 1;
constant x = 5;
var c_var : integer{1..x} = (x - 1) + y;
var c_var_unconstrained_int : integer = 5;
var c_var_well_constrained : integer{0..10} = 5;
var c_var_inherited_constrained : integer{-} = 2^128;
var c_var_no_type_annotation = (x - 1) + y;

// The next two declarations in comment are illegal,
// as inherited constraints require an initializing
// expression.
// var c_var_inherited_illegal : integer{-};
// var c_let_illegal : integer{-};

let c_let : integer{1..x} = (x - 1) + y;
let c_let_unconstrained_int : integer = 5;
let c_let_inherited_constrained : integer{-} = c_var;
let c_let_well_constrained : integer{0..10} = 5;

let c_let_no_type_annotation = (x - 1) + y;
let c_let_no_type_annotation_unconstrained_int = 5;
let c_let_no_type_annotation_inherited_constrained = c_var;
let c_let_no_type_annotation_well_constrained = 5;

let c_let_large_constraint : integer{2^128..2^256} = 2^150;
