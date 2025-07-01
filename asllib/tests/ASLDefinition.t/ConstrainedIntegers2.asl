// a global which may only hold the values 8, 16 or 32
var gWid: integer {8,16,32};

// a global constant which holds the value 64 and has type integer {64}
constant constantValue = 64;

// Constraints are propagated through basic arithmetic operations,
// so constantValue DIV 2 has type integer {32} which type checks
// against the type of configWid
config configWid: integer {8,16,32} = constantValue DIV 2;

// A config variable that has the type integer {64} and can only hold
// the value 64.
config configValueFixed: integer{64} = 64;

// A config variable that has the type integer and can hold any
// integer value.
config configValue: integer = 64;
