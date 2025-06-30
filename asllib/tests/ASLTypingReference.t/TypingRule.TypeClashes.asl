func simple_procedure(i: integer) begin pass; end;
// The following declaration in comment is illegal as the argument is integer-typed:
// func simple_procedure(i: integer{0..32}) begin pass; end;
func simple_procedure(b: boolean) begin pass; end;
func simple_procedure(r: real) begin pass; end;
func simple_procedure(s: string) begin pass; end;
func simple_procedure(bv: bits(8)) begin pass; end;

type Color of enumeration {RED, GREEN, BLUE};
type Status of enumeration {OKAY, ERROR};
func enum_procedure(c : Color) begin pass; end;
func enum_procedure(s : Status) begin pass; end;

func array_procedure(int_arr2 : array[[2]] of integer) begin pass; end;
// The following declarations in comments are illegal as the array index
// does not distinguish between array types for the purpose of determining
// type-clashing.
// func array_procedure(int_arr3 : array[[3]] of integer) begin pass; end;
// func array_procedure(enum_arr : array[[Color]] of integer) begin pass; end;

func array_procedure(boolean_arr : array[[2]] of boolean) begin pass; end;
func array_procedure(real_arr : array[[2]] of real) begin pass; end;

type Rec1 of record{-};
type Rec2 of record{-};
type Exc1 of exception{-};
type Exc2 of exception{-};
func structured_procedure(r: Rec1) begin pass; end;
func structured_procedure(r: Rec2) begin pass; end;
func structured_procedure(e: Exc1) begin pass; end;
func structured_procedure(e: Exc2) begin pass; end;

func tuple_procedure(t: (integer, boolean, real)) begin pass; end;
// The following declaration in comment illegal as the argument clashes
// with (integer, boolean, real).
// func tuple_procedure(t: (integer{5..7}, boolean, real)) begin pass; end;
func tuple_procedure(t: (integer, boolean)) begin pass; end;
func tuple_procedure(t: (integer, real)) begin pass; end;
