//I_VYLK: In the following contrived example, the declared type of only is a //constrained width bitvector of undetermined width. However, its
//initialization expression is a constrained width bitvector of determined 
//width bits(1), so the resulting storage element is the constrained width
//bitvector of determined width bits(1) by R_PLYX.
//
//R_PLYX: Where the initialization expression in a variable_declaration is a //bitvector of determined width, if the initialization expression type
//satisfies the declared type, then the declaration creates a storage
//element whose determined width is the determined width of the
//initialization expression.

// RUN: interp %s | FileCheck %s

var only: bits(1) = Zeros(1);

func main() => integer
begin
    return 0;
end
