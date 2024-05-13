//I_BTCY: Note that compile-time-constant integers are implicitly
//constrained, hence the addition of a constraint to a bitvector width is
//unnecessary for compile-time-constant widths.

// RUN : interp %s | FileCheck %s

// ! I am not sure what this is saying

func main() => integer
begin
     return 0;
end
