//R_ZFFV: Where an expr_atom consists of an identifier which is declared as
//a getter, then the expr_atom is treated in the same way as a function
//invocation. In this case, if a sequence of null_or_slice_lists is present,
//each must consist of a single expression. The sequence of
//null_or_slice_lists shall be used as the actual expressions for the
//invocation of the getter.

// RUN : interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
     return 0;
end
