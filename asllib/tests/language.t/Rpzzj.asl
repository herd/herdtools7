//R_PZZJ: An asserted type conversion 
//  E as S
//may be used to assert that the value of an expression E which has atype
//with the structure T is within the domain of the target type S with the
//structure S' if and only if at least one of the following conditions hold:
//- T and S' are the same type.
//
//- T and S' are both bitvector types with the same determined width. If the
//widths are not equal it is a dynamic error.
//
//- T is a bitvector type with determined width and S' is a bitvector type
//  with undetermined width and the width of T is within the possible widths
//  of S'. If the width of T is not within the possible widths of S' it is
//  dynamic error.
//
//- T is an integer type and the value of E is within the domain of S'. If E
//  is not within the domain of S' it is a dynamic error.

// RUN: interp %s | FileCheck %s

config c = 4;
func main() => integer
begin
    var a = 4;
    var b = a as integer{4};
    var d = c as integer{4};

    return 0;
end
