//D_TRVR: A type T subtype-satisfies type S if and only if all of the
//following conditions hold:
// - If S has the structure of an integer type then T must have the
// structure of an integer type.
// - If S has the structure of a real type then T must have the structure
// of a real type.
// - If S has the structure of a string type then T must have the
// structure of a string type.
// - If S has the structure of a boolean type then T must have the
// structure of a boolean type.
// - If S has the structure of an enumeration type then T must have the
// structure of an enumeration type with exactly the same enumeration
// literals.
// - If S has the structure of a bitvector type with determined width
// then either T must have the structure of a bitvector type of the same
// determined width or T must have the structure of a bitvector type with
// undetermined width.
// - If S has the structure of a bitvector type with undetermined width
// then T must have the structure of a bitvector type.
// - If S has the structure of a bitvector type which has bitfields then
// T must have the structure of a bitvector type of the same width and
// for every bitfield in S there must be a bitfield in T of the same
// name, width and offset, whose type type-satisfies the bitfield in S.
// - If S has the structure of an array type with elements of type E then
// T must have the structure of an array type with elements of type E,
// and T must have the same element indices as S.
// - If S has the structure of a tuple type then T must have the
// structure of a tuple type with same number of elements as S, and each
// element in T must type-satisfy the corresponding element in S.
// - If S has the structure of a record type then T must have the
// structure of a record type with at least the same fields (each with
// the same type) as S.
// - If S has the structure of an exception type then T must have the
// structure of an exception type with at least the same fields (each
// with the same type) as S.
// - If S does not have the structure of an aggregate type or bitvector
// type then the domain of T must be a subset of the domain of S.
// - If either S or T have the structure of a bitvector type with
// undetermined width then the domain of T must be a subset of the domain
// of S.


// RUN : interp %s | FileCheck %s

// ! There are alot of cases to test here and its not interpreter related.

func main() => integer
begin
     return 0;
end
