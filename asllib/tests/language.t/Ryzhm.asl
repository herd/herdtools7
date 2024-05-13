//R_YZHM: The lowest common ancestor ot types S and T is:
//- If S and T are the same type: S (or T).
//- If S and T are both named types: the (unique) common supertype of S and
//T that is a subtype of all other common supertypes of S and T.
//- If S and T both have the structure of array types with the same index
//type and the same element types:
//  – If S is a named type and T is an anonymous type: S
//  – If S is an anonymous type and T is a named type: T
//- If S and T both have the structure of tuple types with the same number
//of elements and the types of elements of S type-satisfy the types of the
//elements of T and vice-versa:
//  – If S is a named type and T is an anonymous type: S
//  – If S is an anonymous type and T is a named type: T
//  – If S and T are both anonymous types: the tuple type with the type of
//  each element the lowest common ancestor of the types of the
//  corresponding elements of S and T.
//- If S and T both have the structure of well-constrained integer types:
// – If S is a named type and T is an anonymous type: S
// – If T is an anonymous type and S is a named type: T
// – If S and T are both anonymous types: the well-constrained integer type
// with domain the union of the domains of S and T.
//- If either S or T have the structure of an unconstrained integer type:
//  – If S is a named type with the structure of an unconstrained integer
//  type and T is an anonymous type: S 
//   – If T is an anonymous type and S is a named type with the structure of
//an unconstrained integer type: T 
//   – If S and T are both anonymous types: the unconstrained integer type.
//- If either S or T have the structure of an under-constrained integer
//type: the under-constrained integer type. 
//- Else: undefined.

// RUN : interp %s | FileCheck %s

// ! There are alot of cases to test here and its not interpreter related.

func main() => integer
begin
     return 0;
end
