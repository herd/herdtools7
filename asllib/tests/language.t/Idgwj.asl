//I_DGWJ: Note that we maintain strong typing of named types since R_FMXK
//only allows named types to be assigned to each other if T subtypes S which
//requires explicit declaration. See also I_MHYB.
//
//R_FMXK: Type T type-satisfies type S if and only if at least one of the
//following conditions holds:
//• T is a subtype of S
//• T subtype-satisfies S and at least one of S or T is an anonymous type
//• T is an anonymous bitvector with no bitfields and S has the structure of
//a bitvector (with or without bitfields) of the same width as T.
//
//I_MHYB: Note that subtype-satisfaction alone does not make named type T a
//subtype of named type S. T must also be declared to be a subtype of S or
//one of the subtypes of S
 
// RUN: not interp %s | FileCheck %s

type a of integer;
type b of integer;

func main() => integer
begin
    var aa: a = 10;
    var bb: b = aa;
    return 0;
end
