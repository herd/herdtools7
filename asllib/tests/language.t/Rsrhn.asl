//R_SRHN: A named type id1 may only be a subtype of a named type id2 if id1
//subtype-satisfies id2.

// RUN: not interp %s | FileCheck %s

type a of integer;
type b of string subtypes a;

func main() => integer
begin
    return 0;
end
