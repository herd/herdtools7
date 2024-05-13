//R_ZWHP: The syntax type id1 subtypes id2 ; is syntactic sugar for type id1
//of id2 subtypes id2 ;.

// RUN: interp %s | FileCheck %s

type a of integer;
type b subtypes a;

func main() => integer
begin
    return 0;
end
