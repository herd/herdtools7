//R_ZRKM: The type declaration type id1 of ty subtypes id2; declares a new
//named type id1 which has the same structure as ty and is also a subtype of
//the named type id2.

// RUN: interp %s | FileCheck %s

type a of integer;
type b of integer subtypes a;

func main() => integer
begin
    return 0;
end
