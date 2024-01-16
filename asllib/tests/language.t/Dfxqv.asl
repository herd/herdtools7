// RUN: interp %s | FileCheck %s

type T1 of integer; // the named type `T1` whose structure is integer
type T2 of (integer, T1); // the named type `T2` whose structure is (integer, integer)
// Note that (integer, T1) is non-primitive since it uses T1
var x: T1;
// the type of x is the named (hence non-primitive) type `T1`
// whose structure is `integer`
var y: integer;
// the type of y is the anonymous primitive type `integer`
var z: (integer, T1);
// the type of z is the anonymous non-primitive type `(integer, T1)`
// whose structure is `(integer, integer)`

func main() => integer
begin
    return 0;
end
