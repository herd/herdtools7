//R_MBRM: The base value of a record type is a record whose elements have
//the base values of their types.

// RUN: interp %s | FileCheck %s
// CHECK: 0

type c of record {
    a: integer
};

func main() => integer
begin
    var b: c;
    var a : integer = b.a;
    print(a);
    return 0;
end
