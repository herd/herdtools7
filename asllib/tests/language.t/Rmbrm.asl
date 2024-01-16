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
