// RUN: interp %s | FileCheck %s
// CHECK: 10

type a of record {
    c: integer,
    b: integer
};

func main() => integer
begin
    var b : a;
    b.c = 10;
    var c: integer = b.c;
    print(c);
    return 0;
end
