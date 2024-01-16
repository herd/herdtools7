// RUN: interp %s | FileCheck %s
// CHECK: TRUE

var a: integer = 10;

func write(b: integer) => integer
begin
    var c: integer = a;
    a = b;
    return c;
end

func main() => integer
begin
    var aa: integer = write(20) + 10;

    a = 10;

    var bb: integer = 10 + write(20);

    print((aa == bb));
    return 0;
end
