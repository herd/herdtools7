// RUN: interp %s | FileCheck %s
// CHECK: HELLO
// CHECK-NEXT: WORLD
// CHECK-NEXT: HELLO
// CHECK-NEXT: HELLO
// CHECK-NEXT: WORLD

func write_hello() => boolean
begin
    print("HELLO");
    return TRUE;
end

func write_world() => boolean
begin
    print("WORLD");
    return TRUE;
end

func main() => integer
begin
    var a: boolean = write_hello() && write_world();
    var b: boolean = write_hello() || write_world();
    var c: boolean = write_hello() --> write_world();

    return 0;
end
