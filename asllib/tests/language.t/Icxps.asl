// RUN: interp %s | FileCheck %s
// CHECK: Hello world

func main() => integer
begin
    print("Hello world");
    return 0;
end
