// RUN: interp %s | FileCheck %s
// CHECK: Hello World

func main() => integer
begin
    print("Hello World");
    return 0;
end
