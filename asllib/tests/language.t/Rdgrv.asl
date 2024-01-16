// RUN: interp %s | FileCheck %s
// CHECK: Hello
// CHECK-NEXT: World
func main() => integer
begin
    try
        print("Hello");
    catch
        when integer => pass;
    end
    print("World");
    return 0;
end
