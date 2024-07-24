// RUN: interp %s | FileCheck %s
// CHECK-NOT: 5

func main() => integer
begin
    for x = 10 to 1 do
        print(x);
    end
    return 0;
end
