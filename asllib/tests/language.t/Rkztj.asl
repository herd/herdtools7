//R_KZTJ: If no TRUE condition is found and there is no else block, no block
//is executed.

// RUN: interp %s | FileCheck %s
// CHECK: 0
// CHECK-NEXT: 3

func main() => integer
begin
    print("0");
    if FALSE then
        print("1");
    elsif FALSE then
        print("2");
    end
    print("3");
    return 0;
end