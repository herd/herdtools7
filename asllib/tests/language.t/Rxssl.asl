// RUN: interp %s | FileCheck %s
// CHECK: 2

func main() => integer
begin
    if FALSE then
        print("1");
    else
        print("2");
    end
    return 0;
end
