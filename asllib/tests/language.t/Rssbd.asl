//R_SSBD: For statements introduce a for-loop counter which is in scope for
//the body of the for statement.

// RUN: interp %s | FileCheck %s
// CHECK: 10

func main() => integer
begin
    var a : integer = 0;

    for x = 0 to 10 do
        a = x;
    end

    print(a);

    return 0;
end
