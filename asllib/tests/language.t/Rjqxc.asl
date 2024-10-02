//R_JQXC: The while loop terminates (without executing the block again) once
//the condition is FALSE.

// RUN: interp %s | FileCheck %s
// CHECK-NOT: Run

func main() => integer
begin
    while FALSE do
        print("Run");
    end
    return 0;
end
