//R_ZYVW: If no case alternative is selected, and there is an otherwise_opt
//block, the otherwise_opt block is executed.

// RUN: interp %s | FileCheck %s
// CHECK: Otherwise

func main() => integer
begin
    case TRUE of
        when FALSE => print("FALSE");
        otherwise => print("Otherwise");
    end
    return 0;
end
