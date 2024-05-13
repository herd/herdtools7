//R_JVTR: If no case alternative is selected, and there is no otherwise_opt
//block, it indicates an error in the specification, and an implementation
//defined exception may be thrown.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    case TRUE of
        when FALSE => print("FALSE");
    end
    return 0;
end
