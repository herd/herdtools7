// RUN: interp %s | FileCheck %s

// a global which may only hold the values 8, 16 or 32
var gWid: integer {8,16,32};
// a global constant which holds the value 64 and has type integer {64}
constant constantValue = 64;
// The initializer for the following config is a compile-time-constant
// whose value is 32 hence its type is integer {32} which type checks
// against the type of configWid
config configWid: integer {8,16,32} = constantValue DIV 2;
// Although constantValue is of type integer, it is a compile-time constant
// so the initializer of configWid is `32: integer{32}`
config halfWid: integer {4,8,16} = configWid DIV 2;

func main() => integer
begin
    return 0;
end
