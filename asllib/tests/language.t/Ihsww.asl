//I_HSWW: Declarations in ASL may be described as either
//• compile-time-constant or non-compile-time-constant
//• execution-time or non-execution-time
//config storage elements are both non-compile-time-constant and
//non-execution-time.

// RUN: interp %s | FileCheck %s
// CHECK: 10

config a: integer = 10;

func main() => integer
begin
    print(a);
    return 0;
end
