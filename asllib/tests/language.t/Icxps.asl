//I_CXPS: Output may be generated using the print procedure. This takes any
//number of arguments, of any type, and makes best efforts to print them to
//diagnostic output. The format of the output is not defined.

// RUN: interp %s | FileCheck %s
// CHECK: Hello world

func main() => integer
begin
    print("Hello world");
    return 0;
end
