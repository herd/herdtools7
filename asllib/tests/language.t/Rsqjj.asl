//R_SQJJ: The type of a local storage element declared with the constant
//keyword must be a compile-time-constant type. 

// RUN: not interp %s | FileCheck %s
config a = 10;

func main() => integer
begin
    constant b = a;

    return 0;
end
