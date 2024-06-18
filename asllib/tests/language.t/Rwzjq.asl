//R_WZJQ: If a global storage element is initialized with an initializer
//expression then the type of the global storage element may be omitted, in
//which case the type of the global storage element is the type of the
//initializer expression.

// RUN: interp %s | FileCheck %s

let a = 10;

func main() => integer
begin
    return 0;
end
