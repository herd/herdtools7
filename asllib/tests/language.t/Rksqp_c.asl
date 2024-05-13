//R_KSQP: A global let, constant or config storage element must be 
//initialized with an initializer expression.

// RUN: not interp %s | FileCheck %s

config a: integer;

func main() => integer
begin
    return 0;
end
