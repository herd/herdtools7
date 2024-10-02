//D_JWXX: A control flow path is a path through the control flow graph
//derived from the syntactic structure of a subprogram. Where a
//conditional branch occurs (e.g. in an if or case statement) the
//condition is ignored; all branches are added to the control flow graph.

// RUN: interp %s | FileCheck %s

// ! Nothing to test

func main() => integer
begin
    return 0;
end
