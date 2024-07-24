//R_WZSL: If an assertion expression is FALSE when the assertion statement
//is executed, it indicates an error in the specification. An implementation
//may throw an implementation-defined exception in this case, but is not
//required to.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    assert(FALSE);
    return 0;
end
