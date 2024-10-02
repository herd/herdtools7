//D_QJYV: An assertion statement takes an expression that is asserted by
//the specification to be TRUE when the assertion statement is executed.

// RUN: interp %s | FileCheck %s

config a: integer = 10;

func main() => integer
begin
    assert(TRUE);
    assert(1 == 1);
    assert(a == 10);
    return 0;
end
