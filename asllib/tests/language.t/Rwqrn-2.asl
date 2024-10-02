//R_WQRN: The expression in an assertion statement must be side-effect-free.

// RUN: not interp %s | FileCheck %s

var a: integer;

func sideeffect() => boolean
begin
    a = a + 1;

    return TRUE;
end

func main() => integer
begin
    assert(sideeffect());
    return 0;
end

// XFAIL: *
