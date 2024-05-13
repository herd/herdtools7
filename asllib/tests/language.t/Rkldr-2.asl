//R_KLDR: The start expression and the end expression must not be
//side-effecting.

// RUN: not interp %s | FileCheck %s

var a : integer = 0;

func sideeffect() => integer
begin
    a = a + 1;
    return 10;
end

func main() => integer
begin
    for x = 0 to sideeffect() do
        pass;
    end
    return 0;
end

// XFAIL: *
