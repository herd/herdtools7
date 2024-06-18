// RUN: interp %s | FileCheck %s

// This test is syntatically invalid and should report an error.
// It is invalid to declare multiple identifiers (using a simple list) AND define an initial value.
// Instead, to declare multiple identifiers and their initial values, use the parenthesized list syntax (see R PNQJ).
func main() => integer
begin
    var a, b = 10;

    return 0;
end

// XFAIL: *
