//R_GHRP: If the type of expr has the structure of the unconstrained integer
//then bits(expr) is illegal.

// RUN: not interp %s | FileCheck %s

config a: integer = 10;

type b of bits(a);

func main() => integer
begin
    return 0;
end
