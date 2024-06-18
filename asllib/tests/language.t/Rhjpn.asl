//R_HJPN: An integer type with a constraint is called a constrained integer.

// RUN: interp %s | FileCheck %s

type constrained of integer {10};

func main() => integer
begin
    return 0;
end
