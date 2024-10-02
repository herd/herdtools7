//R_BKNT: An operation implements an operator if it appears in the same row
//as the operator in one of the Operator definitions tables.

// RUN: interp %s | FileCheck %s

config b: integer = 1;

func main() => integer
begin
    var a = 1 + b;
    return 0;
end
