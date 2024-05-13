//R_GWCP: The syntax
//  integer <constraint>
//denotes a constrained integer whose constraint is the set of values in the
//constraint.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{0, 10};
    return 0;
end
