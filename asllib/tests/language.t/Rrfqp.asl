//R_RFQP: Two statically evaluable expressions being checked for equivalence
//are both reduced to canonical form, and then the two canonical forms are
//compared structurally.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a = (1 + 1 * (3 - 3)) == (4 - (3 - 4));
    return 0;
end
