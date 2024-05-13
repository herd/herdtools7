//R_NHGP: The expressions specifying a bitslice must be such that the width
//of the resulting bitvector has constrained type. For example, in b[j:i],
//both j and i must have constrained type.

// RUN: not interp %s | FileCheck %s

var a = '1111 1111';

func test(b: integer) => integer
begin
    return a[b:0];
end

func main() => integer
begin
    return 0;
end
