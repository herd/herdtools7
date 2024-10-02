//R_ZWGH: The domain of a fixed width bitvector is the set of values which
//can be represented by bitvector literals which are of the same length as
//the fixed width bitvector and which consist of the characters ‘0’ and ‘1’.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(2) = '01';
    return 0;
end
