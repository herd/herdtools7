//R_YYPN: Two anonymous bitvector types are identical if they have the same
//width and they have bitfields with the same names and constituent bits,
//irrespective of the expressions used in their definition.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : bits(2) {[0] a [1] b} = '11';
    var b : bits(2) {[0] a [1] b} = a;

    return 0;
end
