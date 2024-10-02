//R_VBMX: If all arguments of bitvector concatenation are fixed width
//bitvectors then the result of the bitvector concatenation is a fixed width
//bitvector.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(4) = ['11', '00'];

    return 0;
end
