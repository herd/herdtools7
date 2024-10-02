//R_MXYQ: Reads and writes of a bitvector type variable’s field are treated
//as though they were of the field’s type The bits of the field are mapped
//to the bits of the bitvector as though the slices comprising the field
//were concatenated in the order declared in the bitfield.

// RUN: interp %s | FileCheck %s

type a of bits(10) {
    [5:0] a,
    [4] b
};

func main() => integer
begin
    var ab : a = '1111 1111 11';
    var abc : bits(6) = ab.a;
    ab.b = '0';

    return 0;
end
