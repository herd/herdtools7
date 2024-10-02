//R_RMTQ: Each field of a bitvector type is defined using the bitfield
//syntax which specifies the name of the field and one or more valid
//bitslices of the bitvector which comprise that field.

// RUN: interp %s | FileCheck %s

type a of bits(6) {
    [3, 1:0] aa,
};

func main() => integer
begin
    return 0;
end
