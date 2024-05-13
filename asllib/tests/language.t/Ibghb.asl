//I_BGHB: When statically determining whether two bitvector types have the
//same width, only statically evaluable expressions need be considered. 

// RUN: interp %s | FileCheck %s

type b of bits(10) {
    [0] a
};

func main() => integer
begin
    return 0;
end
