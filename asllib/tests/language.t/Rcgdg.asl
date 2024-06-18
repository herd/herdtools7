//R_CGDG: The bitvector type may have bits in its bitvector representation
//which do not correspond to any bitfield.

// RUN: interp %s | FileCheck %s

type a of bits(4) {
    [0] b
};

func main() => integer
begin
    return 0;
end
