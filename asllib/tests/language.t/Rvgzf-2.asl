//R_VGZF: The shift operations are defined as follows:
//   shiftleft_int(x, n) = RoundDown(Real(x) ∗ 2.0n) 
//   shiftright_int(x, n) = RoundDown(Real(x) ∗ 2.0−n)
//where the RoundDown library function rounds down to negative infinity.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert (10 << 3) == 80;
    assert (3  << 5) == 96;
    assert (100 >> 5) == 3;
    assert (50  >> 3) == 6;
    return 0;
end
