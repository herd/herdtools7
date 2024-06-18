//R_LGHS: The offset of each slice in a bitfield must be a non-negative,
//statically evaluable integer expression (including zero).

//I_XPDT: Note that R_LGHS applies to bitfields, not bit slice expressions.

// RUN: not interp %s | FileCheck %s

type a of bits(4) {
    -3 b
};

func main() => integer
begin
    return 0;
end
