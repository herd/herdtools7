//I_JDCC: Note that the wording “valid bitslice” means that the width and 
//offsets of the slice must be correct under the rules for bitslices.

// RUN: not interp %s | FileCheck %s

type a of bits(5) {
    [10:7] aa
};

func main() => integer
begin
    return 0;
end

// XFAIL: *
