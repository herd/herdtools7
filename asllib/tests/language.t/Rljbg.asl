//R_LJBG: If ty has the structure of a well-constrained integer whose domain
//contains more than one value then bits(-: ty) denotes a constrained width
//bitvector of undetermined width.

// RUN: interp %s | FileCheck %s

type a of bits(-:integer{0..10});

func main() => integer
begin
    return 0;
end
