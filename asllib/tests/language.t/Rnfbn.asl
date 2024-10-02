//R_NFBN: If ty has the structure of a well-constrained integer whose domain
//contains only one value then bits(-: ty) denotes a fixed width bitvector
//whose determined width is equal to the value in the domain of ty.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(-:integer{10});
    return 0;
end
