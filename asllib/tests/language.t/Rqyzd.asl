//R_QYZD: If ty has the structure of the unconstrained integer then bits(-:
//ty) is illegal.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(-:integer);
    return 0;
end
