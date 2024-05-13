//I_HVLX: By convention, identifiers that begin with double-underscore are
//reserved for use in the implementation and should not be used in
//specifications.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var __should_error_by_convention: integer;
    return 0;
end

// XFAIL: *
