//All types are either:
// - primitive types: those which only uses the builtin types (see
// R_NZWT)
// - or non-primitive types: those which are named types or which make
// use of named types.

// RUN: interp %s | FileCheck %s

type nonprimitive of integer;
type nonprimitive_a subtypes nonprimitive;
var primitive: integer;

func main() => integer
begin
    return 0;
end
