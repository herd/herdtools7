//D_VMZX: All types are either:
//- named types: those which are declared using the type syntax.
//- or anonymous types: those which are not declared using the type
//syntax.

// RUN: interp %s | FileCheck %s

type named of integer;
var anonymous: integer;

func main() => integer
begin
    return 0;
end
