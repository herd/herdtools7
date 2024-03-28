// RUN: interp %s | FileCheck %s

type nonprimitive of integer;
type nonprimitive_a subtypes nonprimitive;
var primitive: integer;

func main() => integer
begin
    return 0;
end
