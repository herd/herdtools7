// RUN: not interp %s | FileCheck %s

var a: integer;

func sideeffect() => boolean
begin
    a = a + 1;

    return TRUE;
end

func main() => integer
begin
    assert(sideeffect());
    return 0;
end
