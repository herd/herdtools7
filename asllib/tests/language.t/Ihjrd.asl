//I_HJRD: Since the name of a setter must exist in the global namespace as
//the name of a similar getter it is not possible to declare anything (other
//than a getter or setter) with the same name as a setter.

// RUN: not interp %s | FileCheck %s

var b: integer;

getter a[] => integer
begin
    return b;
end

setter a[] = value: integer
begin
    b = value;
    return;
end

var a: string;

func main() => integer
begin
    return 0;
end
