//R_LCSZ: If a setter function is defined, a similar getter function must be
//declared such that the getter may be invoked with the same actual
//arguments as declared in the setter’s formal_list.

// RUN: not interp %s | FileCheck %s

var _a : integer;

setter a = value: integer
begin
    _a = value;
end

func main() => integer
begin
    return 0;
end
