// RUN: interp %s | FileCheck %s
// CHECK: 4

var _a: integer = 10;

getter a => integer
begin
    return _a;
end

setter a = value: integer
begin
    _a = value;
end

func main() => integer
begin
    a = 4;
    print(_a);

    return 0;
end
