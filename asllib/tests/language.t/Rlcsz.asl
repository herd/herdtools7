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
