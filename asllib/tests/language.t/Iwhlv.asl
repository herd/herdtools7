// RUN: interp %s | FileCheck %s

var _a : integer;

getter a => integer
begin
    return _a;
end

setter a = value: integer
begin
    _a = value;
    return;
end

func main() => integer
begin
    a = 5;
    return 0;
end
