// RUN: not interp %s | FileCheck %s

var _a: integer;

getter a => integer
begin
    return _a;
end

getter _a[value: integer] => integer
begin
    return _a;
end

func main() => integer
begin
    return 0;
end
