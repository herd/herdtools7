// RUN: interp %s | FileCheck %s

var _a: integer;

getter a[] => integer
begin
    return _a;
end

setter a[] = value: integer
begin
    _a = value;
    return;
end

getter b[index: integer] => integer
begin
    return _a * index;
end

setter b[index: integer] = value: integer
begin
    _a = value * index;
    return;
end

func main() => integer
begin
    return 0;
end
