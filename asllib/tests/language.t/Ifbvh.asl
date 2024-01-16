// RUN: interp %s | FileCheck %s

var b: integer;

getter a[] =>integer
begin
    return b;
end

getter c[value1 : integer] =>integer
begin
    return b;
end

setter a[] = value: integer
begin
    b = value;
    return;
end

setter c[value1 : integer] = value: integer
begin
    b = value * value1;
    return;
end

func main() => integer
begin
    a[] = 10;
    c[10] = 10;

    return 0;
end
