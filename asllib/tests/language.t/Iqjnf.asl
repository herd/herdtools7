// RUN: interp %s | FileCheck %s

getter a[] => integer
begin
    return 10;
end

getter b[value: integer] => integer
begin
    return value;
end

func main() => integer
begin
    var aa = a[];
    var bb = b[10];

    return 0;
end
