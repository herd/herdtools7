// RUN: interp %s | FileCheck %s

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

func main() => integer
begin
    return 0;
end
