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
