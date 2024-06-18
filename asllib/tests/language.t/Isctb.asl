// RUN: not interp %s | FileCheck %s

var b: integer;

type ty of integer;

getter a[] => integer
begin
    return b;
end

setter a[] = value: integer
begin
    b = value;
    return;
end


getter a[] => ty
begin
    return b;
end

setter a[] = value: ty
begin
    b = value;
    return;
end

func main() => integer
begin
    return 0;
end
