// RUN: interp %s | FileCheck %s

getter a[] => integer
begin
    return 1;
end

setter a[] = b: integer
begin
    return;
end

func main() => integer
begin
    return 0;
end
