//D_MRYB: The RHS argument of a setter declaration is an argument whose
//name and type are given in a setter_declaration following the = symbol.

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
