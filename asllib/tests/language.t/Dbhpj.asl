//D_BHPJ: The formal arguments of a setter are any items in the
//formal_list of the setter’s declaration followed by the RHS
//argument.

// RUN: interp %s | FileCheck %s

var _a: integer;

getter a[index: integer] => integer
begin
    return _a;
end

setter a[index: integer] = value: integer
begin
    _a = value;
    return;
end

func main() => integer
begin
    return 0;
end
