// RUN: not interp %s | FileCheck %s

var globe_var: integer = 0;

func write_to_var(a: integer) => integer
begin
    globe_var = a;
    return a;
end

func main() => integer
begin
    var a: (integer, integer) = (write_to_var(1), write_to_var(2));
    return 0;
end
