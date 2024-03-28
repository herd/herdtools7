// RUN: not interp %s | FileCheck %s

var globe_var: integer = 0;

func write_to_var(a: integer) => integer
begin
    globe_var = a;
    return a;
end

func main() => integer
begin
    var a: integer = write_to_var(10) + globe_var;
    return 0;
end
