// RUN: not interp %s | FileCheck %s

var a: integer;

func a()
begin
    pass;
end

func main() => integer
begin
    return 0;
end
