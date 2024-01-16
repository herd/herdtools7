// RUN: interp %s | FileCheck %s

func nonexecution(a: integer) => integer
begin
    return 10;
end

func main() => integer
begin
    var a = nonexecution(10);
    return 0;
end
