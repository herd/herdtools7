// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(-:integer);
    return 0;
end
