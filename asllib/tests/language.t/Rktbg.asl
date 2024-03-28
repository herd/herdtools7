// RUN: not interp %s | FileCheck %s

func a()
begin
    var b: bits(4);
    var c = b[10:0];
end

func main() => integer
begin
    return 0;
end
