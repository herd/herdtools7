// RUN: not interp %s | FileCheck %s

func a() => integer
begin
    var b: integer{0..10} = 4;
    return b;
end

func main() => integer
begin
    return 0;
end
