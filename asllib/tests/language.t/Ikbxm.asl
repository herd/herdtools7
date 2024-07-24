// RUN: not interp %s | FileCheck %s

func a()
begin
    var b = c;
end

func main() => integer
begin
    var c = 10;
    a();
    return 0;
end
