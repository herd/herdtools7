// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a = 10;
    var b = 5;
    var c = (a + b) as integer{10..20};
    return 0;
end
