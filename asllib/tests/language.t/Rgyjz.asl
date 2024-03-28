// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(10);
    var b = a as bits({0..10});

    return 0;
end
