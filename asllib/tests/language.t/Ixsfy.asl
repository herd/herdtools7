// RUN: interp %s | FileCheck %s

config a = 10;

func main() => integer
begin
    var b = 10 + 10;
    var c = 10 + a;
    return 0;
end
