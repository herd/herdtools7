// RUN: interp %s | FileCheck %s

var a = b;
constant b = 10 + 10 + 10;

func main() => integer
begin
    return 0;
end
