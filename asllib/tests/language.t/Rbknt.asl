// RUN: interp %s | FileCheck %s

config b: integer = 1;

func main() => integer
begin
    var a = 1 + b;
    return 0;
end
