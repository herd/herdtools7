// RUN: interp %s | FileCheck %s

constant a = 10 + 10;
config b = 10 + 10;
var c = a + b;

func main() => integer
begin
    return 0;
end
