// RUN: interp %s | FileCheck %s

config width = 10;
var a : bits(width);

func main() => integer
begin
    return 0;
end
