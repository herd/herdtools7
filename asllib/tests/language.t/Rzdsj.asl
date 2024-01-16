// RUN: interp %s | FileCheck %s

var a : integer{10} = 10;
var b : integer{9} = 0x9;

func main() => integer
begin
    return 0;
end
