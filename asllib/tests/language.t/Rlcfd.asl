// RUN: not interp %s | FileCheck %s

var a = 10;

func main() => integer
begin
    var a = 4;

    return 0;
end
