// RUN: not interp %s | FileCheck %s

var x: integer{} = 10;

func main() => integer
begin
    return 0;
end
