// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var func: integer;
    return 0;
end
