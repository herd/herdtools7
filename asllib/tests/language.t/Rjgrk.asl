// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var _: integer;
    return 0;
end
