// RUN: interp %s | FileCheck %s

getter a => integer
begin
    return 10;
end

func main() => integer
begin
    var b: integer = a;

    return 0;
end
