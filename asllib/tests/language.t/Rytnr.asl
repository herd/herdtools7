// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a : integer = 0;
    for x = a to 10 do
        a = a + 1;
    end
    return 0;
end
