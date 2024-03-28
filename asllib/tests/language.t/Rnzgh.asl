// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a : integer = 0;
    for x = 10 downto a do
        a = a + 1;
    end
    return 0;
end
