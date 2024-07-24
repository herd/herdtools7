//R_NZGH: The end expression shall evaluate to a value that remains
//unchanged if the end expression were to be re-evaluated at the beginning
//of each for-loop iteration.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a : integer = 0;
    for x = 10 downto a do
        a = a + 1;
    end
    return 0;
end
