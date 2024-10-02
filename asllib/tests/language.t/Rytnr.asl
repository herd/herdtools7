//R_YTNR: The start expression shall evaluate to a value that remains 
//unchanged if the start expression were to be re-evaluated at the beginning
//of each for-loop iteration.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a : integer = 0;
    for x = a to 10 do
        a = a + 1;
    end
    return 0;
end
