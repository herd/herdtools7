//D_BMGM: The domain of a type is the set of values which
//storage elements of that type may hold.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : integer = 100;
    var b : bits(1) = '0';
    var c : integer{2, 16} = 16;
    var d : bits({2, 16}) = '00';
    return 0;
end
