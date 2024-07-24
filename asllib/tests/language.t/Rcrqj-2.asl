//R_CRQJ: There are division operations div_int and fdiv_int and a remainder //operation frem_int.

// RUN: interp %s | FileCheck %s


func main() => integer
begin
    assert (10 DIV 2) == 5;
    assert (10 MOD 3) == 1;

    return 0;
end
