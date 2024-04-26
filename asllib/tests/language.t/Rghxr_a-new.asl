// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert ( 6 MOD 3) == 0;
    assert (-5 MOD 3) == 1;
    return 0;
end