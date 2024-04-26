// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert (10 << 3) == 80;
    assert (3  << 5) == 96;
    assert (100 >> 5) == 3;
    assert (50  >> 3) == 6;
    return 0;
end