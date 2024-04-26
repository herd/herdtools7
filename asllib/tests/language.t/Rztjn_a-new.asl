// RUN: interp %s | FileCheck %s

func main() => integer
begin
    assert (6  DIV 3 ) == 2;
    assert (-6 DIV  3) == -2;
    return 0;
end