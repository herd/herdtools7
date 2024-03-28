// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    assert(10);
    return 0;
end
