// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    assert(FALSE);
    return 0;
end
