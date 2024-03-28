// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    Unreachable();
    return 0;
end
