// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    throw 10;
    return 0;
end
