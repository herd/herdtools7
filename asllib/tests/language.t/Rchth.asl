// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    return 1;
end
