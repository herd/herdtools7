// RUN: not interp %s | FileCheck %s
config a = 10;

func main() => integer
begin
    constant b = a;

    return 0;
end
