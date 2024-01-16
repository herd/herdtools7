// RUN: interp %s | FileCheck %s

config a: integer = 10;

func main() => integer
begin
    return 0;
end
