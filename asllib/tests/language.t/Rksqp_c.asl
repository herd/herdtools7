// RUN: not interp %s | FileCheck %s

config a: integer;

func main() => integer
begin
    return 0;
end
