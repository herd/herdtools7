// RUN: not interp %s | FileCheck %s

constant a: integer;

func main() => integer
begin
    return 0;
end
