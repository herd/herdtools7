// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var __should_error_by_convention: integer;
    return 0;
end
