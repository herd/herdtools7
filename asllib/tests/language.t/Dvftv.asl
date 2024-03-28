// RUN: interp %s | FileCheck %s

func alwaysThrow(a: integer)
begin
    if (a == 0) then
        throw;
    else
        throw;
    end
end

func main() => integer
begin
    return 0;
end
