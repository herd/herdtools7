// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    if 10 then
        pass;
    end
    return 0;
end
