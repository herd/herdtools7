// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    for x = 0 to 10 do
        break;
    end
    return 0;
end