// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    for i = 0.0 to 10.0 do
        pass;
    end
    return 0;
end
