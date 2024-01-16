// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    if TRUE then
        var a = 10;
    end

    var b = a;
    return 0;
end
