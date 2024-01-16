// RUN: not interp %s | FileCheck %s

config cond = TRUE;

func main() => integer
begin
    var a = 10;

    if cond then
        var b = a;
    end

    var c = a + b;

    return 0;
end
