// RUN: interp %s | FileCheck %s

config c = 4;
func main() => integer
begin
    var a = 4;
    var b = a as integer{4};
    var d = c as integer{4};

    return 0;
end
