// RUN: interp %s | FileCheck %s


var a: integer{10};
var b: integer{0..10};
var c: integer{5 + 5};
constant d: integer{0..10} = 5;
var e: integer{d..20};

func main() => integer
begin
    return 0;
end
