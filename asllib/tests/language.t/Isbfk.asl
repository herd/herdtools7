// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{0..10} = 5;

    var b: integer{5..6} = a + 1 as integer{5..6};

    return 0;
end