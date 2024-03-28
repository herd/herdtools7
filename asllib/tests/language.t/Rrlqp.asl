// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{0..10, 20..30};

    return 0;
end
