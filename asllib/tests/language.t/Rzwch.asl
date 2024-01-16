// RUN: interp %s | FileCheck %s

type a of exception {
    x: integer,
    y: integer
};

func main() => integer
begin
    var b = a { x = 10, y = 5 };
    return 0;
end
