//R_KCDS: An exception expression shall assign every field of the exception.

// RUN: not interp %s | FileCheck %s

type a of exception {
    x: integer,
    y: integer
};

func main() => integer
begin
    var b = a { x = 10 };
    return 0;
end