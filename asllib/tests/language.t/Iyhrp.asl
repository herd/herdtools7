// RUN: not interp %s | FileCheck %s

func f(x: integer {2, 4}, y: integer {-1..1})
begin
    let z = x DIV y; // Illegal
    // type of z involves constraints with division by zero
    // let ok = x DIV (y as {-1,1});
    // // legal but incurs runtime check
end

func main() => integer
begin
    return 0;
end
