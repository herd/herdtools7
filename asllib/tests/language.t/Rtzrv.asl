//R_TZRV: The initialization expression in a local constant declaration must
//be a compile-time-constant expression.

// RUN: not interp %s | FileCheck %s

config a = 10;

func main() => integer
begin
    constant b = a;

    return 0;
end
