//R_FKGP: The initialization expression in a global constant declaration
//must be a compile-time-constant expression.

// RUN: not interp %s | FileCheck %s

func runtime() => integer
begin
    print("Hello");
    return 10;
end

constant a = runtime();

func main() => integer
begin
    return 0;
end
