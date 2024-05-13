//R_PRZN: The initialization expression in a global config declaration must
//be a non-execution-time expression.

// RUN: not interp %s | FileCheck %s

func runtime() => integer
begin
    print("Hello");
    return 10;
end

config a = runtime();

func main() => integer
begin
    return 0;
end

// XFAIL: *
