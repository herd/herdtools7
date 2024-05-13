//R_XNSK: Uncaught exceptions cause termination of the application by the
//runtime. If an exception is thrown from main it is an uncaught exception.
//The runtime should signal an error to the hosting environment.

// RUN: not interp %s | FileCheck %s

type a of exception{};


func thrower()
begin
    throw a;
end

func main() => integer
begin
    thrower();
    return 0;
end
