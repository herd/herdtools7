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
