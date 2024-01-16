// RUN: not interp %s | FileCheck %s

type a of exception {};

func main() => integer
begin
    throw a{};
    return 0;
end
