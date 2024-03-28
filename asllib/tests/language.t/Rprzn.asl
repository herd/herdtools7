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
