// RUN: not interp %s | FileCheck %s

func a() => integer
begin
    return 10;
end

func main() => integer
begin
    a();
    return 0;
end
