// RUN: interp %s | FileCheck %s

func a() => integer
begin
    return 10;
end

func main() => integer
begin
    return 0;
end
