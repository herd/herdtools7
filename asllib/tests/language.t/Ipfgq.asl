// RUN: not interp %s | FileCheck %s

func a()
begin
    pass;
end

getter a => integer
begin
    return 10;
end

func main() => integer
begin
    return 0;
end
