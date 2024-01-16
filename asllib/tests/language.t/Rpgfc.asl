// RUN: not interp %s | FileCheck %s

func a()
begin
    pass;
end

func a()
begin
    pass;
end

func main() => integer
begin
    return 0;
end
