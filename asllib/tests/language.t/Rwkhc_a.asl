// RUN: interp %s | FileCheck %s

func a()
begin
    throw;
end

func b()
begin
    a();
end

func main() => integer
begin
    return 0;
end
