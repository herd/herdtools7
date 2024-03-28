// RUN: interp %s | FileCheck %s

func a()
begin
    return;
end

func main() => integer
begin
    return 0;
end
