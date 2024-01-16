// RUN: interp %s | FileCheck %s

config a = 10;

func compiletime() => integer
begin
    return 10;
end

func noncompiletime() => integer
begin
    return a;
end

func main() => integer
begin
    return 0;
end
