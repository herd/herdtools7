// RUN: interp %s | FileCheck %s

func a() => integer
begin
    return 10;
end

func a() => string
begin
    return "10";
end

func main() => integer
begin
    var b: integer = a();
    var c: string = a();

    return 0;
end
