// RUN: interp %s | FileCheck %s

func main(a: integer) => integer
begin
    return 0;
end

func main() => integer
begin
    return 0;
end
