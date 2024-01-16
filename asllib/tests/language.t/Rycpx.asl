// RUN: interp %s | FileCheck %s

config a = 10;

func f1() => boolean
begin
    return a == 0;
end

func f2(t: integer{2,4,8}) => boolean
begin
    return a == t;
end

func checkY (y: integer)
begin
    if (f1() && f2(y as {2,4,8})) then pass; end
end

func main() => integer
begin
    checkY(10);
    return 0;
end
