// RUN: interp %s | FileCheck %s

getter test => integer
begin
    return 10;
end

setter test = value : integer
begin
    pass;
end

func main() => integer
begin
    test = 10;
    return 0;
end
