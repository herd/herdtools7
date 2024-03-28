// RUN: interp %s | FileCheck %s

var t: integer;

func a()
begin
    let b = 10;
end

setter c[] = value: integer
begin
    t = value;
end

func main() => integer
begin
    return 0;
end
