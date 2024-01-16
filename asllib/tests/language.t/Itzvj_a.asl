// RUN: not interp %s | FileCheck %s

func invalid() => bits(integer{4, 8})
begin
    return '0000';
end

func main() => integer
begin
    return 0;
end
