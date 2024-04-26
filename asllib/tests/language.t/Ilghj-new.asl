// RUN: interp %s | FileCheck %s

func check{M: integer{4,8}}(flag: boolean, x: bits(M), y: bits(8)) => boolean
begin
    if flag then
        return (x as bits(8)) == y; // valid
    end
    return FALSE;
end

func main() => integer
begin
    return 0;
end