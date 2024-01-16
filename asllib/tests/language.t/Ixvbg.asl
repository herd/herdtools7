// RUN: interp %s | FileCheck %s

func widCheck {M: integer} (N: integer, b: bits(M)) => bits(N)
begin
    if (N==M) then
        return b as bits(N); // Inserts execution-time check
    else
        return Zeros(N);
    end
end

func main() => integer
begin
    var a: bits(10);
    var b = widCheck(10, a);
    return 0;
end
