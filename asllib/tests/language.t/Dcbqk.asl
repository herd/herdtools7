// RUN: interp %s | FileCheck %s

func fixedanddetermined() => bits(10)
begin
    return Zeros(10);
end

func main() => integer
begin
    return 0;
end
