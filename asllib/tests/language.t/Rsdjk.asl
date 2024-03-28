// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    try
        pass;
    catch
        when 10 => pass;
    end
    return 0;
end
