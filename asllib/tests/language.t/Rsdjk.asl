//R_SDJK: The type given in a when clause of a try..catch statement must
//have the structure of exception.

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
