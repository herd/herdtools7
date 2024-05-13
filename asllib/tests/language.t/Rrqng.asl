//R_RQNG: For-loop counters are immutable, execution-time storage elements.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    for x = 0 to 10 do
        x = x + 1;
    end
    return 0;
end
