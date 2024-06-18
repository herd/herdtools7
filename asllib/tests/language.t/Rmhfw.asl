//R_MHFW: An assertion failure arising from the assert keyword is a dynamic
//error.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    assert(FALSE);
    return 0;
end
