//R_CLQJ: Where a hyphen (-) is present instead of an identifier, the
//corresponding initialization value will be discarded after evaluating it.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var - = 10;

    return 0;
end
