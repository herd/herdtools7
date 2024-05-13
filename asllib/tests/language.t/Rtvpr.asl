//R_TVPR: Types can be combined into tuple types whose values consist of
//tuples of values of those types.

// RUN: interp %s | FileCheck %s

type a of (integer, integer);

func main() => integer
begin
    return 0;
end
