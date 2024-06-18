//R_RYMD: Constant bit-masks are written using 1, 0, x and spaces surrounded
//by single-quotes. The x represents a don’t care character.
//Definition of a mask:
//  <bitmask_lit> ::= '\'' ["01x "]* '\''


// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var result = '1000' IN '1xxx';

    return 0;
end
