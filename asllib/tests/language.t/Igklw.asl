//I_GKLW: Multiple subprograms may be defined with the same name provided
//that each definition has a different number or type of arguments to allow
//disambiguation.
//NB: the actual entry point is the one without argument see R_JWPH
// RUN: interp %s | FileCheck %s

func main(a: integer) => integer
begin
    return 0;
end

func main() => integer
begin
    return 0;
end
