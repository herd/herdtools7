//R_MHWM: A named type declaration declares an identifier, associated with a
//new type, with the structure and domain of values of a given base type.
//
//Named types allow the use of strong typing since a named type is not
//considered to be the same as any other type, including the type it derives
//its structure and domain from. Unlike some other languages, ASL does not
//provide a way to create an alias for an existing type. 


// RUN: interp %s | FileCheck %s

type a of integer;

func main() => integer
begin
    return 0;
end
