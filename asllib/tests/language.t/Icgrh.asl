//I_CGRH: A checked type conversion allows code to explicitly mark places
//where uses of constrained types would otherwise cause a type-check error.
//The intent is to reduce the incidence of unintended errors by making such
//uses fail type-checking unless the checked type conversion is provided.

// RUN: interp %s | FileCheck %s

// ! Nothing to test here

func main() => integer
begin
    return 0;
end
