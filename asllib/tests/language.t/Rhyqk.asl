//R_HYQK: There is a single global namespace for all globally declared
//identifiers apart from setters. This namespace associates each identifier
//with the kind of global object to which it may refer.

// RUN : interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
     return 0;
end
