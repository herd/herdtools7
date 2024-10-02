//I_FCGK: Note that the identifier given in the when clause does not map to
//the type of the matched exception.

// RUN : interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
     return 0;
end
