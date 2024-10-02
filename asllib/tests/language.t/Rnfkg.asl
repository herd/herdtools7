//R_NFKG: A local storage element declared with constant is initialized with
//the value of its initialization expression during compilation.

// RUN : interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
     return 0;
end
