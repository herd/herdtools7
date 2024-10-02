//I_DFML: Note that determining the type of a global identifier may require
//the type of its initialization expression to be determined first. That
//type may not depend recursively on itself.

// RUN: not interp %s | FileCheck %s

type a subtypes a;

func main() => integer
begin
    return 0;
end
