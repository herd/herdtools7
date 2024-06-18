//R_DBZZ: The let keyword is used to declare global let identifiers denoting
//global storage elements which are all of the following:
//non-compile-time-constant, execution-time, immutable.

// RUN: interp %s | FileCheck %s

let a: integer = 10;

func main() => integer
begin
    return 0;
end
