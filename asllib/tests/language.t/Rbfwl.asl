//R_BFWL: The let keyword is used to declare local let identifiers denoting
//local storage elements which are all of the following:
//non-compile-time-constant, execution-time, immutable.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    let a = 10;

    return 0;
end
