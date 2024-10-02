//R_CHTH: When main returns (without throwing an exception) the runtime
//should pass the return value to the hosting environment.
//For example: an ASL runtime for native executables may use the return
//value of main as the exit status of the process.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    return 1;
end
