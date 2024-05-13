//R_FRWD: It is an error to use a tab character in ASL files.
// RUN: not interp %s | FileCheck %s

func main() => integer
begin
	return 0;
end

// XFAIL: *
