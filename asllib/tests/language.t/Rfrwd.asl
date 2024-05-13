//R_FRWD: It is an error to use a tab character in ASL files.

// RUN: not interp %s | FileCheck %s

//NB: there is a tab in the file, and the test is checking whether this is
//valid

func main() => integer
begin
	return 0;
end
