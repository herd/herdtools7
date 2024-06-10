// RUN: not interp %s | FileCheck %s
// IVGSP Overload resolution is based only on the name and argument types, and takes no account of how the result is used.
// note this test does NOT check IVGSP, instead this test fails due to the fact
// that the two declarations of a() have a clash since the name and argument
// types are identical.

func a() => integer
begin
   var c: string = a();

   return 0;
end
