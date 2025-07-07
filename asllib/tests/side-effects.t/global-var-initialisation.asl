var X: integer = 0;

func incr_X () => integer
begin
  let x = X;
  X = x + 1;
  return x;
end;

let Y0: integer = incr_X ();
let Y1: integer = incr_X ();
let Y2: integer = incr_X ();
// Notice that Y4 and Y3 are inverted
let Y4: integer = incr_X ();
let Y3: integer = incr_X ();

func main () => integer
begin
  println "X = ", X;
  println "Y0 = ", Y0;
  println "Y1 = ", Y1;
  println "Y2 = ", Y2;
  println "Y3 = ", Y3;
  println "Y4 = ", Y4;

  return 0;
end;
