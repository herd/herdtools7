func foo ()
begin
  let a = UNKNOWN: integer {2..5};
  let b = UNKNOWN: integer {3..6};
  let c: integer {0..10} = a DIV b;

  let d : integer {10} = 10;
  let e : integer {10..20} = 10;
  let f: integer {1..2} = e DIV d;
  let g: integer {1} = d DIV e;
end
