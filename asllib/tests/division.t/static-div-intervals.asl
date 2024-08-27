func foo ()
begin
  let a = UNKNOWN: integer {2..5};
  let b = UNKNOWN: integer {3..6};
  let c: integer {0..10} = a DIV b;
end
