func foo ()
begin
  let a = ARBITRARY: integer  {10..20};
  let b = ARBITRARY: integer {a};
  let c = ARBITRARY: integer {-1000..1000};

  let d: integer {0..20} = c MOD a;
  let e: integer {0..a} = c MOD b;
end;
