func foo ()
begin
  let a = UNKNOWN: integer  {10..20};
  let b = UNKNOWN: integer {a};
  let c = UNKNOWN: integer {-1000..1000};

  let d: integer {0..20} = c MOD a;
  let e: integer {0..a} = c MOD b;
end

