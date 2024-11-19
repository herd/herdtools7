let global_width8: integer {8, 16} = 8;

let global_testA: bits(8) = Zeros{global_width8};

let global_width8b = global_width8;
let global_testB: bits(global_width8) = Zeros{global_width8b};

func tpositive101 ()
begin

  let testA: bits(8) = Zeros {global_width8};
  let width8b = global_width8b;
  let testB: bits(global_width8) = Zeros{width8b};

end;
