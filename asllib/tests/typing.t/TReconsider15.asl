func reconsider15()
begin
    // The following two errors are not statically caught.

    // It is a dynamic error to use an out of bound index.
    let testA = Zeros{2}[8];
    // It is a dynamic error to use a negative width.
    let testB = Zeros{8}[0 +: -1];
end;

