integer main()
    bits(64) x = Zeros(64);
    y = x[1+:10]; // this would be invalid ASL0, but we use it in the XML
    if x IN {'10x1101010'} then  z = 1; // valid
    if x == '10x1101010' then z = 1; // valid
    if x != '10x1101010' then z = 1; // valid
    if x[0+:4] IN '10x1' then // invalid
        z = 1;
    return 1;
