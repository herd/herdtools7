func tpositive8b ()
begin
    // NOTE: this test is not supported by ASLRef.
    for i = 100 as integer {8,16} to 110 as integer {0,31} do
        let testK : integer {8..31} = i;
    end
end

