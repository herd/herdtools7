func negative5(size : integer {0..3})
begin
    // The domain of the output of the ATC is the domain of the ATC, and this domain is used for type inference
    let temp                   = size as integer {8,16}; // temp has type integer {8,16}
    let testA : integer {0..3} = temp; // illegal as value of type integer {8,16} can't be assigned to var of type integer {0..3}.
                                       // Even though temp is guaranteed to be within the range 0..3 because it comes from size

end

