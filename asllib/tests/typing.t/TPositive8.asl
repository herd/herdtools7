////////////////////////////////////////////////////////////////////////////////
// loops
////////////////////////////////////////////////////////////////////////////////
func positive8(N : integer {0..15}, M : integer {7,15})
begin
    // Loop iterators are constrained integers with the constranints based on the constraints (not the values) of the range
    for i = 0 to 7 do
        // i has type integer {0..7} because the type of the start index is integer {0} and the end is integer {7}
        let testA : integer {0..7} = i;
    end
    for i = 7 downto 0 do
        // i has type integer {0..7} because the type of the start index is integer {7} and the end is integer {0}
        let testB : integer {0..7} = i;
    end
    for i = 0 to N do
        // i has type integer {0..15} because the type of the start index is integer {0} and the max value in the domain of N is 15
        let testC : integer {0..15} = i;
    end
    for i = 0 to M do
        // i has type integer {0..15} because the type of the start index is integer {0} and the max value in the domain of M is 15
        let testD : integer {0..15} = i;
    end
    for i = N downto 0 do
        let testE : integer {0..15} = i;
    end
    for i = M downto 0 do
        let testF : integer {0..15} = i;
    end
    for i = N to 31 do
        let testG : integer {0..31} = i; // i has type integer {0..31} as the min value in the domain of N is 0
    end
    for i = M to 31 do
        let testH : integer {7..31} = i; // i has type integer {7..31} as the min value in the domain of M is 7
    end
    for i = 31 downto N do
        let testI : integer {0..31} = i;
    end
    for i = 31 downto M do
        let testJ : integer {7..31} = i;
    end
    // NOTE: testK is required to be legal staticaly, but the ATC's will fail at runtime if this code is reached
    for i = 100 as integer {8,16} to 110 as integer {0,31} do
        let testK : integer {8..31} = i; // i has type integer {8..31} as the min value of the domain of the start
                                         // expression is 8, and the max value of the domain of the end expression is 31.
    end
end

