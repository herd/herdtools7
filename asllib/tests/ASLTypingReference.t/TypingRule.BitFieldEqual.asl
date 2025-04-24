type Nested_Type of bits(32) {       // absolute fields
   [31:16] fmt0 {                    // [31:16] fmt0
        [15] common,                 // [31:31] fmt0.common
        [14:13, 12:2, 1, 0] layer1 { // [30:16] fmt0.layer1
            [14:13] remainder {      // [30:29] fmt0.layer1.remainder
               [1]  moving,          // [30:30] fmt0.layer1.remainder.moving
               [0]  extra            // [29:29] fmt0.layer1.remainder.extra
            },
        },
        [13] extra                   // [29:29] fmt0.extra
   },
   [31:16] fmt1 {                    // [31:16] fmt1
       [15] common,                  // [31:31] fmt1.common
       [0]  moving                   // [16:16] fmt1.moving
   },
   [31] common,                      // [31:31] common
   [0]  fmt                          // [0:0] fmt
};

func main() => integer
begin
    return 0;
end;
