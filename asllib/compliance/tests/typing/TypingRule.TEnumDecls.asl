type SuperEnum of enumeration {LOW, HIGH};
// LOW and HIGH are of type enumeration {LOW, HIGH}
type SubEnum of SuperEnum; // Legal
type OtherEnum of SuperEnum; // Legal
