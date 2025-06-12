type SuperEnum of enumeration {LOW, HIGH};

// Illegal: enumeration {TOP, BOTTOM} does not subtype-satisfy SuperEnum of structure
// enumeration {LOW, HIGH}
type SubEnumIllegal2 of enumeration {TOP, BOTTOM} subtypes SuperEnum;
