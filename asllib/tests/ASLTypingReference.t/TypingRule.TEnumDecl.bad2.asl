type SuperEnum of enumeration {LOW, HIGH};

// Illegal: no enumeration literal may appear in multiple enumeration type declarations.
type SubEnumIllegal1 of enumeration {LOW, HIGH} subtypes SuperEnum;
