type MyException of exception;
// Illegal: only singular types can be used for config storage elements.
config d: MyException = MyException{-};
