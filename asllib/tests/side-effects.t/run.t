No concurrent write:
  $ aslref write-write-01.asl
  File write-write-01.asl, line 12, characters 10 to 30:
  ASL Typing error: concurrent side effects WriteGlobal "X" and WriteGlobal "X"
  [1]

Ok concurrent different writes:
  $ aslref write-write-02.asl

No concurrent write/read:
  $ aslref write-read-01.asl
  File write-read-01.asl, line 17, characters 10 to 29:
  ASL Typing error: concurrent side effects WriteGlobal "X" and ReadGlobal "X"
  [1]

Ok concurrent write/read different:
  $ aslref write-read-02.asl

Ok concurrent reads:
  $ aslref read-read-01.asl

No concurrent throws:
  $ aslref throw-throw-01.asl
  File throw-throw-01.asl, line 11, characters 12 to 34:
  ASL Typing error: concurrent side effects Throwing "E" and Throwing "E"
  [1]

  $ aslref throw-throw-02.asl
  File throw-throw-02.asl, line 17, characters 12 to 36:
  ASL Typing error: concurrent side effects Throwing "E" and Throwing "F"
  [1]

No concurrent throws/write:
  $ aslref throw-write-01.asl
  File throw-write-01.asl, line 19, characters 12 to 34:
  ASL Typing error: concurrent side effects Throwing "E" and WriteGlobal "X"
  [1]

Ok concurrent throws/read:
  $ aslref throw-read-01.asl

Local read/write:
  $ aslref local-write-loop.asl
  File local-write-loop.asl, line 6, character 2 to line 9, character 5:
  ASL Typing error: concurrent side effects WriteLocal "a" and ReadLocal "a"
  [1]

  $ aslref local-read-loop.asl
  0 0 10
  1 0 10
  2 0 10
  3 0 10
  4 0 10
  5 0 10
  6 0 10
  7 0 10
  8 0 10
  9 0 10
  10 0 10

Recursive calls:
  $ aslref recursive-01.asl
  File recursive-01.asl, line 23, characters 11 to 25:
  ASL Typing error: concurrent side effects Throwing "E" and Throwing "E"
  [1]
  $ aslref recursive-02.asl
  File recursive-02.asl, line 23, characters 11 to 25:
  ASL Typing error: concurrent side effects Throwing "E" and Throwing "E"
  [1]

