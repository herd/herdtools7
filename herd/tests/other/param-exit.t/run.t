By default, herd reports a user error and continues with subsequent tests.

  $ herd7 -set-libdir . bad.litmus good.litmus >output 2>&1
  [2]
  $ grep '(User error)$' output
  Warning: File "bad.litmus": "fpac" variant require "pauth2" variant (User error)
  $ grep '^Test Good Allowed$' output
  Test Good Allowed

With -exit enabled, herd exits immediately and does not run subsequent tests.

  $ herd7 -exit true -set-libdir . bad.litmus good.litmus >output 2>&1
  [2]
  $ grep '(User error)$' output
  Warning: File "bad.litmus": "fpac" variant require "pauth2" variant (User error)
  $ grep '^Test Good Allowed$' output
  [1]
