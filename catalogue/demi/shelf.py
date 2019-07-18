record = "demi"

cats = [
  "cats/rien.cat",
  "cats/sc.cat",
  "cats/tso.cat",
]

bells = [
  "bells/demi.bell",
  "bells/tso.bell",
]

cfgs = [
  "cfgs/demi.cfg",
]

illustrative_tests = [
  "tests/R.litmus",
  "tests/R+fence.litmus",
]

sc_tests = [
  "tests/R.litmus",
  "tests/R+fence.litmus",
]

compatibilities = [
    {
        "bells": ["bells/demi.bell"],
        "cats": ["cats/rien.cat"],
        "litmuses": sc_tests,
    },
    {
        "bells": ["bells/demi.bell"],
        "cats": ["cats/sc.cat"],
        "litmuses": sc_tests,
    },
    {
        "bells": ["bells/tso.bell"],
        "cats": ["cats/tso.cat"],
        "litmuses": sc_tests,
    },
]
