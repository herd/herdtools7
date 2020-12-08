import glob

record = "c11popl15"

cats = \
  glob.glob("c11popl15/c11/cats/*.cat")

cfgs = [
    "cfgs/c11web.cfg",
    "cfgs/cpp11.cfg",
    "c11.cfg",
]

tests = []

illustrative_tests = \
  glob.glob("c11popl15/c11/tests/illustrative/*.litmus")

campaign = []

references = [
    {
        "title":
        "Common Compiler Optimisations are Invalid in the C11 Memory Model "
        "and what we can do about it",
        "authors":
        "Viktor Vafeiadis, Thibaut Balabonski, Soham Chakraborty, "
        "Robin Morisset, and Francesco Zappa Nardelli",
        "url":
        "http://plv.mpi-sws.org/c11comp/",
        "last_fetched":
        "Thursday 12 March 2015",
    }
]

notes = '\n'.join([
    "Model(s) as appear in the paper can be found on:",
    "",
    "page 4, Figure 3 for basic definitions",
    "page 5, Figure 5 for standard model axioms",
    "page 6, Section 4 for variant models",
])
