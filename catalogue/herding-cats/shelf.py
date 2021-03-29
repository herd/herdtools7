import glob

record = "herding-cats"

cats = [
    "ppc/cats/ppc.cat",
    "arm/cats/arm.cat",
    "arm/cats/arm-llh.cat",
]

cfgs = [
    "cfgs/web.cfg",
]

tests = [
    "ppc/tests/ppc.allowed",
    "ppc/tests/campaign/ppc.observed.gz",
    "arm/tests/arm.allowed",
    "arm/tests/campaign/arm.observed.gz",
]

illustrative_tests = \
  glob.glob("ppc/tests/illustrative/*.litmus") + \
  glob.glob("arm/tests/illustrative/*.litmus")

campaign = [
    "ppc/tests/campaign/power-tests.tgz",
    "arm/tests/campaign/arm-tests.tgz",
]

references = [
    {
        "title":
        "Herding Cats: Modelling, Simulation, Testing, and Data-mining for Weak Memory",
        "authors":
        "Jade Alglave, Luc Maranget and Michael Tautschnig",
        "url":
        "http://www0.cs.ucl.ac.uk/staff/j.alglave/papers/toplas14.pdf",
        "last_fetched":
        "Wednesday 21 January 2015",
    }
]

notes = '\n'.join([
    "page 16, Figure 5 for the template",
    "pages 23-24, Figures 17-18 for Power and ARM's fences and propagation orders",
    "page 34, Figure 25 for Power's preserved program order",
    "page 54, Table VII for ARM's preserved program order",
    "page 54, Table VII for ARM-llh's preserved program order",
])
