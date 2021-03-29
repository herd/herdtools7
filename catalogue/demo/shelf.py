import glob

record = "demo"

bells = [
    "bells/anarchy.bell",
]

cats = [
    "cats/x86.cat",
    "cats/arm.cat",
    "cats/ptx.cat",
    "cats/c11.cat",
]

cfgs = [
    "cfgs/tutorial.cfg",
    "cfgs/cpp11.cfg",
]

illustrative_tests = \
  glob.glob("tests/*.litmus")

x86_tests = \
  glob.glob("tests/*-x86*.litmus")

arm_tests = \
  glob.glob("tests/*-arm*.litmus")

ptx_tests = \
  glob.glob("tests/*-ptx*.litmus")

c11_tests = \
 glob.glob("tests/*-c11*.litmus")

compatibilities = [
    {
        "bells": ["bells/anarchy.bell"],
        "cats": ["cats/anarchy.cat"],
        "litmuses": illustrative_tests,
    },
    {
        "bells": ["bells/anarchy.bell"],
        "cats": ["cats/x86.cat"],
        "litmuses": x86_tests,
    },
    {
        "bells": ["bells/anarchy.bell"],
        "cats": ["cats/arm.cat"],
        "litmuses": arm_tests,
    },
    {
        "bells": ["bells/anarchy.bell"],
        "cats": ["cats/ptx.cat"],
        "litmuses": ptx_tests,
    },
    {
        "bells": ["bells/anarchy.bell"],
        "cats": ["cats/c11.cat"],
        "litmuses": c11_tests,
    },
]

campaign = [
]

references = [
]

notes = '\n'.join([
])
