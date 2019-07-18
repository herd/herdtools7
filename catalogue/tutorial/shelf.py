record = "tutorial"

cats = [
    "cats/tutorial.cat",
    "cats/kittens.cat",
    "cats/tiger.cat",
    "cats/jaguar.cat",
    "cats/panther.cat",
]

bells = [
    "bells/tutorial.bell",
    "bells/kittens.bell",
    "bells/tiger.bell",
    "bells/jaguar.bell",
    "bells/panther.bell",
]

cfgs = [
    "cfgs/tutorial.cfg",
]

illustrative_tests = [
    "tests/2+2w.litmus",
    "tests/coRR.litmus",
    "tests/coRW1.litmus",
    "tests/coRW2.litmus",
    "tests/coWR.litmus",
    "tests/coWW.litmus",
    "tests/iriw+hws.litmus",
    "tests/iriw.litmus",
    "tests/isa2.litmus",
    "tests/isa2+lwf+dep+dep.litmus",
    "tests/lb+dep+dep.litmus",
    "tests/lb+dep+lw.litmus",
    "tests/lb.litmus",
    "tests/lb+lws.litmus",
    "tests/ledzep.litmus",
    "tests/mp.litmus",
    "tests/mp+lw+dep.litmus",
    "tests/mp-mit-scopes+fcta+fgpu.litmus"
    "tests/mp-mit-scopes+fgpu+fsys.litmus"
    "tests/mp-mit-scopes+fgpus.litmus"
    "tests/mp-mit-scopes.litmus",
    "tests/mp-plain.litmus",
    "tests/mp-special+branch.litmus",
    "tests/r.litmus",
    "tests/sb+fwr+fwr.litmus",
    "tests/sb.litmus",
    "tests/wrc.litmus",
    "tests/wrc+lwf+dep.litmus",
    "tests/w+rw+ww.litmus",
    "tests/w+rw+ww+lws.litmus",
]

tutorial_tests = [
    "tests/mp.litmus",
    "tests/r.litmus",
    "tests/sb.litmus",
    "tests/2+2w.litmus",
    "tests/coRR.litmus",
    "tests/coRW1.litmus",
    "tests/coRW2.litmus",
    "tests/coWR.litmus",
    "tests/coWW.litmus",
    "tests/iriw.litmus",
    "tests/isa2.litmus",
    "tests/wrc.litmus",
    "tests/lb.litmus",
    "tests/w+rw+ww.litmus",
]

ledzep_tests = [
    "tests/ledzep.litmus",
]

kittens_tests = [
    "tests/mp.litmus",
    "tests/sb.litmus",
    "tests/sb+fwr+fwr.litmus",
    "tests/coWW.litmus",
    "tests/coRW1.litmus",
    "tests/coRW2.litmus",
    "tests/coWR.litmus",
    "tests/coRR.litmus",
    "tests/isa2.litmus",
    "tests/wrc.litmus",
    "tests/lb.litmus",
    "tests/2+2w.litmus",
    "tests/w+rw+ww.litmus",
    "tests/iriw.litmus",
]

tiger_tests = [
    "tests/mp.litmus",
    "tests/sb.litmus",
    "tests/sb+fwr+fwr.litmus",
    "tests/coWW.litmus",
    "tests/coRW1.litmus",
    "tests/coRW2.litmus",
    "tests/coWR.litmus",
    "tests/coRR.litmus",
    "tests/lb.litmus",
    "tests/lb+dep+dep.litmus",
    "tests/2+2w.litmus",
    "tests/mp+lw+dep.litmus",
    "tests/lb+lws.litmus",
    "tests/lb+dep+lw.litmus",
    "tests/isa2.litmus",
    "tests/wrc.litmus",
    "tests/isa2+lwf+dep+dep.litmus",
    "tests/wrc+lwf+dep.litmus",
    "tests/w+rw+ww.litmus",
    "tests/w+rw+ww+lws.litmus",
    "tests/iriw.litmus",
    "tests/iriw+hws.litmus",
]

jaguar_tests = [
    "tests/mp-mit-scopes.litmus",
    "tests/mp-mit-scopes+fgpus.litmus",
    "tests/mp-mit-scopes+fgpu+fsys.litmus",
    "tests/mp-mit-scopes+fcta+fgpu.litmus",
]

panther_tests = [
    "tests/mp-plain.litmus",
    "tests/mp-special+branch.litmus",
]

compatibilities = [
    {
        "bells": ["bells/tutorial.bell"],
        "cats": ["cats/tutorial.cat"],
        "litmuses": tutorial_tests,
    },
    {
        "bells": ["bells/ledzep.bell"],
        "cats": ["cats/tutorial.cat"],
        "litmuses": ledzep_tests,
    },
    {
        "bells": ["bells/kittens.bell"],
        "cats": ["cats/kittens.cat"],
        "litmuses": kittens_tests,
    },
    {
        "bells": ["bells/tiger.bell"],
        "cats": ["cats/tiger.cat"],
        "litmuses": tiger_tests,
    },
    {
        "bells": ["bells/jaguar.bell"],
        "cats": ["cats/jaguar.cat"],
        "litmuses": jaguar_tests,
    },
    {
        "bells": ["bells/panther.bell"],
        "cats": ["cats/panther.cat"],
        "litmuses": panther_tests,
    },
]

campaign = []

references = []

notes = ""
