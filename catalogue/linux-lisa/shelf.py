import glob

record = "linux"

bells = [
    "bells/linux.bell",
]

cats = [
    "cats/linux.cat",
]

cfgs = [
    "cfgs/web.cfg"
]

illustrative_tests = \
  glob.glob("tests/memory_barriers/*.litmus") + \
  glob.glob("tests/lwn573436/*.litmus") + \
  glob.glob("tests/lwn573497/*.litmus") + \
  glob.glob("tests/extra/*.litmus")

tests = illustrative_tests

campaign = [
]

references = [
    {
        "title": 
        "memory-barriers.txt",
        "authors":
        "David Howells and Paul E. McKenney",
        "url":
        "https://www.kernel.org/doc/Documentation/memory-barriers.txt",
        "last_fetched":
        "Wednesday 21 January 2015",
    },
    {
        "title": 
        "lwn573436",
        "authors":
        "Paul E. McKenney, Mathieu Desnoyers, Lai Jiangshan, and Josh Triplett",
        "url":
        "https://lwn.net/Articles/573436/",
        "last_fetched":
        "Wednesday 21 January 2015",
    },
    {
        "title": 
        "lwn573497",
        "authors":
        "Paul E. McKenney, Mathieu Desnoyers, Lai Jiangshan, and Josh Triplett",
        "url":
        "https://lwn.net/Articles/573497/",
        "last_fetched":
        "Wednesday 21 January 2015",
    },
     
]

notes = '\n'.join([
])
