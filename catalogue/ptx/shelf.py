import glob

record = "ptx"

cats = [
    "cats/ptx.cat",
]

cfgs = [
    "cfgs/ptx.cfg",
]

tests = [
]

illustrative_tests = \
  glob.glob("tests/*")

campaign = [
]

references = [
    {
        "title":
        "GPU concurrency: Weak behaviours and programming assumptions",
        "authors":
        "Jade Alglave, Mark Batty, Alastair F. Donaldson, "
        "Ganesh Gopalakrishnan, Jeroen Ketema, Daniel Poetzl, "
        "Tyler Sorensen and John Wickerson",
        "url":
        "http://www0.cs.ucl.ac.uk/staff/j.alglave/papers/asplos15.pdf",
        "last_fetched":
        "Wednesday 21 January 2015",
    }
]

notes = '\n'.join([
    "model(s) as appear in the paper can be found on:",
    "page 12, Figures 15 and 16"
])
