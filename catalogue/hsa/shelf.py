import glob

record = "hsa"

bells = [
    "bells/hsa.bell",
]

cats = [
    "cats/hsa.cat",
]

cfgs = [
    "cfgs/hsa.cfg",
]

illustrative_tests = glob.glob("tests/doc2/*.litmus") 

tests = illustrative_tests

campaign = [
]

references = [
    {
        "title": 
        "HSA Platform System Architecture Specification 1.0",
        "authors":
        "HSA Foundation",
        "url":
        "http://www.hsafoundation.com/html/HSA_Library.htm",
        "last_fetched":
        "Wednesday 18 March 2015",
    },
]

notes = '\n'.join([
])
