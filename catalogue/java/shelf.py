import glob

record = "java"

bells = [
    "bells/java.bell",
]

cats = [
    "cats/java.cat",
]

cfgs = [
    "cfgs/tutorial.cfg"
]

tests = \
  glob.glob("tests/illustrative/*.litmus")

illustrative_tests = \
  glob.glob("tests/illustrative/*.litmus") 

campaign = [
]

references = [
    {
        "title": 
        "On validity of program transformations in the Java memory model",
        "authors":
        "Jaroslav Svecik and David Aspinall",
        "url":
        "",
        "last_fetched":
        "Wednesday 21 January 2015",
    },
]

notes = '\n'.join([
])
