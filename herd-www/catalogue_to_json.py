#!/usr/bin/python

import importlib
import json
import re
import sys

def jsonify(fname):
    m = importlib.import_module(fname)
    j = json.dumps({
        'record' : m.record,
        'cats' : m.cats,
        'illustrative_tests' : m.illustrative_tests,
        'cfgs' : m.cfgs,
        'bells' : m.bells if 'bells' in dir(m) else None,
        'compatibilities' : m.compatibilities if 'compatibilities' in dir(m) else None,
    })
    print(j)

if __name__ == "__main__":
    jsonify('shelf')
