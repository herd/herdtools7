#!/usr/bin/env python

import importlib
import json
import os
import sys
from glob import glob

def discover_release_cats():
    """
    Discover extra aarch64.cat files under:
        cats/*/aarch64.cat

    Returns paths relative to the book directory, e.g.:
        cats/ArmARM-L.b/aarch64.cat
    """
    matches = glob(os.path.join('cats', '*', 'aarch64.cat'))
    # Normalize to forward slashes for JSON / browser use
    return sorted(m.replace(os.sep, '/') for m in matches)

def jsonify(fname):
    m = importlib.import_module(fname)

    base_cats = list(m.cats)
    extra_cats = discover_release_cats()

    j = json.dumps({
        'record' : m.record,
        'cats' : base_cats + extra_cats,
        'illustrative_tests' : m.illustrative_tests,
        'cfgs' : m.cfgs,
        'bells' : m.bells if 'bells' in dir(m) else None,
        'compatibilities' : m.compatibilities if 'compatibilities' in dir(m) else None,
    })
    print(j)

if __name__ == "__main__":
    jsonify('shelf')
