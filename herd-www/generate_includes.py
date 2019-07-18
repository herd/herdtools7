#!/usr/bin/python

import os
import sys

CAT_INCLUDES = 'cat_includes'
MAPPING_VAR = 'fname_to_str'

def var_name_of_file_name(fname):
    return fname.lower().replace('.', '_').replace('-', '_') + '_str'

def escape_for_ocaml_string(s):
    return s.replace('\\', '\\\\').replace('"', '\\"')

def generate_header():
    print('(* Warning: file generate by ./generate_includes.py DO NOT EDIT *)')
    print('open Printf')

def generate_string_var(root,fname):
    var_name = var_name_of_file_name(fname)

    f = open(os.path.join(root,fname), 'r')
    contents = ''.join(f.readlines())
    f.close()

    print('let %s = \"%s\"' % (var_name, escape_for_ocaml_string(contents)))

def generate_string_vars(root,files):
    for f in files:
        generate_string_var(root,f)

def generate_mapping(files):
    print('let %s = [' % MAPPING_VAR)
    first = True
    for fname in files:
        semicolon = ''
        if not first:
            semicolon = ';'
        print('  %s(\"%s\", %s)' % (semicolon, fname, var_name_of_file_name(fname)))
        first = False
    print(']')

def generate_autoloader(files):
    print('let autoloader ~prefix ~path =')
    print('  try')
    print('    ignore (prefix) ; Some (List.assoc path %s)' % MAPPING_VAR)
    print('  with Not_found ->')
    print('    None')
    print('')
    print('let register_autoloader () =')
    print('  Js_of_ocaml.Sys_js.mount "." autoloader;')

def generate_ml_file(d):
    files = [f for f in os.listdir(d) if os.path.isfile(os.path.join(d, f)) and f.endswith('.cat')]
    generate_header()
    print('')
    generate_string_vars(d,files)
    print('')
    generate_mapping(files)
    print('')
    generate_autoloader(files)

if __name__ == "__main__":
    assert(len(sys.argv) == 1)
    assert(os.path.isdir(CAT_INCLUDES))
    generate_ml_file(CAT_INCLUDES)
