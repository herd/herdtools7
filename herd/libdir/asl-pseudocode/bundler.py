"""Process ISA XML files into raw asl files.

Working assumptions:
    - for any xml file, the o_path as built by `o_path_of_tree` uniquely
      identifies the pseudocode in the xml file.
    - no other program concurrently modifies our output files
"""

# SPDX-FileCopyrightText: Copyright 2022-2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
# SPDX-License-Identifier: BSD-3-Clause

import argparse
import dataclasses
import itertools
import logging
import re
import textwrap
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Callable, Iterable, Optional, Tuple, Union
from xml.etree.ElementTree import Element, parse

# In this whole module, path names used as input (resp. output) are prefixed with `i_` (resp. `o_`).

_logger = logging.getLogger("bundler0")
_last_run_start = datetime.now()

MESSAGE_ON_TOP = """
// Copyright (c) 2010-2025 Arm Limited or its affiliates. All rights reserved.
// This document is Non-Confidential. This document may only be used and
// distributed in accordance with the terms of the agreement entered into by
// Arm and the party that Arm delivered this document to.

// SPDX-FileCopyrightText: Copyright 2022-2025 Arm Limited and/or its affiliates <open-source-office@arm.com>
// SPDX-License-Identifier: BSD-3-Clause

// More information can be found in notice.html or
//    https://developer.arm.com/documentation/ddi0602/latest/Proprietary-Notice

// This document was automatically extracted from the XML files distributed at
//    https://developer.arm.com/downloads/-/exploration-tools
// by a script authored by Hadrien Renaud <hadrien.renaud.22@ucl.ac.uk> or 
// <hadrien.renaud2@arm.com> and available at bundler.py or
//    https://github.com/herd/herdtools7/blob/master/herd/libdir/asl-pseudocode/bundler.py


"""

DEFAULT_INSTR_DIR = Path("other-instrs")

SEPARATOR_LINE = (
    "// ============================================================================="
)

RE_IDENTIFIER_WITH_SLICES = re.compile(
    r"(?P<id>[a-zA-Z_]\w*)<(?P<slices>\d|(\d+:\d+))>"
)
RE_IDENTIFIER = re.compile(r"(?P<id>[a-zA-Z_]\w*)")
RE_FILENAME = re.compile(r"\W")
RE_VARIABLE_ARRAY = re.compile(r"<[^>]*>")
RE_FIELD_WITH_SLICES_V1 = re.compile(
    r"(?P<name>[a-zA-Z_]\w*)(?P<slices>\[((\d*)|(\d*:\d*))])"
)
RE_FIELD_WITH_SLICES_OPT_V1 = re.compile(
    r"(?P<name>[a-zA-Z_]\w*)(?P<slices>\[((\d*)|(\d*:\d*))])?"
)
RE_RANGE = re.compile(r"\[(?P<n1>\d+):(?P<n2>\d+)]")
RE_SINGLE = re.compile(r"\[(?P<n>\d+)]")
RE_RANGE_PART_SPECIFIER = re.compile(
    r"(?P<factor>\d+)?(\((?P<var>\w)(?P<shift>[+-]\d+)\)|(?P<var2>\w))(?P<constant>[+-]\d+)?"
)


SEEN_REG_NAMES: set[str] = set()


def o_path_of_tree(root: Element, o_dir: Path) -> Path:
    """Constructs and checks the writing file corresponding to this tree.

    For instructions, this uses the path found in the xml attribute
    `ps_section`.

    This function is not used by registers.

    For other types of xml files, this is simply the root id with the `.asl`
    extension.
    """
    root_type = root.get("type")
    if root_type == "instruction":
        ps_name = root.find("ps_section").find("ps").get("name")
        if ps_name.startswith("A64"):
            ftmp = ps_name.split(".", 1)[1]
            ftmp = "aarch64." + ftmp
            ftmp = ftmp.split(".")
            ftmp = "/".join(ftmp) + ".opn"
            file_name = Path(ftmp)
        else:
            _logger.debug("Moving file %s to %s", ps_name, DEFAULT_INSTR_DIR)
            file_name = Path(ps_name + ".opn")
            file_name = DEFAULT_INSTR_DIR / file_name
        o_path = o_dir / file_name
        o_path.parent.mkdir(parents=True, exist_ok=True)

    elif root.tag == "register":
        assert False

    else:
        if root_type != "pseudocode":
            _logger.warning("Unknown root type %s", root_type)

        o_path = o_dir / (root.get("id").lower() + ".asl")

    if (
        o_path.exists()
        and datetime.fromtimestamp(o_path.stat().st_mtime) < _last_run_start
    ):
        # We consider that if it has been modified after `_last_run_start`,
        # then it is this program that edited it, and so we are overriding our
        # own results. The underlying assumption is that 2 different xml files
        # that have the same o_path have the same pseudocode.
        _logger.warning("Overriding %s", o_path)

    return o_path


def header_of_tree(root: Element, o_path: Optional[Path] = None) -> str:
    """Find a nice title for the written file."""
    post_header = []

    if root.tag == "instructionsection":
        title = r if (r := root.get("title")) is not None else ""
        titles = [title]

        if o_path is not None and o_path.exists():
            with o_path.open(mode="r") as f:
                for line in f.readlines():
                    if line.startswith("// =="):
                        break

                    if line.startswith("// "):
                        titles.append(line[2:].strip())

            titles = sorted(frozenset(titles))

        post_header.append("// Execute")
        post_header.append("// =======")
        post_header.append("")

    elif root.tag == "register":
        titles = [
            root.find("reg_long_name").text
            + " ("
            + root.find("reg_short_name").text
            + ")"
        ]

    else:
        titles = [root.get("title")]

    return "\n".join(
        (
            *(f"// {i:^74}" for i in titles),
            SEPARATOR_LINE,
            "",
            MESSAGE_ON_TOP,
            *post_header,
            "",
        )
    )


def asl_for_instruction_fields(regdiagram, instr_id) -> [str]:
    """Reads the xml for an instruction and constructs the ASL from its
    tabular representation of the instruction binary encoding"""
    result = {}

    for box in regdiagram.findall("./box"):
        name = box.get("name")
        if name is None:
            continue

        hibit = box.get("hibit")
        width = box.get("width")

        pos = hibit if width is None else f"{hibit} : (({hibit} - {width}) + 1)"

        name = name.strip()
        if name.isidentifier():
            # Normal case
            if name not in result:
                result[name] = pos
                continue

            # Otherwise we fall back to the slicing case, with an implicit slice
            _logger.debug(
                "Instruction %s: Adding implicit slice to field %s", instr_id, name
            )
            if width is None:
                name = name + "<0>"
            else:
                name = f"{name}<{width}:0>"

        regex_match = RE_IDENTIFIER_WITH_SLICES.fullmatch(name)
        if regex_match is None:
            _logger.warning(
                "Instruction %s: Ignoring uncompatible field name '%s'", instr_id, name
            )
            continue

        name = regex_match.group("id")

        prevs = result.get(name, [])
        if isinstance(prevs, str):
            _logger.debug(
                "Instruction %s: adding implicit slice to field %s", instr_id, name
            )
            prevs = [(("0", "0"), prevs)]

        raw_slices = regex_match.group("slices")
        if ":" in raw_slices:
            hi, lo = raw_slices.split(":")
            slices = (hi, lo)
        else:
            slices = (raw_slices, raw_slices)

        _logger.debug(
            "Adding for field '%s' slices %s for position %s", name, slices, pos
        )

        prevs.append((slices, pos))
        result[name] = prevs

    for name, pos in result.items():
        if isinstance(pos, str):
            continue

        pos.sort(reverse=True)

        result[name] = ", ".join((pos for (_slices, pos) in pos))

        _logger.debug("Constructed for attr '%s' positions: %s.", name, result[name])

    return [f"let {name} = instruction[{pos}];" for (name, pos) in result.items()]


def read_text_in_node(root, path, warn=True) -> str:
    """Reads all the text at that path"""
    nodes = root.findall(path)
    if len(nodes) == 0:
        if warn:
            _logger.warning("Could not find node at %s", path)
        return ""
    if len(nodes) >= 2 and warn:
        _logger.warning("Too many nodes matching path %s", path)

    return "".join(nodes[0].itertext()).strip()


def read_text_in_nodes(root, path) -> [str]:
    """Reads all the texts in the nodes matching that path."""
    return ["".join(n.itertext()) for n in root.findall(path)]


def read_execute(root) -> str:
    """Read 'Execute' section of the xml tree of an instruction."""
    return read_text_in_node(root, ".//ps_section/ps/pstext[@section='Execute']")


def make_body(from_bin: str, decode: str, post_decode: str, execute: str) -> str:
    """Build the body of an ASL function."""
    if len(post_decode) == 0:
        post_decode = ("// No post decode",)
    else:
        post_decode = (
            "// beginning of post decode section",
            post_decode,
            "// end of post decode section",
        )

    return "\n".join(
        (
            "// beginning of binary unpacking",
            *from_bin,
            "// end of binary unpacking",
            "",
            "// beginning of decode section",
            decode,
            "// end of decode section",
            "",
            *post_decode,
            "",
            "// beginning of execute section",
            execute,
            "// end of execute section",
        )
    )


def make_procedure(name: str, body: str, args: Iterable[str], indent=4) -> str:
    """Build an ASL function from a body as an ASL statement, a name, and its
    arguments as a textual list.

    The generated procedure does not return anything.

    The python function `make_procedure` returns the generated ASL function.
    """
    args_text = ", ".join(args)

    return "\n".join(
        (
            f"func {name} ({args_text})",
            "begin",
            textwrap.indent(body, " " * indent),
            "end;",
            "",
        )
    )


def read_one_encoding(
    instr_id: str,
    iclass: Element,
    post_decode: str,
    execute: str,
    add_encoding_id=True,
) -> str:
    """Construct an ASL function from one encoding class of an instruction."""
    from_bin = asl_for_instruction_fields(iclass.find("./regdiagram"), instr_id)
    decode = read_text_in_node(iclass, "./ps_section")
    body = make_body(from_bin, decode, post_decode, execute)

    encoding_id = "_" + iclass.get("id") if add_encoding_id else ""
    fname = "instr_" + instr_id + encoding_id

    return make_procedure(fname, body, ["instruction: bits(32)"])


def one_instruction_to_string(i_path: Path) -> str:
    """Process one instruction, write the decode and write it to output directory"""
    _logger.info("Processing %s", i_path)

    root = parse(i_path).getroot()

    if root.tag != "instructionsection":
        _logger.error("Cannot interpret file %s -- Skipping.", i_path)
        return ""

    root_type = root.get("type")
    if root_type == "pseudocode":
        _logger.info("Skipping shared pseudocode at %s", i_path)
        return ""
    if root_type == "alias":
        _logger.info("Skipping alias at %s.", i_path)
        return ""
    if root_type != "instruction":
        _logger.warning("Unknown xml type at %s -- Skipping", i_path)
        return ""

    execute = read_execute(root)
    post_decode = read_text_in_node(
        root, ".//ps_section/ps/pstext[@section='Postdecode']", warn=False
    )

    instr_id = i_path.stem
    instruction_name = root.get(
        "title", default="Instruction without a name (could not parse it from the xml)"
    )

    iclasses = root.findall("./classes/iclass")
    add_encoding_id = len(iclasses) > 1

    return "\n".join(
        (
            SEPARATOR_LINE,
            f"// {instruction_name}",
            SEPARATOR_LINE,
            "",
            *(
                read_one_encoding(
                    instr_id, iclass, post_decode, execute, add_encoding_id
                )
                for iclass in iclasses
            ),
            "",
            "",
        )
    )


def make_funs(args: argparse.Namespace):
    """Build opn files with the arguments passed in args."""
    mkdirp(args.o_dir)

    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        future_strings = (
            executor.submit(one_instruction_to_string, i_file)
            for i_file in args.i_files
        )

        o_file = args.o_dir / "instructions.asl"
        with open(o_file, "w", encoding="utf8") as f:
            f.write(MESSAGE_ON_TOP)

            for future_string in as_completed(future_strings):
                f.write(future_string.result())


def process_one_instruction_to_a_file(i_file: Path, o_dir: Path):
    """Process one file and write it to output directory."""
    _logger.info("Processing %s", i_file)

    root = parse(i_file).getroot()

    if root.tag != "instructionsection":
        _logger.error("Cannot interpret file %s -- Skipping.", i_file)
        return

    if root.get("type") == "alias":
        _logger.info("Skipping alias at %s.", i_file)
        return

    o_path = o_path_of_tree(root, o_dir)
    _logger.info("Writing to %s", o_path)

    header = header_of_tree(root, o_path)
    with o_path.open("w") as f:
        f.write(header)

        for ps in root.findall("./ps_section/ps"):
            sect_type = ps.get("secttype")
            if sect_type not in ("Library", "Operation"):
                continue

            _logger.debug("Writing section %s", ps.get("name"))
            f.writelines(ps.find("pstext").itertext())
            f.write("\n\n")

    _logger.debug("Processed %s", i_file)


def make_opns(args: argparse.Namespace):
    """Parse the xml files and write an ASL file with the instructions described in it."""
    mkdirp(args.o_dir)

    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        executor.map(
            process_one_instruction_to_a_file,
            args.i_files,
            itertools.repeat(args.o_dir, len(args.i_files)),
        )


def check_attr(elt: Element, attr: str, expected_str: Union[str, Iterable[str]]):
    """Checks that the attribute of elt matches its expected value."""
    actual_str = elt.get(attr)

    if isinstance(expected_str, str):
        if actual_str != expected_str:
            _logger.warning(
                "Unexpected value for attribute %s of element %s: %s expected and got %s.",
                attr,
                elt.tag,
                expected_str,
                actual_str,
            )
    else:
        if all(actual_str != s for s in expected_str):
            _logger.warning(
                "Unexpected value for attribute %s of element %s: one of %s expected and got %s.",
                attr,
                elt.tag,
                expected_str,
                actual_str,
            )


def check_child_text(elt: Element, child_match: str, expected_str: str):
    """Checks if all the children of elt that match child_match have ."""
    children = elt.findall(child_match)

    if len(children) == 0:
        _logger.warning(
            "No children found for element %s and match %s", elt.tag, child_match
        )

    elif any((child_text := child.text) != expected_str for child in children):
        _logger.warning(
            "Child of element %s does not match expected value %s: found %s.",
            elt.tag,
            expected_str,
            child_text,
        )


def check_attr_boolean(elt: Element, child_match: str):
    """Checks if the child is as expected a boolean."""
    check_attr(elt, child_match, ["True", "False"])


def get_boolean_attr(elt: Element, match: str, default=None) -> bool:
    """Gets the boolean value in an attribute"""
    attr_value = elt.get(match)

    if attr_value == "True":
        return True

    if attr_value == "False":
        return False

    _logger.warning(
        "Boolean attribute %s for element %s does not conform to expectation: %s found.",
        match,
        elt.tag,
        attr_value,
    )
    return default


IMPLEMENTATION_DEFINED_STRING = "IMPLEMENTATION DEFINED"


def field_name_is_ignored(name: str) -> bool:
    return name == IMPLEMENTATION_DEFINED_STRING


def format_condition(cond: Optional[str]) -> Optional[str]:
    return f"  /* {cond} */" if cond is not None else None


def add_cond(cond1: Optional[str], cond2: Optional[str]) -> Optional[str]:
    if cond1 is None or cond2 is None or cond1 == cond2:
        return cond1 or cond2
    else:
        return cond1 + " or " + cond2


@dataclass(frozen=True)
class BuiltField:
    name: str
    range: str
    cond: Optional[str] = None
    bitfields: Optional[dict[str, "BuiltField"]] = None

    def to_asl_bitfield(self) -> str:
        bitfields = BuiltField.format_nested_bitfields(self.bitfields, self.cond)
        return f"{self.range} {self.name}{bitfields},"

    def add_cond(self, cond2) -> "BuiltField":
        return dataclasses.replace(self, cond=add_cond(self.cond, cond2))

    def with_name(self, name) -> "BuiltField":
        return dataclasses.replace(self, name=name)

    def add_range(self, range2) -> "BuiltField":
        if self.range.endswith("]") and range2.startswith("["):
            range_ = self.range[:-1] + ", " + range2[1:]
            return dataclasses.replace(self, range=range_)
        else:
            _logger.error("Cannot add ranges, dropping.")

    @staticmethod
    def format_nested_bitfields(
        bfs: Optional[dict[str, "BuiltField"]], cond: Optional[str]
    ) -> str:
        cond = format_condition(cond)
        if bfs is None or len(bfs) == 0:
            return cond or ""
        else:
            non_indented_text = "\n".join(bf.to_asl_bitfield() for bf in bfs.values())
            if cond is not None:
                non_indented_text = cond + "\n" + non_indented_text

            return " {\n" + textwrap.indent(non_indented_text, "  ") + "\n}"


def range_get_length(r: str) -> Optional[int]:
    if (m := RE_RANGE.match(r)) is not None:
        return int(m["n1"]) - int(m["n2"]) + 1
    elif RE_SINGLE.match(r) is not None:
        return 1
    else:
        return None


def range_list_get_length(rs: str) -> Optional[int]:
    ls = [range_get_length(r) for r in rs.split(",")]
    if any(l is None for l in ls):
        return None
    else:
        return sum(ls)


def range_same_length(r1: str, r2: str) -> bool:
    l1 = range_list_get_length(r1)
    l2 = range_list_get_length(r2)
    return l1 is not None and l2 is not None and l1 == l2


def built_field_unions(reg_name, bfs: list[BuiltField]) -> Optional[BuiltField]:
    name = bfs[0].name
    cond = bfs[0].cond
    bitfields = bfs[0].bitfields
    range = bfs[0].range.removesuffix("]")
    for bf in bfs[1:]:
        if bf.name != name:
            return None
        cond = add_cond(cond, bf.cond)
        bitfields = built_field_join_dicts(reg_name, bitfields, bf.bitfields)
        range += ", " + bf.range.removeprefix("[").removesuffix("]")

    return BuiltField(name, range=range + "]", cond=cond, bitfields=bitfields)


def built_field_inter(bf1: BuiltField, bf2: BuiltField) -> Optional[BuiltField]:
    if (name := bf1.name) == bf2.name and (range := bf1.range) == bf2.range:
        return BuiltField(
            name,
            range,
            cond=add_cond(bf1.cond, bf2.cond),
            bitfields={
                n: bf
                for (n, sbf1) in bf1.bitfields.items()
                if n in bf2.bitfields
                and (bf := built_field_inter(sbf1, bf2.bitfields[n])) is not None
            },
        )
    else:
        return None


def built_field_join(
    reg_name: str, bf1: BuiltField, bf2: BuiltField
) -> Optional[BuiltField]:
    assert (name := bf1.name) == bf2.name

    res = built_field_join_dicts(reg_name, {name: bf1}, {name: bf2})

    if name in res:
        return res[name]
    else:
        return None


def built_field_inters(bfs: list[BuiltField]) -> BuiltField:
    res = bfs[0]
    for bf in bfs[1:]:
        res = built_field_inter(res, bf)
    return res


def built_field_join_dicts(
    reg_name: str,
    bfs1: Optional[dict[str, BuiltField]],
    bfs2: Optional[dict[str, BuiltField]],
) -> Optional[dict[str, BuiltField]]:
    if bfs1 is None or bfs2 is None:
        return None

    res: dict[str, BuiltField] = dict()

    for n, f1 in bfs1.items():
        if n in bfs2 and (f2 := bfs2[n]).range == f1.range:
            res[n] = BuiltField(
                n,
                f1.range,
                cond=add_cond(f1.cond, f2.cond),
                bitfields=built_field_join_dicts(reg_name, f1.bitfields, f2.bitfields),
            )

    return res


@dataclass(frozen=True)
class BuiltFields:
    id: str
    fields: dict[str, BuiltField]
    cond: Optional[str] = None

    def to_asl_bitfield(self) -> str:
        return BuiltField.format_nested_bitfields(self.fields, self.cond)

    def to_asl_type(self, type_name, length) -> str:
        return f"type {type_name} of bits({length}){self.to_asl_bitfield()};"


def built_fields_inter(reg_name: str, bfs: list[BuiltFields]) -> BuiltFields:
    assert len(bfs) > 0

    res = bfs[0]
    for bf in bfs[1:]:
        res = BuiltFields(
            "",
            built_field_join_dicts(reg_name, res.fields, bf.fields),
            add_cond(res.cond, bf.cond),
        )

    return res


def build_field(reg_name: str, field: Element) -> list[BuiltField]:
    """Build field described by this xml element."""
    assert field.tag == "field"

    field_id = field.get("id")
    field_name_elt = field.find("field_name")

    if field_name_elt is None:
        _logger.debug("Unnamed field %s. Skipping it.", field_id)
        return []

    field_name = field_name_elt.text
    _logger.debug("processing field %s (%s).", field_name, field_id)

    if field_name_is_ignored(field_name):
        _logger.debug("Field %s(%s) is ignored. Skipping it.", field_name, field_id)
        return []

    check_attr(field, "is_access_restriction_possible", "False")
    check_attr_boolean(field, "is_constant_value")
    check_attr(field, "is_partial_field", "False")

    field_msb = field.find("field_msb").text
    field_lsb = field.find("field_lsb").text

    if get_boolean_attr(field, "has_partial_fieldset", default=False):
        subfields = field.find("partial_fieldset").findall("fields")
        if len(subfields) != 1:
            _logger.error(
                "Subfields for field %s have more than one fields instance. Skipping.",
                field_name,
            )
        bitfields = build_fields(reg_name, subfields[0]).fields
    else:
        bitfields = None

    cond = field.findtext("fields_condition", default=None)

    if (array_indexes := field.find("field_array_indexes")) is not None:
        # TODO check that indexes are coherent with msb/lsb?
        res = inline_array_indexes(
            array_indexes, field_name, bitfields=bitfields, cond=cond
        )
    else:
        if RE_FIELD_WITH_SLICES_OPT_V1.fullmatch(field_name) is None:
            _logger.warning(
                "Field '%s' has bad syntax, Skipping it. (build_field)", field_name
            )
            return []

        field_range = (
            f"[{field_msb}]" if field_msb == field_lsb else f"[{field_msb}:{field_lsb}]"
        )
        res = [BuiltField(field_name, field_range, bitfields=bitfields, cond=cond)]

    _logger.debug("processed field %s (%s).", field_name, field_id)
    return res


def create_range(start_pos: int, length: int) -> str:
    return (
        f"[{start_pos}]" if length == 1 else f"[{start_pos + length - 1}:{start_pos}]"
    )


def process_part_specifier(
    range_part_specifier_text: str,
) -> Optional[Callable[[int], int]]:
    m = RE_RANGE_PART_SPECIFIER.fullmatch(range_part_specifier_text)

    if m is None:
        return None

    factor = int(f) if (f := m["factor"]) is not None else 1
    constant = int(c) if (c := m["constant"]) is not None else 0

    if m["var2"] is not None:
        return lambda x: factor * x + constant

    shift = int(s) if (s := m["shift"]) is not None else 0
    return lambda x: factor * (x + shift) + constant


def range_specifier_to_asl(
    range_specifier_text: str,
) -> Optional[Callable[[int], str]]:
    li = range_specifier_text.split(":")

    if len(li) == 2:
        f1 = process_part_specifier(li[0])
        f2 = process_part_specifier(li[1])

        if f1 is None or f2 is None:
            return None

        return lambda x: f"[{f1(x)}:{f2(x)}]"

    if len(li) == 1:
        f = process_part_specifier(li[0])
        if f is None:
            return None
        return lambda x: f"[{str(f(x))}]"

    return None


def inline_array_indexes(
    array_indexes: Element,
    field_name: str,
    bitfields: Optional[dict[str,]] = None,
    cond: Optional[str] = None,
) -> list[BuiltField]:
    """Build a list of fields from an array index as template."""
    assert array_indexes.tag == "field_array_indexes"
    _logger.debug("Inlining array indexes for field %s", field_name)

    var_name = array_indexes.get("index_variable")
    to_replace = f"<{var_name}>"

    if (
        RE_FIELD_WITH_SLICES_OPT_V1.fullmatch(field_name.replace(to_replace, ""))
        is None
    ):
        _logger.debug("Field '%s' is expecting variable %s", field_name, var_name)
        _logger.warning(
            "Field '%s' has bad syntax, Skipping it. (inline_array_indexes)", field_name
        )
        return []

    ranges = (
        (
            int(ai.findtext("field_array_start")),
            int(ai.findtext("field_array_end")),
        )
        for ai in array_indexes.findall("field_array_index")
    )

    range_specifier = array_indexes.get("range_specifier")
    assert range_specifier is not None
    create_range_from_specifier = range_specifier_to_asl(range_specifier)
    assert create_range_from_specifier is not None

    return [
        BuiltField(
            field_name.replace(to_replace, str(i)),
            create_range_from_specifier(i),
            bitfields=bitfields,
            cond=cond,
        )
        for (array_start, array_end) in ranges
        for i in range(array_start, array_end - 1, -1)
    ]


def get_fields_condition(fields: Element) -> Optional[str]:
    """Format the fields condition as an ASL comment."""
    assert fields.tag == "fields"

    cond1 = fields.findtext("fields_condition", default=None)
    cond2 = fields.findtext("fields_instance", default=None)

    if cond1 is None or cond2 is None or cond1 == "" or cond2 == "":
        return cond1 or cond2

    return cond1 + " i.e. " + cond2


def split_name_slice(name_with_slice: str) -> Optional[Tuple[str, Optional[str]]]:
    m = RE_FIELD_WITH_SLICES_OPT_V1.fullmatch(name_with_slice)

    if m is None:
        return None

    return m["name"], m["slices"]


def merge_fields_with_slices(
    reg_name: str, bfs0: dict[str, Optional[BuiltField]]
) -> dict[str, BuiltField]:
    merged = defaultdict(lambda: defaultdict(list[BuiltField]))
    for name_with_slice, bf in bfs0.items():
        if bf is None:
            continue

        splitted = split_name_slice(name_with_slice)
        if splitted is None:
            _logger.info("Field '%s' has invalid syntax, skipping it.", name_with_slice)
            continue

        name, slice_ = splitted
        merged[name][slice_].append(bf.with_name(name))

    return {
        name: built_field_unions(
            reg_name, [built_field_inters(bfs) for bfs in bfs.values()]
        )
        for (name, bfs) in merged.items()
    }


def build_fields(reg_name, fields: Element) -> BuiltFields:
    """Process subfields for the element passed as argument."""
    assert fields.tag == "fields"

    fields_id = fields.get("id")
    _logger.debug("Processing fields for fieldset %s.", fields_id)

    built_fields: dict[str, Optional[BuiltField]] = dict()
    for field in fields.findall("field"):
        for bf in build_field(reg_name, field):
            n = bf.name
            if n in built_fields:
                old = built_fields[n]
                if old is not None:
                    built_fields[n] = built_field_join(reg_name, bf, old)
            else:
                built_fields[n] = bf

    built_fields = merge_fields_with_slices(reg_name, built_fields)

    cond = get_fields_condition(fields)

    _logger.debug("Processed fields for fieldset %s.", fields_id)

    return BuiltFields(fields_id, built_fields, cond)


def sanitise_to_asl_name(name: str) -> str:
    """Make a proper variable out of a register name."""
    return RE_VARIABLE_ARRAY.sub("", name).replace(" ", "_")


def build_accessor(accessor: Element) -> str:
    """Process the xml element for one accessor instruction."""
    # <access_mechanism accessor="MRS ID_ISAR1_EL1" type="SystemAccessor">
    assert accessor.tag == "access_mechanism"

    raw_accessor_name = accessor.get("accessor")
    if raw_accessor_name is None:
        _logger.error("Unknown accessor")
        return "// Unknown accessor"

    _logger.debug("Processing accessor %s.", raw_accessor_name)

    func_name = "instr_" + sanitise_to_asl_name(raw_accessor_name)

    if accessor.find("access_permission") is None:
        if raw_accessor_name.startswith("MSRimmediate"):
            level = logging.DEBUG
        else:
            level = logging.WARNING
        _logger.log(level, "No pseudocode section for accessor %s.", raw_accessor_name)

        res = f"// No pseudocode for accessor {raw_accessor_name}\n\n"

    else:
        func_body = read_text_in_node(accessor, "./access_permission/ps/pstext")

        res = make_procedure(func_name, func_body, ["t: integer {0..31}"])

    _logger.debug("Processed accessor %s.", raw_accessor_name)

    return res


def build_fieldset(reg_name, type_name: str, fieldset: Element) -> Iterable[str]:
    assert fieldset.tag == "reg_fieldsets"

    length = fieldset.find("fields").get("length")

    built_fields = [
        build_fields(reg_name, fields) for fields in fieldset.findall("fields")
    ]

    if len(built_fields) == 1:
        return [built_fields[0].to_asl_type(type_name, length)]

    # TODO check that all length are equal
    _logger.info(
        "Cannot provide a single static definition for type %s. Defaulting to an intersection of fields.",
        type_name,
    )

    inter_built_field = built_fields_inter(reg_name, built_fields)

    return [
        *(bf.to_asl_type(f"{type_name}_{bf.id}", length) for bf in built_fields),
        "// Conditional fields are not part of ASLv1, and thus we have to use the type without bitfields.\n",
        inter_built_field.to_asl_type(type_name, length),
    ]


ARRAY_ACCESSOR_TEMPLATE = """accessor {accessor_name}(i: integer) <=> v: {type_name}
begin
  getter
    case i of
{getter_case_body}
    end;
  end;
  
  setter
    case i of
{setter_case_body}
    end;
  end;
end;"""

ACCESSOR_TEMPLATE = """accessor {accessor_name}() <=> v: {type_name}
begin
  getter
    return {variable_name};
  end;

  setter
    {variable_name} = v;
  end;
end;"""


def generate_array_accessors(
    reg_name: str, to_replace, reg_min: int, reg_max: int, type_name: str
) -> list[str]:
    accessor_name = reg_name.replace(to_replace, "")

    variable_names = [
        "_" + reg_name.replace(to_replace, str(i)) for i in range(reg_min, reg_max + 1)
    ]

    getter_case_body = "\n".join(
        f"      when {i} => return {name};" for i, name in enumerate(variable_names)
    )
    setter_case_body = "\n".join(
        f"      when {i} => {name} = v;" for i, name in enumerate(variable_names)
    )
    variable_decls = [
        f"var {name}: {type_name};" for i, name in enumerate(variable_names)
    ]

    accessor = ARRAY_ACCESSOR_TEMPLATE.format(
        accessor_name=accessor_name,
        type_name=type_name,
        getter_case_body=getter_case_body,
        setter_case_body=setter_case_body,
    )

    return [*variable_decls, accessor]


def build_global_variable_declarations(
    reg_root: Element, reg_name: str, type_name: str
) -> Iterable[str]:
    """Builds global declarations corresponding to the register and its type."""
    assert reg_root.tag == "register"

    declarations = []
    type_name = type_name

    if (reg_variables := reg_root.find("reg_variables")) is not None:
        assert reg_root.find("reg_array") is not None

        reg_variables = reg_variables.findall("reg_variable")
        assert len(reg_variables) == 1
        reg_var = reg_variables[0]
        reg_max = int(reg_var.get("max"))
        reg_min = int(reg_var.get("min", "0"))
        reg_var_name = reg_var.get("variable")
        to_replace = f"<{reg_var_name}>"

        declarations.extend(
            generate_array_accessors(reg_name, to_replace, reg_min, reg_max, type_name)
        )

    else:
        if (reg_banking := reg_root.find("reg_banking")) is not None:
            bank_text = reg_banking.findtext("reg_bank/bank_text")
            expected_bank_text = f"This register is banked between {reg_name} and {reg_name}_S and {reg_name}_NS."
            assert bank_text == expected_bank_text
            accessor_names = [
                reg_name,
                reg_name + "_S",
                reg_name + "_NS",
            ]
        else:
            accessor_names = [reg_name]

        declarations.extend(
            (f"var _{variable_name}: {type_name};" for variable_name in accessor_names)
        )
        declarations.extend(
            (
                ACCESSOR_TEMPLATE.format(
                    accessor_name=accessor_name,
                    variable_name="_" + accessor_name,
                    type_name=type_name,
                )
                for accessor_name in accessor_names
            )
        )

    return declarations


def is_implementation_defined_register(reg_root: Element) -> bool:
    """Returns True if the register described by the Elements is implementation defined and thus should be ignored."""
    assert reg_root.tag == "register"
    reg_long_name = reg_root.find("reg_long_name").text
    return reg_long_name.startswith(IMPLEMENTATION_DEFINED_STRING)


def make_type_name_of_reg_name(reg_name: str) -> str:
    """Build the type name corresponding to this register."""
    sanitised_reg_name = sanitise_to_asl_name(reg_name)

    return sanitised_reg_name + "_Type"


class ExecState(Enum):
    EXTERNAL = "External"
    AARCH32 = "AArch32"
    AARCH64 = "AArch64"

    @staticmethod
    def of_execution_state(exec_state: str) -> "ExecState":
        if exec_state == "AArch32":
            return ExecState.AARCH32
        elif exec_state == "AArch64":
            return ExecState.AARCH64
        else:
            assert False


def process_one_reg(
    reg_root: Element,
    i_file: Path,
) -> str:
    """Process the xml tree for one register and write an ASL file
    corresponding to this register."""
    assert reg_root.tag == "register"
    res: list[str] = []

    reg_name = reg_root.find("reg_short_name").text
    _logger.debug("Processing register %s.", reg_name)

    title = (
        reg_root.find("reg_long_name").text
        + " ("
        + reg_root.find("reg_short_name").text
        + ")"
    )
    res.append(f"// {title:^74}\n{SEPARATOR_LINE}")

    res.append(f"// Source file: {i_file}")

    check_attr(reg_root, "is_stub_entry", "False")

    if is_implementation_defined_register(reg_root):
        res.append(
            "// Implementation defined register, no type declaration nor variable declaration"
        )

    elif reg_name in SEEN_REG_NAMES:
        _logger.error("Conflicting name for register %s. Skipping", reg_name)
        res.append(
            f"// This register is ignored not to create conflict on name {reg_name}"
        )

    else:
        SEEN_REG_NAMES.add(reg_name)

        if get_boolean_attr(reg_root, "is_register", default=False):
            type_name = make_type_name_of_reg_name(reg_name)

            res.extend(
                build_fieldset(reg_name, type_name, reg_root.find("reg_fieldsets"))
            )
            # TODO Check that field_sets match fields declarations

            res.extend(
                build_global_variable_declarations(
                    reg_root, reg_name.replace(" ", "_"), type_name
                )
            )

        else:
            res.append(
                "// Not a register, no type declaration nor variable declaration"
            )

        build_accessors = False
        if build_accessors:
            res.extend(
                build_accessor(a)
                for a in reg_root.find("access_mechanisms").findall("access_mechanism")
            )
        else:
            res.append("// No accessors as those might be written in ASLv0.")

    # Pass 4 lines between register definitions
    res.append("")
    res.append("")

    return "\n\n".join(res)


def process_one_reg_file(i_file) -> str:
    """Parse the xml file and write an ASL file corresponding to this file with
    the sysreg described in it."""
    _logger.info("Processing %s", i_file)

    file_root = parse(i_file).getroot()

    if file_root.tag != "register_page":
        _logger.error("Cannot interpret file %s -- Skipping.", i_file)
        return ""

    res = "\n\n".join(
        process_one_reg(reg_root, i_file) for reg_root in file_root.find("registers")
    )

    _logger.debug("Processed %s", i_file)

    return res


def make_regs(args: argparse.Namespace):
    """Parse the xml files and write an ASL file with it."""
    if args.o_dir.is_dir():
        o_file = args.o_dir / "system-registers.asl"
    else:
        o_file = args.o_dir

    if args.jobs == 1:
        results = [process_one_reg_file(i_file) for i_file in args.i_files]

        with open(o_file, "w", encoding="utf8") as f:
            f.write(MESSAGE_ON_TOP)

            for res in results:
                f.write(res)

    else:
        with ThreadPoolExecutor(max_workers=args.jobs) as executor:
            future_strings = (
                executor.submit(process_one_reg_file, i_file) for i_file in args.i_files
            )

            with open(o_file, "w", encoding="utf8") as f:
                f.write(MESSAGE_ON_TOP)

                for future_string in as_completed(future_strings):
                    f.write(future_string.result())


def get_all_paths(paths: Iterable[Path]) -> list[Path]:
    """Expand all "*.xml" paths inside this list."""
    i_files = []  # type: list[Path]

    for f in paths:
        f = f.expanduser()
        if f.is_dir():
            _logger.info("Extending directory %s", f)
            i_files.extend(f.glob("*.xml"))
        elif f.exists():
            i_files.append(f)
        else:
            _logger.warning("Ignoring %s", f)

    return i_files


def mkdirp(o_dir: Path):
    """Creates directory o_dir if it does not exist."""
    if not o_dir.exists():
        _logger.info("Output dir does not exist. Creating it.")
        _logger.debug("mkdir %s", o_dir)
        o_dir.mkdir(exist_ok=True, parents=True)

    elif not o_dir.is_dir():
        _logger.error("Output option is not a directory. Might break later.")


def configure_logger(quiet, verbose, log_file):
    """Configure the logging infrastructure in function of 3 parameters."""
    if quiet:
        log_level = logging.CRITICAL
    else:
        log_level = {
            0: logging.ERROR,
            1: logging.WARNING,
            2: logging.INFO,
            3: logging.DEBUG,
        }.get(verbose, logging.DEBUG)

    if log_file is None:
        logging.basicConfig(level=log_level)
    else:
        logging.basicConfig(filename=log_file, filemode="w", level=log_level)


def get_parser() -> argparse.ArgumentParser:
    """Build a command line parser."""
    parser = argparse.ArgumentParser(
        description="Process xml files as released by Arm into raw asl files."
    )

    parser.add_argument(
        "-o",
        "--output",
        action="store",
        type=Path,
        default=Path.cwd() / "asl-pseudocode",
        help="The directory where all pseudocode should be written to.",
    )
    logger_group = parser.add_mutually_exclusive_group()
    logger_group.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="Logger level. Can be repeated.",
    )
    logger_group.add_argument(
        "-q", "--quiet", action="store_true", help="Only report critical errors."
    )
    parser.add_argument(
        "--log-file",
        action="store",
        help="Where to write parsing logs. Default to stderr.",
    )
    parser.add_argument(
        "-j",
        "--jobs",
        action="store",
        type=int,
        help=(
            "Parallelization on parsing and writing jobs. Default to python's ThreadPoolExecutor "
            "default, which should be `min(32, cpu_count)`"
        ),
    )

    options = parser.add_mutually_exclusive_group(required=True)
    options.add_argument(
        "--make-opns",
        help=(
            "Write the shared pseudocode to the file shared_pseudocode.asl, "
            "and instructions in opn files."
        ),
        action="store_const",
        const=make_opns,
        dest="func",
    )
    options.add_argument(
        "--make-funs",
        help=(
            "Write the shared pseudocode to the file shared_pseudocode.asl, "
            "and instructions in an instruction.asl file."
        ),
        action="store_const",
        const=make_funs,
        dest="func",
    )
    options.add_argument(
        "--make-regs",
        help="Write the register definitions in a sysregs.asl file.",
        action="store_const",
        const=make_regs,
        dest="func",
    )

    parser.add_argument(
        "paths",
        metavar="PATH",
        type=Path,
        nargs="+",
        help="The different paths to parse. If this is a directory, this will (non-recursively) "
        "parse all files inside the directory that have the '.xml' extension.",
    )

    return parser


def main():
    """Main entry point."""
    args = get_parser().parse_args()
    configure_logger(args.quiet, args.verbose, args.log_file)
    args.i_files = get_all_paths(args.paths)
    o_dir = args.output.absolute()
    args.o_dir = o_dir

    jobs = args.jobs
    if jobs is None:
        _logger.info("Starting process with default number of parallel workers.")
    else:
        _logger.info("Starting process with %d workers.", jobs)

    args.func(args)


if __name__ == "__main__":
    _last_run_start = datetime.now()
    main()
