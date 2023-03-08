"""Process ISA XML files into raw asl files.

Working assumptions:
    - for any xml file, the o_path as built by `o_path_of_tree` uniquely identifies the pseudocode in the xml file.
    - no other program concurrently modifies our output files
"""

import argparse
import itertools
from concurrent.futures import ThreadPoolExecutor
import logging
from xml.etree.ElementTree import Element, parse
from pathlib import Path
from typing import List, Optional
from datetime import datetime

# In this whole module, path names used as input (resp. output) are prefixed with `i_` (resp. `o_`).

_logger = logging.getLogger("bundler0")
_last_run_start = datetime.now()

MESSAGE_ON_TOP = """
// Copyright (c) 2010-2022 Arm Limited or its affiliates. All rights reserved.
// This document is Non-Confidential. This document may only be used and
// distributed in accordance with the terms of the agreement entered into by
// Arm and the party that Arm delivered this document to.

// More information can be found in notice.html or 
//    https://developer.arm.com/documentation/ddi0602/latest/Proprietary-Notice

// This document was automatically extracted from the XML files distributed at
//    https://developer.arm.com/downloads/-/exploration-tools
// by a script authored by Hadrien Renaud <hadrien.renaud.22@ucl.ac.uk> and
// available at bundler.py or
//    https://github.com/herd/herdtools7/blob/master/herd/libdir/asl-pseudocode/bundler.py


"""

DEFAULT_INSTR_DIR = Path("other-instrs")


def o_path_of_tree(root: Element, o_dir: Path) -> Path:
    """Constructs and checks the writing file corresponding to this tree."""
    root_type = root.get("type")
    if root_type == "instruction":
        ps_name = root.find("ps_section").find("ps").get("name")
        file_name = Path(ps_name + ".opn")

        if not ps_name.startswith("aarch"):
            _logger.debug(f"Moving file {ps_name} to {DEFAULT_INSTR_DIR}")
            file_name = DEFAULT_INSTR_DIR / file_name

        o_path = o_dir / file_name
        o_path.parent.mkdir(parents=True, exist_ok=True)

    else:
        if root_type != "pseudocode":
            _logger.warning(f"Unknown root type {root_type}")

        o_path = o_dir / (root.get("id").lower() + ".asl")

    if (
        o_path.exists()
        and datetime.fromtimestamp(o_path.stat().st_mtime) < _last_run_start
    ):
        # We consider that if it has been modified after `_last_run_start`, then it is this program that edited it, and
        # so we are overriding our own results. The underlying assumption is that 2 different xml files that have the
        # same o_path have the same pseudocode.
        _logger.warning(f"Overriding {o_path}")

    return o_path


def header_of_tree(root: Element, o_path: Path) -> str:
    """Find a nice title for the written file."""
    root_type = root.get("type")

    titles = [root.get("title")]
    post_header = []

    if root_type == "instruction":
        if o_path.exists():
            with o_path.open(mode="r") as f:
                for line in f.readlines():
                    if line.startswith("// =="):
                        break

                    elif line.startswith("// "):
                        titles.append(line[2:].strip())

            titles = sorted(frozenset(titles))

        post_header.append("// Execute")
        post_header.append("// =======")
        post_header.append("")

    return "\n".join(
        (
            *("// {:^74}".format(i) for i in titles),
            "// =============================================================================",
            "",
            MESSAGE_ON_TOP,
            *post_header,
            "",
        )
    )


def process_one_file(i_file: Path, o_dir: Path):
    """Process one file and write it to output directory."""
    _logger.info(f"Processing {i_file}")

    root = parse(i_file).getroot()

    if root.tag != "instructionsection":
        _logger.error(f"Cannot interpret file {i_file} -- Skipping.")
        return

    elif root.get("type") == "alias":
        _logger.info(f"Skipping alias at {i_file}.")
        return

    o_path = o_path_of_tree(root, o_dir)
    _logger.info(f"Writing to {o_path}")

    header = header_of_tree(root, o_path)
    with o_path.open("w") as f:
        f.write(header)

        for ps in root.findall("./ps_section/ps"):
            sect_type = ps.get("secttype")
            if sect_type != "Library" and sect_type != "Operation":
                continue

            _logger.debug(f"Writing section {ps.get('name')}")
            f.writelines(ps.find("pstext").itertext())
            f.write("\n\n")

    _logger.debug(f"Processed {i_file}")


def get_args() -> (List[Path], Path, Optional[int]):
    """Process arguments."""
    parser = argparse.ArgumentParser(
        description="Process ISA XML files into raw asl files."
    )

    parser.add_argument(
        "-o",
        "--output",
        action="store",
        type=Path,
        default=Path.cwd() / "asl-pseudocode",
        help="The directory where all pseudocode should be written to.",
    )
    parser.add_argument(
        "paths",
        metavar="PATH",
        type=Path,
        nargs="+",
        help="The different paths to parse. If this is a directory, this will (non-recursively) parse all files inside"
        " the directory that have the '.xml' extension.",
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
        help="Parallelization on parsing and writing jobs. Default to python's ThreadPoolExecutor default, which"
        " should be `min(32, cpu_count)`",
    )

    args = parser.parse_args()

    if args.quiet:
        log_level = logging.CRITICAL
    else:
        v = args.verbose
        if v == 0:
            log_level = logging.ERROR
        elif v == 1:
            log_level = logging.WARNING
        elif v == 2:
            log_level = logging.INFO
        else:
            log_level = logging.DEBUG

    if args.log_file is None:
        logging.basicConfig(level=log_level)
    else:
        logging.basicConfig(filename=args.log_file, filemode="w", level=log_level)

    i_files = []  # type: List[Path]
    for f in args.paths:  # type: Path
        f = f.expanduser()
        if f.is_dir():
            _logger.info(f"Extending directory {f}")
            i_files.extend(f.glob("*.xml"))
        elif f.exists():
            i_files.append(f)
        else:
            _logger.warning("Ignoring " + str(f))

    o_dir = args.output.absolute()  # type: Path
    if not o_dir.exists():
        _logger.info("Output dir does not exist. Creating it.")
        _logger.debug(f"mkdir {o_dir}")
        o_dir.mkdir(exist_ok=True, parents=True)

    elif not o_dir.is_dir():
        _logger.error(f"Output option is not a directory. Might break later.")

    jobs = args.jobs
    if jobs is None:
        _logger.info("Starting process with default number of parallel workers.")
    else:
        _logger.info(f"Starting process with {jobs} workers.")

    return i_files, o_dir, jobs


def main():
    """Main entry point."""
    (i_files, o_dir, jobs) = get_args()

    with ThreadPoolExecutor(max_workers=jobs) as executor:
        executor.map(process_one_file, i_files, itertools.repeat(o_dir, len(i_files)))


if __name__ == "__main__":
    _last_run_start = datetime.now()
    main()
