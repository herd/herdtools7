#!/usr/bin/env python3
"""Prepare ASL Reference MathJax HTML source and generate the MathJax configuration."""

from __future__ import annotations

import json
import html
import re
import sys
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SOURCES = [
    ROOT / "ASLmacros.tex",
    ROOT / "variable_name_macros.tex",
    ROOT / "generated_macros.tex",
]

MATH_CHUNK_RES = [
    re.compile(r"\\\((.*?)\\\)", re.DOTALL),
    re.compile(r"\\\[(.*?)\\\]", re.DOTALL),
    re.compile(
        r"\\begin\{(?P<env>displaymath|math|equation\*?|align\*?|alignat\*?|eqnarray\*?|gather\*?|multline\*?)\}"
        r".*?"
        r"\\end\{(?P=env)\}",
        re.DOTALL,
    ),
]

TARGET_ONE_ARG_RE = re.compile(r"\\(?:mathhypertarget|texthypertarget)\{([^{}]+)\}")
TARGET_TWO_ARG_RE = re.compile(r"\\hypertarget\{([^{}]+)\}\{([^{}]*)\}")
EXISTING_ID_RE = re.compile(r"\bid=(['\"])(.*?)\1")
WAVY_HELPER_RE = re.compile(r"\\my@wavy\{([^{}]*)\}")
VERB_RE = re.compile(r"\\verb(.)(.*?)\1")
TEXTFUNC_RE = re.compile(r"\\textfunc\{([^{}]*)\}")
STARRED_INFERRULE_RE = re.compile(r"\\inferrule\*")
INFERRULE_OPTION_RE = re.compile(r"\\inferrule\[([^\]]*)\]")
EMPTY_INFERRULE_PREMISE_RE = re.compile(r"(\\inferrule(?:\[[^\]]*\])?)\{\}")
SIMPLE_RULE_LABEL_RE = re.compile(r"(?:[A-Za-z0-9.]+|\\_)+\Z")
ANCHOR_ID_RE = re.compile(r"<[^>]+\bid=(['\"])(.*?)\1")
SOURCE_HYPERLINK_RE = re.compile(r"\\hyperlink\{([^{}]+)\}")
HREF_FRAGMENT_SENTINEL = "__ASL_HASH__"
LET_RE = re.compile(r"\\let\\(?P<alias>[A-Za-z@]+)\\(?P<target>[A-Za-z@]+)")
UNSAFE_AUTO_TRANSLATION_RE = re.compile(
    r"\\(?:hyperlink|ifthenelse|mathchoice|mathaccent|color|Hy@raisedlink)\b"
)

MATHJAX_BUILTINS = {
    "Big",
    "Bigg",
    "Biggl",
    "Bigl",
    "Bigm",
    "Bigr",
    "Delta",
    "Gamma",
    "Lambda",
    "Large",
    "Leftrightarrow",
    "Longrightarrow",
    "Rightarrow",
    "Sigma",
    "Theta",
    "Vert",
    "alpha",
    "and",
    "approx",
    "array",
    "backslash",
    "begin",
    "beta",
    "big",
    "bigg",
    "bigl",
    "bigr",
    "cap",
    "c",
    "cdot",
    "colon",
    "color",
    "cup",
    "dfrac",
    "displaystyle",
    "dots",
    "ell",
    "ellipsis",
    "emph",
    "end",
    "enspace",
    "emptyset",
    "epsilon",
    "equiv",
    "exists",
    "forall",
    "frac",
    "gamma",
    "ge",
    "geq",
    "gt",
    "hbox",
    "hline",
    "href",
    "implies",
    "in",
    "infty",
    "iota",
    "l",
    "label",
    "lambda",
    "land",
    "large",
    "ldots",
    "le",
    "left",
    "leftrightarrow",
    "leq",
    "lnot",
    "Longleftarrow",
    "Longleftrightarrow",
    "longleftarrow",
    "longleftrightarrow",
    "longmapsto",
    "longrightarrow",
    "lor",
    "lt",
    "mapsto",
    "mathbb",
    "mathbin",
    "mathcal",
    "mathbf",
    "mathit",
    "mathop",
    "mathrm",
    "mathsf",
    "mathtt",
    "mbox",
    "medspace",
    "mid",
    "mod",
    "ne",
    "neg",
    "neq",
    "nonumber",
    "not",
    "operatorname",
    "or",
    "otimes",
    "overbrace",
    "overbracket",
    "overline",
    "overset",
    "phantom",
    "pi",
    "prime",
    "providecommand",
    "qquad",
    "quad",
    "r",
    "ref",
    "renewcommand",
    "rightarrow",
    "rightharpoonup",
    "right",
    "scriptstyle",
    "scriptscriptstyle",
    "setminus",
    "sigma",
    "sim",
    "small",
    "square",
    "stackrel",
    "subset",
    "subseteq",
    "supset",
    "supseteq",
    "tag",
    "tau",
    "text",
    "textbackslash",
    "textbf",
    "textit",
    "textrm",
    "textsc",
    "textsf",
    "textstyle",
    "texttt",
    "theta",
    "thickspace",
    "thinspace",
    "to",
    "triangleq",
    "times",
    "underbrace",
    "underbracket",
    "underline",
    "underset",
    "uplus",
    "varepsilon",
    "vert",
    "vdots",
    "vphantom",
    "wedge",
    "xrightarrow",
    "xleftarrow",
}


def mj_string(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


def macro_value(value: str | list[object]) -> str:
    if isinstance(value, str):
        return mj_string(value)
    return json.dumps(value, ensure_ascii=False)


def collect_anchor_map(paths: list[Path]) -> dict[str, str]:
    split_pages = [path for path in paths if re.fullmatch(r"ASL\d+\.html", path.name)]
    if not split_pages:
        return {}

    anchors: dict[str, str] = {}
    for path in split_pages:
        if not path.exists():
            continue
        for match in ANCHOR_ID_RE.finditer(path.read_text()):
            anchors.setdefault(html.unescape(match.group(2)), path.name)
    return anchors


def href_target(target: str, anchor_map: dict[str, str]) -> str:
    # MathJax macro bodies use # for arguments. Keep URL fragments out of the
    # macro body and repair the sentinel after MathJax has created real anchors.
    if target.startswith(("http://", "https://", "mailto:", "#")):
        return target.replace("#", HREF_FRAGMENT_SENTINEL)
    if target in anchor_map:
        return f"{anchor_map[target]}{HREF_FRAGMENT_SENTINEL}{target}"
    return f"{HREF_FRAGMENT_SENTINEL}{target}"


def linked_macro_value(
    value: str | list[object],
    target: str,
    anchor_map: dict[str, str],
) -> str | list[object]:
    href = href_target(target, anchor_map)
    if isinstance(value, str):
        return rf"\href{{{href}}}{{{value}}}"

    linked = list(value)
    linked[0] = rf"\href{{{href}}}{{{linked[0]}}}"
    return linked


def mathjax_link_repair_script() -> str:
    return r"""
function repairAslHref(href) {
  return href
    .replace(/__ASL_HASH__/g, '#')
    .replace(/\.html\/+#/g, '.html#')
    .replace(/\.html##/g, '.html#');
}

function repairAslMathJaxLink(link) {
  const href = link.getAttribute('href');
  if (!href) {
    return;
  }
  const repaired = repairAslHref(href);
  if (repaired !== href) {
    link.setAttribute('href', repaired);
  }
}

function repairAslMathJaxLinks() {
  for (const link of document.querySelectorAll('a[href]')) {
    repairAslMathJaxLink(link);
  }
}

function finishAslMathJaxLoading() {
  for (const placeholder of document.querySelectorAll('.asl-mathjax-pending')) {
    placeholder.classList.remove('asl-mathjax-pending');
    placeholder.removeAttribute('aria-busy');
  }
}

document.addEventListener('click', (event) => {
  const target = event.target && event.target.nodeType === 1
    ? event.target
    : event.target && event.target.parentElement;
  const link = target && target.closest('a[href]');
  if (link) {
    repairAslMathJaxLink(link);
  }
}, true);

document.addEventListener('DOMContentLoaded', repairAslMathJaxLinks);
window.addEventListener('load', repairAslMathJaxLinks);
setTimeout(repairAslMathJaxLinks, 0);
setTimeout(repairAslMathJaxLinks, 100);
setTimeout(repairAslMathJaxLinks, 500);

new MutationObserver(repairAslMathJaxLinks).observe(document.documentElement, {
  childList: true,
  subtree: true,
  attributes: true,
  attributeFilter: ['href']
});

setTimeout(finishAslMathJaxLoading, 15000);
"""


def prepare_html_inputs(paths: list[Path]) -> None:
    for path in paths:
        if not path.exists():
            continue
        path.write_text(rewrite_mathjax_source(path.read_text()))


def already_wrapped_math_chunk(text: str, start: int) -> bool:
    prefix = text[max(0, start - 240) : start]
    return bool(
        re.search(
            r'<(?:span|div)\s+class="asl-mathjax-pending asl-mathjax-(?:inline|display)"'
            r'\s+aria-busy="true"\s+style="[^"]*">\s*\Z',
            prefix,
        )
    )


def mathjax_placeholder(chunk: str) -> str:
    display = chunk.startswith((r"\[", r"\begin"))
    text_length = len(chunk)
    line_count = chunk.count("\n") + 1

    if display:
        height = min(12.0, max(2.6, line_count * 0.9 + text_length / 420.0))
        return (
            '<div class="asl-mathjax-pending asl-mathjax-display" '
            f'aria-busy="true" style="--asl-mathjax-placeholder-height: {height:.1f}rem">'
            f"{chunk}</div>"
        )

    width = min(18.0, max(2.0, text_length / 7.0))
    return (
        '<span class="asl-mathjax-pending asl-mathjax-inline" '
        f'aria-busy="true" style="--asl-mathjax-placeholder-width: {width:.1f}em">'
        f"{chunk}</span>"
    )


def rewrite_mathjax_source(text: str) -> str:
    seen = {match.group(2) for match in EXISTING_ID_RE.finditer(text)}

    def rewrite_chunk(match: re.Match[str]) -> str:
        chunk = match.group(0)
        already_wrapped = already_wrapped_math_chunk(text, match.start())
        ids: list[str] = []

        def drop_one_arg(target: re.Match[str]) -> str:
            ids.append(target.group(1))
            return ""

        def drop_two_arg(target: re.Match[str]) -> str:
            ids.append(target.group(1))
            return target.group(2)

        chunk = TARGET_ONE_ARG_RE.sub(drop_one_arg, chunk)
        chunk = TARGET_TWO_ARG_RE.sub(drop_two_arg, chunk)
        chunk = WAVY_HELPER_RE.sub(r"\1", chunk)
        chunk = VERB_RE.sub(rewrite_verb, chunk)
        chunk = TEXTFUNC_RE.sub(rewrite_textfunc, chunk)
        chunk = STARRED_INFERRULE_RE.sub(r"\\inferrule", chunk)
        chunk = INFERRULE_OPTION_RE.sub(rewrite_inferrule_option, chunk)
        chunk = EMPTY_INFERRULE_PREMISE_RE.sub(r"\1{\\vphantom{x}}", chunk)

        anchors = []
        for target_id in ids:
            if target_id in seen:
                continue
            seen.add(target_id)
            escaped_id = html.escape(target_id, quote=True)
            anchors.append(f'<span id="{escaped_id}"></span>')

        if not already_wrapped:
            chunk = mathjax_placeholder(chunk)

        return "".join(anchors) + chunk

    for math_chunk_re in MATH_CHUNK_RES:
        text = math_chunk_re.sub(rewrite_chunk, text)
    return text


def rewrite_verb(match: re.Match[str]) -> str:
    escaped = (
        match.group(2)
        .replace("\\", r"\backslash ")
        .replace("{", r"\{")
        .replace("}", r"\}")
        .replace("_", r"\_")
    )
    return rf"\mathtt{{\text{{{escaped}}}}}"


def rewrite_textfunc(match: re.Match[str]) -> str:
    label = match.group(1).replace(r"\_", "_").replace("_", r"\_")
    return rf"\textfunc{{{label}}}"


def rewrite_inferrule_option(match: re.Match[str]) -> str:
    # MathJax cannot uppercase rule labels or interpret mathpartir's
    # right=... option, so normalize those before the macro expansion step.
    label = match.group(1).strip()
    if not label or label.startswith("right="):
        return r"\inferrule[]"

    if not SIMPLE_RULE_LABEL_RE.fullmatch(label):
        return match.group(0)

    display_label = label.replace(r"\_", "_").upper().replace("_", r"\_")
    return rf"\inferrule[{display_label}]"


def tex_name(text: str) -> str:
    return text.replace("_", r"\_")


def text_operator(name: str) -> str:
    return rf"\mathop{{\text{{{name}}}}}"


def balanced_braces(text: str) -> bool:
    depth = 0
    escaped = False
    for char in text:
        if escaped:
            escaped = False
            continue
        if char == "\\":
            escaped = True
            continue
        if char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth < 0:
                return False
    return depth == 0


def strip_outer_braces(text: str) -> str:
    text = text.strip()
    while text.startswith("{") and text.endswith("}") and balanced_braces(text[1:-1]):
        text = text[1:-1].strip()
    return text


def braced_arg(text: str, start: int) -> tuple[str, int] | None:
    if start >= len(text) or text[start] != "{":
        return None

    depth = 0
    escaped = False
    for index in range(start, len(text)):
        char = text[index]
        if escaped:
            escaped = False
            continue
        if char == "\\":
            escaped = True
            continue
        if char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return text[start + 1 : index], index + 1
    return None


def strip_full_hyperlink(text: str) -> str | None:
    if not text.startswith(r"\hyperlink"):
        return None

    target = braced_arg(text, len(r"\hyperlink"))
    if target is None:
        return None
    body = braced_arg(text, target[1])
    if body is None or body[1] != len(text):
        return None
    return body[0].strip()


def strip_hyperlink_commands(text: str) -> str:
    result: list[str] = []
    index = 0
    while index < len(text):
        if text.startswith(r"\hyperlink", index):
            target = braced_arg(text, index + len(r"\hyperlink"))
            if target is not None:
                body = braced_arg(text, target[1])
                if body is not None:
                    result.append(body[0])
                    index = body[1]
                    continue
        result.append(text[index])
        index += 1
    return "".join(result)


def collect_let_aliases() -> dict[str, str]:
    aliases: dict[str, str] = {}
    for source in SOURCES:
        if not source.exists():
            continue
        for line in source.read_text().splitlines():
            match = LET_RE.match(line.strip())
            if match:
                aliases[match.group("alias")] = rf"\{match.group('target')}"
    return aliases


def resolve_let_aliases(body: str, aliases: dict[str, str]) -> str:
    for alias, target in sorted(aliases.items(), key=lambda item: len(item[0]), reverse=True):
        body = re.sub(rf"\\{re.escape(alias)}(?![A-Za-z@])", lambda _match: target, body)
    return body


def normalize_text_wrappers(body: str) -> str:
    wrappers = {
        "textsc": r"\mathrm",
        "textsf": r"\mathsf",
        "textit": r"\mathit",
        "texttt": r"\mathtt",
        "textbf": r"\mathbf",
        "terminal": r"\mathtt",
        "nonterminal": r"\mathtt",
    }
    for source, target in wrappers.items():
        body = re.sub(
            rf"\\{source}\{{([^{{}}]+)\}}",
            lambda match, target=target: rf"{target}{{{match.group(1)}}}",
            body,
        )
    body = re.sub(
        r"\\textfunc\{([^{}]+)\}",
        lambda match: text_operator(match.group(1)),
        body,
    )
    return body.replace(r"\xlongrightarrow", r"\xrightarrow")


def normalize_argument_braces(body: str) -> str:
    body = re.sub(r"\{\{(#[0-9]+)\}\}", r"{\1}", body)
    body = re.sub(r"(?<=[(\[,])\{(#[0-9]+)\}", r"\1", body)
    body = re.sub(r"\{(#[0-9]+)\}(?=[)\],])", r"\1", body)
    body = re.sub(r"\{(#[0-9]+)\}(?=\\right)", r"\1", body)
    body = re.sub(r"\s+", " ", body)
    return body.strip()


def auto_math_body(body: str, aliases: dict[str, str]) -> str | None:
    body = strip_outer_braces(body)
    while True:
        linked_body = strip_full_hyperlink(body)
        if linked_body is None:
            break
        body = strip_outer_braces(linked_body)

    body = strip_hyperlink_commands(body)
    body = normalize_text_wrappers(resolve_let_aliases(body, aliases))
    body = normalize_argument_braces(body)
    if not body or UNSAFE_AUTO_TRANSLATION_RE.search(body) or not balanced_braces(body):
        return None
    return body


def auto_arg_body(
    body: str,
    arity: int,
    optional: str | None,
    aliases: dict[str, str],
) -> str | list[object] | None:
    body = auto_math_body(body, aliases)
    if body is None or re.search(r"#[0-9]+", body) is None:
        return None
    if optional is not None and "#1" in body:
        return None
    if optional is None:
        return [body, arity]
    return [body, arity, optional]


def generic_macro(name: str, arity: int, optional: str | None) -> str | list[object]:
    if arity == 0:
        return rf"\mathit{{{tex_name(name)}}}"

    # Most optional arguments in ASL math macros are layout selectors such as
    # [H] or [V]. Generic fallbacks should consume them so MathJax does not
    # print the layout selector as a semantic argument.
    first_mandatory_arg = 2 if optional is not None else 1
    args = ",".join(f"#{i}" for i in range(first_mandatory_arg, arity + 1))
    template = rf"\operatorname{{{tex_name(name)}}}\left({args}\right)"
    if optional is None:
        return [template, arity]
    return [template, arity, optional]


def fallback_macro(name: str) -> str:
    if name.isupper():
        return rf"\mathrm{{{tex_name(name)}}}"
    return rf"\mathit{{{tex_name(name)}}}"


def is_fallback_macro(name: str, value: str | list[object]) -> bool:
    return value == fallback_macro(name)


def is_generic_macro(name: str, value: str | list[object]) -> bool:
    if isinstance(value, str):
        return False
    return bool(value) and value[0] == generic_macro(name, int(value[1]), value[2] if len(value) > 2 else None)[0]


def is_suspicious_zero_arg_body(body: str) -> bool:
    body = body.strip()
    if not body:
        return False
    return bool(re.search(r"\\|[_^{}<>]", body))


def simple_no_arg_body(body: str, aliases: dict[str, str]) -> str | None:
    body = body.strip()
    link_match = re.fullmatch(r"\\hyperlink\{([^{}]*)\}\{(.+)\}", body)
    if link_match:
        body = link_match.group(2)
    body = body.strip()
    body = resolve_let_aliases(body, aliases)

    wrappers = {
        "textsc": r"\mathrm",
        "textsf": r"\mathsf",
        "textit": r"\mathit",
        "texttt": r"\mathtt",
        "mathrm": r"\mathrm",
        "mathsf": r"\mathsf",
        "mathit": r"\mathit",
        "mathtt": r"\mathtt",
        "mathbb": r"\mathbb",
        "mathcal": r"\mathcal",
        "terminal": r"\mathtt",
        "nonterminal": r"\mathtt",
    }
    for source, target in wrappers.items():
        match = re.fullmatch(rf"\\{source}\{{([^{{}}]+)\}}", body)
        if match:
            return rf"{target}{{{match.group(1)}}}"

    match = re.fullmatch(r"\\(?:mathrel|mathbin|mathop)\{(.+)\}", body)
    if match:
        return match.group(1)

    match = re.fullmatch(r"\\[A-Za-z]+", body)
    if match:
        return body

    match = re.fullmatch(r"\\textfunc\{([^{}]+)\}", body)
    if match:
        return text_operator(match.group(1))

    match = re.fullmatch(r"\\verbatimterminal\{[^{}]+\}\{(.+)\}", body)
    if match:
        return rf"\mathtt{{\text{{{match.group(1)}}}}}"

    match = re.fullmatch(r"\{?([A-Za-z0-9_#\-]+)\}?", body)
    if match:
        return rf"\mathrm{{{tex_name(match.group(1))}}}"

    return auto_math_body(body, aliases)


def simple_arg_body(
    body: str,
    arity: int,
    optional: str | None,
    aliases: dict[str, str],
) -> str | list[object] | None:
    body = body.strip()
    body = strip_hyperlink_commands(body)
    body = body.strip()

    match = re.fullmatch(r"\\(textsf|texttt|textit|textsc)\{([^{}]+)\}\(#1\)", body)
    if arity == 1 and match:
        wrapper = {
            "textsf": r"\mathsf",
            "texttt": r"\mathtt",
            "textit": r"\mathit",
            "textsc": r"\mathrm",
        }[match.group(1)]
        return [rf"{wrapper}{{{match.group(2)}}}\left(#1\right)", 1]

    match = re.fullmatch(r"\\([A-Za-z@]+)\\left\(#1\\right\)", body)
    if arity == 1 and match:
        return [rf"\{match.group(1)}\left(#1\right)", 1]

    return auto_arg_body(body, arity, optional, aliases)


COMMAND_RE = re.compile(
    r"\\(?:(?:re)?newcommand|providecommand|providedcommand)(?:\\(?P<name>[A-Za-z@]+)|\{\\(?P<braced_name>[A-Za-z@]+)\})"
    r"(?:\[(?P<arity>\d+)\])?"
    r"(?:\[(?P<optional>[^\]]*)\])?"
    r"(?:\{(?P<body>.*)\})?"
)


MacroSource = tuple[int, str, str | list[object], bool]


def collect_defined_macros() -> tuple[
    dict[str, str | list[object]],
    dict[str, str],
    dict[str, MacroSource],
]:
    macros: dict[str, str | list[object]] = {}
    macro_links: dict[str, str] = {}
    macro_sources: dict[str, MacroSource] = {}
    let_aliases = collect_let_aliases()
    for source in SOURCES:
        if not source.exists():
            continue
        for line in source.read_text().splitlines():
            match = COMMAND_RE.match(line.strip())
            if not match:
                continue

            name = match.group("name") or match.group("braced_name")
            arity = int(match.group("arity") or 0)
            optional = match.group("optional")
            body = match.group("body") or ""
            link_match = SOURCE_HYPERLINK_RE.search(body)
            if link_match:
                macro_links.setdefault(name, link_match.group(1))

            value = (
                simple_no_arg_body(body, let_aliases)
                if arity == 0
                else simple_arg_body(body, arity, optional, let_aliases)
            )
            if name in MATHJAX_BUILTINS:
                continue
            used_generic_fallback = value is None
            generated_value = value or generic_macro(name, arity, optional)
            macros.setdefault(name, generated_value)
            macro_sources.setdefault(name, (arity, body, generated_value, used_generic_fallback))
    return macros, macro_links, macro_sources


def collect_used_macros(paths: list[Path]) -> Counter[str]:
    used: Counter[str] = Counter()
    for path in paths:
        if not path.exists():
            continue
        text = path.read_text()
        for math_chunk_re in MATH_CHUNK_RES:
            for chunk in math_chunk_re.finditer(text):
                used.update(re.findall(r"\\([A-Za-z@]+)\b", chunk.group(0)))
    return used


def write_fallback_report(
    path: Path,
    macros: dict[str, str | list[object]],
    macro_sources: dict[str, MacroSource],
    used: Counter[str],
    auto_fallbacks: set[str],
) -> None:
    fallback_rows = [
        (used[name], name, macros[name])
        for name in auto_fallbacks
        if used[name] and is_fallback_macro(name, macros[name])
    ]
    generic_rows = [
        (used[name], name, macros[name])
        for name in macros
        if used[name] and name not in MANUAL_MACROS and is_generic_macro(name, macros[name])
    ]
    suspicious_zero_arg_rows = [
        (used[name], name, body, value)
        for name, (arity, body, value, used_generic_fallback) in macro_sources.items()
        if used[name]
        and name not in MANUAL_MACROS
        and arity == 0
        and used_generic_fallback
        and is_suspicious_zero_arg_body(body)
    ]

    def render_rows(title: str, rows: list[tuple[int, str, str | list[object]]]) -> list[str]:
        lines = [title, "=" * len(title)]
        for count, name, value in sorted(rows, reverse=True):
            lines.append(f"{count:5d}  {name}: {value!r}")
        if len(lines) == 2:
            lines.append("none")
        lines.append("")
        return lines

    def render_suspicious_zero_arg_rows(
        title: str,
        rows: list[tuple[int, str, str, str | list[object]]],
    ) -> list[str]:
        lines = [title, "=" * len(title)]
        for count, name, body, value in sorted(rows, reverse=True):
            lines.append(f"{count:5d}  {name}:")
            lines.append(f"       source:    {body!r}")
            lines.append(f"       generated: {value!r}")
        if len(lines) == 2:
            lines.append("none")
        lines.append("")
        return lines

    report_path = path.with_name("ASL-mathjax-fallbacks.txt")
    report_path.write_text(
        "\n".join(
            render_rows("Unknown Macro Fallbacks", fallback_rows)
            + render_rows("Generic Operator Fallbacks", generic_rows)
            + render_suspicious_zero_arg_rows(
                "Suspicious Zero-Argument Macro Translations",
                suspicious_zero_arg_rows,
            )
        )
    )


MANUAL_MACROS: dict[str, str | list[object]] = {
    # Keep these manual: target macros are moved into static HTML anchors before
    # MathJax config generation. Any remaining target/link macros in MathJax
    # source should not affect rendering.
    "mathhypertarget": [r"", 1],
    "texthypertarget": [r"", 1],
    "hypertarget": [r"#2", 2],
    "hyperlink": [r"#2", 2],

    # Keep these manual: they are HeVeA/document-structure helpers, not formula
    # content. MathJax should silently consume them if any survive preprocessing.
    "hva": r"",
    "frontmatter": r"",
    "mainmatter": r"",
    "backmatter": r"",

    # Keep these manual: MathJax has its own text/style macros, but configmacros
    # still needs definitions when these names appear in generated TeX fragments.
    "texttt": [r"\mathtt{#1}", 1],
    "textsf": [r"\mathsf{#1}", 1],
    "textsc": [r"\mathrm{#1}", 1],
    "textit": [r"\mathit{#1}", 1],
    "textbf": [r"\mathbf{#1}", 1],
    # Keep this manual: function names are literal text; using \operatorname
    # with escaped underscores makes MathJax place underscores too low in the
    # HTML output.
    "textfunc": [r"\mathop{\text{#1}}", 1],
    "terminal": [r"\mathtt{\text{#1}}", 1],
    "nonterminal": [r"\mathtt{\text{#1}}", 1],
    "verbatimterminal": [r"\mathtt{\text{#1}}", 2],

    # Keep these manual: the direct TeX definitions are legal but render worse in
    # MathJax. These versions preserve labels, stable placeholder width, and the
    # red wrapped-line marker used by the HTML stylesheet.
    "overtext": [r"\overbracket{#1}^{\scriptstyle #2}", 2],
    "overname": [r"\overbracket{#1}^{\scriptstyle #2}", 2],
    "undertext": [r"\underbracket{#1}_{\scriptstyle #2}", 2],
    "Ignore": r"\underline{\phantom{x}}",
    "wrappedline": r"\class{asl-wrappedline}{\hookrightarrow}",

    # Keep these manual: the source definitions either depend on helper macros
    # whose expansion is not useful to MathJax, or they need HTML-specific sizing
    # and spacing to match the inspected rendering.
    "aslrel": r"\mathbin{\Large\times}",
    "typearrow": r"\xrightarrow{\mathsf{type}}",
    "evalarrow": r"\xrightarrow{\mathsf{eval}}",
    "parsesep": r"\;",
    "terminateas": r"\;|\;",
    # Keep this manual: mathpartir uses \and to separate premises or adjacent
    # rules. MathJax does not provide that macro in this context, so render it as
    # visual spacing.
    "and": r"\qquad",
    "opone": r"\mathbin{\mathtt{op1}}",
    "optwo": r"\mathbin{\mathtt{op2}}",
    "PIPE": r"\mid",
    "bigtimes": r"\mathop{\Large\times}",
    "disjointunion": r"\mathbin{\uplus}",
    "parallelgraphs": [r"\mathop{\Large\|}#1", 1],
    # Keep this manual: the source macro has horizontal/vertical branches. The
    # HTML build intentionally consumes the layout selector and uses the compact
    # horizontal form instead of trying to interpret TeX conditionals.
    "ifthenelseop": [r"\mathbf{if}\;#2\;\mathbf{then}\;#3\;\mathbf{else}\;#4", 4, "V"],
    # Keep these manual: MathJax's default \emptyset is visually too close to a
    # slashed zero in these ASL constants; \varnothing matched the inspected HTML
    # better and mirrors the previous hand-tuned rendering.
    "emptyset": r"\varnothing",
    "emptyfunc": r"\varnothing_{\lambda}",
    "emptylist": r"[\;]",
    "emptytenv": r"\varnothing_{\mathbb{SE}}",
    "emptydenv": r"\varnothing_{\mathbb{DE}}",
    "emptyenv": r"\varnothing_{\mathbb{E}}",
    "empty": r"",
    "emptygraph": r"\varnothing_{\mathsf{g}}",

    # Keep these manual: the automatic translator can recover many of these
    # shapes, but the result is often a visually close expansion using helper
    # macros or \mathop{\text{...}}. These definitions keep stable operator
    # spelling, delimiter sizing, and optional-argument consumption.
    "dom": r"\operatorname{dom}",
    "graphof": [r"\operatorname{graph}\left(#1\right)", 1],
    "environof": [r"\operatorname{environ}\left(#1\right)", 1],
    "withgraph": [r"#2\left(\operatorname{graph}\mapsto #3\right)", 3, "H"],
    "withenviron": [r"#2\left(\operatorname{environ}\mapsto #3\right)", 3, "H"],
    "powfin": [r"\mathcal{P}_{\mathrm{fin}}\left(#1\right)", 1],
    "rightarrowfin": r"\rightarrow_{\mathrm{fin}}",
    "domop": [r"\operatorname{dom}\left(#1\right)", 1],
    "cardinality": [r"\left|#1\right|", 1],
    "rangeop": [r"\left[#2\ldots #3\right]", 3, "H"],
    "mapupdate": [r"#2\left[#3\mapsto #4\right]", 4, "H"],
    "mapapplyop": [r"#2\left(#3\right)", 3, "H"],
    "makestructured": [r"#2\left(#3\right)", 3, "H"],
    "listlen": [r"\left|#1\right|", 1],
    "concatlist": [r"\operatorname{concat\_list}\left(#1\right)", 1],
    "listcombine": [r"#2\mathbin{\otimes}#3", 3, "H"],
    "listcombinethree": [r"#2\mathbin{\otimes_3}#3\mathbin{\otimes_3}#4", 4, "H"],
    "listset": [r"\operatorname{list\_set}\left(#1\right)", 1],
    "unionlistop": [r"\operatorname{union\_list}\left(#1\right)", 1],
    "intersectionlistop": [r"\operatorname{intersection\_list}\left(#1\right)", 1],
    "indicesop": [r"\operatorname{indices}\left(#1\right)", 1],
    "bindings": [r"\operatorname{bindings}\left(#1\right)", 1],
    "bindingstomap": [r"\operatorname{bindings\_to\_map}\left(#1\right)", 1],
    "opnot": [r"\neg\left(#1\right)", 1],
    "opnotvar": [r"\neg #1", 1],
    "maybeemptylist": [r"\mathsf{list}^{*}\left(#1\right)", 1],
    "parsenode": [r"\mathbb{PARSE}\left[#1\right]", 1],
    "astlabelop": [r"\operatorname{ast\_label}\left(#1\right)", 1],
    "astversion": [r"{#1}_{\mathtt{ast}}", 1],
    "semanticsconfigurationterm": r"\text{semantics configuration}",
    "Semanticsconfigurationterm": r"\text{Semantics configuration}",
    "nvintop": [r"\mathtt{Int}\left(#1\right)", 1],
    "nvboolop": [r"\mathtt{Bool}\left(#1\right)", 1],
    "nvstringop": [r"\mathtt{String}\left(#1\right)", 1],
    "nvbitvectorop": [r"\mathtt{Bitvector}\left(#1\right)", 1],

    # Inference rules. The HTML preparation phase normalizes simple rule labels
    # to uppercase before MathJax sees them; render the optional label beside
    # the rule instead of silently dropping it.
    "inferrule": [
        r"\dfrac{\begin{array}{c}#2\end{array}}{#3}\quad{\scriptstyle\mathtt{#1}}",
        3,
        "",
    ],
}


def write_config(path: Path, html_inputs: list[Path]) -> None:
    anchor_map = collect_anchor_map(html_inputs)
    macros, macro_links, macro_sources = collect_defined_macros()
    macros.update(MANUAL_MACROS)
    for name, target in macro_links.items():
        if name in macros:
            macros[name] = linked_macro_value(macros[name], target, anchor_map)

    used_macros = collect_used_macros(html_inputs)
    auto_fallbacks = set()
    for name in sorted(set(used_macros) - set(macros) - MATHJAX_BUILTINS):
        macros[name] = fallback_macro(name)
        auto_fallbacks.add(name)

    rendered = ",\n".join(
        f"      {json.dumps(name)}: {macro_value(value)}"
        for name, value in sorted(macros.items())
    )

    path.parent.mkdir(parents=True, exist_ok=True)
    write_fallback_report(path, macros, macro_sources, used_macros, auto_fallbacks)
    path.write_text(
        """// AUTO-GENERATED by generate_mathjax_config.py. Do not edit by hand.
"""
        + mathjax_link_repair_script()
        + """
window.MathJax = {
  loader: {
    load: [
      '[tex]/configmacros',
      '[tex]/html',
      '[tex]/mathtools',
      '[tex]/extpfeil',
      '[tex]/textmacros',
      '[tex]/noerrors',
      '[tex]/noundefined'
    ]
  },
  tex: {
    packages: {'[+]': [
      'configmacros',
      'html',
      'mathtools',
      'extpfeil',
      'textmacros',
      'noerrors',
      'noundefined'
    ]},
    macros: {
"""
        + rendered
        + """
    }
  },
  options: {
    enableMenu: false
  },
  startup: {
    pageReady: () => MathJax.startup.defaultPageReady().then(() => {
      repairAslMathJaxLinks();
      finishAslMathJaxLoading();
    }).catch((error) => {
      finishAslMathJaxLoading();
      throw error;
    })
  }
};
"""
    )


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print("usage: generate_mathjax_config.py OUT.js [HTML ...]", file=sys.stderr)
        return 2
    html_inputs = [Path(arg) for arg in argv[2:]]
    prepare_html_inputs(html_inputs)
    write_config(Path(argv[1]), html_inputs)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
