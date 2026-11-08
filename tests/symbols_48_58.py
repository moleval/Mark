# -*- coding: utf-8 -*-
"""MARK.lsp: каждый вызов mark:/mk:/mc:/c: имеет defun; нет запрещённых имён."""
from pathlib import Path
from collections import Counter
import re

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251")


def strip_lisp(s):
    out = []
    i = 0
    n = len(s)
    in_str = False
    in_com = False
    while i < n:
        c = s[i]
        if in_com:
            if c == "\n":
                in_com = False
                out.append(c)
            i += 1
            continue
        if in_str:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_str = False
            i += 1
            continue
        if c == ";":
            in_com = True
            i += 1
            continue
        if c == '"':
            in_str = True
            i += 1
            continue
        out.append(c)
        i += 1
    return "".join(out)


clean = strip_lisp(text)
defs = re.findall(r"\(defun\s+([^\s()]+)", clean)
defined = set(defs)
dups = [n for n, c in Counter(defs).items() if c > 1]
assert not dups, "повторные defun: " + ", ".join(sorted(dups))

# Прямые вызовы и ссылки на функции проекта.
patterns = [
    r"\(\s*((?:mark|mk|mc|c):[^\s()]+)",
    r"'((?:mark|mk|mc|c):[^\s()]+)",
    r"\(quote\s+((?:mark|mk|mc|c):[^\s()]+)",
    r"\(function\s+((?:mark|mk|mc|c):[^\s()]+)",
]
called = set()
for p in patterns:
    called |= set(re.findall(p, clean))

missing = sorted(n for n in called if n not in defined)
assert not missing, "вызовы без defun: " + ", ".join(missing)

# Запрещённые built-in/устаревшие имена.
BAD = ("fboundp", "consp", "stringp", "atoms-family",
       "vlax-vla-object-available-p", "vlax-safearray-get-ubound")
for name in BAD:
    assert not re.search(r"\(\s*" + re.escape(name) + r"\b", clean), \
        "запрещённый вызов: " + name

# Команды — ровно рабочий набор.
ALLOWED = {
    "c:МАРКАВСЕ", "c:MARKALL",
    "c:МАРКАВ", "c:МАРКАВГЕОМЕТРИЯ", "c:МАРКАВСТ", "c:МАРКАВРГ", "c:МАРКАВТАБЛ",
    "c:МАРКАЗАП", "c:MARKA", "c:МАРКАЗАПБЛОК", "c:MARKFILL",
    "c:МАРКАРОВКАЗАП", "c:MARKZ", "c:МАРКАЗАПРЯД", "c:MARKAR",
    "c:МАРКАЗАПТАБЛ", "c:MARKTABLE",
    "c:МАРКАВДИАГНОЗОПТОМ", "c:MARKVBULKDIAG",
}
commands = {n for n in defined if n.startswith("c:")}
assert commands == ALLOWED, "команды: %s" % sorted(commands ^ ALLOWED)

print("OK: %d defun, %d вызовов, все определены, запрещённых нет"
      % (len(defined), len(called)))
