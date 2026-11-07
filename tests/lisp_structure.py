# -*- coding: utf-8 -*-
"""Структурный разбор AutoLISP без AutoCAD.

Проверяет не только баланс, но и преждевременно закрытые формы, незакрытые
строки и минимальную/максимальную арность основных специальных форм.
Запуск без аргументов проверяет MARK.lsp; можно
передать свои файлы: python3 tests/lisp_structure.py MARK.lsp
"""
from pathlib import Path
import collections
import sys


def parse(text):
    forms = []
    stack = []
    i = 0
    line = 1
    size = len(text)
    while i < size:
        char = text[i]
        if char == "\n":
            line += 1
            i += 1
            continue
        if char.isspace():
            i += 1
            continue
        if char == ";":
            end = text.find("\n", i)
            i = size if end < 0 else end
            continue
        if char == "'":
            i += 1
            continue
        if char == "(":
            node = []
            if stack:
                stack[-1].append(node)
            else:
                forms.append(node)
            stack.append(node)
            i += 1
            continue
        if char == ")":
            if not stack:
                raise ValueError(f"extra ) at line {line}")
            stack.pop()
            i += 1
            continue
        if char == '"':
            end = i + 1
            while end < size:
                if text[end] == "\\":
                    end += 2
                    continue
                if text[end] == '"':
                    break
                if text[end] == "\n":
                    line += 1
                end += 1
            if end >= size:
                raise ValueError(f"unclosed string at line {line}")
            atom = text[i:end + 1]
            if stack:
                stack[-1].append(atom)
            else:
                forms.append(atom)
            i = end + 1
            continue
        end = i
        while end < size and not text[end].isspace() and text[end] not in "();'\"":
            end += 1
        atom = text[i:end]
        if stack:
            stack[-1].append(atom)
        else:
            forms.append(atom)
        i = end
    if stack:
        raise ValueError(f"{len(stack)} unclosed forms")
    return forms


def walk(forms, stats, errors):
    for form in forms:
        if not isinstance(form, list):
            continue
        if form and isinstance(form[0], str):
            op = form[0].lower()
            stats[op] += 1
            argc = len(form)
            if op == "defun" and argc < 4:
                errors.append(("defun", argc))
            # В отличие от Common Lisp, AutoLISP IF имеет только then/else.
            if op == "if" and not 2 <= argc <= 4:
                errors.append(("if", argc))
            # Тело foreach/repeat/while может содержать несколько форм.
            if op == "foreach" and argc < 3:
                errors.append(("foreach", argc))
            if op == "repeat" and argc < 3:
                errors.append(("repeat", argc))
            if op == "while" and argc < 2:
                errors.append(("while", argc))
        walk(form, stats, errors)


def check_file(path):
    raw = path.read_bytes()
    try:
        text = raw.decode("cp1251")
    except UnicodeDecodeError:
        text = raw.decode("utf-8")
    forms = parse(text)
    stats = collections.Counter()
    errors = []
    walk(forms, stats, errors)
    print(
        f"{path}: top-level={len(forms)}, defun={stats['defun']}, "
        f"if={stats['if']}, foreach={stats['foreach']}, "
        f"while={stats['while']}, repeat={stats['repeat']}; "
        + ("OK" if not errors else f"errors={errors[:12]}")
    )
    return not errors


def main():
    root = Path(__file__).resolve().parents[1]
    default = [root / "MARK.lsp"]
    paths = [Path(arg) for arg in sys.argv[1:]] or default
    ok = True
    for path in paths:
        try:
            ok = check_file(path) and ok
        except (OSError, UnicodeError, ValueError) as exc:
            print(f"{path}: {exc}")
            ok = False
    if not ok:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
