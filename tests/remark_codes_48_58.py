# -*- coding: utf-8 -*-
"""Коды замечаний: штатные сообщения не должны молча уезжать в GEN."""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251")
tz = (ROOT / "TZ.md").read_text(encoding="utf-8")


def body(name):
    i = text.index("(defun " + name + " ")
    depth = 0
    ins = False
    com = False
    j = i
    while j < len(text):
        c = text[j]
        if com:
            if c == "\n":
                com = False
        elif ins:
            if c == "\\":
                j += 2
                continue
            if c == '"':
                ins = False
        elif c == ";":
            com = True
        elif c == '"':
            ins = True
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return text[i:j + 1]
        j += 1
    raise AssertionError("не закрыт defun " + name)


code_src = body("mc:remark-code")
name_src = body("mc:remark-code-name")

# Извлекаем пары "подстрока" -> код.
pairs = re.findall(
    r'\(vl-string-search "((?:[^"\\]|\\.)*)" s\) "([A-Z0-9]+)"', code_src)
assert pairs, "в mc:remark-code нет пар текст->код"
mapping = {s.replace('\\"', '"').upper(): code for s, code in pairs}

cases = [
    ("[WARN] Вне ячеек каркаса: 3", "G01S"),
    ("[WARN] Вне сетки: 1", "G02"),
    ("[ERROR] Дубликат: РИГЕЛЬ", "DUP"),
    ("[ERROR] Перекрытие: ЗАПОЛНЕНИЕ", "OVL"),
    ("[WARN] Витраж: 2 блока \"Атрибуты витража\"", "V01"),
    ("[WARN] Марка не разобрана (X) — индекс", "M01"),
    ("[WARN] Атрибут \"Марка\" пуст", "M02"),
    ("[WARN] XLS не записан (файл занят)", "X01"),
    ("[WARN] MarkZ scale-probe недоступен", "SCL"),
    ("[WARN] Проём «Окно» не полностью заполняет ячейку", "OPN"),
    ("[WARN] Неизвестное сообщение", "GEN"),
]

for msg, expected in cases:
    s = msg.upper()
    got = None
    for needle, code in mapping.items():
        if needle in s:
            got = code
            break
    if got is None:
        got = "GEN"
    assert got == expected, f"{msg!r}: {got} != {expected}"

# Все коды из mc:remark-code-name должны быть в TZ и/или иметь явный вызов.
names = set(re.findall(r'\(= code "([A-Z0-9]+)"\)', name_src))
assert names, "нет кодов в mc:remark-code-name"
for code in sorted(names):
    assert code in tz, "код не описан в TZ.md: " + code

assert "(mc:remark (cond" in text, "нет явной передачи спецмарок"
for code in ("D01", "O01", "R01"):
    assert re.search(r'"' + code + r'"', text), \
        "нет явной передачи кода " + code

print("OK: коды замечаний, GEN только для неизвестных, TZ полон")
