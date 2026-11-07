# -*- coding: utf-8 -*-
"""Регрессия правила литер подъярусов MarkZ Ред. 48.38.

AutoCAD здесь не запускается. Модель повторяет два прохода build-marks:
сначала по геометрическому ярусу собираются фактические высоты, затем
литера назначается только если внутри яруса есть разные высоты.
"""
from pathlib import Path

TOL = 0.5
LETTERS = "абвгдежиклмнпрстуфхцчшщъыьэюя"


def same(a, b):
    return abs(a - b) < TOL


def subrows(records):
    """Вернуть (subh, subl) для записей (tierh, height)."""
    tiers = sorted({tier for tier, _ in records})
    alternatives = {}
    full = set()
    for tier, height in records:
        if same(tier, height):
            full.add(tier)
        else:
            alternatives.setdefault(tier, [])
            if not any(same(height, old) for old in alternatives[tier]):
                alternatives[tier].append(height)
    for tier in alternatives:
        alternatives[tier].sort()

    out = []
    for tier, height in records:
        if same(tier, height):
            out.append((None, None))
            continue
        values = alternatives[tier]
        subh = next(i for i, value in enumerate(values, 1)
                    if same(value, height))
        # Одна фактическая высота во всём ярусе не требует отдельной
        # литеры. Полная высота или вторая подъячейка — требует.
        needed = tier in full or len(values) > 1
        out.append((subh, LETTERS[subh - 1] if needed else None))
    return out


def compose(prefix, letter, subl, number, subw=None, suffix=""):
    return (prefix + " " if prefix else "") + letter + \
        ("." + subl if subl else "") + str(number) + \
        ("." + str(subw) if subw else "") + suffix


def row_letter(mark):
    """Буквенная часть марки после префикса, как ar-parse2."""
    tail = mark.split()[-1]
    i = 0
    while i < len(tail) and tail[i].isalpha():
        i += 1
    if i + 1 < len(tail) and tail[i] == "." and tail[i + 1].isalpha():
        i += 2
    return tail[:i]


failures = []


def expect(name, condition, detail=""):
    if not condition:
        failures.append(f"{name}: {detail}")


# Фактический случай из присланного TEST 16: К=1350/810 (58 шт.),
# Л=1850/450 (28 шт.). Полных 1350 и 1850 заполнений нет: 58+28=86 —
# ровно общий счётчик подъячеек по высоте. Литеры не нужны.
current = [(1350, 810)] * 58 + [(1850, 450)] * 28
current_out = subrows(current)
expect("присланный журнал", all(subl is None for _, subl in current_out))
expect("присланный журнал", sum(subh is not None for subh, _ in current_out) == 86)
expect("К без .а", row_letter(compose("ВН-4", "К", current_out[0][1], 20)) == "К")
expect("Л без .а", row_letter(compose("ВН-4", "Л", current_out[-1][1], 21)) == "Л")

# Контроль 1: полное заполнение + одна подъячейка — литера обязательна.
mixed = subrows([(1350, 1350), (1350, 810)])
expect("полная + подъячейка", mixed == [(None, None), (1, "а")], repr(mixed))

# Контроль 2: две разные подъячейки без полного заполнения — .а/.б
# сохраняются и назначаются по возрастанию высоты.
two = subrows([(1350, 810), (1350, 450)])
expect("две подъячейки", two == [(2, "б"), (1, "а")], repr(two))

# Контроль 3: обычный ярус без подъячеек не меняется.
expect("обычный ярус", subrows([(450, 450), (450, 450)]) ==
       [(None, None), (None, None)])

# Цифра ригеля и подындекс ширины не зависят от отказа от избыточной .а.
plain = compose("ВН-4", "К", None, 20, 2, "стм")
with_sub = compose("ВН-4", "К", "а", 20, 2, "стм")
expect("цифра и ширина", plain == "ВН-4 К20.2стм", plain)
expect("цифра и ширина", with_sub == "ВН-4 К.а20.2стм", with_sub)
expect("цифра и ширина", plain.split()[-1].split(".")[-1] ==
       with_sub.split()[-1].split(".")[-1])

# Структурные якоря: правило находится в build-marks, а TEST 16 не
# скрывает геометрические подъячейки, у которых литера не понадобилась.
mz = (Path(__file__).resolve().parents[1] / "backup" / "MARKZ.lsp").read_bytes().decode("cp1251")
build = mz[mz.index("(defun mark:build-marks"):mz.index("(defun mark:num-scale")]
expect("исходник", "fullH" in build and "(> (length lst) 1)" in build)
test16 = mz[mz.index("(defun mark:test-16"):mz.index("(defun mark:ar-parse2")]
expect("TEST 16", "(mark:rec-get q 'subh)" in test16)

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("К/Л: одна фактическая высота в ярусе -> без .а")
print("Полная + подъячейка и две подъячейки -> .а/.б сохранены")
print("Цифра ригеля и подындекс ширины не изменены")
print("\nИТОГ: проверки пройдены")
