# -*- coding: utf-8 -*-
"""Регрессия Ред. 48.39: торцы ригеля не должны сдвигать грань ячейки.

AutoCAD в CI недоступен, поэтому ниже повторена только геометрическая часть
mark:fill-main-axis / mark:fill-frame-segs / mark:fill-inner-edge.  Координаты
семи случаев взяты из сверки MARKZV_MODEL.txt с MARKZ_LOG-3/4: у готового
заполнения и ригеля была одна ширина, а сетка давала другую.
"""
from pathlib import Path
from math import hypot
import re

SLOPE = 0.0033


def axis(seg):
    x0, y0, x1, y1 = seg[:4]
    dx, dy = abs(x1 - x0), abs(y1 - y0)
    length = hypot(dx, dy)
    if length < 1e-6:
        return None
    if dy <= SLOPE * length:
        return "h"
    if dx <= SLOPE * length:
        return "v"
    return "h" if dx >= dy else "v"


def longitudinal(segs):
    """Аналог отбора продольных отрезков у именованного блока каркаса."""
    main = max(segs, key=lambda s: hypot(s[2] - s[0], s[3] - s[1]))
    main_axis = axis(main)
    return [s for s in segs if axis(s) == main_axis]


def item(seg, owner):
    ax = axis(seg)
    x0, y0, x1, y1 = seg[:4]
    hw = seg[4] if len(seg) > 4 else 25.0
    if ax == "v":
        return ((x0 + x1) / 2.0, min(y0, y1), max(y0, y1), hw, owner)
    if ax == "h":
        return ((y0 + y1) / 2.0, min(x0, x1), max(x0, x1), hw, owner)
    return None


def inner_edge(items, sign):
    edges = [it[0] + sign * it[3] for it in items]
    return (max if sign > 0 else min)(edges)


def case_edges(post_axis, cap_axis, sign):
    # Стойка — вертикальный главный отрезок с горизонтальным торцом.
    post = [
        (post_axis, 0.0, post_axis, 1000.0, 25.0),
        (post_axis - 25.0, 0.0, post_axis + 25.0, 0.0, 25.0),
    ]
    # Ригель — горизонтальный главный отрезок с вертикальным торцом.
    rail = [
        (0.0, 0.0, 1000.0, 0.0, 25.0),
        (cap_axis, 0.0, cap_axis, 50.0, 25.0),
    ]
    # В старом кластере участвуют только вертикали; горизонтальные оси
    # относятся к другому набору и здесь не нужны.
    old_vertical = [item(post[0], 1), item(rail[1], 2)]
    new_vertical = [item(s, 1) for s in longitudinal(post) if axis(s) == "v"]
    new_vertical += [item(s, 2) for s in longitudinal(rail) if axis(s) == "v"]
    return inner_edge(old_vertical, sign), inner_edge(new_vertical, sign)


# (витраж, число ячеек, ось стойки, ось торцевой вертикали, сторона,
#  старая грань, правильная)
CASES = [
    ("ВВ-1", 3, 11025.0, 11050.0, 1, 11075.0, 11050.0),
    ("ВВ-2", 4, 4065.0, 4075.0, 1, 4100.0, 4090.0),
    ("ВН-4", 2, 4075.0, 4065.0, -1, 4040.0, 4050.0),
    ("ВН-5", 3, 8920.0, 8905.0, -1, 8880.0, 8895.0),
    ("ВН-7", 3, 51755.0, 51780.0, 1, 51805.0, 51780.0),
    ("ВН-3", 4, 11050.0, 11025.0, -1, 11000.0, 11025.0),
    ("ВН-1", 4, 51780.0, 51755.0, -1, 51730.0, 51755.0),
]

failures = []
if sum(count for _, count, *_ in CASES) != 23:
    failures.append("регрессия должна покрывать 23 ячейки")
for name, count, post, cap, sign, old_expected, new_expected in CASES:
    old, new = case_edges(post, cap, sign)
    if abs(old - old_expected) > 1e-6:
        failures.append(f"{name}: исходная модель {old}, ожидалось {old_expected}")
    if abs(new - new_expected) > 1e-6:
        failures.append(f"{name}: после фильтра {new}, ожидалось {new_expected}")

# Вертикальный ригель/импост не должен потерять свою главную вертикаль.
vertical_rail = [
    (100.0, 0.0, 100.0, 600.0, 25.0),
    (50.0, 0.0, 150.0, 0.0, 25.0),
]
kept = longitudinal(vertical_rail)
if len(kept) != 1 or axis(kept[0]) != "v":
    failures.append("вертикальный ригель: главная вертикаль отброшена")

# Структурные якоря реальной правки в CP1251-файле.
mz = (Path(__file__).resolve().parents[2] / "backup" / "MARKZ.lsp").read_bytes().decode("cp1251")
rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', mz)
if not rev or tuple(map(int, rev.groups())) < (48, 39):
    failures.append("редакция MARKZ старше 48.39")
for marker in ("(defun mark:fill-main-axis", "(defun mark:fill-frame-segs"):
    if marker not in mz:
        failures.append(f"нет {marker}")
owned = mz[mz.index("(defun mark:fill-owned-axes"):]
owned = owned[:owned.index("(defun mark:fill-seg-items")]
if "part (mark:fill-frame-segs" not in owned:
    failures.append("fill-owned-axes не применяет фильтр продольных отрезков")

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("7 групп / 23 ячейки: торцевые вертикали больше не сдвигают грани")
print("Вертикальный ригель/импост: главная ось сохранена")
print("ИТОГ: проверки Ред. 48.39 пройдены")
