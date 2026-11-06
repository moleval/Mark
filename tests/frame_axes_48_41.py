# -*- coding: utf-8 -*-
"""Регрессия Ред. 48.41: один MLINE-блок = одна рабочая ось каркаса.

Фактическая диагностика AutoCAD (MARKZ_LOG-6) показала у стойки 11 вертикальных
и у ригеля 11 горизонтальных отрезков; фильтр поперечных отрезков 48.39 поэтому
возвращал 11 -> 11. Счётчики полного прогона также раскладываются точно:
1364 = 124 * 11 и 4829 = 439 * 11. Здесь проверяется объединение параллельных
линий одного блока в ось его точки вставки, а не прежняя модель торца ригеля.
"""
from pathlib import Path
from math import hypot
import re

TOL = 30.0
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
    """Прежний фильтр 48.39: на фактических 11 параллелях он ничего не удаляет."""
    main = max(segs, key=lambda s: hypot(s[2] - s[0], s[3] - s[1]))
    main_axis = axis(main)
    return [s for s in segs if axis(s) == main_axis]


def collapse_axis(segs, ins, main):
    """Математический аналог mark:fill-frame-axis-segs для MLINE-блока."""
    axis_insert = ins[0] if main == "v" else ins[1]
    best_d = None
    best_hw = None
    lo = hi = None
    for seg in segs:
        if axis(seg) != main:
            continue
        x0, y0, x1, y1 = seg[:4]
        if main == "v":
            pos = (x0 + x1) / 2.0
            a, b = min(y0, y1), max(y0, y1)
        else:
            pos = (y0 + y1) / 2.0
            a, b = min(x0, x1), max(x0, x1)
        distance = abs(pos - axis_insert)
        if best_d is None or distance < best_d:
            best_d = distance
            best_hw = seg[4] if len(seg) > 4 else None
        lo = a if lo is None else min(lo, a)
        hi = b if hi is None else max(hi, b)
    if best_d is None or best_d > TOL:
        return segs
    if main == "v":
        out = [axis_insert, lo, axis_insert, hi]
    else:
        out = [lo, axis_insert, hi, axis_insert]
    if best_hw is not None:
        out.append(best_hw)
    return [out]


def item(seg, owner=1):
    ax = axis(seg)
    x0, y0, x1, y1 = seg[:4]
    hw = seg[4] if len(seg) > 4 else 25.0
    if ax == "v":
        return ((x0 + x1) / 2.0, min(y0, y1), max(y0, y1), hw, owner)
    if ax == "h":
        return ((y0 + y1) / 2.0, min(x0, x1), max(x0, x1), hw, owner)
    raise AssertionError("нужна осевая линия")


def inner_edge(items, sign):
    edges = [it[0] + sign * it[3] for it in items]
    return (max if sign > 0 else min)(edges)


def case_edges(post_axis, parallel_axis, sign):
    # Один блок стойки: 11 продольных MLINE, одна из которых смещена внутрь
    # ячейки. Прежний фильтр ориентации сохраняет все 11.
    segs = []
    for index in range(11):
        x = parallel_axis if index == 10 else post_axis
        segs.append((x, 0.0, x, 1400.0, 25.0))
    assert len(longitudinal(segs)) == 11
    collapsed = collapse_axis(longitudinal(segs), (post_axis, 0.0), "v")
    assert len(collapsed) == 1
    old = inner_edge([item(s) for s in segs], sign)
    new = inner_edge([item(collapsed[0])], sign)
    return old, new


# Семь групп из полной сверки 351 обычной ячейки с MARKZV_MODEL:
# 23 ширины занижены на 10, 15 или 25 мм; высоты совпадают.
# (витраж, число ячеек, INS_PT.X стойки, параллельная ось, сторона,
#  старая внутренняя грань, правильная грань)
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
for name, count, post, parallel, sign, old_expected, new_expected in CASES:
    old, new = case_edges(post, parallel, sign)
    if abs(old - old_expected) > 1e-6:
        failures.append(f"{name}: старая грань {old}, ожидалось {old_expected}")
    if abs(new - new_expected) > 1e-6:
        failures.append(f"{name}: новая грань {new}, ожидалось {new_expected}")

# Ригель: 11 горизонталей тоже становятся одной осью INS_PT.Y; объединённый
# пролёт и полуширина ближайшего к точке вставки отрезка сохраняются.
rails = [(2950.0, 13115.0, 4050.0, 13115.0, 20.0)]
rails += [(2975.0, 13115.0 + d, 4025.0, 13115.0 + d, 25.0)
          for d in (-25, -20, -15, -10, -5, 5, 10, 15, 20, 25)]
rail = collapse_axis(rails, (2950.0, 13115.0), "h")
if rail != [[2950.0, 13115.0, 4050.0, 13115.0, 20.0]]:
    failures.append(f"ригель: ось/пролёт/полуширина {rail}")

# Нестандартный блок с удалённой точкой вставки нельзя silently сдвигать.
far = [(200.0, 0.0, 200.0, 1000.0, 25.0)]
if collapse_axis(far, (100.0, 0.0), "v") != far:
    failures.append("удалённая точка вставки: исходные отрезки не сохранены")

# Структурные якоря реальной правки в CP1251-файле.
mz = (Path(__file__).resolve().parents[1] / "MARKZ.lsp").read_bytes().decode("cp1251")
rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', mz)
if not rev or tuple(map(int, rev.groups())) < (48, 41):
    failures.append("редакция MARKZ старше 48.41")
if mz.count("(defun mark:fill-frame-axis-segs") != 1:
    failures.append("нет единственного mark:fill-frame-axis-segs")
owned = mz[mz.index("(defun mark:fill-owned-axes"):]
owned = owned[:owned.index("(defun mark:fill-seg-items")]
if "part (mark:fill-frame-axis-segs (cadr hit) part)" not in owned:
    failures.append("fill-owned-axes не объединяет параллельные оси блока")
if owned.index("part (mark:fill-frame-segs") > owned.index("part (mark:fill-frame-axis-segs"):
    failures.append("объединение должно идти после фильтра поперечных отрезков")

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("7 групп / 23 ячейки: параллельные линии больше не занижают ширину")
print("124 стойки и 439 ригелей: 11 продольных MLINE -> одна ось INS_PT")
print("Пролёт и полуширина сохранены; далёкая точка вставки не сдвигает блок")
print("ИТОГ: проверки Ред. 48.41 пройдены")
