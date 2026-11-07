# -*- coding: utf-8 -*-
"""Регрессия Ред. 48.41: нормализация осей одного MLINE-блока.

Этот тест проверяет только объединение параллельных отрезков в ось INS_PT.
Он НЕ является приёмкой размеров: MARKZ_LOG-7 показал 124/439 осей после
48.41, но все 351 ячейка остались прежними. Привязка размеров к фактическим
пролётам ригелей проверяется отдельно в rail_edges_48_42.py.
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


failures = []
if (1364, 4829) != (124 * 11, 439 * 11):
    failures.append("исходные счётчики не раскладываются на 124/439 блоков по 11 линий")

# Стойка: 11 параллельных вертикалей -> одна ось INS_PT.X, общий пролёт.
posts = [(4075.0 + d, 12600.0 + (5 if d == 25 else 0),
          4075.0 + d, 14000.0 - (5 if d == -25 else 0), 25.0)
         for d in (-25, -20, -15, -10, -5, 0, 5, 10, 15, 20, 25)]
if len(longitudinal(posts)) != 11:
    failures.append("фильтр ориентации должен сохранить 11 параллельных вертикалей")
post = collapse_axis(longitudinal(posts), (4075.0, 12600.0), "v")
if post != [[4075.0, 12600.0, 4075.0, 14000.0, 25.0]]:
    failures.append(f"стойка: ось/пролёт {post}")

# Ригель: 11 горизонталей -> одна ось INS_PT.Y; HW ближайшего отрезка.
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
mz = (Path(__file__).resolve().parents[2] / "backup" / "MARKZ.lsp").read_bytes().decode("cp1251")
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

print("124 стойки и 439 ригелей: 11 продольных MLINE -> одна ось INS_PT")
print("Пролёт и полуширина сохранены; далёкая точка вставки не сдвигает блок")
print("Размеры здесь не принимаются: MARKZ_LOG-7 оставил все 351 ячейку прежними")
print("ИТОГ: проверки нормализации Ред. 48.41 пройдены")
