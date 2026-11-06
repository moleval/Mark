# -*- coding: utf-8 -*-
"""Регрессия Ред. 48.42: наружная привязка границы ячейки к концу ригеля.

MARKZ_LOG-7 доказал, что нормализация 48.41 дала 124/439 осей, но не изменила
ни одну из 351 ячейки. Семь последующих фактических распечаток показали, что
пролёты ригелей равны правильным границам. Проверка на всей модели дала 29
кандидатов: 23 с ошибкой 10/15/25 мм и 6 ячеек с отличием 5 мм. LOG-8
показал, что порог 5 мм был слишком мягким: изолированные 195 мм расширялись
до 200. Ред. 48.43 оставляет обычной привязке правило (5.5; 30] мм; особый
случай равных Т-половин 1095/1100 проверяется отдельной регрессией 48.43.
"""
from pathlib import Path
import re

TOL = 30.0


def rail_item(x0, x1, owner=1):
    # Формат элемента горизонтальной оси: (Y X0 X1 HW OWNER).
    return (0.0, float(x0), float(x1), 25.0, owner)


def snap_edges(bot, top, xL, xR):
    """Математический аналог mark:fill-rail-cell-x."""
    left = None
    right = None
    for item in bot + top:
        a, b = float(item[1]), float(item[2])
        if a < xL:
            distance = xL - a
            if 5.5 < distance <= TOL and (left is None or a > left):
                left = a
        if b > xR:
            distance = b - xR
            if 5.5 < distance <= TOL and (right is None or b < right):
                right = b
    return (left if left is not None else xL,
            right if right is not None else xR)


# (витраж, количество ячеек, текущие X0/X1, эталонные X0/X1 ригеля)
CASES = [
    ("ВВ-1", 3, 11075.0, 12030.0, 11050.0, 12030.0),
    ("ВВ-2", 4, 4100.0, 5090.0, 4090.0, 5090.0),
    ("ВН-4", 2, 2950.0, 4040.0, 2950.0, 4050.0),
    ("ВН-5", 3, 8345.0, 8880.0, 8345.0, 8895.0),
    ("ВН-7", 3, 51805.0, 52380.0, 51780.0, 52380.0),
    ("ВН-3", 4, 10325.0, 11000.0, 10325.0, 11025.0),
    ("ВН-1", 4, 51380.0, 51730.0, 51380.0, 51755.0),
]

# Изолированные правильные ячейки: конец ригеля отличается ровно на 5 мм.
# Пара 1095/1100 сюда не входит — это отдельное правило равных Т-половин.
ISOLATED_FIVE_MM = [
    ("ВН-5", 3, 5800.0, 5995.0, 5795.0, 5995.0),
]

failures = []
changed = 0
if sum(count for _, count, *_ in CASES) != 23:
    failures.append("положительный набор должен содержать 23 ячейки")
for name, count, x0, x1, expected0, expected1 in CASES:
    for _ in range(count):
        got = snap_edges([rail_item(expected0, expected1, 1)],
                         [rail_item(expected0, expected1, 2)], x0, x1)
        if got != (expected0, expected1):
            failures.append(f"{name}: {x0}..{x1} -> {got}, ждали {expected0}..{expected1}")
        if got != (x0, x1):
            changed += 1
if changed != 23:
    failures.append(f"изменено {changed}, ожидалось 23")

unchanged = 0
for name, count, x0, x1, rail0, rail1 in ISOLATED_FIVE_MM:
    for _ in range(count):
        got = snap_edges([rail_item(rail0, rail1, 1)],
                         [rail_item(rail0, rail1, 2)], x0, x1)
        if got != (x0, x1):
            failures.append(f"{name}: штатные 5 мм не должны менять {x0}..{x1}: {got}")
        else:
            unchanged += 1
if unchanged != 3:
    failures.append(f"сохранено {unchanged}, ожидалось 3 изолированные ячейки с 5 мм")

# ВН-4 Б24.1: нижний длинный ригель 2950..5200 не должен протянуть ячейку
# через соседнюю секцию; ближний конец верхнего ригеля 4050 — правильный.
long_bottom = [rail_item(2950.0, 5200.0, 1)]
short_top = [rail_item(2950.0, 4050.0, 2)]
if snap_edges(long_bottom, short_top, 2950.0, 4040.0) != (2950.0, 4050.0):
    failures.append("длинный ригель ВН-4: ближний правый конец выбран неверно")
if snap_edges(long_bottom, [], 2950.0, 4040.0) != (2950.0, 4040.0):
    failures.append("дальний конец длинного ригеля не должен расширять ячейку")

# Внутренние концы не сужают ячейку; из двух наружных берётся ближайший.
if snap_edges([rail_item(2960.0, 4040.0)], [], 2950.0, 4040.0) != (2950.0, 4040.0):
    failures.append("внутренний конец не должен сужать ячейку")
nearest = snap_edges([rail_item(2940.0, 4040.0), rail_item(2925.0, 4040.0, 2)],
                     [], 2950.0, 4040.0)
if nearest != (2940.0, 4040.0):
    failures.append(f"должен выбираться ближайший наружный конец: {nearest}")

# Структурные якоря реальной правки в CP1251-файле.
mz = (Path(__file__).resolve().parents[1] / "MARKZ.lsp").read_bytes().decode("cp1251")
rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', mz)
if not rev or tuple(map(int, rev.groups())) < (48, 42):
    failures.append("редакция MARKZ старше 48.42")
if mz.count("(defun mark:fill-rail-cell-x") != 1:
    failures.append("нет единственного mark:fill-rail-cell-x")
rail_body = mz[mz.index("(defun mark:fill-rail-cell-x"):]
rail_body = rail_body[:rail_body.index("(defun mark:fill-t-rail-end?")]
if "(> d 5.5)" not in rail_body or "(> d 5.0)" in rail_body:
    failures.append("обычная привязка должна пропускать только отличие >5.5 мм")
body = mz[mz.index("(defun mark:fill-items->cells"):]
body = body[:body.index("(defun mark:fill-closed-cells")]
if "xedges (mark:fill-rail-cell-x bot top xL xR)" not in body:
    failures.append("fill-items->cells не привязывает границы к bot/top")
if "cL (car xedges)" not in body or "cR (cadr xedges)" not in body:
    failures.append("нет независимых cL/cR для каждого горизонтального ряда")
if "xL (car xedges)" in body or "xR (cadr xedges)" in body:
    failures.append("привязка мутирует общие xL/xR и влияет на следующие ряды")
if "boxes (cons (list cL yB cR yT) boxes)" not in body:
    failures.append("уточнённые копии не попадают в принятый BOX")
if "mark:fill-overlap-owners bot xL xR gap" not in body:
    failures.append("привязка не должна менять исходную проверку владельцев")
if 'Ячеек с привязкой границ к ригелям: ' not in body:
    failures.append("нет фактического счётчика привязанных ячеек")
if "(if (> n-rail 0)" in body:
    failures.append("счётчик должен печататься также при нуле привязок")

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("7 групп / 23 ячейки: наружные границы привязаны к фактическим ригелям")
print("3 изолированные ячейки 195 мм сохранены без расширения до 200")
print("Длинный ригель ВН-4 не расширяет ячейку через соседнюю секцию")
print("ИТОГ: проверки Ред. 48.42 пройдены")
