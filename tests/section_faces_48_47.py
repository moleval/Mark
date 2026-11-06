# -*- coding: utf-8 -*-
"""Ред. 48.47: граница секции в смешанном X-кластере.

В одном кластере по X могут оказаться стойки разных, разнесённых по Y
витражей. Старый sec-geo-one сначала брал экстремум всего кластера. После
привязки края ячейки к ригелю этот экстремум мог не пройти проверку края,
и секция перескакивала через собственную стойку.

Здесь повторена только геометрия выбора xl/xr. Формат вертикали тот же,
что у MarkZ: (axis, y0, y1, half_width, owner). Горизонтали и ярусы для
этой регрессии не нужны. Автономный тест не заменяет прогон AutoCAD.
"""
from pathlib import Path
import re

TOL = 30.0
GAP = 55.0


def v(axis, y0, y1, owner=1):
    return (float(axis), float(y0), float(y1), 25.0, owner)


def cluster(items):
    """Математический аналог mark:fill-cluster-items."""
    out = []
    current = []
    for item in sorted(items, key=lambda x: x[0]):
        if current and item[0] - current[0][0] > TOL:
            out.append(current)
            current = [item]
        else:
            current.insert(0, item)
    if current:
        out.append(current)
    return list(reversed(out))


def covers(spans, lo, hi):
    merged = []
    for a, b in sorted(spans):
        if merged and a <= merged[-1][1] + GAP:
            merged[-1][1] = max(b, merged[-1][1])
        else:
            merged.append([a, b])
    return any(a <= lo + GAP and b >= hi - GAP for a, b in merged)


def tier_col(items, y0, y1):
    return any(a <= y1 and b >= y0 and (a <= y0 - 30 or b >= y1 + 30)
               for a, b in ((it[1], it[2]) for it in items))


def section_width(bb, axes, per_item):
    """Старая (aggregate) и новая (per_item) граница секции."""
    x0, y0, x1, y1 = bb
    cols = cluster(axes)
    covering = [c for c in cols
                if covers([(it[1], it[2]) for it in c], y0, y1)]
    xl = xr = None

    def candidates(items, sign):
        if per_item:
            return [it[0] + sign * it[3] for it in items]
        if sign > 0:
            return [max(it[0] + it[3] for it in items)]
        return [min(it[0] - it[3] for it in items)]

    for c in covering:
        for face in candidates(c, 1):
            if face <= x0 + 1 and (xl is None or face > xl):
                xl = face
    for c in covering:
        for face in candidates(c, -1):
            if face >= x1 - 1 and (xr is None or face < xr):
                xr = face

    # Fallback 48.13 сохранён, включая выбор грани внутри кластера.
    if xl is None:
        for c in cols:
            if tier_col(c, y0, y1):
                for face in candidates(c, 1):
                    if face <= x0 + 1 and (xl is None or face > xl):
                        xl = face
    if xr is None:
        for c in cols:
            if tier_col(c, y0, y1):
                for face in candidates(c, -1):
                    if face >= x1 - 1 and (xr is None or face < xr):
                        xr = face
    if xl is None:
        xl = x0
    if xr is None:
        xr = x1
    return round(xr - xl, 1)


failures = []


def check(condition, message):
    if not condition:
        failures.append(message)


def case(name, bb, axes, old, new):
    got_old = section_width(bb, axes, per_item=False)
    got_new = section_width(bb, axes, per_item=True)
    check(got_old == old, f"{name}: старая модель {got_old}, ожидалась {old}")
    check(got_new == new, f"{name}: новая секция {got_new}, ожидалась {new}")


# ВН-3, ячейки 244–247. Стойка ВВ-1 X=11025 находится в другом
# ярусе/витраже, но попадает в один X-кластер с ВН-3 X=11050.
case(
    "ВН-3 X=10325",
    (10325, 200, 11025, 2550),
    [v(10300, 20, 4500), v(11050, 20, 4500),
     v(11025, 17475, 20420, 2), v(11775, 20, 4500)],
    1425, 700,
)

# ВН-7, ячейки 8–10: левая грань не должна перескакивать к 49480.
case(
    "ВН-7 X=51780",
    (51780, 10200, 52380, 12550),
    [v(49455, 10020, 14000), v(51780, 20, 4500),
     v(51755, 10020, 14000), v(52405, 10020, 14000)],
    2900, 600,
)

# ВВ-1, ячейки 237–239: своя левая стойка X=11025 даёт 980.
case(
    "ВВ-1 X=11050",
    (11050, 17525, 12030, 18275),
    [v(8920, 17475, 20420), v(11050, 20, 4500),
     v(11025, 17475, 20420), v(12055, 17475, 20420)],
    3085, 980,
)

# ВН-5, ячейки 255–257: правая грань 8895, а не следующая 11195.
case(
    "ВН-5 X=8345",
    (8345, 10200, 8895, 12550),
    [v(8320, 10020, 14000), v(8920, 10020, 14000),
     v(8905, 16520, 21010, 2), v(11220, 10020, 14000)],
    2850, 550,
)

# Контроль против чрезмерной локализации: принятая изолированная
# секция 195 сохраняется, поскольку её фактический край совпадает с
# гранью 5800 в том же кластере.
case(
    "изолированные 195",
    (5800, 10200, 5995, 12550),
    [v(5775, 20, 4500), v(5770, 10020, 14000),
     v(6020, 10020, 14000)],
    195, 195,
)

# Исходные грани равных Т-половин остаются 2245. В частности, нельзя
# просто отбросить «чужую» ось 5770: её грань 5745 участвует в принятой
# геометрии сетки и затем в 3 парах 2245 -> 2250.
t_axes = [v(3475, 20, 4500), v(5775, 20, 4500),
          v(5770, 10020, 14000), v(6525, 20, 4500)]
for n, bb in enumerate(((4650, 4000, 5745, 4450),
                        (3500, 4000, 4600, 4450)), 1):
    check(section_width(bb, t_axes, per_item=False) == 2245,
          f"Т-пара {n}: изменилась исходная секция в старой модели")
    check(section_width(bb, t_axes, per_item=True) == 2245,
          f"Т-пара {n}: новая грань потеряла 2245")

# После удаления четырёх фантомных З-секций общая шкала содержит
# 18 фактических секций + 1100 (ригель без заполнения) + резерв 2245.
beams = [200, 300, 325, 375, 450, 495, 550, 590, 600, 635, 675, 700,
         740, 980, 1000, 1100, 1250, 2250]
actual_sections = [195, 200, 300, 325, 375, 450, 495, 550, 590, 600,
                   635, 675, 700, 740, 980, 1000, 1250, 2250]
scale = sorted(set(beams + actual_sections + [2245]))
check(scale == [195, 200, 300, 325, 375, 450, 495, 550, 590, 600,
                635, 675, 700, 740, 980, 1000, 1100, 1250, 2245, 2250],
      f"новая общая шкала отличается: {scale}")
check(scale.index(700) + 1 == 13, "700 не сохранило номер 13")
check(scale.index(2245) + 1 == 19 and scale.index(2250) + 1 == 20,
      "резерв 2245 и 2250 получили неверные новые номера")
check(not {1425, 2850, 2900, 3085}.intersection(scale),
      "фантомная З-секция осталась в шкале")

# Структурные якоря реальной CP1251-правки.
mz = (Path(__file__).resolve().parents[1] / "MARKZ.lsp").read_bytes().decode("cp1251")
mz = mz.replace("\r\n", "\n")
rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', mz)
check(bool(rev) and tuple(map(int, rev.groups())) >= (48, 47),
      "редакция MARKZ старше 48.47")
start = mz.index("(defun mark:sec-geo-one")
end = mz.find("\n(defun ", start + 1)
body = mz[start:end if end >= 0 else len(mz)]
check(body.count("(foreach it c") == 4,
      "в sec-geo-one не четыре per-item цикла (основные и fallback)")
check("(mark:fill-inner-edge c" not in body,
      "sec-geo-one всё ещё берёт агрегированную грань кластера")
check("Ред. 48.47" in body, "нет пояснения причины per-item грани")
check("(defun mark:fill-equal-t-cells" in mz,
      "потерян механизм принятых Т-половин")

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("ВН-3: 1425 -> 700 (Н19 -> Н13); ВН-7: 2900 -> 600")
print("ВВ-1: 3085 -> 980; ВН-5: 2850 -> 550")
print("195 и исходные 2245 Т-половин сохранены")
print("Общая шкала после пересчёта: 20 номеров, 700=13, 2245=19, 2250=20")
print("ИТОГ: offline-проверки Ред. 48.47 пройдены; нужен чистый CAD-прогон")
