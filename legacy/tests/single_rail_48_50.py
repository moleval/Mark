# -*- coding: utf-8 -*-
"""Ред. 48.50: одиночная ячейка у стойки 195 -> 200 по ригелю.

Модель mark:fill-single-rail-cells. Проверяются реальные координаты
ВН-4/ВН-5: заполнение 5800x195, ригель 5795..5995 (200), ось стойки 5795.
"""
scale = [200, 300, 325, 375, 450, 495, 550, 590, 600, 635, 675, 700,
         740, 980, 1000, 1100, 1250, 2245, 2250]


def nearest(w, tol=5.5):
    best = None
    bd = None
    for s in scale:
        d = abs(w - s)
        if d <= tol and (bd is None or d < bd):
            bd = d
            best = s
    return best


def single_rail(boxes, hs):
    out = []
    for bb in boxes:
        x0, y0, x1, y1, secw, tierh = bb
        bw = round(x1 - x0)
        L = None if bw in scale else nearest(bw)
        if L:
            r = None
            for e in hs:
                ey, xa, xb, hw = e
                if (abs((xb - xa) - L) <= 0.6
                        and xa <= x0 + 5.6 and xb >= x1 - 5.6
                        and (abs(ey - y0) <= 0.6 or abs(ey - y1) <= 0.6
                             or (hw > 0 and (abs((ey + hw) - y0) <= 0.6
                                             or abs((ey - hw) - y1) <= 0.6)))):
                    r = e
                    break
            if r:
                bb = (r[1], y0, r[2], y1, L, tierh)
        out.append(bb)
    return out


failures = []


def check(cond, msg):
    if not cond:
        failures.append(msg)


# Полоса ВН-5: 5800x195, балки 200 на 5795..5995.
strip = [(5800.0, 10200.0, 5995.0, 12550.0, 195.0, 1350.0)]
hs = [(10175.0, 5795.0, 5995.0, 25.0),
      (12575.0, 5795.0, 5995.0, 25.0)]
res = single_rail(strip, hs)
check(res == [(5795.0, 10200.0, 5995.0, 12550.0, 200.0, 1350.0)],
      f"полоса не расширилась: {res}")
check(round(res[0][2] - res[0][0]) == 200, "ширина полосы не 200")

# Т-половина 1100 уже в шкале — не трогаем.
thalf = [(3500.0, 4000.0, 4600.0, 4450.0, 1100.0, 1850.0)]
check(single_rail(thalf, hs) == thalf, "Т-половина 1100 была изменена")

# 195 без ригеля 200 (нет подходящего hs) — не трогаем.
check(single_rail([(5800.0, 10200.0, 5995.0, 12550.0, 195.0, 1350.0)],
                  [(10175.0, 5795.0, 5895.0, 25.0)]) ==
      [(5800.0, 10200.0, 5995.0, 12550.0, 195.0, 1350.0)],
      "195 расширилась без ригеля 200")

# 495 (точный ригель) — не трогаем.
check(single_rail([(5250.0, 10200.0, 5745.0, 12550.0, 495.0, 1350.0)], hs) ==
      [(5250.0, 10200.0, 5745.0, 12550.0, 495.0, 1350.0)],
      "точная 495 была изменена")

if failures:
    print("ОШИБКИ:")
    for f in failures:
        print("  -", f)
    raise SystemExit(1)
print("OK: полоса 195 -> 200 по ригелю; 1100/495 не тронуты")
