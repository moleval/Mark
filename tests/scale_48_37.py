# -*- coding: utf-8 -*-
"""Проверка логики шкалы номеров MarkZ Ред. 48.37 на синтетике.

Моделирует Lisp-функции:
    mark:scale-beam-keys -> mark:sec-ws -> mark:num-keys -> mark:num-scale
    mark:num-of-width  (резервный путь рядовки mark:ar-reindex)
    mark:num-match?    (сверка TEST 16)
    mark:num-shift-out (смена номеров против нумерации по столбцам 48.20)
"""

TOL = 0.5
MIN_SEG = 50


def same(a, b):
    return abs(a - b) < TOL


def sort_unique(vals):
    out = []
    for v in sorted(vals):
        if not any(same(v, x) for x in out):
            out.append(v)
    return out


def scale_beam_keys(scale1):
    return [k for k in scale1 if k > MIN_SEG]


def sec_ws(secws):
    return sort_unique([float(w) for w in secws if w > 0])


def num_keys(scale1, secws):
    rk = scale_beam_keys(scale1)
    out = list(rk)
    for k in sec_ws(secws):
        if not any(same(ln, k) for ln in rk):
            out.append(k)
    return sort_unique(out)


def index_of(lst, v):
    for i, x in enumerate(lst):
        if same(x, v):
            return i
    return None


def num_scale(scale1, secws):
    keys = num_keys(scale1, secws)
    rk = scale_beam_keys(scale1)
    beam_scale = [(k, i + 1) for i, k in enumerate(rk)]
    return beam_scale, len(rk), keys


def num_of_width(scale1, secws, w):
    i = index_of(num_keys(scale1, secws), w)
    return None if i is None else str(i + 1)


def num_match(scale1, secws, n, w):
    i = index_of(num_keys(scale1, secws), w)
    return i is not None and n == i + 1


def shift_rows(scale1, secws, was):
    keys = num_keys(scale1, secws)
    rows = []
    for i, w in enumerate(was, start=1):
        new = index_of(keys, w)
        if new is not None and new + 1 != i:
            rows.append(f"{w:.0f}: №{i} -> №{new + 1}")
    return rows


def no_rigel(scale1, secws, n):
    """Секция печатается без сверки: номер больше числа ригелей."""
    return n > len(scale_beam_keys(scale1))


failures = []


def expect(name, cond, msg=""):
    if not cond:
        failures.append(f"{name}: {msg}")
    return cond


# --- 1. В-7: 9 столбцов = 9 ригелей, совпадение 1:1 ---------------------
V7 = dict(scale1=[395, 550, 650, 750, 1000, 1150, 1430, 1460, 1700],
          secws=[395, 550, 650, 750, 1000, 1150, 1430, 1460, 1700])
bs, shift, keys = num_scale(V7["scale1"], V7["secws"])
expect("В-7", shift == 9, f"ригелей {shift}")
for i, w in enumerate(V7["secws"], start=1):
    expect("В-7", index_of(keys, w) + 1 == i, f"{w} -> №{index_of(keys, w) + 1}")
expect("В-7", all(num_match(V7["scale1"], V7["secws"], i + 1, w)
                  for i, w in enumerate(V7["secws"])), "сверка TEST 16")

# --- 2. Т-1: 1430 = №10 (эталон §8 SYNC_MARKZV) -------------------------
T1_scale = [200, 500, 700, 900, 1000, 1100, 1240, 1300, 1350, 1430,
            1500, 1600, 1700, 1800]
T1_secs = [200, 500, 900, 1000, 1100, 1240, 1300, 1350, 1430, 1500,
           1600, 1700, 1800]
bs, shift, keys = num_scale(T1_scale, T1_secs)
expect("Т-1", index_of(keys, 1430) + 1 == 10, f"1430 -> №{index_of(keys, 1430) + 1}")
expect("Т-1", shift == 14, f"ригелей {shift}")
expect("Т-1", num_of_width(T1_scale, T1_secs, 1430) == "10")
# 700 — ригель без столбца: номер занят, секции по нему нет
expect("Т-1", index_of(keys, 700) + 1 == 3)
expect("Т-1", not any(same(w, 700) for w in T1_secs))

# --- 3. ТБ-1: «колпачок» 450 занимает номер, столбцы сдвигаются ---------
TB1_scale = [450, 550, 750, 950, 1150, 1250, 1450]
TB1_secs = [550, 750, 950, 1150, 1450]
bs, shift, keys = num_scale(TB1_scale, TB1_secs)
expect("ТБ-1", shift == 7)
expect("ТБ-1", index_of(keys, 550) + 1 == 2, "550 должен быть №2")
rows = shift_rows(TB1_scale, TB1_secs, sort_unique(TB1_secs))
expect("ТБ-1", rows and rows[0] == "550: №1 -> №2", f"{rows}")

# --- 4. Столбец без ригеля — продолжение нумерации (Блок 2) ------------
X_scale = [600, 900]
X_secs = [600, 900, 1400]
bs, shift, keys = num_scale(X_scale, X_secs)
expect("Блок2", shift == 2)
expect("Блок2", index_of(keys, 1400) + 1 == 3)
expect("Блок2", no_rigel(X_scale, X_secs, 3), "№3 — без ригеля")
expect("Блок2", num_of_width(X_scale, X_secs, 1400) == "3", "резерв рядовки")
expect("Блок2", not num_match(X_scale, X_secs, 1, 1400), "сверки нет")

# --- 5. Резервный путь рядовки = цифра марки (без атрибута «Марка») -----
for scale1, secws in [(V7["scale1"], V7["secws"]), (T1_scale, T1_secs),
                      (TB1_scale, TB1_secs), (X_scale, X_secs)]:
    keys = num_keys(scale1, secws)
    for i, w in enumerate(keys, start=1):
        expect("резерв", num_of_width(scale1, secws, w) == str(i),
               f"{w} -> {num_of_width(scale1, secws, w)} != {i}")

# --- 6. Горизонтальная рядовка: цифра из марки = номер ригеля ----------
import re


def ar_number(mark):
    m = re.search(r"[0-9]+(?:\.[0-9]+)?", mark)
    return m.group(0) if m else None


for mark, rig in [("В10", 10), ("С10.1", 10), ("Б10.2", 10), ("А7", 7)]:
    n = ar_number(mark)
    expect("рядовка", n is not None and int(n.split(".")[0]) == rig, mark)

# подындексы ширины сохраняются, буква яруса не участвует
# префикс витража — хвост после последнего пробела (как mark:ar-tail)
expect("рядовка", ar_number("В-7 Ж.б10.2".split()[-1]) == "10.2")

# --- 7. Подындексы: позиция ширины внутри номера не зависит от шкалы ---
def subw_of(secws_children, w):
    lst = sort_unique(secws_children)
    for i, x in enumerate(lst, start=1):
        if same(x, w):
            return i
    return None


expect("подындексы", subw_of([540, 840], 540) == 1 and subw_of([540, 840], 840) == 2)
expect("подындексы", subw_of([650, 1000], 650) == 1 and subw_of([650, 1000], 1000) == 2)

if failures:
    print("ОШИБКИ:")
    for f in failures:
        print("  -", f)
    raise SystemExit(1)

print("В-7: 9 столбцов = 9 ригелей -> OK")
print("Т-1: 1430 = №10, 700 занят без столбца -> OK")
print("ТБ-1: 450 сдвигает нумерацию, есть INFO о смене номеров -> OK")
print("Блок 2: столбец без ригеля продолжает нумерацию -> OK")
print("резерв рядовки и цифра марки совпадают -> OK")
print("\nИТОГ: проверки пройдены")
