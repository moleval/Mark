# -*- coding: utf-8 -*-
"""Регрессия Ред. 48.43: изолированные 5 мм, Т-половины и подындексы.

LOG-8 показал два разных дефекта обычной привязки 48.42:
* 289–291: 195 -> 200 без необходимости;
* 299–301: 1095 -> 1100, после чего половины одной Т-секции получили
  разные секции и необоснованные подындексы.

Здесь математически проверяется узкое правило 48.43 и арифметика шкалы.
Фактический LOG-9 подтвердил эти правила. По решению пользователя ярусы не
нормализуются по фактической высоте: для 299–301 приняты Л21/К21/Б21,
а прежний вариант А21/Ж21 снят.
"""
from pathlib import Path
import re

TOL = 30.0


def rail_item(y, x0, x1, owner=1):
    # Формат горизонтальной оси: (Y X0 X1 HW OWNER).
    return (float(y), float(x0), float(x1), 25.0, owner)


def ordinary_snap_width(width, delta):
    """Математический аналог одного наружного конца 48.43."""
    return width + delta if 5.5 < delta <= TOL else width


def rail_end(hs, box, target, side):
    """Точный аналог mark:fill-t-rail-end? для координат теста."""
    xl, yb, xr, yt = box[:4]
    gap = TOL + 25.0
    for item in hs:
        y, a, b, hw = item[:4]
        on_tier = abs((y + hw) - yb) <= 1.0 or abs((y - hw) - yt) <= 1.0
        if not on_tier or a > xl + gap or b < xr - gap:
            continue
        if side > 0 and abs(a - target) <= 1.0:
            return True
        if side < 0 and abs(b - target) <= 1.0:
            return True
    return False


def nth_set(items, index, value):
    result = list(items)
    result[index] = value
    return result


def equal_t_cells(boxes, hs):
    """Математический аналог mark:fill-equal-t-cells.

    BOX = (xL yB xR yT secW tierH). Возвращает BOXES, резерв и счётчик.
    """
    boxes = list(boxes)
    reserve = []
    changed = 0
    for i in range(len(boxes) - 1):
        for j in range(i + 1, len(boxes)):
            a, b = boxes[i], boxes[j]
            if not (
                abs(a[1] - b[1]) <= 0.1
                and abs(a[3] - b[3]) <= 0.1
                and abs(a[4] - b[4]) <= 0.1
                and abs(a[5] - b[5]) <= 0.1
            ):
                continue
            if a[0] <= b[0]:
                lb, rb, li, ri = a, b, i, j
            else:
                lb, rb, li, ri = b, a, j, i
            lw, rw = lb[2] - lb[0], rb[2] - rb[0]
            dw = abs(lw - rw)
            gap = rb[0] - lb[2]
            union = rb[2] - lb[0]
            oldsec = lb[4]
            if not (
                0.1 < dw <= 5.5
                and 1.0 < gap <= TOL + 25.0
                and abs(oldsec - union) <= 1.0
            ):
                continue
            if lw < rw:
                target = lb[2] - rw
                side, narrower = 1, lb
            else:
                target = rb[0] + lw
                side, narrower = -1, rb
            if not rail_end(hs, narrower, target, side):
                continue
            if lw < rw:
                newsec = rb[2] - target
                boxes = nth_set(boxes, li, (target, lb[1], lb[2], lb[3], newsec, lb[5]))
                boxes = nth_set(boxes, ri, (rb[0], rb[1], rb[2], rb[3], newsec, rb[5]))
            else:
                newsec = target - lb[0]
                boxes = nth_set(boxes, li, (lb[0], lb[1], lb[2], lb[3], newsec, lb[5]))
                boxes = nth_set(boxes, ri, (rb[0], rb[1], target, rb[3], newsec, rb[5]))
            if oldsec not in reserve:
                reserve.append(oldsec)
            changed += 1
    return boxes, reserve, changed


def width_subindexes(records):
    """Аналог mapW/fullW: ((secW W) ...), индексы только подъячеек."""
    by_section = {}
    for secw, width in records:
        by_section.setdefault(secw, set()).add(width)
    result = {}
    for secw, widths in by_section.items():
        subwidths = sorted(width for width in widths if width != secw)
        # Полная ширина + подъячейка либо несколько разных подъячеек.
        # Полноразмерное заполнение остаётся базовым, без собственного .N.
        if secw in widths or len(subwidths) > 1:
            if subwidths:
                result[secw] = {width: str(i + 1)
                                for i, width in enumerate(subwidths)}
    return result


failures = []


def check(condition, message):
    if not condition:
        failures.append(message)


# Изолированные 289–291: ровно 5 мм не является основанием для 195 -> 200.
check(ordinary_snap_width(195.0, 5.0) == 195.0,
      "изолированные 195 мм расширились на 5 мм")
# Положительный контроль: прежняя ошибка 10 мм по-прежнему исправляется.
check(ordinary_snap_width(1090.0, 10.0) == 1100.0,
      "обычная привязка потеряла положительное отличие 10 мм")

# Фактические координаты LOG-7: слева 308–310 (1100), справа 299–301
# (1095). Между половинами стойка 50 мм; вместе они покрывают 2245 мм.
# В скобках — фактическая H и принятый геометрический ярус LOG-9.
# Объединение Т-половин обязано сохранить Л/К/Б, а не заменить их на А/Ж/Б.
tiers = [
    (4000.0, 4450.0, 1850.0),  # 299 / 308: H=450, ярус Л=1850
    (3140.0, 3950.0, 1350.0),  # 300 / 309: H=810, ярус К=1350
    (2600.0, 3090.0, 490.0),   # 301 / 310: H=490, ярус Б=490
]
pair_boxes = []
rails = []
for yb, yt, tier_h in tiers:
    pair_boxes.append((4650.0, yb, 5745.0, yt, 2245.0, tier_h))
    pair_boxes.append((3500.0, yb, 4600.0, yt, 2245.0, tier_h))
    # INS_PT горизонтальной оси — центр профиля; внутренняя грань
    # нижнего ригеля = Y+25, верхнего = Y-25.
    rails.extend([rail_item(yb - 25.0, 3500.0, 5750.0),
                  rail_item(yt + 25.0, 3500.0, 5750.0, 2)])
merged, reserve, changed = equal_t_cells(pair_boxes, rails)
check(changed == 3, f"объединено Т-ярусов: {changed}, ожидалось 3")
check(reserve == [2245.0], f"резерв старой секции неверен: {reserve}")
check(all(abs(box[2] - box[0] - 1100.0) <= 0.1 for box in merged),
      "не все шесть Т-половин получили W=1100")
check(all(abs(box[4] - 2250.0) <= 0.1 for box in merged),
      "не все шесть Т-половин получили общую секцию 2250")
check(all(abs(box[5] - tier[2]) <= 0.1
          for box, tier in zip(merged, [t for tier in tiers for t in (tier, tier)])),
      "объединение изменило высоту яруса")

# Без фактического наружного конца ригеля запрещено даже равное Т-выравнивание.
unmerged, unreserved, unchanged_count = equal_t_cells(pair_boxes[:2], [])
check(unchanged_count == 0 and not unreserved and unmerged == pair_boxes[:2],
      "Т-половины объединились без подтверждённого наружного ригеля")
# Другая старая секция/неполное покрытие — не основание для объединения.
bad_coverage = [(pair_boxes[0][0], pair_boxes[0][1], pair_boxes[0][2],
                 pair_boxes[0][3], 3000.0, pair_boxes[0][5]), pair_boxes[1]]
unmerged, _, unchanged_count = equal_t_cells(bad_coverage, rails[:2])
check(unchanged_count == 0 and unmerged == bad_coverage,
      "Т-половины объединились при непокрытой исходной секции")

# Шкала LOG-8. После отката ошибочных секций 745/3000 и восстановления 195
# старая 2245 исчезает из фактических секций, но резервируется. Без резерва
# 2250 стала бы №20; с резервом остаётся №21.
log8_scale = [200, 300, 325, 375, 450, 495, 550, 590, 600, 635, 675,
              700, 740, 745, 980, 1000, 1100, 1250, 1425, 2245, 2250,
              2850, 2900, 3000, 3085]
actual_scale = (set(log8_scale) - {745, 2245, 3000}) | {195, 2250}
without_reserve = sorted(actual_scale)
with_reserve = sorted(actual_scale | {2245})
expected_scale = [195, 200, 300, 325, 375, 450, 495, 550, 590, 600,
                  635, 675, 700, 740, 980, 1000, 1100, 1250, 1425,
                  2245, 2250, 2850, 2900, 3085]
check(without_reserve.index(2250) + 1 == 20,
      "контроль теста: без резерва 2250 должна была бы стать №20")
check(with_reserve == expected_scale,
      f"итоговая шкала отличается: {with_reserve}")
check(with_reserve.index(2250) + 1 == 21 and with_reserve.index(2245) + 1 == 20,
      "резерв 2245 не сохранил №21 у секции 2250")

# Одинаковые подширины одной секции не различаются. Реальные разные ширины
# сохраняют .1/.2 независимо от типа заполнения (стм/снд здесь не участвует).
check(width_subindexes([(2250.0, 1100.0)] * 144) == {},
      "одиночная подширина 1100 получила подындекс")
check(width_subindexes([(1425.0, 700.0)] * 4) == {},
      "одиночная подширина 700 получила подындекс")
check(width_subindexes([(1000.0, 1000.0), (1000.0, 990.0)]) ==
      {1000.0: {990.0: "1"}},
      "полная ширина + подъячейка: подъячейка должна получить .1")
check(width_subindexes([(1425.0, 700.0), (1425.0, 710.0)]) ==
      {1425.0: {700.0: "1", 710.0: "2"}},
      "две разные подширины потеряли различение .1/.2")

# Структурные якоря реальной CP1251-правки.
mz = (Path(__file__).resolve().parents[2] / "backup" / "MARKZ.lsp").read_bytes().decode("cp1251")
mz = mz.replace("\r\n", "\n")
rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', mz)
check(bool(rev) and tuple(map(int, rev.groups())) >= (48, 43),
      "редакция MARKZ старше 48.43")


def function_body(name):
    start = mz.index("(defun " + name)
    end = mz.find("\n(defun ", start + 1)
    return mz[start:end if end >= 0 else len(mz)]


rail_body = function_body("mark:fill-rail-cell-x")
check("(> d 5.5)" in rail_body and "(> d 5.0)" not in rail_body,
      "обычная привязка не имеет порога 5.5 мм")
for name in ("mark:fill-t-rail-end?", "mark:fill-nth-set",
             "mark:fill-equal-t-cells"):
    check(mz.count("(defun " + name) == 1, f"нет единственного {name}")
check("Выровнено пар равных Т-половин: " in function_body("mark:fill-equal-t-cells"),
      "счётчик Т-пар не позволяет сверить фактический прогон")
grid_body = function_body("mark:fill-mode-all-grid")
check("(mark:fill-equal-t-cells boxes hs)" in grid_body,
      "режим 5 не вызывает выравнивание Т-половин")
check("*mark:sec-reserve* nil" in grid_body,
      "режим 5 не сбрасывает резерв между запусками")
test_data_body = function_body("mark:test-data")
check("*mark:sec-reserve* nil" in test_data_body,
      "fallback без геометрии секций сохраняет старый резерв")
standalone_body = function_body("mark:sec-standalone")
check("(mark:fill-equal-t-cells (reverse out) hs)" in standalone_body,
      "отдельная маркировка не вызывает выравнивание Т-половин")
check("*mark:sec-reserve* nil" in standalone_body,
      "отдельная маркировка не сбрасывает резерв между запусками")
sec_ws_body = function_body("mark:sec-ws")
check("(append ws *mark:sec-reserve*)" in sec_ws_body,
      "резерв старой секции не включён в шкалу")
src_body = function_body("mark:sec-src-of")
check('(if (mark:index-of *mark:sec-reserve* w) "З" "?")' in src_body,
      "резервный номер шкалы получит неизвестный источник")
build_body = function_body("mark:build-marks")
check("(and subw (< w (- secw 0.5))" not in build_body,
      "осталось старое безусловное назначение подындекса по W != secW")
check("(or (member iw fullW) (> (length lst) 1))" in build_body,
      "нет правила: полная ширина + подъячейка либо несколько подширин")
check("(or snd stm)" not in build_body,
      "подындекс ширины ошибочно зависит от стм/снд")

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("289–291: изолированные 195 мм сохранены; контроль +10 мм работает")
print("299–301/308–310: W=1100, секция 2250, принятые ярусы Л/К/Б сохранены")
print("Шкала: 2250 остаётся №21; одиночные подширины — без .1")
print("Разные ширины одной секции сохраняют различение .1/.2")
print("ИТОГ: offline-проверки Ред. 48.43 пройдены; LOG-9 принят в AutoCAD")
