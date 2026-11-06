# -*- coding: utf-8 -*-
"""Ред. 48.48: фактическая ширина одиночной подгруппы = номер ригеля.

Проверяется только правило переназначения маркировочной секции. Координаты,
Т-объединение и высоты ярусов здесь не пересчитываются. Окончательная приёмка —
чистый прогон automatic grid -> MarkV -> MarkZ в AutoCAD.
"""
from pathlib import Path
import re


def same(a, b):
    return abs(a - b) < 0.5


def normalize(records, beams):
    """Модель mark:fill-geo-rigel-widths.

    record = (имя, secW, tierH, фактическая_W или None).
    Возвращает (secW, tierH) для каждой записи.
    """
    groups = {}
    for name, secw, tierh, width in records:
        key = round(secw)
        group = groups.setdefault(key, {"full": False, "bad": False,
                                        "widths": [], "secw": secw})
        if width is None or width <= 0:
            group["bad"] = True
        elif same(width, secw):
            group["full"] = True
        elif not any(same(width, old) for old in group["widths"]):
            group["widths"].append(width)

    direct = {}
    for key, group in groups.items():
        widths = sorted(group["widths"])
        if (not group["full"] and not group["bad"] and len(widths) == 1
                and any(same(widths[0], beam) for beam in beams)):
            direct[key] = widths[0]

    return [(direct.get(round(secw), secw), tierh)
            for name, secw, tierh, width in records]


failures = []


def check(condition, message):
    if not condition:
        failures.append(message)


beams = [200, 300, 325, 375, 450, 495, 550, 590, 600, 635, 675, 700,
         740, 980, 1000, 1100, 1250, 2250]
counts = {"ВН-1": 12, "ВН-2": 24, "ВН-3": 48, "ВН-4": 8,
          "ВН-5": 16, "ВН-6": 4, "ВН-7": 32}
records = []
for vitrage, count in counts.items():
    records.extend((vitrage, 2250.0, 1350.0, 1100.0)
                   for _ in range(count))
normalized = normalize(records, beams)
check(len(normalized) == 144, "изменилось количество заполнений")
check(all(secw == 1100 and tierh == 1350
          for secw, tierh in normalized),
      "одиночные W=1100 не получили маркировочную секцию 1100")
check(sum(counts.values()) == 144, "контрольная сумма витражей неверна")

# Полное заполнение + подъячейка: родительская секция и .1 сохраняются.
mixed = [("full", 1000.0, 1350.0, 1000.0),
         ("child", 1000.0, 1350.0, 990.0)]
check(normalize(mixed, beams + [990]) == [(1000.0, 1350.0)] * 2,
      "полная ширина + подъячейка была ошибочно переназначена")

# Несколько разных подширин: нельзя заменять родительский номер глобально.
multiple = [("a", 1430.0, 1350.0, 540.0),
            ("b", 1430.0, 1350.0, 840.0)]
check(normalize(multiple, beams + [540, 840, 1430]) ==
      [(1430.0, 1350.0)] * 2,
      "разные подширины потеряли родительскую секцию")

# Нет ригеля такой длины или ширина неизвестна — консервативный выход.
check(normalize([("no-beam", 2250.0, 1350.0, 1095.0)], beams) ==
      [(2250.0, 1350.0)], "ширина без ригеля была переназначена")
check(normalize([("unknown", 2250.0, 1350.0, None),
                 ("known", 2250.0, 1350.0, 1100.0)], beams) ==
      [(2250.0, 1350.0)] * 2,
      "неизвестная ширина не блокировала переназначение группы")

# После замены фактических секций 2250 остаётся в шкале как ригель,
# 2245 — принятый резерв, а 1100 становится секцией с ригелем.
new_sections = [secw for secw, tierh in normalized]
# В beams намеренно нет 195: добавляем изолированную секцию отдельно.
scale = sorted(set(beams + new_sections + [195, 2245]))
check(scale == [195, 200, 300, 325, 375, 450, 495, 550, 590, 600,
                635, 675, 700, 740, 980, 1000, 1100, 1250, 2245, 2250],
      "итоговая шкала 48.48 отличается")
check(scale.index(1100) + 1 == 17 and scale.index(2245) + 1 == 19
      and scale.index(2250) + 1 == 20,
      "номера 1100/2245/2250 неверны")

# Структурные якоря реальной CP1251-правки.
root = Path(__file__).resolve().parents[1]
mz_bytes = (root / "MARKZ.lsp").read_bytes()
check(mz_bytes.count(b"\r\n") == mz_bytes.count(b"\n"),
      "MARKZ.lsp потерял CRLF")
mz = mz_bytes.decode("cp1251").replace("\r\n", "\n")
rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', mz)
build = re.search(r'\*mark:build\*\s+"Ред\. (\d+)\.(\d+)"', mz)
check(bool(rev) and tuple(map(int, rev.groups())) >= (48, 48),
      "редакция MARKZ старше 48.48")
check(bool(build) and tuple(map(int, build.groups())) >= (36, 7),
      "build MARKZ старше 36.7")
check(mz.count("(defun mark:fill-geo-rigel-widths ") == 1,
      "нет единственного помощника фактической ширины")
helper_start = mz.index("(defun mark:fill-geo-rigel-widths ")
helper_end = mz.find("\n(defun ", helper_start + 1)
helper = mz[helper_start:helper_end if helper_end >= 0 else len(mz)]
for anchor in ("(not (member key full))", "(not (member key bad))",
               "(= (length lst) 1)", "(mark:index-of *mark:scale1* (car lst))"):
    check(anchor in helper, f"помощник потерял условие {anchor}")
check("mark:get-dyn" not in helper and "vla-" not in helper,
      "probe/расчёт ширины получил лишний ActiveX-доступ")
for name in ("mark:fill-apply", "mark:sec-standalone"):
    start = mz.index("(defun " + name + " ")
    end = mz.find("\n(defun ", start + 1)
    body = mz[start:end if end >= 0 else len(mz)]
    check("mark:fill-geo-rigel-widths" in body,
          f"{name} не вызывает новое правило")
check(mz.count("(mark:fill-equal-t-cells") >= 2,
      "потерян принятый расчёт Т-половин")

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("144 заполнения W=1100 -> маркировочная секция 1100 = Рг17")
print("Полная ширина + подъячейка и разные подширины сохраняют родителя")
print("Шкала сохраняет 1100=17, 2245=19, 2250=20; геометрия не меняется")
print("ИТОГ: offline-проверки Ред. 48.48 пройдены; нужен чистый CAD-прогон")
