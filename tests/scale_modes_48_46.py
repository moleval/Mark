# -*- coding: utf-8 -*-
"""Ред. 48.46 / MarkV 33.19: область общей и локальной шкал.

AutoCAD в CI не запускается. Числовая часть повторяет правило объединения
ключей MarkZ и фактические локальные шкалы LOG-5; структурная часть проверяет,
что в совместном режиме MarkV получает полный read-only probe MarkZ, а в
раздельном оба модуля получают независимые scoped-шкалы по рамкам. Фактическая приёмка — только
по MARKV_LOG/MARKZ_LOG и чертежу.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
MZ = (ROOT / "MARKZ.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")
MV = (ROOT / "MarkV.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")
failures = []


def expect(name, condition, detail=""):
    if not condition:
        failures.append(f"{name}: {detail}")


def fn(text, name):
    start = text.index("(defun " + name + " ")
    end = text.find("\n(defun ", start + 1)
    return text[start:end if end >= 0 else len(text)]


# Исторические локальные шкалы MARKV_LOG-5: используются только для проверки
# области/объединения, не как ожидаемые номера нового separate-прогона.
LOCAL = {
    "ВВ-1": [980],
    "ВВ-2": [590, 1000, 1250],
    "ВН-4": [375, 495, 550, 1100, 2250],
    "ВН-5": [200, 300, 550, 600, 740, 1100, 2250],
    "ВН-6": [635, 1100, 2250],
    "ВН-7": [325, 450, 600, 675, 700, 1100, 2250],
    "ВН-3": [450, 550, 600, 675, 700, 1100, 2250],
    "ВН-2": [200, 550, 600, 740, 1100, 2250],
    "ВН-1": [375, 495, 700, 1100, 2250],
}
# Исторические заполнения/резерв LOG-11 (48.46). Ред. 48.47 отдельно
# проверяет удаление четырёх фантомных З-секций из этого старого набора.
Z_ONLY = [195, 1425, 2245, 2850, 2900, 3085]
BEAMS = sorted({v for values in LOCAL.values() for v in values})
COMMON = sorted(set(BEAMS + Z_ONLY))

expect("LOG-5: девять витражей", len(LOCAL) == 9)
expect("LOG-5: 18 длин ригелей", len(BEAMS) == 18, repr(BEAMS))
expect("LOG-11: 24 общих номера", len(COMMON) == 24, repr(COMMON))
expect("195 = 1", COMMON.index(195) + 1 == 1)
expect("1100 = 17", COMMON.index(1100) + 1 == 17)
expect("2245 = 20", COMMON.index(2245) + 1 == 20)
expect("2250 = 21", COMMON.index(2250) + 1 == 21)
expect("локальные шкалы не склеены", len({tuple(v) for v in LOCAL.values()}) > 1)
expect("2250 локально отличается от общего",
       any(v.index(2250) + 1 != 21 for v in LOCAL.values() if 2250 in v))

# Независимость локальных областей: добавление З-позиции в одной рамке не
# может сдвинуть номера другой. Это offline-модель правила, не новый эталон
# фактических локальных номеров LOG-5.
def scoped_scale(beams, additions):
    return sorted(set(beams + additions))

local_a = scoped_scale([500, 2250], [195])
local_b = scoped_scale([500, 2250], [])
expect("З-позиция входит только в свою рамку", local_a == [195, 500, 2250]
       and local_b == [500, 2250])
expect("локальные номера не наследуются", local_a.index(2250) + 1 == 3
       and local_b.index(2250) + 1 == 2)
serialized = [(name, i + 1, value)
              for name, scale in (("A", local_a), ("B", local_b))
              for i, value in enumerate(scale)]
expect("локальные шкалы сериализуются с владельцем",
       ("A", 1, 195) in serialized and ("B", 1, 500) in serialized
       and ("B", 3, 2250) not in serialized)

# Рамки из MARKZV_MODEL: максимальная площадь пересечения, а не близость
# геометрических кластеров. Проверяем девять владельцев и граничные случаи.
FRAMES = {
    "ВВ-1": (11025, 17475, 15145, 20420),
    "ВВ-2": (25, 16520, 8905, 21010),
    "ВН-4": (-400, 10020, 5770, 14000),
    "ВН-5": (5795, 10020, 17610, 14000),
    "ВН-6": (20660, 10020, 24330, 14000),
    "ВН-7": (27380, 10020, 55205, 14000),
    "ВН-3": (25, 20, 28075, 4500),
    "ВН-2": (33370, 20, 45435, 4500),
    "ВН-1": (45460, 20, 51780, 4500),
}
EXPECTED_COUNTS = {
    "ВВ-1": 12, "ВВ-2": 35, "ВН-4": 21, "ВН-5": 39,
    "ВН-6": 12, "ВН-7": 90, "ВН-3": 120, "ВН-2": 52, "ВН-1": 28,
}


def overlap(a, b):
    x1, x2 = max(a[0], b[0]), min(a[2], b[2])
    y1, y2 = max(a[1], b[1]), min(a[3], b[3])
    return max(0, x2 - x1) * max(0, y2 - y1)


def owner(bb):
    scores = {name: overlap(bb, frame) for name, frame in FRAMES.items()}
    name = max(scores, key=scores.get)
    return name if scores[name] > 0 else None


# Один реальный представитель каждой рамки + пограничные заполнения, которые
# старый geometric-кластер склеивал с соседом.
samples = []
for name, (x0, y0, x1, y1) in FRAMES.items():
    samples.append((name, (x0 + 50, y0 + 50, x0 + 150, y0 + 150)))
samples += [
    ("ВН-4", (5250, 13140, 5745, 13950)),
    ("ВН-5", (6000, 13140, 6500, 13950)),
    ("ВН-3", (0, 0, 10, 10)),  # вне всех рамок: контроль OUT
]
expect("409 заполнений в девяти рамках", sum(EXPECTED_COUNTS.values()) == 409)
for expected, bb in samples[:9]:
    expect("рамка " + expected, owner(bb) == expected,
           f"получено {owner(bb)}")
expect("соседняя ВН-4 не уходит в ВВ-2", owner(samples[9][1]) == "ВН-4")
expect("соседняя ВН-5 не уходит в ВН-4", owner(samples[10][1]) == "ВН-5")
expect("вне рамки не назначается", owner(samples[11][1]) is None)

# Структурные связи реализации.
public_split = fn(MZ, "mark:vz-split")
mode_split = fn(MZ, "mark:vz-split-for-mode")
frame_split = fn(MZ, "mark:vz-frame-split")
filter_fn = fn(MZ, "mark:vz-carcass-filter")
sec_fn = fn(MZ, "mark:sec-standalone")
probe = fn(MZ, "mark:scale-probe")
probe_run = fn(MZ, "mark:scale-probe-run")
state_save = fn(MZ, "mark:vz-state-save")
reset_fn = fn(MZ, "mark:reset-state")
rows_fn = fn(MZ, "mark:vz-rows")
marks_fn = fn(MZ, "mark:vz-marks")
length_fn = fn(MV, "mk:length-scale")
joint_fn = fn(MV, "mk:joint-length-scale")
vitrage_fn = fn(MV, "mk:vitrage-length-scale")
prepare_fn = fn(MV, "mk:prepare-run")

expect("рамки имеют приоритет", "mark:vz-frame-split" in public_split
       and "mark:vz-split-geometric" in public_split)
expect("рамки только для separate", '(= *mark:vz-mode* "separate")' in mode_split
       and "(mark:vz-split fills glazings)" in mode_split
       and "(mark:vz-split-geometric fills glazings)" in mode_split)
expect("joint сохраняет LOG-9 группировку", "cls  (mark:vz-split-geometric af ag)" in marks_fn
       and "cls  (mark:vz-split-for-mode af ag)" in rows_fn)
expect("separate пересчитывает девять рамок", '(if (= *mark:vz-mode* "separate")' in marks_fn
       and "(mark:vz-split af ag)" in marks_fn)
expect("максимум площади", "mark:vz-overlap-area" in frame_split
       and "besta" in frame_split)
expect("каркас по текущей рамке", "mark:vz-carcass-filter" in sec_fn
       and "mark:vz-carcass-map-build" in filter_fn)
expect("probe сохраняет fill-geo", "*mark:fill-geo*" in probe
       and "mark:probe-restore" in probe)
expect("probe сохраняет шкалу и счётчики", "*mark:scale1*" in probe
       and "*mark:widths*" in probe and "*mark:errors*" in probe)
expect("probe без записи марок", "mark:apply-marks" not in probe_run
       and "mark:ar-insert" not in probe_run and "entmod" not in probe_run)
expect("probe использует общий standalone", "mark:sec-standalone" in probe_run
       and "mark:num-keys" in probe_run)
expect("probe read-only для dynamic blocks", "*mark:probe-readonly* t" in probe_run
       and "*mark:probe-unsupported*" in probe_run
       and "mark:fill-explode-segs-impl" in MZ)
expect("probe не создаёт временные копии", "mark:fill-explode-segs-impl e" in MZ
       and "(if *mark:probe-readonly*" in MZ)
expect("MarkV вызывает joint probe", "mark:scale-probe" in joint_fn
       and "*mk:last-scope*" in joint_fn and "mk:joint-length-scale" in length_fn)
expect("MarkV вызывает separate probe по рамке", "mark:scale-probe" in vitrage_fn
       and "(nth 2 grp)" in vitrage_fn and "*mk:vitrage-scales*" in vitrage_fn
       and "(mk:vitrage-length-scale out)" in length_fn)
expect("separate кэширует шкалы независимо", "(assoc tag *mk:vitrage-scales*)" in vitrage_fn
       and "*mk:last-prefix*" in vitrage_fn)
expect("MarkV имеет режим", "Маркировка нескольких витражей" in prepare_fn
       and '*mk:mark-mode* "separate"' in prepare_fn)
expect("состояние не сбрасывается между проходами", "*mark:vz-state-list*" not in reset_fn
       and "*mark:vz-current-frame*" not in reset_fn)
expect("separate сохраняет состояние", "(mark:vz-state-save c)" in marks_fn
       and "*mark:scale1*" in state_save and "*mark:heights*" in state_save)
expect("separate восстанавливает состояние", "mark:vz-state-restore" in rows_fn)
expect("раздельный файл имеет витраж", "витраж;индекс;размер_мм;источник" in MZ)
rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', MZ)
expect("редакции обновлены", bool(rev) and tuple(map(int, rev.groups())) >= (48, 46)
       and re.search(r'\*mk:build\*\s+"Ред\. 33\.19"', MV))

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("LOG-5: 9 локальных шкал; LOG-11: общая шкала 24 номера")
print("195=1, 1100=17, 2245=20, 2250=21; 409 заполнений / 9 рамок")
print("MarkV joint/separate -> scoped read-only probes; joint keeps LOG-9 geometry")
print("ИТОГ: offline-проверки Ред. 48.46 / MarkV 33.19 пройдены; нужна приёмка AutoCAD")
