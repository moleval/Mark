# -*- coding: utf-8 -*-
"""Ред. 48.45: fill-geo после готовых блоков и шкала LOG-11."""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "backup"
RAW = (SRC / "MARKZ.lsp").read_bytes()
TEXT = RAW.decode("cp1251").replace("\r\n", "\n")
assert not RAW.startswith(b"\xef\xbb\xbf") and b"\r\n" in RAW
start = TEXT.index("(defun mark:vz-marks")
end = TEXT.find("\n(defun ", start + 1)
VZ = TEXT[start:end]

# Регрессия состояния 48.45 действует и в следующих редакциях.
assert '(setq *mark:rev*' in TEXT
assert "ageo geo-out cls" in VZ
assert "geo-out nil" in VZ
assert VZ.count("(setq geo-out *mark:fill-geo*)") == 2  # joint + один витраж
assert "(setq geo-out (append geo-out *mark:fill-geo*))" in VZ  # separate
assert "*mark:fill-geo*  (if ageo ageo geo-out)" in VZ

# Фактические данные LOG-11: 18 длин MarkV, резерв 2245 и 22 секции.
beams = [200, 300, 325, 375, 450, 495, 550, 590, 600, 635, 675, 700,
         740, 980, 1000, 1100, 1250, 2250]
reserve = [2245]
sections = [195, 200, 300, 325, 375, 450, 495, 550, 590, 600, 635, 675,
            700, 740, 980, 1000, 1250, 1425, 2250, 2850, 2900, 3085]
final_widths = sorted(set(sections + reserve + beams))


def num_keys(fill_geo):
    return sorted(set(beams + reserve + fill_geo))


# Старое восстановление nil точно воспроизводит ошибочную Шкала-3.txt:
# 19 номеров, 2250=19, пять секций вне нумерации.
lost = num_keys([])
assert len(lost) == 19 and lost.index(2250) + 1 == 19
assert [w for w in final_widths if w not in lost] == [195, 1425, 2850, 2900, 3085]

# Сохранённая standalone-геометрия возвращает принятую LOG-9 шкалу.
kept = num_keys(sections)
assert kept == final_widths and len(kept) == 24
assert kept.index(195) + 1 == 1
assert kept.index(1100) + 1 == 17
assert kept.index(2245) + 1 == 20
assert kept.index(2250) + 1 == 21
assert all(w in kept for w in final_widths)

# Модель состояния всех трёх веток. Исходная геометрия сетки не заменяется;
# при ageo=nil сохраняются результаты joint/одиночного или всех separate-проходов.
def restored(ageo, passes):
    geo_out = []
    for current in passes:
        if len(passes) == 1:
            geo_out = list(current)
        else:
            geo_out.extend(current)
    return ageo if ageo else geo_out


computed = [("ячейка", 195, 810)]
assert restored([], [computed]) == computed                 # joint/одиночный
assert restored([], [[("A", 200, 450)], [("B", 300, 490)]]) == [
    ("A", 200, 450), ("B", 300, 490)]                      # separate
original = [("исходная", 2250, 1350)]
assert restored(original, [computed]) is original           # сетка не меняется

print("LOG-11: потеря fill-geo воспроизведена — 19 номеров, 2250=19")
print("48.45: сохранённая геометрия — 24 номера, 195=1, 2245=20, 2250=21")
print("joint/одиночный/separate: геометрия сохраняется; исходная сетка не заменяется")
print("ИТОГ: offline-проверки Ред. 48.45 пройдены; нужна запись Шкала.txt в AutoCAD")
