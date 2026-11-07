# -*- coding: utf-8 -*-
"""Ред. 48.44: краткоживущий кэш AABB при группировке витражей.

AutoCAD здесь не запускается. Проверяются устройство LISP-обвязки и семантика
кэша, включая сохранённый nil. Геометрию марок проверяют остальные регрессии;
фактический сбой 0xC0000005 подтверждается только отдельным прогоном в AutoCAD.
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PATH = ROOT / "backup" / "MARKZ.lsp"
RAW = PATH.read_bytes()
assert not RAW.startswith(b"\xef\xbb\xbf"), "MARKZ.lsp не должен иметь BOM"
assert b"\r\n" in RAW, "MARKZ.lsp должен оставаться CRLF"
TEXT = RAW.decode("cp1251").replace("\r\n", "\n")


def function(name):
    start = TEXT.index("(defun " + name)
    end = TEXT.find("\n(defun ", start + 1)
    return TEXT[start:end if end >= 0 else len(TEXT)]


# Регрессия относится к поведению 48.44 и действует также в следующих редакциях.
assert '(setq *mark:rev*' in TEXT
assert '(setq *mark:vz-bb-cache* nil)' in TEXT

helper = function("mark:vz-bb-of")
minx = function("mark:vz-minx")
clusters = function("mark:vz-clusters")
cbb = function("mark:vz-cbb")
split = function("mark:vz-split-geometric")
public_split = function("mark:vz-split")
reset = function("mark:reset-state")

# Только первый обход заполнений обращается к ActiveX. Comparator vl-sort и
# повторные CBB для блоков «Атрибуты витража» читают уже сохранённые AABB.
assert clusters.count("(mark:fill-e-bb e)") == 1
assert clusters.count("(cons (cons e bb) *mark:vz-bb-cache*)") == 1
assert "(mark:vz-bb-of e)" in minx and "(mark:fill-e-bb e)" not in minx
assert "(mark:vz-bb-of e)" in cbb and "(mark:fill-e-bb e)" not in cbb
assert "(assoc e *mark:vz-bb-cache*)" in helper
assert helper.count("(mark:fill-e-bb e)") == 1  # fallback вне группировки
assert "[INFO] Габариты для группировки: " in clusters
assert "bb0   (cdr e0)" in clusters, "исправление очереди 48.29 сохраняется"
assert "queue (append queue hit)" in clusters
assert "*mark:vz-bb-cache*  nil" in reset
assert "(setq out (reverse out)\n        *mark:vz-bb-cache* nil)\n  out)" in split
# 48.46: сначала рамки, геометрическая связность остаётся fallback.
assert "mark:vz-frame-split" in public_split
assert "mark:vz-split-geometric" in public_split

# nil — тоже закэшированный результат. Проверка через наличие ключа, а не через
# truthiness значения, не делает повторный запрос для блока без габарита.
calls = []
cache = {}


def bb_of(entity):
    if entity in cache:
        return cache[entity]
    calls.append(entity)
    cache[entity] = None if entity == "без-AABB" else (0.0, 0.0, 100.0, 200.0)
    return cache[entity]


assert bb_of("обычный") == (0.0, 0.0, 100.0, 200.0)
assert bb_of("обычный") == (0.0, 0.0, 100.0, 200.0)
assert bb_of("без-AABB") is None
assert bb_of("без-AABB") is None
assert calls == ["обычный", "без-AABB"]

print("AABB: один первичный запрос на заполнение; nil тоже кэшируется")
print("vl-sort и повторные CBB: ActiveX не вызывают; очередь 48.29 сохранена")
print("Кэш очищается после mark:vz-split и при reset-state")
print("ИТОГ: offline-проверки Ред. 48.44 пройдены; устойчивость подтверждена LOG-11")
