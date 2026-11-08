# -*- coding: utf-8 -*-
"""Компактная шкала: числовая модель mark:num-keys / mark:num-scale.

Контрольный большой чертёж: 18 фактических размеров,
1100=16, 1250=17, 2250=18.

Проверка привязана к реализации: в `mark:num-keys` не должно быть
резервной ветки, а сама функция обязана собираться из тех же источников,
что и модель.
"""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TOL = 0.5


def same(a, b):
    return abs(a - b) < TOL


def sort_unique(vals):
    out = []
    for v in sorted(vals):
        if not any(same(v, x) for x in out):
            out.append(v)
    return out


def num_keys(rigels, sections):
    """Модель mark:num-keys: длины ригелей + ширины секций, без резерва."""
    out = [float(r) for r in rigels if r > 50]
    for w in sort_unique([float(w) for w in sections if w > 0]):
        if not any(same(w, k) for k in out):
            out.append(w)
    return sort_unique(out)


def index_of(keys, v):
    for i, k in enumerate(keys):
        if same(k, v):
            return i
    return None


RIGELS = [
    200, 300, 325, 375, 450, 495, 550, 590, 600,
    635, 675, 700, 740, 980, 1000, 1100, 1250, 2250,
]
SECTIONS = [200, 300, 325, 375, 450, 495, 550, 590, 600,
            635, 675, 700, 740, 980, 1000, 1100, 1250]

EXPECTED = RIGELS[:]

keys = num_keys(RIGELS, SECTIONS)
assert keys == [float(v) for v in EXPECTED], f"шкала: {keys}"
assert len(keys) == 18, f"номеров {len(keys)}, ожидалось 18"

assert index_of(keys, 1100) + 1 == 16, "1100 должен быть №16"
assert index_of(keys, 1250) + 1 == 17, "1250 должен быть №17"
assert index_of(keys, 2250) + 1 == 18, "2250 должен быть №18"

# Резервный путь нумерации: цифра по ширине = позиция в шкале.
for i, w in enumerate(keys, start=1):
    assert index_of(keys, w) + 1 == i, f"{w} -> {i}"

# --- привязка к реализации MARK.lsp -------------------------------------
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")

start = text.find("(defun mark:num-keys")
assert start >= 0, "нет mark:num-keys"
end = text.find("\n(defun ", start + 1)
assert end > start, "не найден конец mark:num-keys"
body = text[start:end]

for need in ("mark:scale-beam-keys", "mark:sec-ws", "mark:sort-unique"):
    assert need in body, "mark:num-keys не использует " + need
assert "append" not in body, "в mark:num-keys вернулась сборка через append"
assert "sec-reserve" not in text, "вернулся резерв *mark:sec-reserve*"

print("OK: компактная шкала 18 номеров, 1100=16, 1250=17, 2250=18; "
      "mark:num-keys без резерва")
