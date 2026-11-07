# -*- coding: utf-8 -*-
"""Компактная шкала: резерв старых ширин Т-половин удалён из сборки."""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")


def has(s, msg):
    assert s in text, msg


# Резерва больше нет ни в переменных, ни в шкале, ни в состоянии/пробе.
for marker in ("sec-reserve", "reserve-t-halves",
               "(append ws *mark:sec-reserve*)"):
    assert marker not in text, "резерв остался: " + marker

# Шкала столбцов — только фактические ширины.
has("(defun mark:sec-ws (/ ws geo)", "нет mark:sec-ws")
has("(mark:sort-unique (mark:positives ws)))", "sec-ws не сортирует ширины")

# Для ширины без записи в scale-final источник остаётся «?», а не «З».
has('    (nth 1 row)\n    "?"))', "источник резерва не удалён")

print("OK: резерв призрачных размеров удалён, шкала компактная")
