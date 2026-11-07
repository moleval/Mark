# -*- coding: utf-8 -*-
"""Совместная шкала: общий probe, при сбое — союз локальных."""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")


def has(s, msg):
    assert s in text, msg


# союз локальных шкал — запасной путь; общий проход идёт первым
has("(defun mk:scale-merge (a b / out v hit x)", "нет объединения шкал")
has("(<= (abs (- x v)) *mk:tol-size*)", "объединение без допуска 0.5 мм")
has("(defun mk:joint-probe-union (/ out ok grp r)", "нет probe по витражам")
has("'mark:scale-probe (list (nth 2 grp))", "probe не получает витраж")
has("(setq r (vl-catch-all-apply 'mark:scale-probe (list *mk:last-scope*)))",
    "joint не начинает с общего прохода")
has("(if (or (null r) (vl-catch-all-error-p r))\n        (setq r (mk:joint-probe-union)))",
    "нет перехода к union после общего прохода")
has("(mk:joint-fallback-scale)", "пропала шкала по ригелям")

# частичный union не принимается за полный
has("(if ok out nil)", "неполный union не отбрасывается")

# предупреждение о fallback имеет свой код
has('(vl-string-search "SCALE-PROBE" s) "SCL"', "нет кода SCL")
has('((= code "SCL")', "у SCL нет пояснения")

print("OK: совместная шкала = союз локальных probe")
