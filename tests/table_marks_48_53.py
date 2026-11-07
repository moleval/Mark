# -*- coding: utf-8 -*-
"""Ведомость MarkV: марки элементов без атрибута (MLINE) не теряются."""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")


def has(s, msg):
    assert s in text, msg


# карта «ename -> марка» существует и читается таблицей
has("(defun mk:mark-map-put (plan)", "нет заполнения карты марок")
has("(setq *mk:mark-map*\n", "карта марок не наполняется")
has("((setq hit (assoc e *mk:mark-map*)) (cdr hit))",
    "mk:read-mark не использует карту марок")

# план каждой стадии пишется в карту: и стойки, и ригели
assert text.count("(if plan (mk:mark-map-put plan))") == 2, \
    "карта марок должна заполняться на стойках и на ригелях"

# карта сбрасывается на каждый прогон, иначе в ведомость попадут старые марки
assert text.count("(setq *mk:mark-map* nil)") >= 2, \
    "карта марок не сбрасывается перед прогоном"

# таблица берёт марку через mk:read-mark для обеих секций
has("(defun mk:tab-rows (posts beams / out)", "нет mk:tab-rows")
assert text.count("(mk:read-mark el)") == 2, \
    "mk:tab-rows должен читать марку стойки и ригеля"

# атрибут по-прежнему главнее карты (блоки с «Марка» не ломаются)
has('(if (and e (mk:has-attr? e *mk:attr-mark*))', "атрибут марки перестал быть главным")

# м.п. считается от той же длины, что напечатана в ведомости
has("(defun mk:tab-mp (len cnt / l)", "нет округления длины для м.п.")
has("(setq l (fix (+ (float len) 0.5)))", "м.п. считается от сырой длины")
assert "(/ (* (float len) cnt) 1000.0)" not in text, "старый расчёт м.п. вернулся"

print("OK: марки MLINE-элементов попадают в ведомость")
