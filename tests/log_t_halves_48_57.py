# -*- coding: utf-8 -*-
"""Построчные сообщения Т-половин — только в диагностическом режиме."""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")


def has(s, msg):
    assert s in text, msg


# Построчная строка обёрнута в диагностический флаг.
has('(if *mark:sec-diag*\n'
    '                    (mark:out\n'
    '                      (strcat "[INFO] Равные Т-половины: секция "',
    "построчный лог Т-половин не под флагом")

# Итог печатается всегда — по нему и принимают прогон.
has('(mark:out\n'
    '    (strcat "[INFO] Выровнено пар равных Т-половин: "',
    "нет итоговой строки Т-половин")

# Флаг по умолчанию выключен.
has("(setq *mark:sec-diag*    nil)", "диагностический флаг включён по умолчанию")

print("OK: Т-половины — построчно только при *mark:sec-diag*, итог всегда")
