# -*- coding: utf-8 -*-
"""Производственный набор MARK.lsp: только рабочие команды, без внешних файлов."""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")
cmds = set(re.findall(r"\(defun\s+(c:[^\s()]+)", text))

ALLOWED = {
    "c:МАРКАВСЕ", "c:MARKALL",
    "c:МАРКАВ",
    "c:МАРКАВГЕОМЕТРИЯ", "c:МАРКАВСТ", "c:МАРКАВРГ", "c:МАРКАВТАБЛ",
    "c:МАРКАЗАП", "c:MARKA",
    "c:МАРКАЗАПБЛОК", "c:MARKFILL",
    "c:МАРКАРОВКАЗАП", "c:MARKZ",
    "c:МАРКАЗАПРЯД", "c:MARKAR",
    "c:МАРКАЗАПТАБЛ", "c:MARKTABLE",
    "c:МАРКАВДИАГНОЗОПТОМ", "c:MARKVBULKDIAG",
}

assert cmds == ALLOWED, "лишние команды: %s; потеряны: %s" % (
    sorted(cmds - ALLOWED), sorted(ALLOWED - cmds))

# Внешних файлов нет ни при загрузке, ни в командах.
assert "(mk:load-bases)" not in text, "автозагрузка MarkV-bases.lsp осталась"
assert "(findfile" not in text, "поиск внешних файлов остался"
assert "MarkV-bases" not in text, "имя MarkV-bases.lsp осталось"
assert not re.search(r"^\s*\(load\s", text, re.M), "остался вызов load"

# Разработческие команды не должны остаться даже в help-баннере.
for name in ("МАРКАЗАПТЕСТ", "МАРКАЗАПГЕО", "МАРКАЗАПКОПИИ", "МАРКАДИАГ",
             "SNAPSHOT", "МАРКАВСКОБКИ", "МАРКАВБАЗЫ", "MARKVGEOMDIAG",
             "MARKVSUBCELLS", "MARKVFRAME", "MARKVSYNC", "Сетка-мультилинии",
             "Полилинии"):
    assert "c:" + name not in text, "разработческая команда осталась: " + name

# Help-баннер перечисляет рабочий набор.
for name in ("МАРКАВСЕ / MARKALL", "МАРКАЗАП / MARKA", "МАРКАВГЕОМЕТРИЯ",
             "МАРКАВДИАГНОЗОПТОМ"):
    assert name in text, "нет строки help: " + name

print("OK: производственный набор: %d команд, внешних файлов нет"
      % len(cmds))
