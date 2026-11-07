# -*- coding: utf-8 -*-
"""Безопасный отчёт и спецмарки Дв/Ок/Вр без reuse."""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
text = (ROOT / "MARK.lsp").read_bytes().decode("cp1251").replace("\r\n", "\n")


def has(s, msg):
    assert s in text, msg


# реестр и отчёт
has("(defun mc:remark-capture", "нет реестра замечаний")
has("(if (not *mark:probe-quiet*)", "probe добавляет замечания в отчёт")
has("(member tail '(\"Дв\" \"Ок\" \"Вр\"))", "спецмарка не распознаётся по хвосту")
has("(defun mc:remark-reset", "нет сброса реестра")
has("(defun mark:report-write", "нет .txt-отчёта")
has("(defun mark:remarks-summary", "нет сводки")
has("(defun mark:save-run-counters", "нет сохранения счётчиков")
has("(defun mark:sec-local-scale", "нет локальной шкалы MarkZ")
has("(mark:sec-local-scale)", "локальная шкала не вызывается")
assert "(or *mark:written-total* 0)" not in text, "or вернёт T вместо числа"
has("(+ *mark:written-total* *mark:written*)", "нет числового суммирования марок")
has('"Витражи"', "нет строки Витражи")
has('"Спецмарки Дв"', "нет спецмарок в отчёте")
assert text.count('"Спецмарки Дв"') == 1, "блок спецмарок должен быть один"
assert "Заполнения MarkV" not in text, "лишняя строка Заполнения MarkV"
has("[ERROR] ", "нет ERROR в сводке")
has('"   [INFO] "', "INFO не отделён от WARN")
has('(vl-string-search "НЕ ПОЛНОСТЬЮ ЗАПОЛНЯЕТ" s) "OPN"',
    "проёмный WARN не получает код OPN")
has('((= code "OPN")', "у кода OPN нет пояснения")
has('((= code "GEN")', "у кода GEN нет пояснения")

# спецмарки только заполнениям, без сканирования цикла
has("(defun mark:special-outside", "нет спецмарок")
has("(foreach e *mark:fills*", "кандидаты должны быть только из mark:fills")
has("(if (not (assoc e *mark:fill-geo*))", "кандидат должен быть без гео")
assert "(mark:special-outside)" in text, "спецмарки не вызываются"
assert "mark:special-candidates" not in text, "нельзя сканировать всю выборку"
has("(defun mark:special-mark?", "нет распознавания спецмарок")
has("(member e *mark:special-enames*)", "рядовка не пропускает спецмарки")
has("*mark:special-enames*", "нет списка спецмарок по ename")

# вентрешётки — быстрый фильтр по имени блока
has('(2 . "*ВЕНТ*")', "нет фильтра вентрешёток")
has("ВЕНТРЕШ", "нет маски Вентрешётка")
has("(defun mark:opening-zones", "нет зон проёмов")

# reuse не должен вернуться
for bad in ("mark:fill-existing", "mark:fill-move-dxf",
            "mark:fill-dims-ok?", "mark:enames-union"):
    assert bad not in text, "reuse вернулся: " + bad
assert "MARK_LOG.txt" not in text, "единый журнал вернулся"

# журналы прежние
has("MARKZ_LOG.txt", "нет MARKZ_LOG")
has("MARKV_LOG.txt", "нет MARKV_LOG")

print("OK: безопасный отчёт + спецмарки без reuse")
