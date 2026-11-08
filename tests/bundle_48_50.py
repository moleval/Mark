# -*- coding: utf-8 -*-
"""MARK.lsp: единый рабочий файл — структура, команды, цикл, отчёт.

Запуск: python tests/bundle_48_50.py
"""
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "tests"))
from lisp_structure import parse  # noqa: E402

raw = (ROOT / "MARK.lsp").read_bytes()
text = raw.decode("cp1251")


def names(t):
    return re.findall(r"\(defun\s+([^\s()]+)", t)


def cmds(t):
    return set(re.findall(r"\(defun\s+(c:[^\s()]+)", t))


ALLOWED = {
    "c:МАРКАВСЕ", "c:MARKALL",
    "c:МАРКАВ", "c:МАРКАВГЕОМЕТРИЯ", "c:МАРКАВСТ", "c:МАРКАВРГ", "c:МАРКАВТАБЛ",
    "c:МАРКАЗАП", "c:MARKA", "c:МАРКАЗАПБЛОК", "c:MARKFILL",
    "c:МАРКАРОВКАЗАП", "c:MARKZ", "c:МАРКАЗАПРЯД", "c:MARKAR",
    "c:МАРКАЗАПТАБЛ", "c:MARKTABLE",
    "c:МАРКАВДИАГНОЗОПТОМ", "c:MARKVBULKDIAG",
}

PRUNED_CMDS = {
    "c:МАРКАЗАПТЕСТ", "c:МАРКАЗАПГЕО", "c:МАРКАЗАПКОПИИ", "c:МАРКАДИАГ",
    "c:SNAPSHOT", "c:МАРКАВДИАГНОЗ", "c:МАРКАВСКОБКИ", "c:МАРКАВБАЗЫ",
    "c:MARKVGEOMDIAG", "c:MARKVSUBCELLS", "c:MARKVFRAME", "c:MARKVSYNC",
    "c:MARKVFRAMEASSIGN", "c:MARKVFRAMEINFO",
    "c:Сетка-мультилинии", "c:Точка-мультилинии", "c:Сетка-динамика",
    "c:Точка-динамика", "c:Полилинии",
}

# --- кодировка и переводы строк -----------------------------------------
assert not raw.startswith(b"\xef\xbb\xbf"), "BOM в MARK.lsp"
assert raw.count(b"\xef\xbf\xbd") == 0, "в MARK.lsp есть U+FFFD (битая кодировка)"
assert raw.count(b"\n") == raw.count(b"\r\n"), "MARK.lsp должен быть целиком CRLF"
assert text.encode("cp1251") == raw, "MARK.lsp не является корректным cp1251"

# --- формы AutoLISP закрыты, арности специальных форм в норме -----------
parse(text)
bundle = names(text)

# --- команды: 19 имён, разработческие отсутствуют ------------------------
assert cmds(text) == ALLOWED, "лишние/потерянные команды: %s" % (
    sorted(cmds(text) - ALLOWED), sorted(ALLOWED - cmds(text)))
assert not (PRUNED_CMDS & cmds(text)), \
    "разработческие команды остались: %s" % sorted(PRUNED_CMDS & cmds(text))

# --- единый цикл ---------------------------------------------------------
assert "(defun mk:a-all (preset" in text, "mk:a-all не принимает выборку"
assert "(setq ss (if preset preset (mk:select-scope)))" in text, \
    "mk:a-all не переиспользует выборку"
assert "(defun c:МАРКАВ () (mk:a-all nil))" in text, "c:МАРКАВ не обёртка"
assert "(defun mark:a-all (preset-kw" in text, "mark:a-all без ключевого слова"
assert '(mark:a-all "Сетка")' in text, "цикл не передаёт источник в MarkZ"
assert "(append enames *mark:fills*)" in text, \
    "цикл не добавляет заполнения в выборку MarkV"
assert text.find('(mark:a-all "Сетка")') < text.find("(mk:a-all enames)"), \
    "MarkZ должен идти до MarkV: probe должен видеть заполнения"
assert "(if *mark:cycle-ss*" in text, "mark:fill-all-ss не переиспользует выбор"
assert "(defun mark:all-cycle (" in text, "mark:all-cycle не создан"
assert "(defun mark:cycle-body (" in text, "mark:cycle-body не создан"
assert "(defun c:МАРКАВСЕ () (mark:all-cycle))" in text, "c:МАРКАВСЕ не создан"
assert "(defun c:MARKALL () (mark:all-cycle))" in text, "c:MARKALL не создан"
assert "*mark:cycle-ss*" in text, "нет единой выборки цикла"
assert '(strcat "=== MARK.lsp " *mark:file-rev* ": MarkZ "' in text, \
    "нет метки цикла с редакцией файла"

# --- Т-соединения: симметрия узла считается по заполнениям --------------
assert "(mk:split-tjoints posts beams (append fills windows doors))" in text, \
    "mk:split-tjoints не получает заполнения (Т-соединения)"
assert "*mk:tol-tjoint*" in text, "нет *mk:tol-tjoint*"
assert "(defun mk:tj-suffix (roles)" in text, "нет mk:tj-suffix"
assert "(defun mark:fill-equal-t-cells" in text, "нет mark:fill-equal-t-cells"

# --- смежные витражи через стойку и таблица MarkZ -----------------------
assert "(defun mark:fill-single-rail-cells" in text, \
    "нет раскладки одиночной ячейки по ригелю"
assert "(setq boxes (mark:fill-single-rail-cells boxes hs))" in text, \
    "grid не расширяет одиночную ячейку после Т-половин"
assert "(mark:fill-single-rail-cells (mark:fill-equal-t-cells (reverse out) hs) hs)" in text, \
    "standalone не расширяет одиночную ячейку после Т-половин"
assert "(defun mark:vz-split-frame-first" in text, \
    "нет frame-first выбора групп (or вернул бы T)"
assert "(mark:vz-split-frame-first af ag)" in text, \
    "mark:vz-marks не использует frame-first"
assert "(defun mark:vz-map-build-by-frames" in text, \
    "нет карты префиксов по рамкам"
assert "(mark:vz-map-build-by-frames af ag)" in text, \
    "mark:vz-marks не использует карту по рамкам"
assert "(not *mk:cycle-tables-cleared*)" in text, \
    "mk:clear-old-tables не защищает таблицу MarkZ"
assert "(mk:clear-old-tables)" in text, "цикл не чистит старые таблицы"

# --- журналы, отчёт, спецмарки, запрет reuse ----------------------------
assert "MARKZ_LOG.txt" in text, "нет MARKZ_LOG.txt"
assert "MARKV_LOG.txt" in text, "нет MARKV_LOG.txt"
assert "MARK_LOG.txt" not in text, "вернулся единый MARK_LOG"
assert "(t (setq e nil))" in text, "clear-attrs не обрывает цепочку на SEQEND"
assert "(defun mc:remark-capture" in text, "нет реестра замечаний"
assert "(defun mark:report-write" in text, "нет .txt-отчёта"
assert "(defun mark:save-run-counters" in text, "нет сохранения счётчиков"
assert "(defun mark:special-outside" in text, "нет спецмарок Дв/Ок/Вр"
assert "(mark:special-outside)" in text, "спецмарки не вызываются"
assert "(mark:report-write)" in text, "отчёт не записывается"
assert '"Витражи"' in text, "нет строки Витражи"
assert text.count('"Спецмарки Дв"') == 1, "блок спецмарок должен быть один"
for bad in ("mark:fill-existing", "mark:fill-move-dxf", "mark:fill-dims-ok?"):
    assert bad not in text, "reuse не должен вернуться: " + bad

# --- инициализация -------------------------------------------------------
assert text.count("(vl-load-com)") >= 1
assert raw.count(b"\n(vl-load-com)") == 1, "верхнеуровневый (vl-load-com) не один"
assert "(mark:core-ensure)" not in text, "загрузчик ядра MarkZ остался"
assert "(mk:core-ensure)" not in text, "загрузчик ядра MarkV остался"
assert "(mk:load-bases)" not in text, "внешние базы MarkV не отключены"
assert "(findfile" not in text, "остался поиск внешних файлов"
assert "MarkV-bases" not in text, "осталась внешняя база MarkV"
assert "(mark:reset-state)" in text, "потеряна инициализация MarkZ"
for part in ('"Mark.lsp " *mark:file-rev*',
             '" — MarkZ " *mark:rev*',
             '" + MarkV " *mk:ver*'):
    assert part in text, "в баннере нет редакции: " + part
assert '*mark:file-note*' not in text, "в баннер вернулась changelog-заметка"
assert '"Mark.lsp " *mark:file-rev* " | MarkZ "' in text, \
    "в отчёте нет редакции файла"
assert "BUILD:CUT-" not in text, "в сборке остались служебные маркеры"

print("OK: MARK.lsp — %d функций, %d команд, формы закрыты"
      % (len(bundle), len(cmds(text))))
