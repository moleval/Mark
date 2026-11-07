# -*- coding: utf-8 -*-
"""MARK.lsp: сборка полная, свежая, без повреждений и дублей между модулями.

Проверяет именно сборку (этап A плана слияния), а не логику MarkZ/MarkV.
Запуск: python3 tests/bundle_48_50.py
"""
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "backup"
sys.path.insert(0, str(ROOT / "tests"))
from lisp_structure import parse  # noqa: E402

raw = (ROOT / "MARK.lsp").read_bytes()
text = raw.decode("cp1251")


def names(t):
    return re.findall(r"\(defun\s+([^\s()]+)", t)


def cmds(t):
    return set(re.findall(r"\(defun\s+(c:[^\s()]+)", t))


# Производственная сборка: разработческие команды и помощники вырезаны.
PRUNED_CMDS = {
    "c:МАРКАЗАПТЕСТ", "c:МАРКАЗАПГЕО", "c:МАРКАЗАПКОПИИ", "c:МАРКАДИАГ",
    "c:SNAPSHOT", "c:МАРКАВДИАГНОЗ", "c:МАРКАВСКОБКИ", "c:МАРКАВБАЗЫ",
    "c:MARKVGEOMDIAG", "c:MARKVSUBCELLS", "c:MARKVFRAME", "c:MARKVSYNC",
    "c:MARKVFRAMEASSIGN", "c:MARKVFRAMEINFO",
    "c:Сетка-мультилинии", "c:Точка-мультилинии", "c:Сетка-динамика",
    "c:Точка-динамика", "c:Полилинии",
}
PRUNED_FUNCS = {
    "mark:snapshot-path", "mark:file-open?", "mark:find-source",
    "mk:find-source", "mk:check-brackets", "mk:ext-checker?",
    "mk:load-bases", "c:МАРКАВБАЗЫ",
    "mark:diag-name", "mark:spds?", "mark:copies-make-group",
    "mark:test-insert", "mark:test-make-block", "mark:test-purge",
    "mk:diag-block", "mk:draw-test-model", "mk:dump-data",
    "mark:ar-sort-by", "mark:fill-bb-area", "mark:fill-block-segs-at",
    "mark:fill-bname-local", "mark:fill-hit-area", "mark:fill-hits-merge",
    "mark:fill-pick-insert", "mark:fill-seg-components", "mark:fill-set-near",
    "mark:hex-val", "mark:sort-heights", "mark:sort-widths",
    "mk:get-vitrage-prefix", "mk:scope-ss", "mk:tab-prefix",
    "mk:validate-beam", "mk:validate-fill", "mk:vitrage-rec-from-frame",
    "mk:help-border", "mk:help-header", "mk:help-row",
    "mark:ax-objname", "mark:copies-group-name",
    "mark:dyn-item-value", "mark:insert-sorted", "mark:name-match?",
    "mark:fill-as-insert", "mark:fill-inserts-at", "mark:fill-pt-in-ins",
    "mark:fill-seg-touch?", "mark:test-line",
    "mk:beam-touches-post?", "mk:draw-door-label", "mk:draw-fill-cross",
    "mk:draw-grid", "mk:draw-window-label", "mk:dynamic?", "mk:fmt",
    "mk:ljust", "mk:ss-all-inserts",
    "mark:fill-def-bb-cached", "mark:fill-insert-hits", "mark:fill-mat-inv",
    "mk:repeat", "mark:fill-def-bb", "mark:fill-near-inserts",
    "mark:fill-ss-hits", "mark:fill-hits-sort",
    "mark:fill-hits-best", "mark:fill-hits-drop",
}


# --- кодировка и переводы строк -----------------------------------------
assert not raw.startswith(b"\xef\xbb\xbf"), "BOM в MARK.lsp"
assert raw.count(b"\xef\xbf\xbd") == 0, "в MARK.lsp есть U+FFFD (битая кодировка)"
assert raw.count(b"\n") == raw.count(b"\r\n"), "MARK.lsp должен быть целиком CRLF"
assert text.encode("cp1251") == raw, "MARK.lsp не является корректным cp1251"

# --- формы AutoLISP закрыты, арности специальных форм в норме -----------
parse(text)

# --- все defun из исходников попали в сборку ----------------------------
bundle = names(text)
for part in ("MARKCORE.lsp", "MARKZ.lsp", "MarkV.lsp"):
    for n in names((SRC / part).read_bytes().decode("cp1251")):
        # загрузчики ядра намеренно вырезаны: ядро уже внутри сборки
        if n in ("mark:core-ensure", "mk:core-ensure"):
            continue
        if n in PRUNED_CMDS or n in PRUNED_FUNCS:
            continue
        assert n in bundle, f"{n} потерян при сборке ({part})"

# --- нет пересечений имён между модулями --------------------------------
core_names = set(names((SRC / "MARKCORE.lsp").read_bytes().decode("cp1251")))
markz_names = set(names((SRC / "MARKZ.lsp").read_bytes().decode("cp1251")))
markv_names = set(names((SRC / "MarkV.lsp").read_bytes().decode("cp1251")))
assert not (core_names & markz_names), core_names & markz_names
assert not (core_names & markv_names), core_names & markv_names
assert not (markz_names & markv_names), markz_names & markv_names

# --- команды MarkZ и MarkV сохранены ------------------------------------
src_cmds = set()
for part in ("MARKCORE.lsp", "MARKZ.lsp", "MarkV.lsp"):
    src_cmds |= cmds((SRC / part).read_bytes().decode("cp1251"))
assert (src_cmds - PRUNED_CMDS) <= cmds(text), \
    "потеряны команды: %s" % ((src_cmds - PRUNED_CMDS) - cmds(text))
assert not (PRUNED_CMDS & cmds(text)), \
    "разработческие команды остались: %s" % sorted(PRUNED_CMDS & cmds(text))
assert not (PRUNED_FUNCS & set(bundle)), \
    "разработческие функции остались: %s" % sorted(PRUNED_FUNCS & set(bundle))
# Новые команды полного цикла (этап 4).
extra = cmds(text) - src_cmds
assert extra == {"c:МАРКАВСЕ", "c:MARKALL"}, extra
for cmd in ("c:MARKZ", "c:MARKAR", "c:MARKA", "c:MARKFILL", "c:MARKTABLE",
            "c:МАРКАВ", "c:МАРКАВТАБЛ", "c:МАРКАЗАП"):
    assert cmd in src_cmds, f"команда {cmd} отсутствует в исходниках"
for cmd in ("c:МАРКАВСЕ", "c:MARKALL"):
    assert cmd in cmds(text), f"команда {cmd} отсутствует в сборке"

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
assert "=== MARK.lsp: MarkZ " in text, "нет метки цикла в журналах"

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
# журналы прежние; отчёт и спецмарки — без reuse
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
assert "BUILD:CUT-" not in text, "в сборке остались служебные маркеры"

# --- сборка свежая: пересборка не меняет файл ---------------------------
subprocess.run(
    [sys.executable, str(ROOT / "tools" / "build_single.py"), "--check"],
    check=True,
    capture_output=True,
)

print(
    "OK: сборка %d функций, %d команд, формы закрыты, файл свежий"
    % (len(bundle), len(cmds(text)))
)
