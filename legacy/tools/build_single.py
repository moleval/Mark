# -*- coding: utf-8 -*-
"""Сборка одного загрузочного файла MARK.lsp из MARKCORE + MARKZ + MarkV.

Исходники НЕ изменяются: нужные фрагменты вырезаются по точным анкерам.
Результат детерминирован: одинаковый вход -> одинаковый MARK.lsp (cp1251, CRLF).

Этап 3: тело c:МАРКАВ выделено в mk:a-all, c:МАРКАВ — тонкая обёртка.
Этап 4: МАРКАВСЕ / MARKALL — полный цикл: один выбор, один внешний UNDO,
        MarkV (каркас) -> MarkZ (заполнения, марки, рядовка, ведомость).
"""
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "backup"
OUT = ROOT / "MARK.lsp"


def read(name):
    return (SRC / name).read_bytes().decode("cp1251").replace("\r\n", "\n")


def require(cond, msg):
    if not cond:
        raise SystemExit("СБОРКА ОСТАНОВЛЕНА: " + msg)


def sub_once(text, old, new, label):
    require(text.count(old) == 1, "%s: ожидалось 1 вхождение, найдено %d"
            % (label, text.count(old)))
    return text.replace(old, new, 1)


def cut_from(text, anchor, label):
    i = text.find(anchor)
    require(i >= 0, "не найден анкер: " + label)
    return text[:i]


def cut_between(text, start, end, label):
    i = text.find(start)
    require(i >= 0, "не найден начальный анкер: " + label)
    j = text.find(end, i)
    require(j >= 0, "не найден конечный анкер: " + label)
    return text[:i] + text[j + len(end):]


def strip_form(text, head, label, nth=1):
    """Удалить верхнеуровневую форму, начинающуюся с head (после перевода строки)."""
    key = "\n" + head
    i = -1
    for _ in range(nth):
        i = text.find(key, i + 1)
        require(i >= 0, "не найден удаляемый блок #%d: %s" % (nth, label))
    start = i + 1
    depth = 0
    in_str = False
    in_com = False
    n = len(text)
    j = start
    while j < n:
        c = text[j]
        if in_com:
            if c == "\n":
                in_com = False
        elif in_str:
            if c == "\\":
                j += 2
                continue
            if c == '"':
                in_str = False
        elif c == ";":
            in_com = True
        elif c == '"':
            in_str = True
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                end = j + 1
                if end < n and text[end] == "\n":
                    end += 1
                return text[:start] + text[end:]
        j += 1
    raise SystemExit("СБОРКА ОСТАНОВЛЕНА: не закрыт блок " + label)


def names(t):
    return re.findall(r"\(defun\s+([^\s()]+)", t)


# --- ядро: без завершающего флага ядра и princ ---------------------------
core = cut_from(read("MARKCORE.lsp"), "(setq *mc:core-ready* t)", "MARKCORE tail")

# --- MarkZ: без (vl-load-com) ... (mark:core-ensure), без финального баннера
markz = read("MARKZ.lsp")
markz = cut_between(markz, "(vl-load-com)", "(mark:core-ensure)", "MARKZ head")
markz = cut_from(markz, '(mark:emit (strcat "\\n[MARKZ] "', "MARKZ tail")

# --- компактная шкала: резерв старых ширин Т-половин удалён ---------------
markz = sub_once(
    markz,
    "(setq *mark:sec-reserve*  nil)     ;; ширины объединённых секций: резерв номеров\n",
    "", "MarkZ reserve config")
markz = sub_once(
    markz,
    "      ;; Нет геометрии секций — резерв объединённых Т-половин не переносим\n"
    "      ;; из предыдущего прогона.\n"
    "      (setq *mark:sec-reserve* nil)\n"
    "      (setq wlist",
    "      (setq wlist", "MarkZ reserve reset")
markz = sub_once(
    markz,
    "  ;; Объединение равных Т-половин не должно сдвигать номера следующих\n"
    "  ;; секций: прежняя ширина остаётся незанятым резервным номером.\n"
    "  (setq ws (append ws *mark:sec-reserve*))\n"
    "  (mark:sort-unique (mark:positives ws)))",
    "  (mark:sort-unique (mark:positives ws)))", "MarkZ reserve in sec-ws")
markz = sub_once(
    markz,
    "  (setq *mark:fill-geo* nil\n"
    "        *mark:cell-hs*  nil\n"
    "        *mark:sec-reserve* nil)",
    "  (setq *mark:fill-geo* nil\n"
    "        *mark:cell-hs*  nil)", "MarkZ reserve in fill-main")
markz = sub_once(
    markz,
    "            (cons '*mark:sec-reserve*  *mark:sec-reserve*)\n",
    "", "MarkZ reserve in state save")
markz = sub_once(
    markz,
    "                  (if (not (member oldsec *mark:sec-reserve*))\n"
    "                    (setq *mark:sec-reserve* (cons oldsec *mark:sec-reserve*)))\n",
    "", "MarkZ reserve add")
markz = sub_once(
    markz,
    "  (if row\n"
    "    (nth 1 row)\n"
    "    (if (mark:index-of *mark:sec-reserve* w) \"З\" \"?\")))",
    "  (if row\n"
    "    (nth 1 row)\n"
    "    \"?\"))", "MarkZ reserve in src-of")
markz = sub_once(
    markz,
    "  (setq ss nil\n"
    "        *mark:sec-reserve* nil)",
    "  (setq ss nil)", "MarkZ reserve in standalone")
markz = sub_once(
    markz,
    "            *mark:heights* nil\n"
    "            *mark:sec-reserve* nil\n"
    "            *mark:cell-hs* nil",
    "            *mark:heights* nil\n"
    "            *mark:cell-hs* nil", "MarkZ reserve in probe reset")
markz = markz.replace(" *mark:sec-reserve*", "")
require("*mark:sec-reserve*" not in markz, "резерв Т-половин остался в MarkZ")

markz = sub_once(
    markz,
    "                  (setq changed (1+ changed))\n"
    "                  (mark:out\n"
    "                    (strcat \"[INFO] Равные Т-половины: секция \"\n"
    "                            (rtos oldsec 2 0) \" -> \" (rtos newsec 2 0)\n"
    "                            \", ширина \" (rtos (if (< lw rw) rw lw) 2 0) \".\"))",
    "                  (setq changed (1+ changed))\n"
    "                  (if *mark:sec-diag*\n"
    "                    (mark:out\n"
    "                      (strcat \"[INFO] Равные Т-половины: секция \"\n"
    "                              (rtos oldsec 2 0) \" -> \" (rtos newsec 2 0)\n"
    "                              \", ширина \" (rtos (if (< lw rw) rw lw) 2 0) \".\")))",
    "MarkZ T-half per-pair log")

# --- комментарии: убрать упоминания удалённого резерва --------------------
markz = sub_once(
    markz,
    "  ;; Обе получают новую общую ширину секции. Старая ширина резервируется\n"
    "  ;; в шкале, чтобы техническое объединение не сдвинуло следующие номера.\n",
    "  ;; Обе получают новую общую ширину секции. Старая ширина в шкалу\n"
    "  ;; не попадает: шкала содержит только фактические размеры.\n",
    "MarkZ T-half comment")
markz = sub_once(
    markz,
    ";;; длины ригелей и резерв Т-половин, что в обычном МАРКАЗАП; атрибуты,\n",
    ";;; длины ригелей и ширины ячеек, что в обычном МАРКАЗАП; атрибуты,\n",
    "MarkZ probe comment")


# --- MarkV: без (vl-load-com) ... (mk:core-ensure), без финального баннера.
#     Внешние базы MarkV-bases.lsp больше не подхватываются.
markv = read("MarkV.lsp")
markv = cut_between(markv, "(vl-load-com)", "(mk:core-ensure)", "MarkV head")
markv = cut_from(markv, '(mk:emit (strcat "\\n[MarkV] ', "MarkV tail")

# --- комментарии MarkV: без упоминаний удалённого резерва ----------------
markv = sub_once(
    markv,
    ";; длины ригелей, но и ширины ячеек/резерв MarkZ; поэтому объединять только\n",
    ";; длины ригелей, но и ширины ячеек MarkZ; поэтому объединять только\n",
    "MarkV joint fallback comment")
markv = sub_once(
    markv,
    ";; независимые полные шкалы MarkZ (ригели + ячейки + локальный резерв), а не\n",
    ";; независимые полные шкалы MarkZ (ригели + ячейки), а не\n",
    "MarkV separate comment")
markv = sub_once(
    markv,
    "  ;; Поэтому Блок 2 (З) и резерв не теряются и не расходятся между модулями.\n",
    "  ;; Поэтому Блок 2 (З) не теряется и не расходится между модулями.\n",
    "MarkV length-scale comment")

# =====================================================================
# ЭТАП 3: c:МАРКАВ -> mk:a-all + тонкая обёртка
# =====================================================================
markv = sub_once(markv, "(defun c:МАРКАВ (", "(defun mk:a-all (", "MarkV rename")
markv = sub_once(markv, "(defun mk:a-all (/ ss keep doc n r pt0)",
                 "(defun mk:a-all (preset / ss keep doc n r pt0)",
                 "MarkV preset arg")
markv = sub_once(markv, "(setq ss (mk:select-scope))",
                 "(setq ss (if preset preset (mk:select-scope)))",
                 "MarkV preset selection")
markv = sub_once(
    markv,
    "(princ))\n;;;=====================================================================\n"
    ";;; 20b.",
    "(princ))\n\n"
    ";;;--- ПАКЕТ: тонкая обёртка команды -----------------------------------\n"
    "(defun c:МАРКАВ () (mk:a-all nil))\n"
    ";;;=====================================================================\n"
    ";;; 20b.",
    "MarkV wrapper")

# =====================================================================
# ЭТАП 4: MarkZ — ключевое слово без вопроса и единая выборка
# =====================================================================
markz = sub_once(markz,
                 "(defun mark:a-all (/ kw mode doc r cells pts new-fills)",
                 "(defun mark:a-all (preset-kw / kw mode doc r cells pts new-fills)",
                 "MarkZ preset-kw arg")
markz = sub_once(
    markz,
    '  (initget "Сетка Заполнения С З S Z 1 2")\n'
    '  (setq kw (getkword "\\nИсточник [Сетка/Заполнения] <Сетка>: "))',
    '  (if (mark:strp preset-kw)\n'
    '    (setq kw preset-kw)\n'
    '    (progn\n'
    '      (initget "Сетка Заполнения С З S Z 1 2")\n'
    '      (setq kw (getkword "\\nИсточник [Сетка/Заполнения] <Сетка>: "))))',
    "MarkZ preset-kw use")
require(markz.count("(mark:a-all))") == 2,
        "ожидалось 2 обёртки mark:a-all, найдено %d" % markz.count("(mark:a-all))"))
markz = markz.replace("(mark:a-all))", "(mark:a-all nil))")
markz = sub_once(
    markz,
    '(defun mark:fill-all-ss (/ filter ss)\n'
    '  (setq filter (list (cons 0 "INSERT,LINE,LWPOLYLINE,POLYLINE,MLINE,ARC")))\n'
    "  (setq ss (vl-catch-all-apply 'ssget (list filter)))\n"
    '  (if (or (vl-catch-all-error-p ss) (null ss)) nil ss))',
    '(defun mark:fill-all-ss (/ filter ss)\n'
    '  (if *mark:cycle-ss*\n'
    '    *mark:cycle-ss*\n'
    '    (progn\n'
    '      (setq filter (list (cons 0 "INSERT,LINE,LWPOLYLINE,POLYLINE,MLINE,ARC")))\n'
    "      (setq ss (vl-catch-all-apply 'ssget (list filter)))\n"
    '      (if (or (vl-catch-all-error-p ss) (null ss)) nil ss))))',
    "MarkZ cycle selection")

# --- смежные витражи через одну стойку: карта префиксов по рамкам -------
# В совместном режиме mark:vz-marks строил карту из геометрических
# кластеров, а они сливают витражи, разделённые одной стойкой. Сначала
# пробуем frame-разбиение, затем прежний геометрический fallback.
markz = sub_once(
    markz,
    '            cls  (mark:vz-split-geometric af ag)',
    '            cls  (mark:vz-split-frame-first af ag)',
    "MarkZ vz frame-first split")
markz = sub_once(
    markz,
    '            *mark:vz-map*  nil\n'
    '            *mark:vz-mode* nil',
    '            *mark:vz-map*  nil\n'
    '            *mark:vz-by-frames* nil\n'
    '            *mark:vz-mode* nil',
    "MarkZ vz by-frames reset")
markz = sub_once(
    markz,
    '(setq *mark:vz-map* (mark:vz-map-build cls))',
    '(setq *mark:vz-map* (mark:vz-map-build-by-frames af ag))\n'
    '           (if (null *mark:vz-map*)\n'
    '             (setq *mark:vz-map* (mark:vz-map-build cls)))',
    "MarkZ vz map by frames")

# --- одиночная ячейка 195 у стойки: раскладка по оси/ригелю --------------
# После Т-половин (они уже 1100) одиночную ячейку, чья ширина не в шкале
# ригелей и отличается от ближайшего ригеля <=5.5 мм, тянем к торцу ригеля:
# 5800x195 -> 5795x200. 195 не резервируем — она исчезает из шкалы.
markz = sub_once(
    markz,
    '      (setq *mark:scale1* (mark:sec-scale1 msegs rblocks))\n'
    '      (if *mark:sec-diag* (mark:sec-diag vs hs vs2))\n'
    '      ;; Фильтр проёмов: зона сетки, лог, пропуск ячеек с окнами/дверями.\n'
    '      (foreach bb (mark:fill-filter-opening-cells boxes)',
    '      (setq *mark:scale1* (mark:sec-scale1 msegs rblocks))\n'
    '      (if *mark:sec-diag* (mark:sec-diag vs hs vs2))\n'
    '      ;; Одиночная ячейка у стойки: 195 -> 200 по ригелю (после Т-половин).\n'
    '      (setq boxes (mark:fill-single-rail-cells boxes hs))\n'
    '      ;; Фильтр проёмов: зона сетки, лог, пропуск ячеек с окнами/дверями.\n'
    '      (foreach bb (mark:fill-filter-opening-cells boxes)',
    "MarkZ single rail cells (grid)")
markz = sub_once(
    markz,
    '(mark:fill-equal-t-cells (reverse out) hs)',
    '(mark:fill-single-rail-cells (mark:fill-equal-t-cells (reverse out) hs) hs)',
    "MarkZ single rail cells (standalone)")

# --- обрыв цепочки атрибутов: не заходить в чужие ATTRIB -----------------
# Старый while после SEQEND шёл по всему чертежу и мог очистить атрибуты
# рамок «Атрибуты витража» (марка Витраж пропадала).
markz = sub_once(
    markz,
    '        (setq ed (entget e))\n'
    '        (if (and ed (= "ATTRIB" (cdr (assoc 0 ed))))\n'
    '          (progn\n'
    '            (if (assoc 1 ed)\n'
    '              (entmod (subst (cons 1 "") (assoc 1 ed) ed))\n'
    '              (entmod (append ed (list (cons 1 "")))))\n'
    '            (setq cnt (1+ cnt))))\n'
    '        (setq e (entnext e)))))',
    '        (setq ed (entget e))\n'
    '        (cond\n'
    '          ((null ed) (setq e nil))\n'
    '          ((= "ATTRIB" (cdr (assoc 0 ed)))\n'
    '           (progn\n'
    '             (if (assoc 1 ed)\n'
    '               (entmod (subst (cons 1 "") (assoc 1 ed) ed))\n'
    '               (entmod (append ed (list (cons 1 "")))))\n'
    '             (setq cnt (1+ cnt))\n'
    '             (setq e (entnext e))))\n'
    '          (t (setq e nil))))))',
    "MarkZ clear-attrs chain stop")

# --- раздельная маркировка: общий счётчик записанных марок ---------------
markz = sub_once(
    markz,
    '           (setq *mark:vz-map* (mark:vz-map-build-by-frames af ag))\n'
    '           (if (null *mark:vz-map*)\n'
    '             (setq *mark:vz-map* (mark:vz-map-build cls)))\n'
    '           (mark:main)\n'
    '           (setq geo-out *mark:fill-geo*))',
    '           (setq *mark:vz-map* (mark:vz-map-build-by-frames af ag))\n'
    '           (if (null *mark:vz-map*)\n'
    '             (setq *mark:vz-map* (mark:vz-map-build cls)))\n'
    '           (mark:main)\n'
    '           (setq *mark:written-total* *mark:written*)\n'
    '           (setq *mark:stem-total* *mark:cnt-stem*\n'
    '                 *mark:sand-total* *mark:cnt-sand*)\n'
    '           (setq geo-out *mark:fill-geo*))',
    "MarkZ written total joint")
markz = sub_once(
    markz,
    '             (mark:vz-set c ageo)\n'
    '             (mark:main)\n'
    '             (mark:vz-state-save c)',
    '             (mark:vz-set c ageo)\n'
    '             (mark:main)\n'
    '             (setq *mark:written-total*\n'
    '                   (if (numberp *mark:written-total*)\n'
    '                     (+ *mark:written-total* *mark:written*)\n'
    '                     *mark:written*))\n'
    '             (setq *mark:stem-total*\n'
    '                   (if (numberp *mark:stem-total*)\n'
    '                     (+ *mark:stem-total* *mark:cnt-stem*)\n'
    '                     *mark:cnt-stem*)\n'
    '                   *mark:sand-total*\n'
    '                   (if (numberp *mark:sand-total*)\n'
    '                     (+ *mark:sand-total* *mark:cnt-sand*)\n'
    '                     *mark:cnt-sand*))\n'
    '             (mark:vz-state-save c)',
    "MarkZ written total separate")
markz = sub_once(
    markz,
    '        (t\n'
    '         (mark:main)\n'
    '         (setq geo-out *mark:fill-geo*)))',
    '        (t\n'
    '         (mark:main)\n'
    '         (setq *mark:written-total* *mark:written*)\n'
    '         (setq *mark:stem-total* *mark:cnt-stem*\n'
    '               *mark:sand-total* *mark:cnt-sand*)\n'
    '         (setq geo-out *mark:fill-geo*)))',
    "MarkZ written total single")

# --- кэш проёмов сохраняется внутри пакета (раздельные витражи) ----------
markz = sub_once(
    markz,
    '  (setq *mark:opening-bbs*     nil\n'
    '        *mark:opening-loaded*  nil\n'
    '        *mark:opening-partial* nil\n'
    '        *mark:grid-zone*       nil))',
    '  (if (not *mark:reuse-sel*)\n'
    '    (setq *mark:opening-bbs*     nil\n'
    '          *mark:opening-loaded*  nil\n'
    '          *mark:opening-partial* nil\n'
    '          *mark:grid-zone*       nil)))',
    "MarkZ opening cache per package")

# --- раздельная маркировка: префикс витража из текущей рамки -------------
markz = sub_once(
    markz,
    '        *mark:vz-current-frame*\n'
    '          (if (and *mark:vz-by-frames* (cadr c)) (car (cadr c)) nil)\n'
    '        *mark:prefix*       ""\n'
    '        *mark:prefix-found* nil\n'
    '        out                 nil)',
    '        *mark:vz-current-frame*\n'
    '          (if (and *mark:vz-by-frames* (cadr c)) (car (cadr c)) nil)\n'
    '        *mark:prefix*\n'
    '          (if (and *mark:vz-by-frames* (cadr c))\n'
    '            (mark:vz-pfx-of (list (car (cadr c))))\n'
    '            "")\n'
    '        *mark:prefix-found* nil\n'
    '        out                 nil)\n'
    '  (setq *mark:prefix-found*\n'
    '        (if (and (mark:strp *mark:prefix*)\n'
    '                 (/= *mark:prefix* "")) t nil))',
    "MarkZ separate prefix")

# --- раздельная маркировка: геометрия считается один раз до деления -----
markz = sub_once(
    markz,
    '      (if (= *mark:vz-mode* "separate")\n'
    '        (progn\n'
    '          (setq cls (mark:vz-split af ag)\n'
    '                n (length cls))\n'
    '          (mark:out\n'
    '            (strcat "[INFO] Раздельная привязка по рамкам: витражей "\n'
    '                    (itoa n) " (" (mark:vz-join (mark:vz-names cls) ", ") ")."))))\n'
    '      (cond',
    '      (if (= *mark:vz-mode* "separate")\n'
    '        (progn\n'
    '          (setq cls (mark:vz-split af ag)\n'
    '                n (length cls))\n'
    '          (mark:out\n'
    '            (strcat "[INFO] Раздельная привязка по рамкам: витражей "\n'
    '                    (itoa n) " (" (mark:vz-join (mark:vz-names cls) ", ") ")."))))\n'
    '      ;; Готовые заполнения: общая геометрия считается один раз, до деления\n'
    '      ;; на витражи. Иначе локальный фильтр каркаса теряет общие стойки.\n'
    '      (if (null ageo)\n'
    '        (setq ageo (vl-catch-all-apply \'mark:sec-standalone nil)))\n'
    '      (cond',
    "MarkZ separate global geo")

# --- раздельная маркировка: локальная шкала MarkZ по витражам ------------
markz = sub_once(
    markz,
    '             (mark:vz-set c ageo)\n'
    '             (mark:main)\n'
    '             (setq *mark:written-total*',
    '             (mark:vz-set c ageo)\n'
    '             (mark:sec-local-scale)\n'
    '             (mark:main)\n'
    '             (setq *mark:written-total*',
    "MarkZ separate local scale call")

# --- реестр замечаний: перехват [WARN]/[ERROR] в эмиттерах ---------------
markz = sub_once(
    markz,
    '  (if (not *mark:probe-quiet*)\n'
    '    (progn\n'
    '      (mark:log-write msg)\n'
    '      (princ msg)))',
    '  (if (not *mark:probe-quiet*)\n'
    '    (progn\n'
    '      (mc:remark-capture msg)\n'
    '      (mark:log-write msg)\n'
    '      (princ msg)))',
    "MarkZ emit remark capture")
markv = sub_once(
    markv,
    '  (mk:log-write msg)\n'
    '  (princ msg)\n'
    '  nil)',
    '  (mc:remark-capture msg)\n'
    '  (mk:log-write msg)\n'
    '  (princ msg)\n'
    '  nil)',
    "MarkV emit remark capture")

# --- G01: координаты и handle заполнения вне ячеек -----------------------
markz = sub_once(
    markz,
    '        (if hit\n'
    '          (progn\n'
    '            (setq out (cons (cons e hit) out)\n'
    '                  geo-widths (cons (cons e hitw) geo-widths)))\n'
    '          (setq cnt (1+ cnt))))',
    '        (if hit\n'
    '          (progn\n'
    '            (setq out (cons (cons e hit) out)\n'
    '                  geo-widths (cons (cons e hitw) geo-widths)))\n'
    '          (progn\n'
    '            (mc:remark "G01" "WARN" "вне ячеек каркаса" e\n'
    '                       (if ins (car ins) nil)\n'
    '                       (if ins (cadr ins) nil))\n'
    '            (setq cnt (1+ cnt)))))',
    "MarkZ G01 details")
# Сводная строка — INFO: часть блоков получит спецмарку.
markz = sub_once(
    markz,
    '      (if (> cnt 0)\n'
    '        (progn\n'
    '          (mark:out\n'
    '            (strcat "[WARN] Вне ячеек каркаса: " (itoa cnt)\n'
    '                    " заполнени(й) — марка не формируется (причина в TEST 13)."))\n'
    '          (mark:note-warning)))',
    '      (if (> cnt 0)\n'
    '        (mark:out\n'
    '          (strcat "[INFO] Вне ячеек каркаса: " (itoa cnt)\n'
    '                  " заполнени(й) — проверка зон проёмов (Дв/Ок/Вр).")))',
    "MarkZ outside-cells info")

# --- спецмарки в mark:main (только вне цикла) ----------------------------
markz = sub_once(
    markz,
    '      (if *mark:precheck-ok*\n'
    '        (progn\n'
    '          (setq r (vl-catch-all-apply \'mark:apply-marks nil))\n'
    '          (if (or (vl-catch-all-error-p r) (null r))\n'
    '            (mark:report nil)\n'
    '            (mark:report t)))',
    '      (if *mark:precheck-ok*\n'
    '        (progn\n'
    '          (setq r (vl-catch-all-apply \'mark:apply-marks nil))\n'
    '          (if (or (vl-catch-all-error-p r) (null r))\n'
    '            (mark:report nil)\n'
    '            (progn\n'
    '              ;; Вне ячеек: Дв/Ок/Вр по зонам проёмов.\n'
    '              ;; В цикле внеячеечных заполнений нет — там не сканируем.\n'
    '              (if (not *mark:cycle-ss*) (mark:special-outside))\n'
    '              (mark:report t))))',
    "MarkZ special outside call")

# --- сброс реестра и счётчиков в mark:a-all ------------------------------
markz = sub_once(
    markz,
    '(defun mark:a-all (preset-kw / kw mode doc r cells pts new-fills)\n'
    '  (mark:log-start)',
    '(defun mark:a-all (preset-kw / kw mode doc r cells pts new-fills)\n'
    '  (mark:log-start)\n'
    '  ;; После отдельного МАРКАВ оставляем его замечания для общего отчёта.\n'
    '  (if (and (not *mark:cycle-ss*) (null *mk:last-groups*))\n'
    '    (mc:remark-reset))',
    "MarkZ remarks reset")
markz = sub_once(
    markz,
    '  (mark:reset-state)\n'
    '  (mark:out "========================================")\n'
    '  (mark:out (strcat " МАРКАЗАП — универсальный пакет  " *mark:rev*))',
    '  (mark:reset-state)\n'
    '  (setq *mark:run-written* nil *mark:run-stem* nil *mark:run-sand* nil\n'
    '        *mark:written-total* 0 *mark:stem-total* 0 *mark:sand-total* 0\n'
    '        *mark:special-dv* 0 *mark:special-ok* 0\n'
    '        *mark:special-vr* 0 *mark:special-left* 0\n'
    '        *mark:special-enames* nil\n'
    '        *mark:zone-list* nil)\n'
    '  (mark:out "========================================")\n'
    '  (mark:out (strcat " МАРКАЗАП — универсальный пакет  " *mark:rev*))',
    "MarkZ run counters reset")
require(markz.count('      (mark:vz-marks)') == 2,
        "ожидалось 2 вызова mark:vz-marks, найдено %d"
        % markz.count('      (mark:vz-marks)'))
markz = markz.replace('      (mark:vz-marks)',
                      '      (mark:vz-marks)\n      (mark:save-run-counters)')
markz = sub_once(
    markz,
    '  (setq *mark:reuse-sel*  nil\n'
    '        *mark:batch-undo* nil)\n'
    '  (mark:out (strcat "[LOG] Протокол: " (mark:log-path)))',
    '  (setq *mark:reuse-sel*  nil\n'
    '        *mark:batch-undo* nil)\n'
    '  ;; Одиночный пакет (не МАРКАВСЕ): отчёт и сводка.\n'
    '  (if (not *mark:cycle-ss*)\n'
    '    (progn\n'
    '      (mark:report-write)\n'
    '      (mark:remarks-summary)))\n'
    '  (mark:out (strcat "[LOG] Протокол: " (mark:log-path)))',
    "MarkZ standalone report")

# --- MarkV отдельно: свежий реестр ---------------------------------------
markv = sub_once(
    markv,
    '(defun mk:a-all (preset / ss keep doc n r pt0)\n'
    '  (mk:log-start)',
    '(defun mk:a-all (preset / ss keep doc n r pt0)\n'
    '  (mk:log-start)\n'
    '  (if (not *mk:cycle-tables-cleared*) (mc:remark-reset))',
    "MarkV remarks reset")

# --- спецмарка Дв/Ок/Вр — не считать «марка не разобрана» ----------------
markz = sub_once(
    markz,
    '           ((null pa)\n'
    '            (mark:out\n'
    '              (strcat "[WARN] Марка не разобрана (" mval\n'
    '                      ") — индекс будет от размера."))))',
    '           ((and (null pa) (member e *mark:special-enames*)) nil)\n'
    '           ((null pa)\n'
    '            (mark:out\n'
    '              (strcat "[WARN] Марка не разобрана (" mval\n'
    '                      ") — индекс будет от размера."))))',
    "MarkZ special mark row warning")

# --- итоговый счётчик марок: считать элементы, а не только атрибуты -------
require(markv.count(
    '                    lab (+ lab (caddr res))))\n'
    '            (setq tot (+ tot cnt))') == 2,
        "ожидалось 2 итоговых счётчика Маркав")
markv = markv.replace(
    '                    lab (+ lab (caddr res))))\n'
    '            (setq tot (+ tot cnt))',
    '                    lab (+ lab (caddr res))\n'
    '                    tot (+ tot (- (length (cadr item)) (cadr res)))))')

# --- марки элементов без атрибута -> ведомость ---------------------------
# mk:read-mark умеет брать марку из *mk:mark-map*, но карту никто не
# заполнял: элементы из мультилиний (Вт-1) попадали в ведомость с «—».
markv = sub_once(
    markv,
    '              plan (mk:group-plan-posts grp))\n'
    '        (if (null plan)',
    '              plan (mk:group-plan-posts grp))\n'
    '        (if plan (mk:mark-map-put plan))\n'
    '        (if (null plan)',
    "MarkV post mark map")
markv = sub_once(
    markv,
    '              plan (mk:group-plan-beams grp))\n'
    '        (if (null plan)',
    '              plan (mk:group-plan-beams grp))\n'
    '        (if plan (mk:mark-map-put plan))\n'
    '        (if (null plan)',
    "MarkV beam mark map")
# карта марок — своя на каждый прогон
markv = sub_once(
    markv,
    '  (mk:emit "\\n[МАРКАВСТ] Марки стоек по витражам...")\n'
    '  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)',
    '  (mk:emit "\\n[МАРКАВСТ] Марки стоек по витражам...")\n'
    '  (setq *mk:mark-map* nil)\n'
    '  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)',
    "MarkV mark map reset")

# --- м.п. считать от округлённой длины ----------------------------------
markv = sub_once(
    markv,
    '(defun mk:tab-mp (len cnt) (/ (* (float len) cnt) 1000.0))',
    '(defun mk:tab-mp (len cnt / l)\n'
    '  ;; В ведомости длина печатается целыми мм — от неё же считаем м.п.,\n'
    '  ;; иначе 1455.0 и 1454.9999 дают в соседних строках 1.46 и 1.45.\n'
    '  (setq l (fix (+ (float len) 0.5)))\n'
    '  (/ (* (float l) cnt) 1000.0))',
    "MarkV tab-mp rounding")

# --- совместная шкала: союз локальных probe, а не один общий проход ------
markv = sub_once(
    markv,
    '(defun mk:joint-length-scale (/ r)',
    ';; Общая шкала joint = объединение локальных шкал MarkZ. mark:num-keys —\n'
    ';; чистое объединение «ригели + ширины ячеек», поэтому союз по\n'
    ';; витражам даёт ту же шкалу, что один общий проход по всей выборке.\n'
    '(defun mk:scale-merge (a b / out v hit x)\n'
    '  (setq out a)\n'
    '  (foreach v b\n'
    '    (setq hit nil)\n'
    '    (foreach x out\n'
    '      (if (<= (abs (- x v)) *mk:tol-size*) (setq hit t)))\n'
    '    (if (null hit) (setq out (cons v out))))\n'
    '  (vl-sort out \'<))\n'
    '\n'
    '(defun mk:joint-probe-union (/ out ok grp r)\n'
    '  (setq out nil ok t)\n'
    '  (foreach grp *mk:last-groups*\n'
    '    (setq r (vl-catch-all-apply \'mark:scale-probe (list (nth 2 grp))))\n'
    '    (if (and (not (vl-catch-all-error-p r)) r)\n'
    '      (setq out (mk:scale-merge out r))\n'
    '      (setq ok nil)))\n'
    '  (if ok out nil))\n'
    '\n'
    '(defun mk:joint-length-scale (/ r)',
    "MarkV joint scale union")
markv = sub_once(
    markv,
    '    (progn\n'
    '      ;; Undefined function тоже перехватывается: MarkV можно загрузить\n'
    '      ;; отдельно, но тогда честно сообщаем о неполной fallback-шкале.\n'
    '      (setq r (vl-catch-all-apply \'mark:scale-probe (list *mk:last-scope*)))\n'
    '      (if (and (not (vl-catch-all-error-p r)) r)\n'
    '        (progn\n'
    '          (setq *mk:joint-scale* r\n'
    '                *mk:joint-scale-source* "MarkZ"\n'
    '                *mk:current-scale-source* "MarkZ")\n'
    '          (mk:emit (strcat "\\n  [INFO] Общая шкала MarkZ: "\n'
    '                           (itoa (length r)) " номеров (ригели + ячейки + резерв).")))\n'
    '        (progn\n'
    '          (setq *mk:joint-scale* (mk:joint-fallback-scale)\n'
    '                *mk:joint-scale-source* "fallback")\n'
    '          (mk:emit\n'
    '            "\\n  [WARN] MarkZ scale-probe недоступен или dynamic block требует Explode: использована только общая шкала ригелей; синхронизация не подтверждена.")))))',
    '    (progn\n'
    '      ;; Сначала общий проход — как в принятой редакции: он видит всю\n'
    '      ;; геометрию и потому даёт правильную шкалу. Если общий проход\n'
    '      ;; упёрся в динамический блок (read-only Explode запрещён),\n'
    '      ;; берём союз локальных probe по витражам.\n'
    '      (setq r (vl-catch-all-apply \'mark:scale-probe (list *mk:last-scope*)))\n'
    '      (if (or (null r) (vl-catch-all-error-p r))\n'
    '        (setq r (mk:joint-probe-union)))\n'
    '      (if (and (not (vl-catch-all-error-p r)) r)\n'
    '        (progn\n'
    '          (setq *mk:joint-scale* r\n'
    '                *mk:joint-scale-source* "MarkZ"\n'
    '                *mk:current-scale-source* "MarkZ")\n'
    '          (mk:emit (strcat "\\n  [INFO] Общая шкала MarkZ: "\n'
    '                           (itoa (length r)) " номеров.")))\n'
    '        (progn\n'
    '          (setq *mk:joint-scale* (mk:joint-fallback-scale)\n'
    '                *mk:joint-scale-source* "fallback")\n'
    '          (mk:emit\n'
    '            "\\n  [WARN] MarkZ scale-probe недоступен или dynamic block требует Explode: использована только общая шкала ригелей; синхронизация не подтверждена.")))))',
    "MarkV joint scale union fallback")

# --- таблица заполнений MarkZ не должна удаляться на этапе MarkV --------
markv = sub_once(
    markv,
    '  (if (and *mk:reuse-table-layer* (tblsearch "LAYER" *mk:layer-table*))',
    '  (if (and (not *mk:cycle-tables-cleared*)\n'
    '           *mk:reuse-table-layer* (tblsearch "LAYER" *mk:layer-table*))',
    "MarkV cycle table guard")

# --- пересечения имён между модулями недопустимы -------------------------
core_names = set(names(core))
markz_names = set(names(markz))
markv_names = set(names(markv))
require(not (core_names & markz_names), "общие defun ядро/MarkZ: %s" %
        sorted(core_names & markz_names))
require(not (core_names & markv_names), "общие defun ядро/MarkV: %s" %
        sorted(core_names & markv_names))
require(not (markz_names & markv_names), "общие defun MarkZ/MarkV: %s" %
        sorted(markz_names & markv_names))

HEADER = """;;;=====================================================================
;;;  MARK.lsp - ЕДИНАЯ СБОРКА MarkZ + MarkV
;;;  Состав: MARKCORE.lsp (ядро), MARKZ.lsp (заполнения), MarkV.lsp (каркас)
;;;  Собрано tools/build_single.py; править исходники, а не этот файл.
;;;=====================================================================
"""

TAIL = """
;;;=====================================================================
;;;  ЕДИНЫЙ ЦИКЛ: МАРКАВСЕ / MARKALL
;;;  Один выбор, один внешний UNDO; внутри — обычные пакеты модулей.
;;;=====================================================================
(setq *mark:cycle-ss* nil)
(setq *mk:cycle-tables-cleared* nil)

;; Набор SSGET -> список ename (MarkV ждёт список).
(defun mark:ss->enames (ss / i out)
  (setq i 0 out nil)
  (if ss
    (repeat (sslength ss)
      (setq out (cons (ssname ss i) out)
            i   (1+ i))))
  (reverse out))

;; Ближайшая длина ригеля из *mark:scale1* в пределах допуска.
(defun mark:nearest-rigel (w tol / best bd d)
  (setq best nil bd nil)
  (foreach s *mark:scale1*
    (if (numberp s)
      (progn
        (setq d (abs (- (float w) (float s))))
        (if (and (<= d tol) (or (null bd) (< d bd)))
          (setq bd d best s)))))
  best)

;; После Т-половин: одиночную ячейку, чья ширина не в шкале ригелей,
;; тянем к торцу ближайшего ригеля (край стойки 5 мм). 195 -> 200.
(defun mark:fill-single-rail-cells (boxes hs / out bb x0 y0 x1 y1 secw tierh
                                         bw L r e ey hw)
  (setq out nil)
  (foreach bb boxes
    (setq x0    (nth 0 bb)
          y0    (nth 1 bb)
          x1    (nth 2 bb)
          y1    (nth 3 bb)
          secw  (nth 4 bb)
          tierh (nth 5 bb)
          bw    (mark:round1 (- (float x1) (float x0)))
          L     nil)
    (if (and (numberp bw) (> bw 0.0)
             (not (mark:index-of *mark:scale1* bw)))
      (setq L (mark:nearest-rigel bw 5.5)))
    (if L
      (progn
        (setq r nil)
        (foreach e hs
          (if (and (null r)
                   (numberp (nth 0 e)) (numberp (nth 1 e)) (numberp (nth 2 e))
                   (<= (abs (- (- (float (nth 2 e)) (float (nth 1 e)))
                               (float L))) 0.6)
                   (<= (float (nth 1 e)) (+ (float x0) 5.6))
                   (>= (float (nth 2 e)) (- (float x1) 5.6)))
            (progn
              (setq ey (float (nth 0 e))
                    hw (if (numberp (nth 3 e)) (float (nth 3 e)) 0.0))
              (if (or (<= (abs (- ey (float y0))) 0.6)
                      (<= (abs (- ey (float y1))) 0.6)
                      (and (> hw 0.0)
                           (or (<= (abs (- (+ ey hw) (float y0))) 0.6)
                               (<= (abs (- (- ey hw) (float y1))) 0.6))))
                (setq r e)))))
        (if r
          (setq bb (list (float (nth 1 r)) y0 (float (nth 2 r)) y1 L tierh)))))
    (setq out (cons bb out)))
  (reverse out))

;; В AutoLISP `or` возвращает T/nil, а не значение: выбираем группы явно.
(defun mark:vz-split-frame-first (fills glazings / r)
  (setq r (mark:vz-frame-split fills glazings))
  (if r r (mark:vz-split-geometric fills glazings)))

;; Префикс каждого заполнения — по его собственной рамке. Приоритет у точки
;; вставки (она всегда внутри своего витража), затем площадь пересечения,
;; затем ближайшая рамка. Это разделяет витражи через общую стойку.
(defun mark:vz-map-build-by-frames
  (fills glazings / frames map e pt bb best besta ar bd d hits)
  (setq frames (mark:vz-frame-list glazings)
        map    nil)
  (foreach e fills
    (setq pt   (cdr (assoc 10 (entget e)))
          hits nil)
    (foreach fr frames
      (if (and pt
               (<= (nth 0 (cadr fr)) (car pt))
               (<= (car pt) (nth 2 (cadr fr)))
               (<= (nth 1 (cadr fr)) (cadr pt))
               (<= (cadr pt) (nth 3 (cadr fr))))
        (setq hits (cons fr hits))))
    (setq best nil besta 0.0)
    (if hits
      (progn
        (setq bb (mark:vz-bb-of e))
        (foreach fr hits
          (setq ar (mark:vz-overlap-area bb (cadr fr)))
          (if (> ar besta) (setq besta ar best fr)))
        (if (null best) (setq best (car hits))))
      (progn
        (setq bb (mark:vz-bb-of e))
        (foreach fr frames
          (setq ar (mark:vz-overlap-area bb (cadr fr)))
          (if (> ar besta) (setq besta ar best fr)))
        (if (null best)
          (progn
            (setq bd nil)
            (foreach fr frames
              (setq d (mark:vz-dist pt (cadr fr)))
              (if (or (null bd) (< d bd)) (setq bd d best fr)))))))
    (if best
      (setq map (cons (cons e (caddr best)) map))))
  (reverse map))


;; ---- локальная шкала MarkZ в раздельном режиме ------------------------
;; Геометрия ячеек общая, но длины ригелей — только текущего витража.
(defun mark:sec-local-scale (/ ss i e ed typ nm r msegs rblocks)
  (setq ss (if *mark:carc-cache* (mark:carc-ss) *mark:cycle-ss*))
  (if ss
    (progn
      (setq ss (mark:vz-carcass-filter ss)
            msegs nil rblocks nil)
      (if ss
        (progn
          (setq i (sslength ss))
          (while (> i 0)
            (setq i (1- i)
                  e (ssname ss i)
                  ed (entget e)
                  typ (if ed (cdr (assoc 0 ed)) nil))
            (cond
              ((= typ "MLINE")
               (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
               (if (and r (not (vl-catch-all-error-p r)))
                 (setq msegs (append r msegs))))
              ((= typ "INSERT")
               (setq nm (mark:fill-eff-name e))
               (if (and (mark:strp nm)
                        (wcmatch (strcase nm) "*РИГЕЛЬ*"))
                 (setq rblocks (cons e rblocks))))))
          (setq *mark:scale1* (mark:sec-scale1 msegs rblocks))))))
  *mark:scale1*)

;; ---- счётчики для отчёта ---------------------------------------------
(setq *mark:run-written* nil)
(setq *mark:run-stem* nil)
(setq *mark:run-sand* nil)
(setq *mark:written-total* nil)
(setq *mark:stem-total* nil)
(setq *mark:sand-total* nil)

(defun mark:save-run-counters ()
  (setq *mark:run-written* (if (numberp *mark:written-total*)
                              *mark:written-total*
                              *mark:written*)
        *mark:run-stem*    (if (numberp *mark:stem-total*)
                              *mark:stem-total*
                              *mark:cnt-stem*)
        *mark:run-sand*    (if (numberp *mark:sand-total*)
                              *mark:sand-total*
                              *mark:cnt-sand*))
  nil)

;; ---- спецмарки Дв/Ок/Вр (только заполнениям) --------------------------
(setq *mark:special-dv*   0)
(setq *mark:special-ok*   0)
(setq *mark:special-vr*   0)
(setq *mark:special-left* 0)
(setq *mark:special-enames* nil)
(setq *mark:vent-masks*   '("*ВЕНТРЕШ*" "*ВЕНТ*"))
(setq *mark:zone-list*    nil)

(defun mark:special-mark? (m / s n tail)
  (setq s (mark:trim (if (mark:strp m) m ""))
        n (strlen s))
  ;; Проверяем ровно последние два символа. В CP1251 кириллица однобайтовая,
  ;; а wcmatch со звёздочкой в некоторых сборках не ловит кириллицу.
  (setq tail (if (> n 1) (substr s (- n 1) 2) s))
  (member tail '("Дв" "Ок" "Вр")))

(defun mark:zone-kind (name / nm)
  (setq nm (strcase (if (mark:strp name) name "")))
  (cond
    ((wcmatch nm "*ДВЕР*") "Дв")
    ((or (wcmatch nm "*ОКН*") (wcmatch nm "*СТВОР*")) "Ок")
    (t nil)))

(defun mark:opening-zones (/ doors wins kind)
  (if (not *mark:opening-loaded*) (mark:fill-load-openings))
  (setq doors nil wins nil)
  (foreach rec *mark:opening-bbs*
    (setq kind (mark:zone-kind (car rec)))
    (if (cadr rec)
      (cond
        ((= kind "Дв") (setq doors (cons (list "Дв" (cadr rec)) doors)))
        ((= kind "Ок") (setq wins  (cons (list "Ок" (cadr rec)) wins))))))
  (append doors wins))

(defun mark:vent-name? (nm)
  (or (vl-string-search "ВЕНТ" nm)
      (vl-string-search "вент" nm)
      (vl-string-search "Вент" nm)))

(defun mark:vent-zones (/ ss i e ed nm bb hit out)
  (setq out nil i 0)
  ;; Быстрый фильтр по имени блока: не перебираем все INSERT чертежа.
  (setq ss (ssget "_X" '((0 . "INSERT") (2 . "*ВЕНТ*"))))
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i) i (1+ i)
            ed (entget e)
            nm (if ed (mark:fill-eff-name e) nil)
            hit nil)
      (if (mark:strp nm)
        (progn
          (if (mark:vent-name? nm) (setq hit t))
          (foreach m *mark:vent-masks*
            (if (and (null hit) (wcmatch (strcase nm) (strcase m)))
              (setq hit t)))))
      (if hit
        (progn
          (setq bb (mark:fill-vla-bb e))
          (if bb (setq out (cons (list "Вр" bb) out)))))))
  out)

(defun mark:prefix-at (e / p)
  (setq p (cdr (assoc e *mark:vz-map*)))
  (if p p
    (if *mark:vz-current-frame*
      (mark:vz-pfx-of (list *mark:vz-current-frame*))
      *mark:prefix*)))

(defun mark:special-outside (/ zones cands obj e ins bb best besta ar kind pfx txt n)
  (setq *mark:special-left* 0)
  (setq cands nil)
  (foreach e *mark:fills*
    (if (not (assoc e *mark:fill-geo*)) (setq cands (cons e cands))))
  (setq cands (reverse cands)
        zones (append (mark:opening-zones) (mark:vent-zones)))
  (setq *mark:zone-list* zones)
  (foreach e cands
    (setq obj (mark:vla e)
          ins (cdr (assoc 10 (entget e)))
          bb  (mark:fill-e-bb e)
          best nil besta 0.0)
    (if (and obj ins bb)
      (progn
        (foreach z zones
          (setq ar (mark:vz-overlap-area bb (cadr z)))
          (if (> ar besta) (setq besta ar best z)))
        (if best
          (progn
            (setq kind (car best)
                  pfx  (mark:prefix-at e))
            (if (not (mark:strp pfx)) (setq pfx ""))
            (setq txt (if (= pfx "") kind (strcat pfx " " kind)))
            (if (mark:ar-set-attr obj *mark:attr-mark* txt)
              (progn
                (if (/= pfx "")
                  (mark:ar-set-attr obj *mark:attr-vitrage* pfx))
                (mark:ar-set-attr obj *mark:ar-attr* "")
                (mc:remark-drop "G01" e)
                (mc:remark (cond ((= kind "Дв") "D01")
                                 ((= kind "Ок") "O01")
                                 (t "R01"))
                           "INFO" (strcat "марка " txt)
                           e (car ins) (cadr ins))
                (setq *mark:special-enames*
                      (cons e *mark:special-enames*))
                (cond
                  ((= kind "Дв") (setq *mark:special-dv* (1+ *mark:special-dv*)))
                  ((= kind "Ок") (setq *mark:special-ok* (1+ *mark:special-ok*)))
                  (t (setq *mark:special-vr* (1+ *mark:special-vr*)))))))))))
  (setq n (length cands)
        *mark:special-left*
        (- n (+ *mark:special-dv* *mark:special-ok* *mark:special-vr*)))
  (if (> n 0)
    (progn
      (mark:out
        (strcat "[INFO] Вне ячеек: " (itoa n)
                " — Дв " (itoa *mark:special-dv*)
                ", Ок " (itoa *mark:special-ok*)
                ", Вр " (itoa *mark:special-vr*)))
      (if (> *mark:special-left* 0)
        (progn
          (mark:out
            (strcat "[WARN] Вне ячеек и без зоны проёма: "
                    (itoa *mark:special-left*)))
          (mark:note-warning)))))
  nil)

;; ---- отчёт ------------------------------------------------------------
(defun mark:report-path ()
  (mc:log-path
    (strcat (vl-filename-base (getvar "dwgname")) " Отчёт.txt")))

(defun mark:report-stats (/ out d posts beams z dv ok vr ngeo grp
                            written stem sand)
  (setq out nil posts 0 beams 0)
  (setq grp (if *mk:last-groups*
              (mapcar 'car *mk:last-groups*)
              (mark:vz-map-pfxs)))
  (if (and (null grp) (mark:strp *mark:prefix*) (/= *mark:prefix* ""))
    (setq grp (list *mark:prefix*)))
  (setq out (list (list "Витражи"
                        (if grp (mark:vz-join grp ", ") "—"))))
  (if *mk:last-groups*
    (progn
      (foreach g *mk:last-groups*
        (setq d (mk:group-data g))
        (setq posts (+ posts (length (cdr (assoc 'POSTS d))))
              beams (+ beams (length (cdr (assoc 'BEAMS d))))))
      (setq out (append out
                  (list (list "Стойки" (itoa posts))
                        (list "Ригели" (itoa beams)))))
      (foreach r *mk:last-acc*
        (setq out (append out
                    (list (list (strcat (if (mark:strp (nth 0 r)) (nth 0 r) "?")
                                        (if (and (mark:strp (nth 1 r))
                                                 (> (strlen (nth 1 r)) 0))
                                          (strcat " " (nth 1 r)) ""))
                                (itoa (nth 2 r)))))))))
  (setq z (if *mark:zone-list*
            *mark:zone-list*
            (append (mark:opening-zones) (mark:vent-zones)))
        dv 0 ok 0 vr 0)
  (setq *mark:zone-list* z)
  (foreach x z
    (cond
      ((= (car x) "Дв") (setq dv (1+ dv)))
      ((= (car x) "Ок") (setq ok (1+ ok)))
      (t (setq vr (1+ vr)))))
  (setq ngeo (if *mark:fill-geo* (length *mark:fill-geo*) 0))
  (setq written (if (numberp *mark:run-written*) *mark:run-written*
                    (if (numberp *mark:written*) *mark:written* 0))
        stem    (if (numberp *mark:run-stem*) *mark:run-stem*
                    (if (numberp *mark:cnt-stem*) *mark:cnt-stem* 0))
        sand    (if (numberp *mark:run-sand*) *mark:run-sand*
                    (if (numberp *mark:cnt-sand*) *mark:cnt-sand* 0)))
  (setq out (append out
    (list (list "Заполнения" (itoa (length *mark:fills*)))
          (list "  в ячейках" (itoa ngeo))
          (list "  вне ячеек" (itoa (max 0 (- (length *mark:fills*) ngeo))))
          (list "Двери" (itoa dv))
          (list "Окна" (itoa ok))
          (list "Вентрешётки" (itoa vr))
          (list "Спецмарки Дв" (itoa *mark:special-dv*))
          (list "Спецмарки Ок" (itoa *mark:special-ok*))
          (list "Спецмарки Вр" (itoa *mark:special-vr*))
          (list "Стемалит" (itoa stem))
          (list "Сэндвич" (itoa sand))
          (list "Марок записано"
                (itoa (+ written *mark:special-dv*
                         *mark:special-ok* *mark:special-vr*)))
          (list "Ошибки" (itoa (mc:remark-count-sev "ERROR")))
          (list "Предупреждения" (itoa (mc:remark-count-sev "WARN"))))))
  out)

(defun mark:report-write-lines (path lines / f)
  (setq f (vl-catch-all-apply 'open (list path "w")))
  (if (or (null f) (vl-catch-all-error-p f))
    nil
    (progn
      (foreach l lines (write-line l f))
      (close f)
      t)))

(defun mark:rec-handle (e / d)
  (setq d (if e (entget e) nil))
  (if (and d (cdr (assoc 5 d))) (cdr (assoc 5 d)) "—"))

(defun mark:report-lines (/ out r n codes c)
  (setq out (list (strcat "=== ОТЧЁТ ПРОГОНА: " (getvar "DWGNAME") " ===")
                  (strcat "MarkZ " *mark:rev* " / " *mark:build*
                          " + MarkV " *mk:ver* " / " *mk:build*)
                  ""))
  (foreach r (mark:report-stats)
    (setq out (append out (list (strcat (car r) "; " (cadr r))))))
  (setq codes nil)
  (foreach r *mc:remarks*
    (if (not (member (car r) codes)) (setq codes (cons (car r) codes))))
  (setq out (append out
              (list "" "ИТОГИ ПО КОДАМ" "Код;Количество;Пояснение")))
  (foreach c (reverse codes)
    (setq out (append out
                (list (strcat c ";"
                              (itoa (mc:remark-count c)) ";"
                              (if (mc:remark-code-name c)
                                (mc:remark-code-name c) ""))))))
  (setq out (append out
              (list "" "ЗАМЕЧАНИЯ"
                    "№;Код;Важность;Объект;X;Y;Причина")))
  (setq n 0)
  (foreach r (reverse *mc:remarks*)
    (setq n (1+ n))
    (setq out (append out
                (list (strcat (itoa n) ";"
                              (car r) ";"
                              (cadr r) ";"
                              (mark:rec-handle (nth 3 r))
                              ";"
                              (if (and (numberp (nth 4 r)) (numberp (nth 5 r)))
                                (strcat (rtos (nth 4 r) 2 0) ","
                                        (rtos (nth 5 r) 2 0))
                                "") ";"
                              (nth 2 r))))))
  out)

(defun mark:report-write (/ path)
  (setq path (mark:report-path))
  (if (mark:report-write-lines path (mark:report-lines))
    (mark:emit (strcat (chr 10) "[OK] Отчёт: " path))
    (mark:emit (strcat (chr 10) "[WARN] Отчёт не записан: " path)))
  path)

(defun mark:remarks-summary (/ errs warns infos codes stats r c)
  (setq errs 0 warns 0 infos 0 codes nil stats (mark:report-stats))
  (foreach r *mc:remarks*
    (cond
      ((= (cadr r) "ERROR") (setq errs (1+ errs)))
      ((= (cadr r) "WARN")  (setq warns (1+ warns)))
      (t (setq infos (1+ infos))))
    (if (not (member (car r) codes)) (setq codes (cons (car r) codes))))
  (setq *mc:remark-hold* t)
  (mark:emit (strcat (chr 10) "========================================"))
  (mark:emit (strcat (chr 10) " ОТЧЁТ ПРОГОНА"))
  (mark:emit (strcat (chr 10) "========================================"))
  (foreach r stats
    (mark:emit (strcat (chr 10) (mark:pad-line (car r) (cadr r)))))
  (mark:emit (strcat (chr 10) "----------------------------------------"))
  (mark:emit (strcat (chr 10) " [ERROR] " (itoa errs)
                     "   [WARN] " (itoa warns)
                     "   [INFO] " (itoa infos)))
  (foreach c (reverse codes)
    (mark:emit (strcat (chr 10) "   " (mark:rjust c 5) "  "
                       (itoa (mc:remark-count c))
                       (if (mc:remark-code-name c)
                         (strcat "  — " (mc:remark-code-name c)) ""))))
  (mark:emit (strcat (chr 10) " Отчёт: " (mark:report-path)))
  (mark:emit (strcat (chr 10) "========================================" (chr 10)))
  (setq *mc:remark-hold* nil)
  nil)

(defun mark:cycle-body (enames)
  ;; Сначала заполнения: MarkZ строит шкалу с З-ширинами (195).
  (mark:emit (strcat (chr 10) "===== [1/2] МАРКАЗАП — заполнения ====="))
  (mark:a-all "Сетка")
  ;; Добавляем вставленные заполнения в выборку MarkV: probe MarkZ должен
  ;; видеть их, иначе теряется З-ширина 195 и номера ригелей сдвигаются на 1.
  (setq enames (append enames *mark:fills*))
  (mark:emit (strcat (chr 10) "===== [2/2] МАРКАВ — каркас ====="))
  (mk:a-all enames))

(defun mark:all-cycle (/ ss enames doc r)
  (mc:remark-reset)
  (mark:emit (strcat (chr 10) "[МАРКАВСЕ] Полный цикл: каркас -> заполнения -> ведомости."))
  (setq ss (ssget))
  (if (null ss)
    (mark:emit (strcat (chr 10) "[INFO] Выбор отменён — цикл прерван."))
    (progn
      (setq *mark:cycle-ss* ss
            *mark:reuse-sel* nil
            enames (mark:ss->enames ss)
            doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (mark:ax-invoke-ok doc "StartUndoMark" nil))
      ;; Таблицы прошлых прогонов убираем ОДИН раз. MarkV не должен
      ;; удалять таблицу заполнений, вставленную MarkZ.
      (setq *mk:reuse-table-layer* t)
      (mk:clear-old-tables)
      (setq *mk:cycle-tables-cleared* t)
      (setq r (vl-catch-all-apply 'mark:cycle-body (list enames)))
      (setq *mark:cycle-ss* nil
            *mk:cycle-tables-cleared* nil)
      (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))
      (if (vl-catch-all-error-p r)
        (mark:emit (strcat (chr 10) "[ERROR] МАРКАВСЕ: "
                           (vl-catch-all-error-message r)))
        (mark:emit (strcat (chr 10) "[OK] МАРКАВСЕ завершён. Откат всего цикла — один U.")))
      (mark:report-write)
      (mark:remarks-summary)
      ;; метка цикла в оба журнала (модули уже создали их)
      (mc:log-append
        (mark:log-path)
        (strcat "=== MARK.lsp: MarkZ " *mark:rev* " / " *mark:build*
                " + MarkV " *mk:ver* " / " *mk:build* " ==="))
      (mc:log-append
        (mk:log-path)
        (strcat "=== MARK.lsp: MarkZ " *mark:rev* " / " *mark:build*
                " + MarkV " *mk:ver* " / " *mk:build* " ==="))))
  (princ))

(defun c:МАРКАВСЕ () (mark:all-cycle))
(defun c:MARKALL () (mark:all-cycle))

;;;=====================================================================
;;;  СБОРКА: ядро и оба модуля загружены
;;;=====================================================================
(setq *mc:core-ready* t)

(mark:emit (strcat (chr 10) "[MARK] Сборка MARKCORE " (mc:core-version)
                   " + MarkZ " *mark:rev* " / " *mark:build*
                   " + MarkV " *mk:ver* " / " *mk:build*))
(mark:help-border)
(mark:help-header)
(mark:help-row "МАРКАВСЕ / MARKALL" "Полный цикл: всё за один запуск")
(mark:help-row "МАРКАЗАП / MARKA" "Заполнения: полный цикл")
(mark:help-row "МАРКАЗАПБЛОК" "Только раскладка по сетке")
(mark:help-row "МАРКАРОВКАЗАП / MARKZ" "Только марки заполнений")
(mark:help-row "МАРКАЗАПРЯД" "Только рядовка")
(mark:help-row "МАРКАЗАПТАБЛ" "Только ведомость заполнений")
(mark:help-row "МАРКАВ" "Каркас: полный цикл")
(mark:help-row "МАРКАВГЕОМЕТРИЯ" "Этап 1: сбор и 2D-модель")
(mark:help-row "МАРКАВСТ / МАРКАВРГ" "Этапы 2 и 3: стойки / ригели")
(mark:help-row "МАРКАВТАБЛ" "Этап 4: ведомость профилей")
(mark:help-row "МАРКАВДИАГНОЗОПТОМ" "Диагностика объектов каркаса")
(mark:help-border)
(princ)
"""

REGISTRY = r"""
;;;=====================================================================
;;;  РЕЕСТР ЗАМЕЧАНИЙ (MarkZ + MarkV). Только mc:*.
;;;=====================================================================
(setq *mc:remarks* nil)
(setq *mc:remark-hold* nil)

(defun mc:remark (code sev text ename x y)
  ;; Read-only probe MarkZ не должен добавлять замечания в отчёт.
  (if (not *mark:probe-quiet*)
    (setq *mc:remarks*
          (cons (list code sev text ename x y) *mc:remarks*)))
  nil)

(defun mc:remark-reset ()
  (setq *mc:remarks* nil)
  nil)

(defun mc:remark-drop (code ename / out r)
  (setq out nil)
  (foreach r *mc:remarks*
    (if (not (and (= (car r) code) (eq (nth 3 r) ename)))
      (setq out (cons r out))))
  (setq *mc:remarks* (reverse out))
  nil)

(defun mc:remark-count (code / n r)
  (setq n 0)
  (foreach r *mc:remarks*
    (if (= (car r) code) (setq n (1+ n))))
  n)

(defun mc:remark-count-sev (sev / n r)
  (setq n 0)
  (foreach r *mc:remarks*
    (if (= (cadr r) sev) (setq n (1+ n))))
  n)

(defun mc:remark-code (text / s)
  (setq s (strcase text))
  (cond
    ((vl-string-search "ВНЕ ЯЧЕЕК" s) "G01S")
    ((vl-string-search "ВНЕ СЕТКИ" s) "G02")
    ((vl-string-search "ДУБЛИКАТ" s) "DUP")
    ((vl-string-search "ПЕРЕКРЫТИЕ" s) "OVL")
    ((vl-string-search "2 БЛОКА" s) "V01")
    ((vl-string-search "НЕ РАЗОБРАНА" s) "M01")
    ((vl-string-search "МАРКА\" ПУСТ" s) "M02")
    ((vl-string-search "XLS" s) "X01")
    ((vl-string-search "SCALE-PROBE" s) "SCL")
    ((vl-string-search "НЕ ПОЛНОСТЬЮ ЗАПОЛНЯЕТ" s) "OPN")
    (t "GEN")))

(defun mc:remark-code-name (code)
  (cond
    ((= code "G01")  "вне ячеек каркаса")
    ((= code "G01S") "вне ячеек без зоны проёма")
    ((= code "G02")  "вне сетки MarkV")
    ((= code "DUP")  "дубликаты")
    ((= code "OVL")  "перекрытия")
    ((= code "V01")  "два блока «Атрибуты витража»")
    ((= code "M01")  "марка не разобрана")
    ((= code "M02")  "атрибут «Марка» пуст")
    ((= code "X01")  "XLS/отчёт не записаны")
    ((= code "SCL")  "шкала MarkV без подтверждения MarkZ")
    ((= code "OPN")  "проём не полностью покрывает ячейку")
    ((= code "GEN")  "прочее замечание прогона")
    ((= code "D01")  "спецмарка Дв")
    ((= code "O01")  "спецмарка Ок")
    ((= code "R01")  "спецмарка Вр")
    (t nil)))

(defun mc:remark-capture (msg / s)
  (if (and (not *mc:remark-hold*) (mc:strp msg))
    (progn
      (setq s (mc:trim msg))
      (cond
        ((wcmatch s "`[ERROR]*") (mc:remark (mc:remark-code s) "ERROR" s nil nil nil))
        ((wcmatch s "`[WARN]*")  (mc:remark (mc:remark-code s) "WARN"  s nil nil nil)))))
  msg)
"""

text = HEADER + REGISTRY + core + markz + markv + TAIL

# --- производственная сборка: без разработческих команд и внешних файлов ---
# Список команд, которые остаются в поставке:
#   МАРКАВСЕ/MARKALL, МАРКАВ, МАРКАВГЕОМЕТРИЯ, МАРКАВСТ, МАРКАВРГ,
#   МАРКАВТАБЛ, МАРКАЗАП/MARKA, МАРКАЗАПБЛОК/MARKFILL, МАРКАРОВКАЗАП/MARKZ,
#   МАРКАЗАПРЯД/MARKAR, МАРКАЗАПТАБЛ/MARKTABLE,
#   МАРКАВДИАГНОЗОПТОМ/MARKVBULKDIAG.
PROD_DROP_CMDS = [
    "c:МАРКАЗАПТЕСТ",
    "c:МАРКАЗАПГЕО",
    "c:МАРКАЗАПКОПИИ",
    "c:МАРКАДИАГ",
    "c:SNAPSHOT",
    "c:МАРКАВДИАГНОЗ",
    "c:МАРКАВСКОБКИ",
    "c:MARKVGEOMDIAG",
    "c:MARKVSUBCELLS",
    "c:MARKVFRAME",
    "c:MARKVSYNC",
    "c:MARKVFRAMEASSIGN",
    "c:MARKVFRAMEINFO",
    "c:Сетка-мультилинии",
    "c:Точка-мультилинии",
    "c:Сетка-динамика",
    "c:Точка-динамика",
    "c:Полилинии",
]
for name in PROD_DROP_CMDS:
    text = strip_form(text, "(defun " + name + " ", "команда " + name)

# В исходниках есть по два одинаковых определения — оставляем первое.
for name in ("c:МАРКАРОВКАЗАП", "c:МАРКАЗАПБЛОК", "c:MARKFILL"):
    text = strip_form(text, "(defun " + name + " ", "дубль " + name, 2)

# Помощники, которые существовали только для поиска исходников, снимков,
# разработческих команд и старых проверок. В рабочем цикле не вызываются.
PROD_DROP_FUNCS = [
    # поиск исходников / снимок / базы
    "mark:snapshot-path", "mark:file-open?", "mark:find-source",
    "mk:find-source", "mk:check-brackets", "mk:ext-checker?",
    # команды разработки
    "mark:diag-name", "mark:spds?", "mark:copies-make-group",
    "mark:test-insert", "mark:test-make-block", "mark:test-purge",
    "mk:diag-block", "mk:draw-test-model", "mk:dump-data",
    # старые неиспользуемые утилиты
    "mark:ar-sort-by", "mark:fill-bb-area", "mark:fill-block-segs-at",
    "mark:fill-bname-local", "mark:fill-hit-area", "mark:fill-hits-merge",
    "mark:fill-pick-insert", "mark:fill-seg-components", "mark:fill-set-near",
    "mark:hex-val", "mark:sort-heights", "mark:sort-widths",
    "mk:get-vitrage-prefix", "mk:scope-ss", "mk:tab-prefix",
    "mk:validate-beam", "mk:validate-fill", "mk:vitrage-rec-from-frame",
    # хвосты после удаления диагностики и тестовой ячейки
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
    # дубли help-функций MarkV (баннер использует mark:help-*)
    "mk:help-border", "mk:help-header", "mk:help-row",
]
for name in PROD_DROP_FUNCS:
    text = strip_form(text, "(defun " + name + " ", "функция " + name)

text = strip_form(text, "(setq *mark:source-candidates*", "кандидаты исходника")
text = strip_form(text, "(setq *mark:source-file* nil)", "путь исходника")

# Внешние базы MarkV-bases.lsp: секция 20b целиком и её настройка.
text = cut_between(text, ";;; 20b. ВНЕШНИЕ БАЗЫ",
                   ";;; 20c. MarkZ/MarkV", "MarkV external bases")
text = sub_once(
    text,
    ";;;   Внешние базы кладутся в файл MarkV-bases.lsp рядом с модулем\n"
    ";;;   (или в любую папку из путей поиска AutoCAD) и подхватываются при загрузке.\n"
    ";;;   Формат файла базы:\n"
    ";;;     (mk:register-base \"АЛЮТЕХ\" '((\"ALT-W72-01\" . 72.0) (\"ALT-W72-05\" . 116.0)))\n"
    ";;;     (setq *mk:article-prefixes* (cons \"ALT\" *mk:article-prefixes*))\n",
    ";;;   Базы можно зарегистрировать из Lisp: (mk:register-base \"ИМЯ\" '(...)).\n",
    "MarkV bases comment")
text = sub_once(text,
                '(setq *mk:bases-file* "MarkV-bases.lsp")   ; файл внешних баз\n',
                "", "MarkV bases file variable")

# --- чистка комментариев от удалённых механизмов ------------------------
text = sub_once(
    text,
    ";;;--------------------- Исходник MARKZ.lsp (путь для снимка) ---------\n",
    "", "MarkZ source comment")
text = sub_once(
    text,
    ";; Не привязан к одному диску: findfile (пути поддержки) и явные кандидаты.\n",
    "", "findfile comment")
text = cut_between(text, ";;; 0. АВТОПРОВЕРКА СКОБОК", ";;; 1. КОНФИГУРАЦИЯ",
                   "MarkV bracket section")
text = sub_once(
    text,
    "Ред. 48.17 по данным МАРКАЗАПГЕО",
    "Ред. 48.17 по данным диагностики секций",
    "МАРКАЗАПГЕО comment")
text = sub_once(
    text,
    "командой МАРКАЗАПГЕО, печатается один раз и сбрасывает флаг сама.",
    "флагом *mark:sec-diag*, печатается один раз и сбрасывает флаг сама.",
    "МАРКАЗАПГЕО comment 2")
text = sub_once(
    text,
    ";;;  Проверка скобок: AutoExtraction, CHKALL / CHKFILE (вручную).",
    ";;;  Проверка скобок: внешний AutoExtraction (CHKALL / CHKFILE).",
    "MarkZ header CHKALL comment")

raw = text.replace("\r\n", "\n").replace("\n", "\r\n").encode("cp1251")

# --- самопроверки сборки -------------------------------------------------
require(b"\xef\xbb\xbf" not in raw, "BOM в начале файла")
require(b"\xef\xbf\xbd" not in raw, "в сборке есть символ U+FFFD (битая кодировка)")
require(b"(fboundp" not in raw, "fboundp в сборке")
require(raw.count(b"\n(vl-load-com)") == 1,
        "верхнеуровневый (vl-load-com) должен быть один")
require(b"(mark:core-ensure)" not in raw and b"(mk:core-ensure)" not in raw,
        "загрузчик ядра остался в сборке")
require(b"(mk:load-bases)" not in raw, "внешние базы MarkV не отключены")
require(b"(findfile" not in raw, "остался поиск внешних файлов")
require(b"MarkV-bases" not in raw, "осталось имя внешней базы MarkV")
for name in PROD_DROP_CMDS:
    require(("(defun " + name + " ").encode("cp1251") not in raw,
            "разработческая команда не удалена: " + name)
require(b"sec-reserve" not in raw, "резерв Т-половин остался в сборке")
require(b"(defun mk:a-all (" in raw, "mk:a-all отсутствует в сборке")
require(b"(defun c:\xcc\xc0\xd0\xca\xc0\xc2 () (mk:a-all nil))" in raw,
        "обёртка c:МАРКАВ отсутствует в сборке")
require(b"(defun c:\xcc\xc0\xd0\xca\xc0\xc2\xd1\xc5 () (mark:all-cycle))" in raw,
        "команда c:МАРКАВСЕ отсутствует в сборке")
require(b"(defun c:MARKALL () (mark:all-cycle))" in raw,
        "команда c:MARKALL отсутствует в сборке")
require(b"(defun mark:all-cycle (" in raw, "mark:all-cycle отсутствует")
require(b"*mark:cycle-ss*" in raw, "нет *mark:cycle-ss*")
require(b"(append enames *mark:fills*)" in raw, "цикл не добавляет заполнения")
require(b"=== MARK.lsp: MarkZ " in raw, "нет метки цикла в журналах")
require(b"(mk:split-tjoints posts beams (append fills windows doors))" in raw,
        "mk:split-tjoints не получает заполнения")
require(b"(defun mk:tj-suffix (roles)" in raw, "нет mk:tj-suffix")
require(b"(defun mark:fill-equal-t-cells" in raw, "нет mark:fill-equal-t-cells")
require(b"(defun mark:fill-single-rail-cells" in raw,
        "нет раскладки одиночной ячейки по ригелю")
require(b"(setq boxes (mark:fill-single-rail-cells boxes hs))" in raw,
        "grid не расширяет одиночную ячейку после Т-половин")
require(b"(mark:fill-single-rail-cells (mark:fill-equal-t-cells (reverse out) hs) hs)" in raw,
        "standalone не расширяет одиночную ячейку после Т-половин")
require(b"(defun mark:vz-split-frame-first" in raw, "нет frame-first выбора групп")
require(b"(mark:vz-split-frame-first af ag)" in raw, "mark:vz-marks не использует frame-first")
require(b"(defun mark:vz-map-build-by-frames" in raw, "нет карты префиксов по рамкам")
require(b"(mark:vz-map-build-by-frames af ag)" in raw,
        "mark:vz-marks не использует карту по рамкам")
require(b"(not *mk:cycle-tables-cleared*)" in raw,
        "mk:clear-old-tables не защищает таблицу MarkZ")
# обрыв цепочки атрибутов
require(b"(t (setq e nil))" in raw, "clear-attrs не обрывает цепочку на SEQEND")
# откат: отчёт/спецмарки/reuse не должны вернуться
for bad in (b"mark:fill-existing", b"mark:fill-move-dxf"):
    require(bad not in raw, "в безопасной сборке остался " + bad.decode())
require(b"MARK_LOG.txt" not in raw, "единый журнал не должен вернуться")

# --- отчёт и спецмарки --------------------------------------------------
require(b"(defun mc:remark-capture" in raw, "нет реестра замечаний")
require(b"(defun mark:report-write" in raw, "нет .txt-отчёта")
require(b"(defun mark:special-outside" in raw, "нет спецмарок Дв/Ок/Вр")
require(b"(mark:special-outside)" in raw, "спецмарки не вызываются")
require(b"(mark:report-write)" in raw, "отчёт не записывается")
require(b"(mark:remarks-summary)" in raw, "нет сводки замечаний")
require(b"(defun mark:save-run-counters" in raw, "нет сохранения счётчиков")
require(b"(mc:remark \"G01\"" in raw, "нет деталей G01")
require(b"mark:fill-existing" not in raw, "reuse не должен вернуться")
require(b"MARK_LOG.txt" not in raw, "единый журнал не должен вернуться")

if "--check" in sys.argv:
    current = OUT.read_bytes() if OUT.exists() else b""
    if current != raw:
        raise SystemExit("MARK.lsp устарел: пересоберите tools/build_single.py")
    print("MARK.lsp актуален")
else:
    OUT.write_bytes(raw)
    print("MARK.lsp: строк %d, байт %d, defun %d"
          % (raw.count(b"\n"), len(raw), len(names(text))))
