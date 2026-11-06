# -*- coding: utf-8 -*-
"""Структурная проверка протоколов MarkZ / MarkV.

AutoCAD в CI недоступен, поэтому проверяются именно точки вывода и запуска
протокола, а не результат работы ActiveX. Расчёты маркировки здесь не меняются.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
mz = (ROOT / "MARKZ.lsp").read_bytes().decode("cp1251")
mv = (ROOT / "MarkV.lsp").read_bytes().decode("cp1251")

checks = [
    ("MARKZ build", bool(re.search(r'\*mark:build\*\s+"Ред\. \d+\.\d+"', mz))),
    ("MarkV build", bool(re.search(r'\*mk:build\*\s+"Ред\. \d+\.\d+"', mv))),
    ("MARKZ default path", 'DWGPREFIX' in mz and 'MARKZ_LOG.txt' in mz),
    ("MarkV default path", 'DWGPREFIX' in mv and 'MARKV_LOG.txt' in mv),
    ("MARKZ tee", '(defun mark:emit' in mz and '(mark:log-write msg)' in mz
     and '(princ msg)' in mz),
    ("MarkV tee", '(defun mk:emit' in mv and '(mk:log-write msg)' in mv
     and '(princ msg)' in mv),
    ("MARKZ no bypass prompt", '(prompt' not in mz),
    ("MarkV no bypass prompt", '(prompt' not in mv),
    ("MARKZ package starts log", '(defun mark:a-all' in mz
     and '(mark:log-start)' in mz[mz.index('(defun mark:a-all'):]),
    ("MarkV package starts log", '(defun c:МАРКАВ ' in mv
     and '(mk:log-start)' in mv[mv.index('(defun c:МАРКАВ '):]),
    ("MARKZ explicit override", '*mark:log-file*' in mz),
    ("MarkV explicit override", '*mk:log-file*' in mv),
]
failed = [name for name, ok in checks if not ok]
if failed:
    raise SystemExit("FAIL: " + ", ".join(failed))
mz_build = re.search(r'\*mark:build\*\s+"(Ред\. \d+\.\d+)"', mz).group(1)
mv_build = re.search(r'\*mk:build\*\s+"(Ред\. \d+\.\d+)"', mv).group(1)
print(f"OK: {len(checks)} проверок протоколов; MARKZ {mz_build}, MarkV {mv_build}")
