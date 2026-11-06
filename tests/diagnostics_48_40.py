# -*- coding: utf-8 -*-
"""Регрессия Ред. 48.40: MarkZ не запускает сканер при load/RELOAD.

AutoExtraction сам диагностирует файл при ошибке загрузки.  Плагин не должен
повторно вызывать project-wide CHKALL ни в хвосте загрузки, ни в diagnostics.
Поиск исходника при этом сохраняется для MARKZ_SNAPSHOT.txt.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
raw = (ROOT / "MARKZ.lsp").read_bytes()
mz = raw.decode("cp1251")
failures = []


def expect(name, condition, detail=""):
    if not condition:
        failures.append(f"{name}: {detail}")


# Поставка AutoCAD: CP1251 без BOM, только CRLF.
expect("кодировка", not raw.startswith(b"\xef\xbb\xbf"), "найден UTF-8 BOM")
expect("переводы строк", raw.count(b"\n") == raw.count(b"\r\n"),
       "есть LF без CR")

rev = re.search(r'\*mark:rev\*\s+"Ред\. (\d+)\.(\d+)"', mz)
expect("редакция", bool(rev) and tuple(map(int, rev.groups())) >= (48, 40),
       "нужна редакция не ниже 48.40")
expect("шапка", "Режим самодиагностики TEST 01 ... TEST 16." in mz)
expect("AutoExtraction", "Проверка скобок: AutoExtraction" in mz)

# Ни определений, ни вызовов собственного теста не должно остаться.
for symbol in ("mark:test-parens", "mark:scan-parens", "mark:has-command?"):
    expect("удалён " + symbol, symbol not in mz)
expect("нет делегирования CHKALL", "'c:CHKALL" not in mz)
stale_test = "TEST " + "00"
expect("старый номер удалён", stale_test not in mz)

# diagnostics начинается с TEST 01, а хвост load — сразу с баннера.
diag = mz[mz.index("(defun mark:diagnostics"):mz.index("(defun mark:apply-impl")]
expect("первый тест diagnostics",
       "(mark:safe 'mark:test-fillings" in diag and
       diag.index("(mark:safe 'mark:test-fillings") <
       diag.index("(mark:safe 'mark:test-glazing-attr"))
tail = mz[mz.index("(defun mark:help-header"):]
expect("загрузка без сканера", '(mark:emit (strcat "\\n[MARKZ] "' in tail)

# Номера существующих тестов не перенумерованы. TEST 15 отсутствовал и раньше;
# эта правка не создаёт новый тест и не сдвигает TEST 16 на его место.
for number in list(range(1, 15)) + [16]:
    expect(f"TEST {number:02d} сохранён", f"TEST {number:02d}" in mz)

# Эти функции нельзя удалять вместе со сканером: snapshot-path использует их.
for marker in ("(setq *mark:source-candidates*", "(setq *mark:source-file* nil)",
               "(defun mark:file-open?", "(defun mark:find-source",
               "(defun mark:snapshot-path"):
    expect("сохранён " + marker, mz.count(marker) == 1)
snapshot = mz[mz.index("(defun mark:snapshot-path"):
              mz.index("(defun mark:ax-objname")]
expect("путь снимка", "(mark:find-source)" in snapshot and
       'MARKZ_SNAPSHOT.txt' in snapshot)

# В пользовательских и агентских документах не остаётся старого критерия.
for name in ("README.md", "TZ.md", "AGENTS.md"):
    doc = (ROOT / name).read_text(encoding="utf-8")
    expect(name + " без старого теста", stale_test not in doc)
    expect(name + " с новой редакцией", "48.40" in doc)

if failures:
    print("ОШИБКИ:")
    for failure in failures:
        print("  -", failure)
    raise SystemExit(1)

print("load/RELOAD и diagnostics: собственный сканер и CHKALL не вызываются")
print("TEST 01–14 и TEST 16 сохранены; TEST 15 не добавлялся")
print("mark:find-source и MARKZ_SNAPSHOT.txt сохранены")
print("MARKZ.lsp: CP1251 без BOM, CRLF")
print("ИТОГ: проверки Ред. 48.40 пройдены")
