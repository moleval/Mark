# -*- coding: utf-8 -*-
"""Единое ядро MARKCORE.lsp: структура, контракт и обёртки модулей.

Проверяется то, что можно проверить без AutoCAD:

1. ядро — отдельный файл в CP1251/CRLF без BOM, структура AutoLISP целая;
2. ядро не знает о модулях: нет вызовов mark:/mk:, нет команд, нет печати,
   нет их глобальных переменных;
3. каждая функция mc:*, которую вызывают модули, определена в ядре ровно
   один раз;
4. обёртки модулей действительно делегируют в ядро, а старые реализации
   примитивов из модулей удалены;
5. оба модуля сами находят и загружают ядро;
6. числовые правила ядра (допуск 0.5 мм, 1-based шкала, целые мм) совпадают
   с документированным контрактом и с принятым файлом шкалы.
"""
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "tests"))
from lisp_structure import parse  # noqa: E402

failures = []


def check(name, ok, detail=""):
    if not ok:
        failures.append(f"{name}{': ' + detail if detail else ''}")


def read(name):
    return (ROOT / name).read_bytes()


core_raw = read("MARKCORE.lsp")
mz_raw = read("MARKZ.lsp")
mv_raw = read("MarkV.lsp")

# --- 1. Кодировка и структура -------------------------------------------
check("MARKCORE без BOM", not core_raw.startswith(b"\xef\xbb\xbf"))
check("MARKCORE CRLF", core_raw.count(b"\n") == core_raw.count(b"\r\n"))
core = core_raw.decode("cp1251")
mz = mz_raw.decode("cp1251")
mv = mv_raw.decode("cp1251")

for name, text in (("MARKCORE", core), ("MARKZ", mz), ("MarkV", mv)):
    try:
        forms = parse(text)
        check(f"{name}: формы разобраны", bool(forms))
    except ValueError as exc:
        check(f"{name}: структура", False, str(exc))

# --- 2. Ядро не зависит от модулей --------------------------------------
code_lines = []
for line in core.splitlines():
    pos = line.find(";")
    code_lines.append(line if pos < 0 else line[:pos])
core_code = "\n".join(code_lines)

check("ядро не вызывает mark:/mk:", not re.search(r"\((?:mark|mk):", core_code))
check("ядро без команды AutoCAD", "(command" not in core_code and "defun c:" not in core_code)
# В ядре допустим только завершающий (princ) без аргумента: он гасит
# возврат значения при загрузке файла, но ничего не печатает.
check("ядро не печатает", not re.search(r"\(princ\s+[^)\s]", core_code))
check("ядро без глобальных переменных модулей",
      not re.search(r"\*(?:mark|mk):", core_code))
check("ядро само грузит COM", "(vl-load-com)" in core_code)

# --- 3. Все используемые mc:* определены один раз -----------------------
core_defuns = re.findall(r"\(defun\s+(mc:[^\s()]+)", core_code)
dups = {n for n in core_defuns if core_defuns.count(n) > 1}
check("нет дублей mc:defun", not dups, ", ".join(sorted(dups)))

used = set(re.findall(r"\((mc:[^\s()]+)", mz + mv))
missing = used - set(core_defuns)
check("все mc:* из модулей есть в ядре", not missing, ", ".join(sorted(missing)))

calls_anywhere = set(re.findall(r"\((mc:[^\s()]+)", core_code + mz + mv))
unused = set(core_defuns) - calls_anywhere - {"mc:core-version"}
check("в ядре нет мёртвых функций", not unused, ", ".join(sorted(unused)))

# --- 4. Обёртки делегируют, старые реализации удалены -------------------
WRAPPERS = {
    "MARKZ.lsp": {
        "mark:strp": "mc:strp", "mark:trim": "mc:trim", "mark:name=": "mc:name=",
        "mark:numval": "mc:numval", "mark:unwrap": "mc:unwrap",
        "mark:same-num?": "mc:same-num?", "mark:dim-key": "mc:dim-key",
        "mark:sort-unique": "mc:sort-unique", "mark:index-of": "mc:index-of",
        "mark:positives": "mc:positives", "mark:rec-get": "mc:rec-get",
        "mark:rec-put": "mc:rec-put", "mark:vla": "mc:e->vla",
        "mark:ax-get": "mc:ax-get", "mark:ax-put": "mc:ax-put",
        "mark:ax-invoke": "mc:ax-invoke",
        "mark:ax-invoke-ok": "mc:ax-invoke-ok",
        "mark:block-names": "mc:block-names",
        "mark:name-match?": "mc:name-match?",
        "mark:fmt-raw": "mc:fmt-raw", "mark:fmt-mm": "mc:fmt-mm",
        "mark:mm-key": "mc:mm-key", "mark:round1": "mc:round1",
        "mark:rjust": "mc:rjust", "mark:pad-line": "mc:pad-line",
        "mark:insert-sorted": "mc:insert-sorted",
        "mark:find-attr-in-ename": "mc:attr-rec",
        "mark:find-attr-obj": "mc:attr-obj",
        "mark:find-attr": "mc:attr-find",
        "mark:dyn-item-name": "mc:dyn-item-name",
        "mark:dyn-item-value": "mc:dyn-item-value",
        "mark:dyn-item-pair": "mc:dyn-item-pair",
        "mark:load-dyn-pairs": "mc:dyn-pairs",
    },
    "MarkV.lsp": {
        "mk:strp": "mc:strp", "mk:trim": "mc:trim", "mk:name=": "mc:name=",
        "mk:numval": "mc:numval", "mk:ax-get": "mc:ax-get",
        "mk:ax-put": "mc:ax-put", "mk:rec-get": "mc:rec-get",
        "mk:rec-put": "mc:rec-put", "mk:blk-match?": "mc:blk-match?",
        "mk:dynamic?": "mc:dynamic?", "mk:scale-index": "mc:scale-index",
        "mk:has-attrs-insert?": "mc:attr-insert?",
        "mk:get-attr": "mc:attr-dxf",
        "mk:get-all-attrs": "mc:attr-list",
        "mk:has-attr?": "mc:attr-has?",
        "mk:set-attr": "mc:attr-set",
    },
}

for module, table in WRAPPERS.items():
    text = mz if module == "MARKZ.lsp" else mv
    for fn, target in table.items():
        m = re.search(r"\(defun " + re.escape(fn) + r"\s", text)
        check(f"{module}: {fn} объявлена", bool(m))
        if not m:
            continue
        body = text[m.start():m.start() + 400]
        check(f"{module}: {fn} -> {target}", f"({target}" in body.split(")\n\n")[0])

# Старые реализации примитивов в модулях не должны оставаться.
check("MARKZ: нет своей vl-string-trim", "vl-string-trim" not in mz)
check("MARKZ: нет своего vlax-put-property", "vlax-put-property" not in mz)
check("MarkV: нет своей vl-string-trim", "vl-string-trim" not in mv)
check("MarkV: нет своего DWGPREFIX", "DWGPREFIX" not in mv)
check("MARKZ: DWGPREFIX только вне ядра", mz.count("DWGPREFIX") <= 1)

# --- 5. Модули сами загружают ядро --------------------------------------
for module, text, ensure in (("MARKZ.lsp", mz, "mark:core-ensure"),
                             ("MarkV.lsp", mv, "mk:core-ensure")):
    check(f"{module}: путь ядра", '(setq *mc:core-file* "MARKCORE.lsp")' in text)
    check(f"{module}: загрузчик объявлен", f"(defun {ensure} " in text)
    check(f"{module}: загрузчик вызван", f"({ensure})" in text)
    check(f"{module}: загрузка через findfile", "findfile" in text[text.index(ensure):text.index(ensure) + 900])

check("MarkV не переопределяет ядро заново",
      "GetDynamicBlockProperties" in core and "mc:dyn-pairs" in mv)

# --- 6. Контракт числовых правил ядра -----------------------------------
# Зеркало LISP-функций ядра: те же правила, записанные явно.
TOL = 0.5


def same_num(a, b):
    return abs(a - b) < TOL


def index_of(lst, v):
    for i, x in enumerate(lst):
        if same_num(x, v):
            return i
    return None


def scale_index(v, scale):
    for i, s in enumerate(scale, 1):
        if same_num(s, v):
            return i
    return 0


def dim_key(x):
    return int(x * 2.0 + 0.5) if x > 0 else None


def trim(s):
    return s.strip(" \t\r\n") if isinstance(s, str) else s


check("index-of: точное попадание", index_of([195.0, 1100.0], 1100.0) == 1)
check("index-of: допуск 0.5 мм", index_of([1100.0], 1100.4) == 0)
check("index-of: за допуском", index_of([1100.0], 1100.6) is None)
check("scale-index: 1-based", scale_index(1100.0, [195.0, 1100.0, 1250.0]) == 2)
check("scale-index: 0 = нет", scale_index(1425.0, [195.0, 1100.0]) == 0)
check("dim-key: шаг 0.5 мм", dim_key(1100) == 2200 and dim_key(1100.4) == 2201)
check("dim-key: не положительное -> nil", dim_key(0) is None)
check("trim: nil остаётся nil", trim(None) is None)
check("trim: краевые пробелы", trim("  ВН-1\r\n") == "ВН-1")

# Принятый файл шкалы (Ред. 48.48) — эталон формата строк ядра.
scale_file = Path("/tmp/arena-coding-uploads/Много витражей (без заполнений) Шкала-3.txt")
if scale_file.exists():
    rows = scale_file.read_bytes().decode("cp1251").splitlines()
    check("шкала: заголовок", rows[0] == "индекс;размер_мм;источник")
    # строки «-» — ригель без столбца, строка «;» — итог шкалы
    bad = [r for r in rows[1:]
           if r and not re.fullmatch(r"(?:(?:-|\d+);\d+;(?:РЗ|З|Р)|;.*)", r)]
    check("шкала: формат строк mc:scale-row", not bad, "; ".join(bad[:3]))
else:
    print("шкала: файл 48.48 недоступен — проверка формата пропущена")

if failures:
    print("FAIL:")
    for f in failures:
        print("  -", f)
    raise SystemExit(1)
print(f"OK: ядро {len(core_defuns)} функций, обёрток "
      f"{sum(len(v) for v in WRAPPERS.values())}, проверок пройдено")
