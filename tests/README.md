# tests/ — активные offline-проверки

Все тесты запускаются из корня репозитория и проверяют **`MARK.lsp`**.
`backup/` в активных тестах не используется.

```bat
python tests\lisp_structure.py
python tests\bundle_48_50.py
python tests\production_48_55.py
python tests\compact_scale_48_56.py
python tests\report_48_52.py
python tests\table_marks_48_53.py
python tests\joint_scale_48_54.py
python tests\log_t_halves_48_57.py
python tests\scale_compact_48_58.py
python tests\dist_48_58.py
```

| Тест | Что проверяет |
|---|---|
| `lisp_structure.py` | формы, арности, CP1251/CRLF |
| `bundle_48_50.py` | команды, цикл, отчёт, Т-соединения, нет reuse |
| `production_48_55.py` | 19 команд, нет внешних файлов |
| `compact_scale_48_56.py` | нет резерва `sec-reserve`; компактная шкала |
| `scale_compact_48_58.py` | числовая модель: 1100=16, 1250=17, 2250=18, 2245 нет |
| `dist_48_58.py` | zip содержит текущий MARK.lsp, SHA256 совпадает |
| `report_48_52.py` | отчёт, спецмарки, локальная шкала |
| `table_marks_48_53.py` | марки MLINE в ведомости, м.п. от целой длины |
| `joint_scale_48_54.py` | совместная шкала: общий probe → union |
| `log_t_halves_48_57.py` | Т-половины построчно только в диагностике |

## Быстрый прогон

```powershell
Get-ChildItem tests\*.py | ForEach-Object {
    python $_.FullName
    if ($LASTEXITCODE -ne 0) { throw "FAIL: $($_.Name)" }
}
```

Ожидаемо: **10 из 10 exit=0**.

## История

Тесты 48.38–48.50, которые проверяли `backup/MARKZ.lsp`/`MarkV.lsp`, лежат в
[`legacy/tests/`](../legacy/tests/). Они сохранены для справки и в приёмку не
входят.
