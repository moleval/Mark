# tests/ — offline-проверки

Все тесты — Python 3, без AutoCAD. Запуск из корня репозитория:

```bat
python tests\lisp_structure.py
python tests\bundle_48_50.py
...
```

`lisp_structure.py` без аргументов проверяет `MARK.lsp` и исходники из
`backup/`.

## Продуктовые (проверяют `MARK.lsp`)

| Тест | Что проверяет |
|---|---|
| `lisp_structure.py` | формы, арности, CP1251/CRLF, структура |
| `bundle_48_50.py` | сборка, команды, отсутствие мусора, свежесть |
| `production_48_55.py` | 19 команд, нет внешних файлов |
| `compact_scale_48_56.py` | нет резерва `sec-reserve`; компактная шкала |
| `report_48_52.py` | отчёт, спецмарки, локальная шкала, без reuse |
| `table_marks_48_53.py` | марки MLINE в ведомости, м.п. от целой длины |
| `joint_scale_48_54.py` | совместная шкала: общий probe → union |
| `log_t_halves_48_57.py` | Т-половины построчно только в диагностике |

## Исторические (проверяют исходники 48.38–48.50)

Они остаются зелёными и фиксируют принятые правила старых редакций:

```text
core_48_49.py            ядро и обёртки
diagnostics_48_40.py     отказ от встроенного сканера
frame_axes_48_39.py      оси рамок
frame_axes_48_41.py      одна ось MLINE
logging_36_5.py          журналы
rail_edges_48_42.py      наружная привязка к ригелям
rigel_fill_48_48.py      фактическая ширина подгруппы
scale_48_37.py           шкала по ригелям
scale_modes_48_46.py     joint/separate шкалы
scale_state_48_45.py     сохранение fill-geo
sections_48_43.py        Т-половины
section_faces_48_47.py   грани секций
single_rail_48_50.py     одиночная ячейка
subrow_48_38.py          литера подъяруса
vz_bbox_cache_48_44.py   AABB-кэш
```

Эти тесты читают `backup/MARKZ.lsp`, `backup/MarkV.lsp`,
`backup/MARKCORE.lsp`. Они не обязаны описывать текущую компактную шкалу —
для неё есть `compact_scale_48_56.py`.

## Быстрый прогон всех

```powershell
Get-ChildItem tests\*.py | ForEach-Object {
    python $_.FullName
    if ($LASTEXITCODE -ne 0) { throw "FAIL: $($_.Name)" }
}
```

Ожидаемо: **23 из 23 exit=0**.
