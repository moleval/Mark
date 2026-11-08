# dist/ — файл для скачивания

`MARK.lsp.zip` содержит `MARK.lsp` в **CP1251 + CRLF**, как нужно AutoCAD.

## Зачем

GitHub хранит `.lsp` как UTF-8 из-за `.gitattributes`
(`working-tree-encoding=CP1251`). Кнопка **Raw** отдаёт UTF-8, и кириллические
команды могут перестать распознаваться. Zip сохраняет исходные байты.

## Как использовать

1. Скачать `MARK.lsp.zip`.
2. Распаковать.
3. В AutoCAD:

```lisp
(load "D:/MARK.lsp")
```

Или клонировать репозиторий — при checkout Git сам создаст CP1251.

SHA256: `540378ABD2A3F627E933B6A0563E6EF518C5D83098C2258833FB3B3A0B5B69ED`
