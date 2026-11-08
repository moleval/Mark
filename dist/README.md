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

SHA256: `451FC16CC319BC8D944B9C2F26D0A980DD3A0018E0EEF149C6FC9B31FF3375CE`
