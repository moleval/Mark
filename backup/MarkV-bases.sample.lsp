;;; Пример внешней базы MarkV.
;;; Скопируйте файл под именем MarkV-bases.lsp рядом с MarkV.lsp.

(mk:register-base
  "ПРИМЕР"
  '(("КП45372" . 148.0)
    ("КП45551" . 116.0)
    ("КП45303" . 70.0)))

;;; Для нового префикса:
;;; (setq *mk:article-prefixes*
;;;       (cons "ALT" *mk:article-prefixes*))
