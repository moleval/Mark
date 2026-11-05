;;;=====================================================================
;;; MarkV.lsp  (ранее MarkZV.lsp)
;;; Модуль сбора данных, построения 2D модели витража
;;; и автозаполнения атрибутов стоек и ригелей.
;;;
;;; Марки СТОЕК и ригелей ставятся по витражам: каждая рамка «Атрибуты
;;; витража» обрабатывается отдельно — свой префикс, свои шкалы номеров,
;;; свои группы на чертеже; ведомость одна и общая.
;;;=====================================================================
(vl-load-com)

;;;=====================================================================
;;; 0. АВТОПРОВЕРКА СКОБОК
;;;=====================================================================
(defun mk:check-brackets (filepath / f line num depth in-str i c len err-line ch)
  (setq f (open filepath "r"))
  (if (null f)
    (progn
      (prompt (strcat "\n[CHECK] Файл не найден: " filepath))
      nil)
    (progn
      (setq num 0 depth 0 in-str nil err-line nil)
      (while (setq line (read-line f))
        (setq num (1+ num))
        (setq i 1)
        (setq len (strlen line))
        (while (<= i len)
          (setq ch (substr line i 1))
          (cond
            (in-str
             (if (and (= ch "\\") (< i len))
               (setq i (1+ i))  ; пропустить следующий символ после \
             )
             (if (= ch "\"")
               (setq in-str nil)
             )
            )
            ((= ch ";")
             (setq i len)  ; пропустить до конца строки
            )
            ((= ch "\"")
             (setq in-str t)
            )
            ((= ch "(")
             (setq depth (1+ depth))
            )
            ((= ch ")")
             (setq depth (1- depth))
             (if (and (< depth 0) (null err-line))
               (setq err-line num)
             )
            )
          )
          (setq i (1+ i))
        )
      )
      (close f)
      (cond
        (err-line
         (prompt (strcat "\n[CHECK ERROR] Лишняя ')' на строке: " (itoa err-line)))
         nil
        )
        ((> depth 0)
         (prompt (strcat "\n[CHECK ERROR] Не закрыто '(' : " (itoa depth) " шт."))
         nil
        )
        (in-str
         (prompt "\n[CHECK ERROR] Не закрыта кавычка")
         nil
        )
        (t
         (prompt (strcat "\n[CHECK OK] Скобки сбалансированы. Строк: " (itoa num)))
         t
        )
      )
    )
  )
)

;; Путь к исходнику: несколько кандидатов + findfile (донор: mark:find-source)
(defun mk:find-source (/ cand out)
  (setq out nil)
  (foreach cand (list "D:/MarkZ/MarkV.lsp" "D:/MarkV.lsp" "D:/MarkZV/MarkV.lsp"
                      "C:/Work/MarkZV/MarkV.lsp" "MarkV.lsp")
    (if (and (null out) (findfile cand))
      (setq out (findfile cand))))
  out)

;; Проверка синтаксиса. Основной тестер — в AutoExtraction: CHKALL (все модули
;; и плагины), CHKFILE (один файл), CHKLOAD (поиск места сбоя загрузки).
;; CHKALL вызывается как команда AutoCAD: имена команд в AutoLISP не видны как
;; функции, поэтому наличие тестера ловится через getc: no-arg (не трогает
;; CMDECHO и не печатает «Неизвестная команда»). Если AutoExtraction загружен —
;; он и проверяет; иначе работает встроенный сканер по исходнику MarkV.lsp.
(defun mk:ext-checker? (cmd)
  (if (and (mk:strp cmd) (getc: cmd))
    (progn (getc: nil) t)
    (progn (getc: nil) nil)))

(defun c:МАРКАВСКОБКИ (/ result path)
  (prompt "\n[МАРКАВСКОБКИ] Проверка MarkV.lsp...")
  (if (mk:ext-checker? "CHKALL")
    (progn
      (prompt "\n  Тестер AutoExtraction найден: CHKALL (все модули и плагины).")
      ;; пакетный прогон может быть прерван — состояние сбрасываем до вызова
      (setq *mk:batch* nil *mk:batch-ss* nil *mk:batch-mode* nil
            *mk:run-scope* nil *mk:in-group* nil)
      (c:CHKALL)
      (prompt "\n[МАРКАВСКОБКИ] Готово: отчёт CHKALL — выше."))
    (progn
      (setq path (mk:find-source))
      (cond
        ((null path)
         (prompt "\n  [WARN] Файл MarkV.lsp не найден среди кандидатов."))
        (t
         (prompt (strcat "\n  Файл: " path))
         (prompt "\n  AutoExtraction не загружен — встроенный сканер MarkV.")
         (setq result (mk:check-brackets path))
         (if result
           (prompt "\n[OK] Файл корректен.")
           (prompt "\n[ERROR] Обнаружены проблемы. См. сообщение выше."))))))
  (princ))

;;;=====================================================================
;;; 1. КОНФИГУРАЦИЯ
;;;=====================================================================
;; Версия — как у заказчика: редакция и билд отдельными записями «Ред. N.N».
;; В логах печатается «Ред. 7.0 / Ред. 33.5» — редакция и билд.
(setq *mk:ver*            "Ред. 7.0")
(setq *mk:build*          "Ред. 33.5")

(setq *mk:block-fill*     "Заполнение в витраж")
(setq *mk:block-vitrage*  "Атрибуты витража")
;; Окна и двери — по маскам: точное имя блока зависит от системы и проекта,
;; а поиск идёт через wcmatch, поэтому достаточно имени-маски.
;; Переопределить можно одной строкой, например:
;;   (setq *mk:block-door* "Дверной блок КПТ74*")
(setq *mk:block-window*   "Окно КПТ60")
(setq *mk:block-door*     "Дверной блок*")
;; Маски проверяются раньше основного имени; сравнение идёт через strcase,
;; поэтому регистр в маске роли не играет.
;; Окна: "*ОКН*" — тот же корень, что в MarkZ (*mark:opening-masks*), плюс
;; основное имя *mk:block-window*. Створки ("*СТВОР*") в набор НЕ входят:
;; в приёмочном чертеже окон не нашлось ни по одному имени, и включать
;; створки «на всякий случай» значило бы поменять уже сверенные счётчики
;; (створка попала бы в окна и в «всего»). Если в проекте окна — это створки,
;; включается одной строкой в чертеже:
;;   (setq *mk:window-masks* (append *mk:window-masks* (list "*СТВОР*")))
(setq *mk:door-masks*     (list "*ДВЕР*"))
(setq *mk:window-masks*   (list "*ОКН*"))
(setq *mk:opening-skip-masks* (list "*КАССЕТ*"))  ; глухие заполнения — не проём
(setq *mk:block-post*     "стойка")
(setq *mk:block-beam*     "ригель")

(setq *mk:attr-article*   "Артикул")
(setq *mk:attr-thickness* "Толщина")
(setq *mk:attr-mark*      "Марка")
(setq *mk:attr-name*      "Название")
(setq *mk:attr-vitrage*   "Витраж")

;; Маски для поиска динамических свойств
(setq *mk:mask-length*    "*лин*")
(setq *mk:mask-height*    "*ысот*")
(setq *mk:mask-width*     "*ирин*")

(setq *mk:vis-candidates*
  '("Видимость1" "Видимость" "Visibility" "Visibility1"
    "Видимость состояния" "Visibility State" "Вид"))

(setq *mk:offset-fill*    25.0)
(setq *mk:tol-adjacency*  5.0)
(setq *mk:tol-skew*       5.0)
(setq *mk:tol-threshold*  50.0)
(setq *mk:tol-size*       0.5)
(setq *mk:slope-tol*      0.0033)
(setq *mk:out-file*       "D:/MARKZV_MODEL.txt")
(setq *mk:diag-file*      "D:/MARKZV_DIAG.txt")
(setq *mk:diag-all-file*  "D:/MARKZV_DIAG_ALL.txt")   ; МАРКАВДИАГНОЗОПТОМ
(setq *mk:cached-data*    nil)
(setq *mk:last-ss*        nil)   ; выборка последней МАРКАВГЕОМЕТРИЯ
(setq *mk:dyn-cache*      nil)   ; кэш динамических свойств: ename -> alist

(setq *mk:suffix-window-one*    "ок")
(setq *mk:suffix-window-both*   "окх2")
(setq *mk:suffix-door-one*      "дв")
(setq *mk:suffix-threshold*     "н")
(setq *mk:suffix-warm-cold*     "тх")
(setq *mk:suffix-cold-warm*     "хт")
(setq *mk:mirror-suffixes*      '(".1" ".2"))  ; зеркальная пара: Ст1.1 / Ст1.2
(setq *mk:conn-none*            "ПРИМЫКАНИЕ")  ; сторона без элементов каркаса
(setq *mk:suffix-small*         "м")     ; малый профиль
(setq *mk:suffix-big*           "б")     ; большой профиль
(setq *mk:suffix-mid*           "ср")    ; средний профиль (при 4 и более артикулах)
;; Ручная таблица артикул -> "м"/"б" (приоритет над автоопределением по габариту).
;; Заполняется из базы СИАЛ: (("КП45551" . "м") ("КП45364" . "б"))
(setq *mk:article-size*         nil)
(setq *mk:layer-model*          "Сетка витража")
(setq *mk:layer-test*           "Сетка витража тест")
(setq *mk:group-model*          "Сетка_витража")        ; имя группы — без пробелов
(setq *mk:group-test*           "Сетка_витража_тест")
;;; ДЛИНА РИГЕЛЯ
;;;   КП50   — длина ригеля равна длине элемента (световому проёму), припуск 0;
;;;   КП50К  — длина элемента + 12.5 x 2 = 25 мм;
;;;   вертикальный ригель («т») — всегда как КП50, припуск 0.
;;; Система определяется по артикулу: список *mk:articles-kp50k* (артикулы,
;;; встречающиеся только на листе «База СИАЛ КП50К»).
(setq *mk:beam-allowance*       0.0)    ; по умолчанию (КП50 / артикул неизвестен)
(setq *mk:allowance-kp50k*      25.0)   ; КП50К
;; Припуск для ригелей с нераспознанным артикулом (неименованные мультилинии).
;; nil — спросить один раз за прогон; число — использовать молча.
(setq *mk:allowance-unknown*    nil)
(setq *mk:ask-unknown-allowance* t)     ; задавать ли вопрос
;; Припуск по артикулу профиля: ((маска . припуск) ...), маска — wcmatch,
;; ВЕРХНИЙ регистр. Проверяется раньше значения по умолчанию.
;; Пример для разных систем:
;;   (setq *mk:beam-allowance-by-article*
;;     '(("КП50К*" . 25.0) ("КП453*" . 25.0)))
(setq *mk:beam-allowance-by-article* nil)
;; РИГЕЛИ системы КП50К (перечень заказчика). Только эти артикулы получают
;; припуск +25 мм, всё остальное считается как КП50 (припуск 0).
(setq *mk:articles-kp50k*
  '(
    "КП45369" "КП45371" "КП45453" "КП45550"
    "КПС1163" "КПС344" "КПС345" "КПС371"
    "КПС372" "КПС475" "КПС586" "КПС636"
    "КПС718" "КПС801" "КПС818" "КПС829"
    "КПС921" "КПС926" "КПС998"
  ))
;; Пакетный режим (команда МАРКАВ): общая выборка, один UNDO, без повторных вопросов
(setq *mk:batch*                nil)
(setq *mk:batch-ss*             nil)
(setq *mk:batch-mode*           nil)   ; "Блоки" / "Мультилинии" / "Все-типы"
(setq *mk:scope-mode*           "Блоки") ; режим последнего сбора — его же берут МАРКАВСТ/МАРКАВРГ
(setq *mk:batch-keep-grid*      nil)   ; оставлять ли сетку после пакета
;; Режим «Все-типы»: слияние параллельных линий одного профиля и отсев мусора
(setq *mk:merge-width*          200.0) ; макс. расстояние между линиями профиля
(setq *mk:min-seg*              50.0)  ; короче — не элемент каркаса
;; Сшивать ли коллинеарные куски одного профиля, идущие встык (мм зазора).
;; 0 — не сшивать. Для стоек, нарисованных по ячейкам, поставьте 60.0.
(setq *mk:join-posts-gap*       0.0)
(setq *mk:join-beams-gap*       0.0)
;; Выноски марок для элементов без атрибута «Марка»
(setq *mk:layer-label*          "Маркировка")    ; слой текстовых марок стоек и ригелей
(setq *mk:layer-table*          "Размеры")       ; слой таблиц
(setq *mk:group-label*          "Марки_выноски")  ; группа выносок
(setq *mk:label-color*          2)                ; жёлтый
(setq *mk:label-style*          "Основной стиль (надписи без наклона)")
(setq *mk:label-height*         40.0)             ; высота текста
(setq *mk:label-offset*         25.0)             ; отступ от грани профиля
(setq *mk:label-end-gap*        100.0)            ; недоход до края профиля
(setq *mk:label-line-gap*       1.4)              ; межстрочие (x высоту)
(setq *mk:label-rot-post*       90.0)             ; поворот подписи стойки, град
(setq *mk:cross-fallback*       50.0)             ; габарит сечения, если неизвестен
;; Типы объектов, считающихся профилем (полилинии и линии игнорируются).
(setq *mk:seg-types*            '("MLINE"))
;;; Т-соединения
(setq *mk:tol-tjoint*           30.0)   ; допуск примыкания торца к ригелю
(setq *mk:tol-tcenter*          30.0)   ; допуск «Т строго по центру ригеля»
(setq *mk:tj-symmetric*         t)      ; симметричный узел -> оба ригеля «.1»
(setq *mk:suffix-tjoint-lo*     ".1")   ; Т-соединение снизу (импост идёт вверх)
(setq *mk:suffix-tjoint-hi*     ".2")   ; Т-соединение сверху (импост идёт вниз)
(setq *mk:suffix-tjoint-both*   ".3")   ; Т-соединения с обеих сторон
(setq *mk:suffix-vert-beam*     "т")    ; вертикальный элемент с двумя Т
(setq *mk:label-short-len*      400.0)  ; короткий ригель: подпись по центру
;; Надпись длины L=xxx мм по оси элемента
(setq *mk:label-len-show*       t)      ; выводить ли надпись длины
(setq *mk:label-len-height*     30.0)   ; высота текста длины
(setq *mk:label-len-prefix*     "L=")
(setq *mk:label-len-suffix*     " мм")
(setq *mk:label-len-shift*      0.0)    ; смещение от оси поперёк элемента
(setq *mk:thick-warm-min*       42.0)
(setq *mk:thick-warm-max*       60.0)
(setq *mk:thick-cold-min*       4.0)
(setq *mk:thick-cold-max*       32.0)

;;;=====================================================================
;;; 2a. БАЗЫ ПРОФИЛЬНЫХ СИСТЕМ — ГАБАРИТ СЕЧЕНИЯ, мм
;;;   Любое число систем: каждая регистрируется своим именем.
;;;   Внешние базы кладутся в файл MarkV-bases.lsp рядом с модулем
;;;   (или в любую папку из путей поиска AutoCAD) и подхватываются при загрузке.
;;;   Формат файла базы:
;;;     (mk:register-base "АЛЮТЕХ" '(("ALT-W72-01" . 72.0) ("ALT-W72-05" . 116.0)))
;;;     (setq *mk:article-prefixes* (cons "ALT" *mk:article-prefixes*))
;;;=====================================================================
(setq *mk:size-bases* nil)          ; ((имя . таблица) ...)
(setq *mk:bases-file* "MarkV-bases.lsp")   ; файл внешних баз

;; Префиксы артикулов, распознаваемые в именах стилей мультилиний и слоёв
(setq *mk:article-prefixes* '("КП" "KP"))

(defun mk:register-base (name tbl / hit)
  (setq hit (assoc name *mk:size-bases*))
  (if hit
    (setq *mk:size-bases* (subst (cons name tbl) hit *mk:size-bases*))
    (setq *mk:size-bases* (cons (cons name tbl) *mk:size-bases*)))
  (length tbl))

;;;=====================================================================
;;; 2b. БАЗА СИАЛ — ГАБАРИТ СЕЧЕНИЯ ПРОФИЛЯ, мм
;;;   Источник: "База СИАЛ.xlsx", листы "База СИАЛ КП50" и "База СИАЛ КП50К",
;;;   разделы "Стойка" и "Ригель" (колонка "Габарит").
;;;   Сгенерировано tools/sial_parse.py --emit.
;;;=====================================================================
(setq *mk:sial-sizes*
  '(
    ("КП45453" . 21.0)
    ("КПС993" . 23.0)
    ("КП45367" . 27.0)
    ("КП45371" . 46.0)
    ("КПС372" . 46.0)
    ("КП45388" . 48.0)
    ("КПС009" . 54.0)
    ("КП45369" . 68.0)
    ("КПС371" . 68.0)
    ("КП45303-2" . 70.0)
    ("КП45303-3" . 70.0)
    ("КПС180" . 70.0)
    ("КП45366" . 76.0)
    ("КПС998" . 76.0)
    ("КПС913" . 86.0)
    ("КП45304" . 88.0)
    ("КПС919" . 90.0)
    ("КПС921" . 90.0)
    ("КПС1209" . 94.0)
    ("КПС1067" . 98.0)
    ("КП45302-1" . 100.0)
    ("КП45302-2" . 100.0)
    ("КП45370" . 104.0)
    ("КПС1272" . 104.0)
    ("КПС818" . 104.0)
    ("КПС1164" . 106.0)
    ("КПС1161" . 110.0)
    ("КПС1163" . 110.0)
    ("КПС298" . 114.0)
    ("КП45551" . 116.0)
    ("КП45551-3" . 116.0)
    ("КП45548" . 120.0)
    ("КП45550" . 120.0)
    ("КПС1275" . 120.0)
    ("КП45562" . 128.0)
    ("КПС1165" . 130.0)
    ("КПС299" . 130.0)
    ("КП45387" . 144.0)
    ("КП45372" . 148.0)
    ("КПС344" . 148.0)
    ("КПС491" . 148.0)
    ("КПС927" . 152.0)
    ("КПС924" . 155.0)
    ("КПС926" . 155.0)
    ("КПС492" . 158.0)
    ("КПС584" . 165.0)
    ("КПС586" . 165.0)
    ("КП45364" . 172.0)
    ("КП45392" . 178.0)
    ("КПС345" . 178.0)
    ("КПС494" . 187.0)
    ("КПС170" . 200.0)
    ("КПС634" . 205.0)
    ("КПС636" . 205.0)
    ("КПС015" . 210.0)
    ("КПС014" . 215.0)
    ("КПС475" . 215.0)
    ("КПС496" . 224.0)
    ("КПС171" . 235.0)
    ("КПС370" . 240.0)
    ("КПС426" . 240.0)
    ("КПС718" . 240.0)
    ("КПС1025" . 250.0)
    ("КПС633" . 270.0)
    ("КПС829" . 270.0)
    ("КПС437" . 280.0)
    ("КПС439" . 280.0)
    ("КПС801" . 280.0)
   )
)

;; Ключ артикула: без пробелов, в верхнем регистре ("КПС 993" -> "КПС993")
(defun mk:art-key (s / out i c)
  (if (null s)
    ""
    (progn
      (setq out "" i 1)
      (while (<= i (strlen s))
        (setq c (substr s i 1))
        (if (and (/= c " ") (/= c "\t")) (setq out (strcat out c)))
        (setq i (1+ i)))
      (strcase out))))

;; Габарит сечения профиля по базе СИАЛ; nil, если артикул не найден.
;; Сначала точное совпадение, затем исполнение профиля: "КП45303" -> "КП45303-2".
(mk:register-base "СИАЛ" *mk:sial-sizes*)

;; Поиск артикула в одной таблице: точное совпадение, затем исполнение
;; профиля («КП45303» -> «КП45303-2»)
(defun mk:size-in-table (k tbl / hit out)
  (setq out nil hit (assoc k tbl))
  (if hit
    (setq out (cdr hit))
    (foreach pair tbl
      (if (and (null out)
               (= (substr (car pair) 1 (1+ (strlen k))) (strcat k "-")))
        (setq out (cdr pair)))))
  out)

;; Габарит сечения по всем зарегистрированным базам; nil, если не найден
(defun mk:sial-size (art / k out)
  (setq k (mk:art-key art) out nil)
  (if (> (strlen k) 0)
    (foreach b *mk:size-bases*
      (if (null out) (setq out (mk:size-in-table k (cdr b))))))
  out)

;; Габарит сечения: база СИАЛ в приоритете, иначе замер по блоку
(defun mk:cross-size (el / s)
  (setq s (mk:sial-size (mk:rec-get el 'ARTICLE)))
  (if s s (if (numberp (mk:rec-get el 'CROSS)) (mk:rec-get el 'CROSS) 0.0)))

;;;=====================================================================
;;; 2. УТИЛИТЫ
;;;=====================================================================
(defun mk:strp (x) (and x (eq (type x) 'STR)))

;; Обновление поля записи: замена через subst, а не append.
;; (append …) добавлял бы ВТОРУЮ пару с тем же ключом, а assoc
;; всегда возвращает первую — значение молча не менялось.
(defun mk:rec-put (rec key val / old)
  (setq old (assoc key rec))
  (if old
    (subst (cons key val) old rec)
    (append rec (list (cons key val)))))

(defun mk:rec-get (rec key)
  (cdr (assoc key rec)))

(defun mk:numval (x)
  (cond ((numberp x) (float x))
        ((and (mk:strp x) (distof x 2)) (distof x 2))
        (t nil)))

(defun mk:ax-get (obj prop / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (vl-catch-all-error-p r) nil r))

;; Симметричная запись свойства — через vlax-put-property, как и чтение
(defun mk:ax-put (obj prop val / r)
  (setq r (vl-catch-all-apply 'vlax-put-property (list obj prop val)))
  (if (vl-catch-all-error-p r) nil t))

(defun mk:trim (s)
  (if (mk:strp s) (vl-string-trim " \t\n\r" s) s))

(defun mk:name= (s1 s2)
  (and (mk:strp s1) (mk:strp s2)
       (= (strcase (mk:trim s1)) (strcase (mk:trim s2)))))

(defun mk:blk-match? (ename target / obj eff-name names nm found ed)
  (setq found nil names nil ed (entget ename))
  ;; ВАЖНО: только вставки блоков. У MLINE в DXF 2 лежит имя стиля мультилинии
  ;; (например «Стойка (вертикальный профиль)») — без этой проверки
  ;; мультилинии принимались за блоки.
  (if (not (= (cdr (assoc 0 ed)) "INSERT"))
    (setq names nil ed nil))
  (if (null ed)
    nil
    (progn
  (setq nm (cdr (assoc 2 ed)))
  (if (mk:strp nm) (setq names (cons nm names)))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (and (not (vl-catch-all-error-p obj)) obj)
    (progn
      (setq nm (mk:ax-get obj "EffectiveName"))
      (if (and (mk:strp nm) (not (member nm names)))
        (setq names (cons nm names)))))
  (foreach nm names
    (if (and (null found) (wcmatch (strcase nm) (strcase target)))
      (setq found t)))))
  found)

;;;=====================================================================
;;; 3. ИЗВЛЕЧЕНИЕ АТРИБУТОВ
;;;=====================================================================
;; У обычных объектов (MLINE, LINE, LWPOLYLINE…) entnext возвращает СЛЕДУЮЩИЙ
;; объект чертежа, а не подобъект. Без этой проверки обход уходил в соседние
;; блоки и читал/писал ЧУЖИЕ атрибуты «Марка».
(defun mk:has-attrs-insert? (ename / ed)
  (setq ed (if ename (entget ename) nil))
  (and ed
       (= (cdr (assoc 0 ed)) "INSERT")
       (equal (cdr (assoc 66 ed)) 1)))

(defun mk:get-attr (ename tag / sub data val)
  (if (not (mk:has-attrs-insert? ename))
    (setq sub nil val nil)
    (setq sub (entnext ename) val nil))
  (while sub
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (if (mk:name= (cdr (assoc 2 data)) tag)
         (setq val (cdr (assoc 1 data)))))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if sub (setq sub (entnext sub))))
  (if (mk:strp val) (mk:trim val) nil))

(defun mk:get-all-attrs (ename / sub data attrs)
  (setq sub (entnext ename) attrs nil)
  (while sub
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (setq attrs (cons (cons (cdr (assoc 2 data))
                                (cdr (assoc 1 data))) attrs)))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if sub (setq sub (entnext sub))))
  (reverse attrs))

(defun mk:has-attr? (ename tag / sub data found)
  (if (not (mk:has-attrs-insert? ename))
    (setq sub nil found nil)
    (setq sub (entnext ename) found nil))
  (while (and sub (not found))
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (if (mk:name= (cdr (assoc 2 data)) tag)
         (setq found t)))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if sub (setq sub (entnext sub))))
  found)

;;;=====================================================================
;;; 4. ДИНАМИЧЕСКИЕ СВОЙСТВА И ГАБАРИТЫ
;;;=====================================================================
(defun mk:dynamic? (obj / v)
  (setq v (mk:ax-get obj "IsDynamicBlock"))
  (and v (= v :vlax-true)))

;; Все пары (имя . значение) динамического блока; VARIANT развёрнут
(defun mk:dyn-pairs-raw (obj / props out p nm val)
  (setq out nil)
  (if (mk:dynamic? obj)
    (progn
      (setq props (vl-catch-all-apply 'vlax-invoke-method
                    (list obj "GetDynamicBlockProperties")))
      (if (not (vl-catch-all-error-p props))
        (progn
          (if (= (type props) 'VARIANT)
            (setq props (vl-catch-all-apply 'vlax-safearray->list
                          (list (vlax-variant-value props)))))
          (if (and props (listp props) (not (vl-catch-all-error-p props)))
            (foreach p props
              (setq nm  (mk:ax-get p "PropertyName")
                    val (mk:ax-get p "Value"))
              (if (= (type val) 'VARIANT)
                (progn
                  (setq val (vl-catch-all-apply 'vlax-variant-value (list val)))
                  (if (vl-catch-all-error-p val) (setq val nil))))
              (if (mk:strp nm)
                (setq out (cons (cons (mk:trim nm) val) out)))))))))
  (reverse out))

;; То же, но с кэшем по ename (сбор на большом фасаде идёт в разы быстрее)
(defun mk:dyn-pairs (obj / e hit pairs)
  (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
  (if (vl-catch-all-error-p e) (setq e nil))
  (setq hit (if e (assoc e *mk:dyn-cache*) nil))
  (if hit
    (cdr hit)
    (progn
      (setq pairs (mk:dyn-pairs-raw obj))
      (if e (setq *mk:dyn-cache* (cons (cons e pairs) *mk:dyn-cache*)))
      pairs)))

;; Числовое динамическое свойство по маске имени (*лин*, *ысот*, *ирин*)
(defun mk:get-dyn (obj prop-mask / val)
  (setq val nil)
  (foreach pr (mk:dyn-pairs obj)
    (if (and (null val)
             (wcmatch (strcase (car pr)) (strcase prop-mask)))
      (setq val (mk:numval (cdr pr)))))
  (if (numberp val) val nil))

;;;=====================================================================
;;; 5. ВИДИМОСТЬ
;;;=====================================================================
(defun mk:get-vis (obj / val)
  (setq val nil)
  (foreach pr (mk:dyn-pairs obj)
    (if (and (null val)
             (member (car pr) *mk:vis-candidates*)
             (mk:strp (cdr pr)))
      (setq val (mk:trim (cdr pr)))))
  val)

;;;=====================================================================
;;; 5a. ГАБАРИТ БЛОКА (фолбэк размеров, донор: mark:fill-e-bb из MarkZ)
;;;=====================================================================
(defun mk:e-bb (obj / r a b)
  (if (null obj)
    nil
    (progn
      (setq a nil b nil)
      (setq r (vl-catch-all-apply 'vlax-invoke-method
                (list obj "GetBoundingBox" 'a 'b)))
      (if (or (vl-catch-all-error-p r) (null a) (null b))
        nil
        (progn
          (setq a (vl-catch-all-apply 'vlax-safearray->list (list a))
                b (vl-catch-all-apply 'vlax-safearray->list (list b)))
          (if (or (vl-catch-all-error-p a) (vl-catch-all-error-p b)
                  (null a) (null b))
            nil
            (list (float (car a)) (float (cadr a))
                  (float (car b)) (float (cadr b)))))))))

(defun mk:bb-w (bb) (if bb (abs (- (nth 2 bb) (nth 0 bb))) nil))
(defun mk:bb-h (bb) (if bb (abs (- (nth 3 bb) (nth 1 bb))) nil))

;; Размер: сначала динамическое свойство, иначе габарит -> (значение источник)
(defun mk:dim-src (obj mask bb-fn / v bb)
  (setq v (mk:get-dyn obj mask))
  (if (numberp v)
    (list v "DYN")
    (progn
      (setq bb (mk:e-bb obj)
            v  (if bb (apply bb-fn (list bb)) nil))
      (if (and (numberp v) (> v 1e-6))
        (list v "BBOX")
        (list nil "НЕТ")))))

;;;=====================================================================
;;; 6. ИЗВЛЕЧЕНИЕ ГЕОМЕТРИИ
;;;=====================================================================
(defun mk:get-geom (obj / ins-pt rot xsc ysc pt-list)
  (if (null obj)
    (list (cons 'INS_PT nil) (cons 'ROTATION 0.0)
          (cons 'XSCALE 1.0) (cons 'YSCALE 1.0))
    (progn
      (setq ins-pt (vl-catch-all-apply 'vlax-get-property (list obj "InsertionPoint")))
      (setq rot    (mk:ax-get obj "Rotation"))
      (setq xsc    (mk:ax-get obj "XScale"))
      (setq ysc    (mk:ax-get obj "YScale"))
      (setq pt-list nil)
      (if (and ins-pt (not (vl-catch-all-error-p ins-pt)))
        (progn
          (cond
            ((= (type ins-pt) 'SAFEARRAY)
             (setq pt-list (vl-catch-all-apply 'vlax-safearray->list (list ins-pt))))
            ((= (type ins-pt) 'VARIANT)
             (setq pt-list (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value ins-pt)))))
            ((listp ins-pt)
             (setq pt-list ins-pt)))))
      (if (and pt-list (vl-catch-all-error-p pt-list))
        (setq pt-list nil))
      (list
        (cons 'INS_PT (if (and pt-list (listp pt-list) (>= (length pt-list) 3))
                        (list (float (nth 0 pt-list))
                              (float (nth 1 pt-list))
                              (float (nth 2 pt-list)))
                        nil))
        (cons 'ROTATION (if (numberp rot) rot 0.0))
        (cons 'XSCALE   (if (numberp xsc) xsc 1.0))
        (cons 'YSCALE   (if (numberp ysc) ysc 1.0))))))

;;;=====================================================================
;;; 7. ЧТЕНИЕ БЛОКА «АТРИБУТЫ ВИТРАЖА»
;;;=====================================================================
(setq *mk:vitrage-src*   "")    ; откуда взят блок: выборка / чертёж
(setq *mk:vitrage-cache* nil)   ; кэш блока на время одной команды

;; Все вставки «Атрибуты витража» набора; ss = nil -> весь чертёж
(defun mk:vitrage-blocks (ss / i e out)
  (setq out nil)
  (if (null ss) (setq ss (ssget "_X" (list (cons 0 "INSERT")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (and (= (cdr (assoc 0 (entget e))) "INSERT")
                 (mk:blk-match? e (strcat "*" *mk:block-vitrage* "*")))
          (setq out (cons e out)))
        (setq i (1+ i)))))
  (reverse out))

;; Запись блока атрибутов витража по ename
(defun mk:vitrage-rec (e)
  (if (null e)
    nil
    (list
      (cons 'TYPE "АТРИБУТЫ_ВИТРАЖА")
      (cons 'ENAME e)
      (cons 'INS_PT (cdr (assoc 'INS_PT (mk:get-geom (vlax-ename->vla-object e)))))
      (cons 'ATTRS (mk:get-all-attrs e)))))

;; Центр габарита выборки (для выбора ближайшего блока атрибутов)
(defun mk:ss-center (ss / i e bb x0 y0 x1 y1 obj)
  (setq x0 nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e   (ssname ss i)
              obj (vl-catch-all-apply 'vlax-ename->vla-object (list e))
              bb  (if (vl-catch-all-error-p obj) nil (mk:e-bb obj)))
        (if bb
          (if (null x0)
            (setq x0 (nth 0 bb) y0 (nth 1 bb) x1 (nth 2 bb) y1 (nth 3 bb))
            (setq x0 (min x0 (nth 0 bb)) y0 (min y0 (nth 1 bb))
                  x1 (max x1 (nth 2 bb)) y1 (max y1 (nth 3 bb)))))
        (setq i (1+ i)))))
  (if x0 (list (/ (+ x0 x1) 2.0) (/ (+ y0 y1) 2.0)) nil))

;; Ближайшая к точке вставка из списка
(defun mk:nearest-insert (lst pt / best bd p d)
  (setq best nil bd nil)
  (foreach e lst
    (setq p (cdr (assoc 10 (entget e))))
    (if (and p pt)
      (progn
        (setq d (distance (list (car p) (cadr p)) (list (car pt) (cadr pt))))
        (if (or (null bd) (< d bd)) (setq bd d best e)))
      (if (null best) (setq best e))))
  (if best best (car lst)))

;; Блок «Атрибуты витража»: сначала в текущей выборке, затем по чертежу.
;; На чертеже с несколькими витражами это не даёт взять чужой префикс.
(defun mk:read-vitrage-block-scan (/ ss n lst pt e)
  (setq ss (if *mk:last-ss* *mk:last-ss* nil)
        lst (if ss (mk:vitrage-blocks ss) nil)
        pt (if ss (mk:ss-center ss) nil))
  (cond
    ((= (length lst) 1) (setq *mk:vitrage-src* "выборка" e (car lst)))
    ((> (length lst) 1)
     (setq *mk:vitrage-src* "выборка, ближайший")
     (prompt (strcat "\n  [WARN] В выборке несколько блоков «" *mk:block-vitrage* "». Взят ближайший."))
     (setq e (mk:nearest-insert lst pt)))
    (t
     (setq *mk:vitrage-src* "не выбран" e nil)
     (prompt (strcat "\n  [WARN] Блок «" *mk:block-vitrage* "» не выбран в текущей области."))))
  (mk:vitrage-rec e))

(defun mk:read-vitrage-block ()
  (if (null *mk:vitrage-cache*)
    (setq *mk:vitrage-cache* (list (mk:read-vitrage-block-scan))))
  (car *mk:vitrage-cache*))

(defun mk:get-vitrage-prefix (/ vitrage attrs prefix)
  (setq vitrage (mk:read-vitrage-block) prefix nil)
  (if vitrage
    (progn
      (setq attrs (cdr (assoc 'ATTRS vitrage)))
      (foreach attr attrs (if (mk:name= (car attr) *mk:attr-vitrage*) (setq prefix (cdr attr))))
      (if (and (null prefix) attrs) (setq prefix (cdr (car attrs))))))
  (if vitrage
    (setq *mk:no-vitrage-prompted* nil)
    (if (null *mk:no-vitrage-prompted*)
      (progn
        (prompt "\n  Блок атрибутов не выбран. Введите префикс витража или нажмите Enter: ")
        (setq prefix (getstring T)
              *mk:manual-vitrage-prefix* prefix
              *mk:no-vitrage-prompted* t))))
  (if (and (null vitrage) (null prefix) (mk:strp *mk:manual-vitrage-prefix*))
    (setq prefix *mk:manual-vitrage-prefix*))
  (setq *mk:current-vitrage-prefix* (if (mk:strp prefix) prefix ""))
  *mk:current-vitrage-prefix*)

;;;=====================================================================
;;; 8. ИЗВЛЕЧЕНИЕ МУЛЬТИЛИНИЙ
;;;=====================================================================
(defun mk:get-mline-verts (ename / ed verts)
  (setq ed (entget ename) verts nil)
  (foreach p ed
    (if (= (car p) 10)
      (setq verts (cons (list (float (cadr p)) (float (caddr p))) verts))))
  (setq verts (reverse verts))
  (if (< (length verts) 2)
    (progn
      (setq verts nil)
      (foreach p ed
        (if (= (car p) 11)
          (setq verts (cons (list (float (cadr p)) (float (caddr p))) verts))))
      (setq verts (reverse verts))))
  verts)

(defun mk:verts-to-segs (verts / segs n i p0 p1)
  (setq segs nil n (length verts) i 0)
  (while (< i (1- n))
    (setq p0 (nth i verts) p1 (nth (1+ i) verts))
    (setq segs (cons (list (car p0) (cadr p0) (car p1) (cadr p1)) segs))
    (setq i (1+ i)))
  (reverse segs))

(defun mk:classify-seg (seg / dx dy adx ady len)
  (setq dx (- (nth 2 seg) (nth 0 seg))
        dy (- (nth 3 seg) (nth 1 seg))
        adx (abs dx) ady (abs dy)
        len (sqrt (+ (* dx dx) (* dy dy))))
  (if (< len 1e-6)
    nil
    (cond
      ((<= ady (* *mk:slope-tol* len)) 'h)
      ((<= adx (* *mk:slope-tol* len)) 'v)
      ((>= adx ady) 'h)
      (t 'v))))

;; Извлечение стоек и ригелей с нормализацией направления
(defun mk:extract-posts-beams-from-mlines (mlines / verts segs posts beams cls len ins-x ins-y)
  (setq posts nil beams nil)
  (foreach mline mlines
    (setq verts (mk:get-mline-verts mline))
    (if (and verts (>= (length verts) 2))
      (progn
        (setq segs (mk:verts-to-segs verts))
        (foreach seg segs
          (setq cls (mk:classify-seg seg))
          (if cls
            (progn
              (if (= cls 'v)
                (progn
                  (setq len (abs (- (nth 3 seg) (nth 1 seg))))
                  (if (> len 50.0)
                    (progn
                      (if (< (nth 3 seg) (nth 1 seg))
                        (setq ins-x (nth 2 seg) ins-y (nth 3 seg))
                        (setq ins-x (nth 0 seg) ins-y (nth 1 seg)))
                      (setq posts (cons (list
                        (cons 'TYPE "СТОЙКА")
                        (cons 'ENAME mline)
                        (cons 'INS_PT (list ins-x ins-y 0.0))
                        (cons 'LENGTH len)
                        (cons 'ARTICLE nil)
                        (cons 'VISIBILITY nil)
                        (cons 'PROTRUDING nil)
                        (cons 'LEFT_CONN nil)
                        (cons 'RIGHT_CONN nil)
                      ) posts)))))
                (progn
                  (setq len (abs (- (nth 2 seg) (nth 0 seg))))
                  (if (> len 50.0)
                    (progn
                      (if (< (nth 2 seg) (nth 0 seg))
                        (setq ins-x (nth 2 seg) ins-y (nth 3 seg))
                        (setq ins-x (nth 0 seg) ins-y (nth 1 seg)))
                      (setq beams (cons (list
                        (cons 'TYPE "РИГЕЛЬ")
                        (cons 'ENAME mline)
                        (cons 'INS_PT (list ins-x ins-y 0.0))
                        (cons 'LENGTH len)
                        (cons 'ARTICLE nil)
                        (cons 'VISIBILITY nil)
                        (cons 'TOP_ELEM nil)
                        (cons 'BOT_ELEM nil)
                        (cons 'SUFFIX nil)
                      ) beams))))))))))))
  (list (reverse posts) (reverse beams)))

;;;---------------------------------------------------------------------
;;; 8a. РЕЖИМ «ВСЕ-ТИПЫ»: КАРКАС ИЗ ЛЮБОЙ ГЕОМЕТРИИ ВЫБОРКИ
;;;   LINE / LWPOLYLINE / POLYLINE / MLINE + блоки стоек и ригелей.
;;;   Донор идеи: mark:fill-extract-segs / mark:fill-verts-to-segs (MarkZ).
;;;---------------------------------------------------------------------
;; Артикул профиля из строки: «Стойка КП45302 …» -> «КП45302», «КПС 1155» -> «КПС1155»
(defun mk:article-from-name (s / up n i c out j ch pfx plen k)
  (setq out nil)
  (if (mk:strp s)
    (progn
      (setq up (strcase s) n (strlen up) i 1)
      (while (and (<= i n) (null out))
        ;; какой из известных префиксов начинается в позиции i?
        (setq pfx nil)
        (foreach pr *mk:article-prefixes*
          (if (and (null pfx)
                   (= (substr up i (strlen pr)) (strcase pr)))
            (setq pfx (strcase pr))))
        (if pfx
          (progn
            (setq plen (strlen pfx) out pfx j (+ i plen) k 0)
            ;; до двух необязательных букв исполнения (КПС, КПБ и т. п.)
            (while (and (< k 2)
                        (not (member (substr up j 1)
                                     '("" " " "0" "1" "2" "3" "4" "5" "6" "7"
                                       "8" "9" "-" "(" ")" "." ",")))) 
              (setq out (strcat out (substr up j 1)) j (1+ j) k (1+ k)))
            (while (= (substr up j 1) " ") (setq j (1+ j)))
            (setq ch (substr up j 1))
            (if (or (= ch "") (null (member ch '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))
              (setq out nil)                     ; префикс без цифр — не артикул
              (progn
                (while (member (substr up j 1)
                               '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-"))
                  (setq out (strcat out (substr up j 1)) j (1+ j)))
                ;; хвостовой дефис не нужен
                (if (= (substr out (strlen out) 1) "-")
                  (setq out (substr out 1 (1- (strlen out)))))))))
        (setq i (1+ i)))))
  out)

;;; Подбор артикула «по похожести»: если явный артикул в строке не найден,
;;; строка сравнивается со всеми артикулами зарегистрированных баз.
;;; Совпадением считается вхождение артикула в очищенную строку
;;; (без пробелов, точек, дефисов, скобок, в ВЕРХНЕМ регистре) либо вхождение
;;; его цифровой части (от 4 цифр). Берётся самое длинное совпадение.
(setq *mk:art-guess-cache* nil)      ; строка -> артикул или "" (не найден)
(setq *mk:art-guess-log*   nil)      ; что и как распозналось

;; Строка без пробелов и разделителей, ВЕРХНИЙ регистр
(defun mk:squash (s / out i c)
  (if (not (mk:strp s))
    ""
    (progn
      (setq out "" i 1 s (strcase s))
      (while (<= i (strlen s))
        (setq c (substr s i 1))
        (if (null (member c (list " " (chr 9) "." "," "-" "_" "(" ")")))
          (setq out (strcat out c)))
        (setq i (1+ i)))
      out)))

;; Есть ли подстрока needle в hay?
(defun mk:str-in? (needle hay / n h i out)
  (setq n (strlen needle) h (strlen hay) i 1 out nil)
  (if (and (> n 0) (>= h n))
    (while (and (<= i (1+ (- h n))) (null out))
      (if (= (substr hay i n) needle) (setq out t))
      (setq i (1+ i))))
  out)

;; Только цифры артикула («КПС1155» -> «1155»)
(defun mk:digits-of (s / out i c)
  (setq out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (member c '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (setq out (strcat out c)))
    (setq i (1+ i)))
  out)

;; Подбор артикула по всем базам
(defun mk:article-guess (name / key best bestlen d hit)
  (setq key (mk:squash name) best nil bestlen 0)
  (if (> (strlen key) 3)
    (progn
      (setq hit (assoc key *mk:art-guess-cache*))
      (if hit
        (setq best (if (> (strlen (cdr hit)) 0) (cdr hit) nil))
        (progn
          ;; 1) артикул целиком входит в строку
          (foreach b *mk:size-bases*
            (foreach pair (cdr b)
              (if (and (> (strlen (car pair)) bestlen)
                       (mk:str-in? (mk:squash (car pair)) key))
                (setq best (car pair) bestlen (strlen (car pair))))))
          ;; 2) иначе — совпадение по цифровой части (от 4 цифр)
          (if (null best)
            (foreach b *mk:size-bases*
              (foreach pair (cdr b)
                (setq d (mk:digits-of (car pair)))
                (if (and (>= (strlen d) 4)
                         (> (strlen d) bestlen)
                         (mk:str-in? d key))
                  (setq best (car pair) bestlen (strlen d))))))
          (setq *mk:art-guess-cache*
                (cons (cons key (if best best "")) *mk:art-guess-cache*))
          (if best
            (setq *mk:art-guess-log*
                  (cons (strcat name " -> " best) *mk:art-guess-log*)))))))
  best)

;; Артикул источника: имя стиля мультилинии (DXF 2), иначе имя слоя;
;; сначала явный разбор, затем подбор по базам
(defun mk:mline-style-article (style / u)
  (setq u (if (mk:strp style) (strcase style) ""))
  (cond
    ((wcmatch u "*КП45372*") "КП45372")
    ((wcmatch u "*КП45551*") "КП45551")
    ((wcmatch u "*КП45303*") "КП45303")
    ((wcmatch u "*КП45369*") "КП45369")
    ((wcmatch u "*КПС900*") "КПС 900")
    ((wcmatch u "*КПС344*") "КПС 344")
    (t nil)))

(defun mk:article-from-bases (style / key best bestlen b pair art ak)
  (setq key (mk:squash style) best nil bestlen 0)
  (if (> (strlen key) 0)
    (foreach b *mk:size-bases*
      (foreach pair (cdr b)
        (setq art (car pair) ak (mk:squash art))
        (if (and (> (strlen ak) 0)
                 (>= (strlen key) (strlen ak))
                 (> (strlen ak) bestlen)
                 (mk:str-in? ak key))
          (setq best art bestlen (strlen ak))))))
  best)

(defun mk:ent-article (e / ed a nm ly)
  (setq ed (if e (entget e) nil) a nil)
  (if ed
    (progn
      (setq nm (if (= (cdr (assoc 0 ed)) "MLINE") (cdr (assoc 2 ed)) nil)
            ly (cdr (assoc 8 ed)))
      (if nm (setq a (mk:article-from-bases nm)))
      (if (and (null a) nm) (setq a (mk:mline-style-article nm)))
      (if (and (null a) nm) (setq a (mk:article-from-name nm)))
      (if (null a) (setq a (mk:article-from-name ly)))
      (if (and (null a) nm) (setq a (mk:article-guess nm)))
      (if (null a) (setq a (mk:article-guess ly)))))
  a)

;; Отрезки одного объекта: список (x0 y0 x1 y1)
(defun mk:ent-segs (e / ed typ verts closed segs p sub)
  (setq ed (entget e) segs nil)
  (if (null ed)
    nil
    (progn
      (setq typ (cdr (assoc 0 ed)))
      (cond
        ((= typ "LINE")
         (setq p (cdr (assoc 10 ed)) sub (cdr (assoc 11 ed)))
         (if (and p sub)
           (setq segs (list (list (float (car p))   (float (cadr p))
                                  (float (car sub)) (float (cadr sub)))))))
        ((= typ "LWPOLYLINE")
         (setq verts nil closed nil)
         (if (and (cdr (assoc 70 ed)) (= 1 (logand 1 (cdr (assoc 70 ed)))))
           (setq closed t))
         (foreach p ed
           (if (= (car p) 10)
             (setq verts (cons (list (float (cadr p)) (float (caddr p))) verts))))
         (setq verts (reverse verts))
         (if closed (setq verts (append verts (list (car verts)))))
         (if (>= (length verts) 2) (setq segs (mk:verts-to-segs verts))))
        ((= typ "POLYLINE")
         (setq verts nil closed nil)
         (if (and (cdr (assoc 70 ed)) (= 1 (logand 1 (cdr (assoc 70 ed)))))
           (setq closed t))
         (setq sub (entnext e))
         (while sub
           (setq p (entget sub))
           (if (or (null p) (= "SEQEND" (cdr (assoc 0 p))))
             (setq sub nil)
             (progn
               (if (= "VERTEX" (cdr (assoc 0 p)))
                 (setq verts (cons (list (float (cadr (assoc 10 p)))
                                         (float (caddr (assoc 10 p)))) verts)))
               (setq sub (entnext sub)))))
         (setq verts (reverse verts))
         (if closed (setq verts (append verts (list (car verts)))))
         (if (>= (length verts) 2) (setq segs (mk:verts-to-segs verts))))
        ((= typ "MLINE")
         (setq verts (mk:get-mline-verts e))
         (if (and verts (>= (length verts) 2))
           (setq segs (mk:verts-to-segs verts))))
        (t nil))
      segs)))

;; Все отрезки выборки (кроме блоков)
(defun mk:collect-segs-in-ss (ss / i e typ out segs)
  (setq out nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e   (ssname ss i)
              typ (cdr (assoc 0 (entget e))))
        (if (member typ *mk:seg-types*)
          (progn
            (setq segs (mk:ent-segs e))
            (foreach sg segs (setq out (cons (cons e sg) out)))))
        (setq i (1+ i)))))
  (reverse out))

;; Слияние параллельных отрезков одного профиля:
;; ось = среднее, протяжённость = объединение, сечение = разброс осей.
;; rec: (ename ось нач кон)
(defun mk:merge-tracks (recs gap / sorted out cur base ax a b)
  (setq sorted (vl-sort recs '(lambda (p q) (< (nth 1 p) (nth 1 q))))
        out nil cur nil)
  (foreach r sorted
    (if (and cur
             (<= (- (nth 1 r) (nth 1 (car cur))) *mk:merge-width*)
             (> (+ (min (nth 3 r) (apply 'max (mapcar '(lambda (x) (nth 3 x)) cur))) gap)
                (max (nth 2 r) (apply 'min (mapcar '(lambda (x) (nth 2 x)) cur)))))
      (setq cur (cons r cur))
      (progn
        (if cur (setq out (cons (reverse cur) out)))
        (setq cur (list r)))))
  (if cur (setq out (cons (reverse cur) out)))
  (reverse out))

;; Габарит сечения мультилинии: (max-min смещений стиля) x масштаб элемента
(defun mk:mline-cross (e / ed sd offs w sc)
  (setq ed (if e (entget e) nil) w nil)
  (if (and ed (= (cdr (assoc 0 ed)) "MLINE") (cdr (assoc 340 ed)))
    (progn
      (setq sc   (if (numberp (cdr (assoc 40 ed))) (abs (cdr (assoc 40 ed))) 1.0)
            sd   (entget (cdr (assoc 340 ed)))
            offs nil)
      (foreach pair sd
        (if (= (car pair) 49) (setq offs (cons (cdr pair) offs))))
      (if (> (length offs) 1)
        (setq w (* sc (- (apply 'max offs) (apply 'min offs)))))))
  (if (and (numberp w) (> w 1.0)) w nil))

(defun mk:track-elem (grp etype / axs a0 a1 s0 s1 cross)
  (setq axs   (mapcar '(lambda (x) (nth 1 x)) grp)
        a0    (apply 'min axs)
        a1    (apply 'max axs)
        s0    (apply 'min (mapcar '(lambda (x) (nth 2 x)) grp))
        s1    (apply 'max (mapcar '(lambda (x) (nth 3 x)) grp))
        cross (- a1 a0))
  (list (nth 0 (car grp))            ; ename (первый объект группы)
        (/ (+ a0 a1) 2.0)            ; ось
        s0 s1                        ; протяжённость
        ;; габарит сечения: из геометрии, иначе из стиля мультилинии
        (cond
          ((> cross 1.0) cross)
          ((mk:mline-cross (nth 0 (car grp))) (mk:mline-cross (nth 0 (car grp))))
          (t 0.0))
        etype))

;; Каркас из геометрии + блоков. Блоки в приоритете (у них артикул и «Марка»).
(defun mk:extract-posts-beams-all (ss / segs vrecs hrecs cls len posts beams
                                     bposts bbeams tr e ax s0 s1 cross)
  (setq bposts (mapcar 'mk:collect-post
                 (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*")))
        bbeams (mapcar 'mk:collect-beam
                 (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam* "*"))))
  (setq segs (mk:collect-segs-in-ss ss) vrecs nil hrecs nil)
  (foreach r segs
    (setq e   (car r)
          cls (mk:classify-seg (cdr r)))
    (cond
      ((= cls 'v)
       (setq len (abs (- (nth 4 r) (nth 2 r))))
       (if (> len *mk:min-seg*)
         (setq vrecs (cons (list e (/ (+ (nth 1 r) (nth 3 r)) 2.0)
                                 (min (nth 2 r) (nth 4 r))
                                 (max (nth 2 r) (nth 4 r))) vrecs))))
      ((= cls 'h)
       (setq len (abs (- (nth 3 r) (nth 1 r))))
       (if (> len *mk:min-seg*)
         (setq hrecs (cons (list e (/ (+ (nth 2 r) (nth 4 r)) 2.0)
                                 (min (nth 1 r) (nth 3 r))
                                 (max (nth 1 r) (nth 3 r))) hrecs))))))
  (setq posts nil beams nil)
  (foreach grp (mk:merge-tracks vrecs *mk:join-posts-gap*)
    (setq tr (mk:track-elem grp "СТОЙКА")
          ax (nth 1 tr) s0 (nth 2 tr) s1 (nth 3 tr) cross (nth 4 tr))
    (if (not (mk:elem-covered? ax s0 s1 bposts "СТОЙКА"))
      (setq posts (cons (list
        (cons 'TYPE "СТОЙКА") (cons 'ENAME (nth 0 tr))
        (cons 'INS_PT (list ax s0 0.0))
        (cons 'LENGTH (- s1 s0)) (cons 'SIZE_SRC "GEOM")
        (cons 'CROSS cross) (cons 'ARTICLE (mk:ent-article (nth 0 tr)))
        (cons 'VISIBILITY nil)
        (cons 'LEFT_CONN nil) (cons 'RIGHT_CONN nil) (cons 'PROTRUDING nil)
      ) posts))))
  (foreach grp (mk:merge-tracks hrecs *mk:join-beams-gap*)
    (setq tr (mk:track-elem grp "РИГЕЛЬ")
          ax (nth 1 tr) s0 (nth 2 tr) s1 (nth 3 tr) cross (nth 4 tr))
    (if (not (mk:elem-covered? ax s0 s1 bbeams "РИГЕЛЬ"))
      (setq beams (cons (list
        (cons 'TYPE "РИГЕЛЬ") (cons 'ENAME (nth 0 tr))
        (cons 'INS_PT (list s0 ax 0.0))
        (cons 'LENGTH (- s1 s0)) (cons 'SIZE_SRC "GEOM")
        (cons 'CROSS cross) (cons 'ARTICLE (mk:ent-article (nth 0 tr)))
        (cons 'VISIBILITY nil)
        (cons 'TOP_ELEM nil) (cons 'BOT_ELEM nil) (cons 'SUFFIX nil)
      ) beams))))
  (prompt (strcat "\n  Все-типы: отрезков " (itoa (length segs))
                  ", из геометрии стоек " (itoa (length posts))
                  ", ригелей " (itoa (length beams))
                  "; из блоков " (itoa (length bposts))
                  "/" (itoa (length bbeams))))
  (list (append bposts (reverse posts))
        (append bbeams (reverse beams))))

;; Совпадает ли геометрический элемент с уже собранным блоком
(defun mk:elem-covered? (ax s0 s1 blocks etype / hit p bx b0 b1)
  (setq hit nil)
  (foreach b blocks
    (if (null hit)
      (progn
        (setq p (cdr (assoc 'INS_PT b)))
        (if p
          (progn
            (if (= etype "СТОЙКА")
              (setq bx (car p)
                    b0 (cadr p)
                    b1 (+ (cadr p) (if (cdr (assoc 'LENGTH b)) (cdr (assoc 'LENGTH b)) 0.0)))
              (setq bx (cadr p)
                    b0 (car p)
                    b1 (+ (car p) (if (cdr (assoc 'LENGTH b)) (cdr (assoc 'LENGTH b)) 0.0))))
            (if (and (<= (abs (- ax bx)) *mk:merge-width*)
                     (> (min s1 b1) (max s0 b0)))
              (setq hit t)))))))
  hit)

;;;=====================================================================
;;; 9. СБОР ДАННЫХ ПО БЛОКАМ (маски *ысот* *ирин* *лин*)
;;;=====================================================================
(defun mk:collect-fill (ename / obj geom dh dw)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dh   (mk:dim-src obj *mk:mask-height* 'mk:bb-h)
        dw   (mk:dim-src obj *mk:mask-width*  'mk:bb-w))
  (list
    (cons 'TYPE       "ЗАПОЛНЕНИЕ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (car dh))
    (cons 'WIDTH      (car dw))
    (cons 'SIZE_SRC   (strcat (cadr dh) "/" (cadr dw)))
    (cons 'THICKNESS  (mk:get-attr ename *mk:attr-thickness*))
    (cons 'ARTICLE    (mk:get-attr ename *mk:attr-article*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-window (ename / obj geom dh dw)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dh   (mk:dim-src obj *mk:mask-height* 'mk:bb-h)
        dw   (mk:dim-src obj *mk:mask-width*  'mk:bb-w))
  (list
    (cons 'TYPE       "ОКНО")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (car dh))
    (cons 'WIDTH      (car dw))
    (cons 'SIZE_SRC   (strcat (cadr dh) "/" (cadr dw)))
    (cons 'NAME       (mk:get-attr ename *mk:attr-name*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-door (ename / obj geom dh dw)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dh   (mk:dim-src obj *mk:mask-height* 'mk:bb-h)
        dw   (mk:dim-src obj *mk:mask-width*  'mk:bb-w))
  (list
    (cons 'TYPE       "ДВЕРЬ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (car dh))
    (cons 'WIDTH      (car dw))
    (cons 'SIZE_SRC   (strcat (cadr dh) "/" (cadr dw)))
    (cons 'NAME       (mk:get-attr ename *mk:attr-name*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-post (ename / obj geom dl)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dl   (mk:dim-src obj *mk:mask-length* 'mk:bb-h))
  (list
    (cons 'TYPE       "СТОЙКА")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'LENGTH     (car dl))
    (cons 'SIZE_SRC   (cadr dl))
    (cons 'CROSS      (mk:bb-w (mk:e-bb obj)))
    (cons 'ARTICLE    (mk:get-attr ename *mk:attr-article*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'LEFT_CONN  nil)
    (cons 'RIGHT_CONN nil)
    (cons 'PROTRUDING nil)
  ))

(defun mk:collect-beam (ename / obj geom dl)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dl   (mk:dim-src obj *mk:mask-length* 'mk:bb-w))
  (list
    (cons 'TYPE       "РИГЕЛЬ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'LENGTH     (car dl))
    (cons 'SIZE_SRC   (cadr dl))
    (cons 'CROSS      (mk:bb-h (mk:e-bb obj)))
    (cons 'ARTICLE    (mk:get-attr ename *mk:attr-article*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'TOP_ELEM   nil)
    (cons 'BOT_ELEM   nil)
    (cons 'SUFFIX     nil)
  ))

;;;=====================================================================
(defun mk:assign-vitrage-records (records frames / out r e o bb fr ar best besta mark)
  (setq out nil)
  (foreach r records
    (setq e (cdr (assoc 'ENAME r)) best nil besta 0.0 mark "<ВНЕ РАМОК>")
    (if e
      (progn
        (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
        (if (not (vl-catch-all-error-p o))
          (progn
            (setq bb (mk:e-bb o))
            (foreach fr frames
              (setq ar (mk:bb-overlap-area bb (cadr fr)))
              (if (> ar besta) (setq besta ar best fr)))
            (if best (setq mark (caddr best)))))))
    (setq out (cons (cons (cons 'VITRAGE mark) r) out)))
  (reverse out))

;;; 10. ПОИСК БЛОКОВ В ВЫБОРКЕ
;;;=====================================================================
;; Элемент каркаса пригоден, если есть точка вставки и положительная длина
(defun mk:valid-frame? (el / p l)
  (setq p (cdr (assoc 'INS_PT el))
        l (cdr (assoc 'LENGTH el)))
  (and p (listp p) (numberp (car p)) (numberp (cadr p))
       (numberp l) (> l 0.0)))

;; Панель пригодна, если есть точка вставки и положительные габариты
(defun mk:valid-panel? (el / p w h)
  (setq p (cdr (assoc 'INS_PT el))
        w (cdr (assoc 'WIDTH el))
        h (cdr (assoc 'HEIGHT el)))
  (and p (listp p) (numberp (car p)) (numberp (cadr p))
       (numberp w) (> w 0.0) (numberp h) (> h 0.0)))

(defun mk:sanitize-panels (els label / out bad)
  (setq out nil bad 0)
  (foreach el els
    (if (mk:valid-panel? el)
      (setq out (cons el out))
      (setq bad (1+ bad))))
  (if (> bad 0)
    (prompt (strcat "\n  [WARN] " label ": отброшено без габаритов — " (itoa bad))))
  (reverse out))

(defun mk:sanitize-frame (els label / out bad)
  (setq out nil bad 0)
  (foreach el els
    (if (mk:valid-frame? el)
      (setq out (cons el out))
      (setq bad (1+ bad))))
  (if (> bad 0)
    (prompt (strcat "\n  [WARN] " label ": отброшено без размеров — " (itoa bad))))
  (reverse out))

(defun mk:find-blocks-in-ss (ss mask / i e lst)
  (setq lst nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (mk:blk-match? e mask)
          (setq lst (cons e lst)))
        (setq i (1+ i)))))
  lst)

;; Блоки выборки по списку имён/масок — без дублей (один блок, подходящий под
;; две маски, попадает в список один раз)
(defun mk:find-blocks-any (ss masks / i e lst hit)
  (setq lst nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) hit nil)
        (foreach m masks
          (if (and (null hit) m (mk:blk-match? e m)) (setq hit t)))
        (if hit (setq lst (cons e lst)))
        (setq i (1+ i)))))
  lst)

;; Проёмы выборки по маскам с учётом масок-исключений («Кассета» и т.п.):
;; глухое заполнение, случайно совпавшее с "*ОКН*", окном не становится.
(defun mk:find-proems (ss masks / out)
  (setq out nil)
  (foreach e (mk:find-blocks-any ss masks)
    (if (not (mk:opening-skipped? e)) (setq out (cons e out))))
  (reverse out))

(defun mk:find-mlines-in-ss (ss / i e lst)
  (setq lst nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (= (cdr (assoc 0 (entget e))) "MLINE")
          (setq lst (cons e lst)))
        (setq i (1+ i)))))
  lst)

;;;=====================================================================
;;; 11. ОПРЕДЕЛЕНИЕ ГРАНИЦ ВИТРАЖА
;;;=====================================================================
(defun mk:find-vitrage-bounds (posts beams / min-x max-x min-y max-y x y len)
  (setq min-x nil max-x nil min-y nil max-y nil)
  (foreach post posts
    (if (cdr (assoc 'INS_PT post))
      (progn
        (setq x (car (cdr (assoc 'INS_PT post))))
        (setq y (cadr (cdr (assoc 'INS_PT post))))
        (setq len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
        (if (or (null min-x) (< x min-x)) (setq min-x x))
        (if (or (null max-x) (> x max-x)) (setq max-x x))
        (if (or (null min-y) (< y min-y)) (setq min-y y))
        (if (or (null max-y) (> (+ y len) max-y)) (setq max-y (+ y len))))))
  (foreach beam beams
    (if (cdr (assoc 'INS_PT beam))
      (progn
        (setq x (car (cdr (assoc 'INS_PT beam))))
        (setq y (cadr (cdr (assoc 'INS_PT beam))))
        (setq len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
        (if (or (null min-x) (< x min-x)) (setq min-x x))
        (if (or (null max-x) (> (+ x len) max-x)) (setq max-x (+ x len)))
        (if (or (null min-y) (< y min-y)) (setq min-y y))
        (if (or (null max-y) (> y max-y)) (setq max-y y)))))
  (list
    (cons 'MIN_X (if min-x min-x 0.0))
    (cons 'MAX_X (if max-x max-x 0.0))
    (cons 'MIN_Y (if min-y min-y 0.0))
    (cons 'MAX_Y (if max-y max-y 0.0))))

;;;=====================================================================
;;; 12. НОРМАЛИЗАЦИЯ ПАНЕЛЕЙ
;;;=====================================================================
(defun mk:normalize-panel (panel bounds / ins-pt h w xsc ysc new-x new-y min-x max-x min-y max-y)
  (setq ins-pt (cdr (assoc 'INS_PT panel))
        h      (if (cdr (assoc 'HEIGHT panel)) (cdr (assoc 'HEIGHT panel)) 0.0)
        w      (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0)
        xsc    (if (cdr (assoc 'XSCALE panel)) (cdr (assoc 'XSCALE panel)) 1.0)
        ysc    (if (cdr (assoc 'YSCALE panel)) (cdr (assoc 'YSCALE panel)) 1.0)
        min-x  (cdr (assoc 'MIN_X bounds))
        max-x  (cdr (assoc 'MAX_X bounds))
        min-y  (cdr (assoc 'MIN_Y bounds))
        max-y  (cdr (assoc 'MAX_Y bounds)))
  (if ins-pt
    (progn
      (setq new-x (car ins-pt) new-y (cadr ins-pt))
      (if (< xsc 0.0)
        (progn (setq new-x (+ new-x w))
               (prompt (strcat "\n  [WARN] Панель скорректирована по X: " (rtos new-x 2 1)))))
      (if (< ysc 0.0)
        (progn (setq new-y (+ new-y h))
               (prompt (strcat "\n  [WARN] Панель скорректирована по Y: " (rtos new-y 2 1)))))
      ;; Панель у самого края витража — НЕ зеркалим: раньше здесь вычиталась
      ;; ширина, и панель уезжала через весь витраж (так появлялись «панель за
      ;; правой границей» и перекрытия на ровном месте). Сдвиг — только если
      ;; панель действительно выступает за габарит, и не дальше допуска.
      (if (> (+ new-x w) (+ max-x *mk:tol-skew*))
        (progn (setq new-x (- new-x w))
               (prompt (strcat "\n  [WARN] Панель выступает за правую границу витража,"
                               " сдвинута на ширину: " (rtos new-x 2 1)))))
      (if (> (+ new-y h) (+ max-y *mk:tol-skew*))
        (progn (setq new-y (- new-y h))
               (prompt (strcat "\n  [WARN] Панель за верхней границей: " (rtos new-y 2 1)))))
      (setq panel (mk:rec-put panel 'INS_PT (list new-x new-y 0.0)))))
  panel)

;;;=====================================================================
;;; 13. ПРОВЕРКИ И ВАЛИДАЦИЯ
;;;=====================================================================
(defun mk:check-duplicates (elements / i j pt1 pt2 tol count)
  (setq tol *mk:tol-adjacency* count 0 i 0)
  (foreach el1 elements
    (setq j 0)
    (foreach el2 elements
      (if (> j i)
        (progn
          (setq pt1 (cdr (assoc 'INS_PT el1))
                pt2 (cdr (assoc 'INS_PT el2)))
          (if (and pt1 pt2
                   (< (abs (- (car pt1) (car pt2))) tol)
                   (< (abs (- (cadr pt1) (cadr pt2))) tol))
            (progn
              (prompt (strcat "\n  [ERROR] Дубликат: " (cdr (assoc 'TYPE el1))
                              " в точке " (rtos (car pt1) 2 1) "," (rtos (cadr pt1) 2 1)))
              (setq count (1+ count))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (if (> count 0)
    (prompt (strcat "\n  [ИТОГО] Дубликатов: " (itoa count))))
  count)

;; Пара «заполнение + окно/дверь» одного проёма — не ошибка: это один
;; световой проём, на который поставлены два блока (заполнение и дверь/окно).
;; Пара «заполнение + окно/дверь» одного проёма — не ошибка: это один световой
;; проём с двумя блоками (заполнение и дверь/окно). Габариты у блоков могут
;; отличаться (дверь шире/ниже), поэтому сравниваем не точное равенство, а
;; соизмеримость: размеры различаются не более чем в *mk:same-cell-ratio* раз.
(setq *mk:same-cell-ratio* 1.6)

(defun mk:same-cell-dim? (a b tol / mx mn)
  (and (numberp a) (numberp b) (> a 0.0) (> b 0.0)
       (progn (setq mx (max a b) mn (min a b))
              (or (>= mx (- mn tol))
                  (<= (/ mx mn) *mk:same-cell-ratio*)))))

(defun mk:same-cell? (p1 p2 / t1 t2)
  (setq t1 (cdr (assoc 'TYPE p1)) t2 (cdr (assoc 'TYPE p2)))
  (and (or (and (= t1 "ЗАПОЛНЕНИЕ") (member t2 '("ОКНО" "ДВЕРЬ")))
           (and (= t2 "ЗАПОЛНЕНИЕ") (member t1 '("ОКНО" "ДВЕРЬ"))))
       (mk:same-cell-dim? (cdr (assoc 'WIDTH p1))  (cdr (assoc 'WIDTH p2))  200.0)
       (mk:same-cell-dim? (cdr (assoc 'HEIGHT p1)) (cdr (assoc 'HEIGHT p2)) 200.0)))

(defun mk:check-overlaps (panels / i j count x1 y1 w1 h1 x2 y2 w2 h2)
  (setq count 0 i 0)
  (foreach p1 panels
    (setq j 0)
    (foreach p2 panels
      (if (> j i)
        (progn
          (setq x1 (car (cdr (assoc 'INS_PT p1)))
                y1 (cadr (cdr (assoc 'INS_PT p1)))
                w1 (if (cdr (assoc 'WIDTH p1)) (cdr (assoc 'WIDTH p1)) 0.0)
                h1 (if (cdr (assoc 'HEIGHT p1)) (cdr (assoc 'HEIGHT p1)) 0.0))
          (setq x2 (car (cdr (assoc 'INS_PT p2)))
                y2 (cadr (cdr (assoc 'INS_PT p2)))
                w2 (if (cdr (assoc 'WIDTH p2)) (cdr (assoc 'WIDTH p2)) 0.0)
                h2 (if (cdr (assoc 'HEIGHT p2)) (cdr (assoc 'HEIGHT p2)) 0.0))
          (if (and (not (mk:same-cell? p1 p2))
                   (< x1 (+ x2 w2)) (> (+ x1 w1) x2)
                   (< y1 (+ y2 h2)) (> (+ y1 h1) y2))
            (progn
              (prompt (strcat "\n  [ERROR] Перекрытие: " (cdr (assoc 'TYPE p1))
                              " и " (cdr (assoc 'TYPE p2))
                              " в области " (rtos x1 2 1) "," (rtos y1 2 1)))
              (setq count (1+ count))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (if (> count 0)
    (prompt (strcat "\n  [ИТОГО] Перекрытий: " (itoa count))))
  count)

(defun mk:check-out-of-grid (elements bounds / min-x max-x min-y max-y count x y)
  (setq min-x (cdr (assoc 'MIN_X bounds))
        max-x (cdr (assoc 'MAX_X bounds))
        min-y (cdr (assoc 'MIN_Y bounds))
        max-y (cdr (assoc 'MAX_Y bounds))
        count 0)
  (foreach el elements
    (if (cdr (assoc 'INS_PT el))
      (progn
        (setq x (car (cdr (assoc 'INS_PT el)))
              y (cadr (cdr (assoc 'INS_PT el))))
        (if (or (< x (- min-x *mk:tol-skew*)) (> x (+ max-x *mk:tol-skew*))
                (< y (- min-y *mk:tol-skew*)) (> y (+ max-y *mk:tol-skew*)))
          (progn
            (prompt (strcat "\n  [WARN] Вне сетки: " (cdr (assoc 'TYPE el))
                            " в точке " (rtos x 2 1) "," (rtos y 2 1)))
            (setq count (1+ count)))))))
  (if (> count 0)
    (prompt (strcat "\n  [ИТОГО] Вне сетки: " (itoa count))))
  count)

(defun mk:identify-protruding-posts (posts bounds / min-y max-y new-posts protruding-count x y len)
  (setq min-y (cdr (assoc 'MIN_Y bounds))
        max-y (cdr (assoc 'MAX_Y bounds))
        new-posts nil protruding-count 0)
  (foreach post posts
    (if (cdr (assoc 'INS_PT post))
      (progn
        (setq x (car (cdr (assoc 'INS_PT post))))
        (setq y (cadr (cdr (assoc 'INS_PT post))))
        (setq len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
        (if (or (< y (- min-y *mk:tol-skew*))
                (> (+ y len) (+ max-y *mk:tol-skew*)))
          (progn
            (prompt (strcat "\n  [INFO] Выступающая стойка: " (rtos x 2 1) "," (rtos y 2 1)))
            (setq post (mk:rec-put post 'PROTRUDING t))
            (setq protruding-count (1+ protruding-count)))
          (setq post (mk:rec-put post 'PROTRUDING nil)))
        (setq new-posts (cons post new-posts)))
      (setq new-posts (cons post new-posts))))
  (list (reverse new-posts) protruding-count))

;; Валидация ригеля — проверка ОБОИХ концов.
;; Ригель вставляется с отступом от оси стойки (ширина профиля), поэтому
;; допуск по X = *mk:offset-fill* + *mk:tol-adjacency*, а не голый допуск 5 мм.
(defun mk:post-span (post / py plen)
  (setq py   (cadr (cdr (assoc 'INS_PT post)))
        plen (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
  (list (- py *mk:tol-adjacency*) (+ py plen *mk:tol-adjacency*)))

(defun mk:beam-touches-post? (bx by post / px span tol)
  (setq tol  (+ *mk:offset-fill* *mk:tol-adjacency*)
        px   (car (cdr (assoc 'INS_PT post)))
        span (mk:post-span post))
  (and (<= (abs (- bx px)) tol)
       (>= by (car span))
       (<= by (cadr span))))

(defun mk:validate-beam (beam posts beams / has-connection bx by bx2 ox oy beam-len)
  ;; Ред. 3.9: вертикальный ригель (импост Т-узла) примыкает торцами
  ;; к серединам ригелей — это норма, а не ошибка.
  (if (mk:rec-get beam 'VERT)
    (setq has-connection t)
    (setq has-connection nil))
  (if (not has-connection)
   (progn
  (setq beam-len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
  (setq bx  (car (cdr (assoc 'INS_PT beam)))
        by  (cadr (cdr (assoc 'INS_PT beam)))
        bx2 (+ bx beam-len))
  (foreach post posts
    (if (and (cdr (assoc 'INS_PT post)) (not has-connection))
      (if (or (mk:beam-touches-post? bx  by post)
              (mk:beam-touches-post? bx2 by post))
        (setq has-connection t))))
  (if (not has-connection)
    (foreach other-beam beams
      (if (and (not (eq beam other-beam))
               (cdr (assoc 'INS_PT other-beam))
               (not has-connection))
        (progn
          (setq ox (car (cdr (assoc 'INS_PT other-beam)))
                oy (cadr (cdr (assoc 'INS_PT other-beam))))
          (if (or (and (< (abs (- bx ox)) *mk:tol-adjacency*)
                       (< (abs (- by oy)) *mk:tol-adjacency*))
                  (and (< (abs (- bx2 ox)) *mk:tol-adjacency*)
                       (< (abs (- by oy)) *mk:tol-adjacency*)))
            (setq has-connection t))))))
  (if (not has-connection)
    (prompt (strcat "\n  [ERROR] Ригель без примыкания: "
                    (rtos bx 2 1) "," (rtos by 2 1))))))
  has-connection)

(defun mk:validate-fill (fill posts beams / left-post right-post bounds b)
  ;; Ред. 3.9: границей ячейки может быть и вертикальный ригель (импост
  ;; Т-узла) — подъячейки под фрамугой не должны считаться ошибкой.
  (setq bounds posts)
  (foreach b beams
    (if (mk:rec-get b 'VERT) (setq bounds (cons b bounds))))
  (setq left-post (mk:find-left-post fill bounds)
        right-post (mk:find-right-post fill bounds))
  (if (or (null left-post) (null right-post))
    (progn
      (prompt (strcat "\n  [ERROR] Заполнение вне ячейки: "
                      (mk:safe-num (car (cdr (assoc 'INS_PT fill)))) ","
                      (mk:safe-num (cadr (cdr (assoc 'INS_PT fill))))))
      nil)
    t))

;;;=====================================================================
;;; 14. ПОСТРОЕНИЕ ТОПОЛОГИИ СВЯЗЕЙ
;;;=====================================================================
(defun mk:find-left-post (panel posts / panel-x left-post best-dist dist post-x)
  (setq panel-x (car (cdr (assoc 'INS_PT panel)))
        left-post nil best-dist nil)
  (foreach post posts
    (setq post-x (car (cdr (assoc 'INS_PT post))))
    (if (< post-x panel-x)
      (progn
        (setq dist (- panel-x post-x))
        (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                 (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
          (if (or (null best-dist) (< dist best-dist))
            (progn (setq best-dist dist) (setq left-post post)))))))
  left-post)

(defun mk:find-right-post (panel posts / panel-x panel-w right-post best-dist dist post-x)
  (setq panel-x (car (cdr (assoc 'INS_PT panel)))
        panel-w (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0)
        right-post nil best-dist nil)
  (foreach post posts
    (setq post-x (car (cdr (assoc 'INS_PT post))))
    (if (> post-x (+ panel-x panel-w))
      (progn
        (setq dist (- post-x (+ panel-x panel-w)))
        (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                 (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
          (if (or (null best-dist) (< dist best-dist))
            (progn (setq best-dist dist) (setq right-post post)))))))
  right-post)

(defun mk:find-connected-posts (panel posts / left right)
  (setq left  (mk:find-left-post panel posts)
        right (mk:find-right-post panel posts))
  (list (cons 'LEFT left) (cons 'RIGHT right)))

(defun mk:find-left-elements (post panels beams / post-x left-list panel-x panel-w beam-x)
  (setq post-x (car (cdr (assoc 'INS_PT post))) left-list nil)
  (foreach panel panels
    (if (cdr (assoc 'INS_PT panel))
      (progn
        (setq panel-x (car (cdr (assoc 'INS_PT panel)))
              panel-w (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0))
        (if (and (<= (+ panel-x panel-w) (+ post-x *mk:tol-skew*))
                 (>= (+ panel-x panel-w) (- post-x 5000.0)))
          (setq left-list (cons panel left-list))))))
  (foreach beam beams
    (if (cdr (assoc 'INS_PT beam))
      (progn
        (setq beam-x (car (cdr (assoc 'INS_PT beam))))
        (if (and (<= beam-x (+ post-x *mk:tol-skew*))
                 (>= beam-x (- post-x 5000.0)))
          (setq left-list (cons beam left-list))))))
  (reverse left-list))

(defun mk:find-right-elements (post panels beams / post-x right-list panel-x beam-x)
  (setq post-x (car (cdr (assoc 'INS_PT post))) right-list nil)
  (foreach panel panels
    (if (cdr (assoc 'INS_PT panel))
      (progn
        (setq panel-x (car (cdr (assoc 'INS_PT panel))))
        (if (and (>= panel-x (- post-x *mk:tol-skew*))
                 (<= panel-x (+ post-x 5000.0)))
          (setq right-list (cons panel right-list))))))
  (foreach beam beams
    (if (cdr (assoc 'INS_PT beam))
      (progn
        (setq beam-x (car (cdr (assoc 'INS_PT beam))))
        (if (and (>= beam-x (- post-x *mk:tol-skew*))
                 (<= beam-x (+ post-x 5000.0)))
          (setq right-list (cons beam right-list))))))
  (reverse right-list))

;; Панель считается примыкающей, только если она перекрывается с ригелем по X
(defun mk:beam-panel-overlap? (beam panel / br pr)
  (setq br (mk:el-xrange beam)
        pr (mk:el-xrange panel))
  (and (< (car br) (- (cadr pr) *mk:tol-adjacency*))
       (> (cadr br) (+ (car pr) *mk:tol-adjacency*))))

(defun mk:find-top-element (beam panels / beam-y top-elem best-dist dist panel-y)
  (setq beam-y (cadr (cdr (assoc 'INS_PT beam)))
        top-elem nil best-dist nil)
  (foreach panel panels
    (if (and (cdr (assoc 'INS_PT panel))
             (mk:beam-panel-overlap? beam panel))
      (progn
        (setq panel-y (cadr (cdr (assoc 'INS_PT panel))))
        (if (> panel-y beam-y)
          (progn
            (setq dist (- panel-y beam-y))
            (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                     (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
              (if (or (null best-dist) (< dist best-dist))
                (progn (setq best-dist dist) (setq top-elem panel)))))))))
  top-elem)

(defun mk:find-bot-element (beam panels / beam-y bot-elem best-dist dist panel-y panel-h)
  (setq beam-y (cadr (cdr (assoc 'INS_PT beam)))
        bot-elem nil best-dist nil)
  (foreach panel panels
    (if (and (cdr (assoc 'INS_PT panel))
             (mk:beam-panel-overlap? beam panel))
      (progn
        (setq panel-y (cadr (cdr (assoc 'INS_PT panel)))
              panel-h (if (cdr (assoc 'HEIGHT panel)) (cdr (assoc 'HEIGHT panel)) 0.0))
        (if (< (+ panel-y panel-h) beam-y)
          (progn
            (setq dist (- beam-y (+ panel-y panel-h)))
            (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                     (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
              (if (or (null best-dist) (< dist best-dist))
                (progn (setq best-dist dist) (setq bot-elem panel)))))))))
  bot-elem)

(defun mk:build-topology (posts beams fills windows doors bounds /
                          all-panels new-posts new-beams new-fills
                          new-windows new-doors post beam panel)
  (setq all-panels (append fills windows doors))
  (setq new-fills nil)
  (foreach panel fills
    (setq panel (mk:normalize-panel panel bounds))
    (setq panel (mk:rec-put panel 'CONN_POSTS (mk:find-connected-posts panel posts)))
    (setq new-fills (cons panel new-fills)))
  (setq fills (reverse new-fills))
  (setq new-windows nil)
  (foreach panel windows
    (setq panel (mk:normalize-panel panel bounds))
    (setq panel (mk:rec-put panel 'CONN_POSTS (mk:find-connected-posts panel posts)))
    (setq new-windows (cons panel new-windows)))
  (setq windows (reverse new-windows))
  (setq new-doors nil)
  (foreach panel doors
    (setq panel (mk:normalize-panel panel bounds))
    (setq panel (mk:rec-put panel 'CONN_POSTS (mk:find-connected-posts panel posts)))
    (setq new-doors (cons panel new-doors)))
  (setq doors (reverse new-doors))
  (setq all-panels (append fills windows doors))
  (setq new-posts nil)
  (foreach post posts
    (setq post (mk:rec-put post 'LEFT_CONN  (mk:find-left-elements post all-panels beams)))
    (setq post (mk:rec-put post 'RIGHT_CONN (mk:find-right-elements post all-panels beams)))
    (setq new-posts (cons post new-posts)))
  (setq posts (reverse new-posts))
  (setq new-beams nil)
  (foreach beam beams
    (setq beam (mk:rec-put beam 'TOP_ELEM (mk:find-top-element beam all-panels)))
    (setq beam (mk:rec-put beam 'BOT_ELEM (mk:find-bot-element beam all-panels)))
    (setq new-beams (cons beam new-beams)))
  (setq beams (reverse new-beams))
  (list posts beams fills windows doors))

;;;=====================================================================
;;; 15. ВИЗУАЛИЗАЦИЯ (БАЗОВАЯ)
;;;=====================================================================
;; Все создаваемые объекты копятся, чтобы собрать их в группу
(setq *mk:drawn* nil)

;; Создание слоя через таблицу символов: имя может содержать пробелы,
;; поэтому (command "_.LAYER" ...) не годится — пробел там = Enter.
(defun mk:ensure-layer (name color)
  (if (null (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   (cons 70 0)
                   (cons 62 color)
                   '(6 . "Continuous"))))
  name)

(defun mk:emk (dxf / e)
  (setq e (entmakex dxf))
  (if e (setq *mk:drawn* (cons e *mk:drawn*)))
  e)

;; Группа объектов модели — чтобы можно было удалить одним выбором
;; (донор: mark:ar-make-group из MarkZ)
(defun mk:make-group (name enames / doc groups old grp arr i n)
  (if (and enames (> (length enames) 0))
    (progn
      (setq doc    (mk:ax-get (vlax-get-acad-object) "ActiveDocument")
            groups (if doc (mk:ax-get doc "Groups") nil))
      (if (null groups)
        (prompt "\n  [WARN] Коллекция Groups недоступна — группа не создана.")
        (progn
          (setq old (vl-catch-all-apply 'vlax-invoke-method
                      (list groups "Item" name)))
          (if (and old (not (vl-catch-all-error-p old)))
            (vl-catch-all-apply 'vlax-invoke-method (list old "Delete")))
          (setq grp (vl-catch-all-apply 'vlax-invoke-method
                      (list groups "Add" name)))
          (if (or (vl-catch-all-error-p grp) (null grp))
            (prompt (strcat "\n  [WARN] Группа \"" name "\" не создана."))
            (progn
              (setq n   (length enames)
                    arr (vlax-make-safearray vlax-vbObject (cons 0 (1- n)))
                    i   0)
              (foreach e enames
                (vlax-safearray-put-element arr i (vlax-ename->vla-object e))
                (setq i (1+ i)))
              (vl-catch-all-apply 'vlax-invoke-method
                (list grp "AppendItems" arr))
              (prompt (strcat "\n[OK] Группа \"" name "\": "
                              (itoa n) " объект(ов)"))))))))
  nil)
(defun mk:draw-post (post / pt len)
  (setq pt  (cdr (assoc 'INS_PT post))
        len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
  (if pt
    (progn
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                      '(62 . 1)))
      (mk:emk (list '(0 . "POINT")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      '(62 . 2))))))

(defun mk:draw-beam (beam / pt len)
  (setq pt  (cdr (assoc 'INS_PT beam))
        len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
  (if pt
    (mk:emk (list '(0 . "LINE")
                    (cons 10 (list (car pt) (cadr pt) 0.0))
                    (cons 11 (list (+ (car pt) len) (cadr pt) 0.0))
                    '(62 . 3)))))

(defun mk:draw-panel (panel / pt h w)
  (setq pt (cdr (assoc 'INS_PT panel))
        h  (if (cdr (assoc 'HEIGHT panel)) (cdr (assoc 'HEIGHT panel)) 0.0)
        w  (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0))
  (if (and pt (> h 0) (> w 0))
    (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 5)
                    (cons 10 (list (car pt) (cadr pt)))
                    (cons 10 (list (+ (car pt) w) (cadr pt)))
                    (cons 10 (list (+ (car pt) w) (+ (cadr pt) h)))
                    (cons 10 (list (car pt) (+ (cadr pt) h)))))))

(defun mk:draw-bounds (bounds / min-x max-x min-y max-y)
  (setq min-x (cdr (assoc 'MIN_X bounds))
        max-x (cdr (assoc 'MAX_X bounds))
        min-y (cdr (assoc 'MIN_Y bounds))
        max-y (cdr (assoc 'MAX_Y bounds)))
  (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 2)
                  (cons 10 (list min-x min-y))
                  (cons 10 (list max-x min-y))
                  (cons 10 (list max-x max-y))
                  (cons 10 (list min-x max-y)))))

(defun mk:draw-model (posts beams fills windows doors bounds / layer prev-layer)
  (setq layer *mk:layer-model*)
  (setq *mk:drawn* nil)
  (mk:ensure-layer layer 7)
  (setq prev-layer (getvar "CLAYER"))
  (setvar "CLAYER" layer)
  (mk:draw-bounds bounds)
  (foreach post posts (mk:draw-post post))
  (foreach beam beams (mk:draw-beam beam))
  (foreach panel (append fills windows doors) (mk:draw-panel panel))
  (setvar "CLAYER" prev-layer)
  (prompt (strcat "\n[OK] 2D модель отрисована на слое: " layer))
  (mk:make-group *mk:group-model* (reverse *mk:drawn*)))

;;;=====================================================================
;;; 16. ТЕСТОВАЯ ВИЗУАЛИЗАЦИЯ
;;;=====================================================================
(defun mk:draw-fill-cross (fill / pt h w x1 y1 x2 y2)
  (setq pt (cdr (assoc 'INS_PT fill))
        h  (if (cdr (assoc 'HEIGHT fill)) (cdr (assoc 'HEIGHT fill)) 0.0)
        w  (if (cdr (assoc 'WIDTH fill)) (cdr (assoc 'WIDTH fill)) 0.0))
  (if (and pt (> h 0) (> w 0))
    (progn
      (setq x1 (car pt) y1 (cadr pt)
            x2 (+ (car pt) w) y2 (+ (cadr pt) h))
      (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 5)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list x1 y1 0.0))
                      (cons 11 (list x2 y2 0.0))
                      '(62 . 5)))
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list x2 y1 0.0))
                      (cons 11 (list x1 y2 0.0))
                      '(62 . 5))))))

(defun mk:draw-window-label (win / pt h w x1 y1 x2 y2 cx cy name)
  (setq pt (cdr (assoc 'INS_PT win))
        h  (if (cdr (assoc 'HEIGHT win)) (cdr (assoc 'HEIGHT win)) 0.0)
        w  (if (cdr (assoc 'WIDTH win)) (cdr (assoc 'WIDTH win)) 0.0)
        name (if (cdr (assoc 'NAME win)) (cdr (assoc 'NAME win)) "ОКНО"))
  (if (and pt (> h 0) (> w 0))
    (progn
      (setq x1 (car pt) y1 (cadr pt)
            x2 (+ (car pt) w) y2 (+ (cadr pt) h)
            cx (/ (+ x1 x2) 2.0) cy (/ (+ y1 y2) 2.0))
      (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 4)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (mk:emk (list '(0 . "TEXT")
                      (cons 10 (list cx cy 0.0))
                      (cons 40 100.0) (cons 1 "ОКНО")
                      (cons 72 1) (cons 11 (list cx cy 0.0))
                      '(62 . 4)))
      (if (and name (mk:strp name))
        (mk:emk (list '(0 . "TEXT")
                        (cons 10 (list cx (- cy 120.0) 0.0))
                        (cons 40 80.0) (cons 1 name)
                        (cons 72 1) (cons 11 (list cx (- cy 120.0) 0.0))
                        '(62 . 4)))))))

(defun mk:draw-door-label (door / pt h w x1 y1 x2 y2 cx cy name)
  (setq pt (cdr (assoc 'INS_PT door))
        h  (if (cdr (assoc 'HEIGHT door)) (cdr (assoc 'HEIGHT door)) 0.0)
        w  (if (cdr (assoc 'WIDTH door)) (cdr (assoc 'WIDTH door)) 0.0)
        name (if (cdr (assoc 'NAME door)) (cdr (assoc 'NAME door)) "ДВЕРЬ"))
  (if (and pt (> h 0) (> w 0))
    (progn
      (setq x1 (car pt) y1 (cadr pt)
            x2 (+ (car pt) w) y2 (+ (cadr pt) h)
            cx (/ (+ x1 x2) 2.0) cy (/ (+ y1 y2) 2.0))
      (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 6)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (mk:emk (list '(0 . "TEXT")
                      (cons 10 (list cx cy 0.0))
                      (cons 40 100.0) (cons 1 "ДВЕРЬ")
                      (cons 72 1) (cons 11 (list cx cy 0.0))
                      '(62 . 6)))
      (if (and name (mk:strp name))
        (mk:emk (list '(0 . "TEXT")
                        (cons 10 (list cx (- cy 120.0) 0.0))
                        (cons 40 80.0) (cons 1 name)
                        (cons 72 1) (cons 11 (list cx (- cy 120.0) 0.0))
                        '(62 . 6)))))))

(defun mk:draw-grid (posts beams / pt len)
  (foreach post posts
    (setq pt  (cdr (assoc 'INS_PT post))
          len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
    (if pt
      (progn
        (if (cdr (assoc 'PROTRUDING post))
          (mk:emk (list '(0 . "LINE")
                          (cons 10 (list (car pt) (cadr pt) 0.0))
                          (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                          '(62 . 1) (cons 6 "DASHED")))
          (mk:emk (list '(0 . "LINE")
                          (cons 10 (list (car pt) (cadr pt) 0.0))
                          (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                          '(62 . 1))))
        (mk:emk (list '(0 . "POINT")
                        (cons 10 (list (car pt) (cadr pt) 0.0))
                        '(62 . 2))))))
  (foreach beam beams
    (setq pt  (cdr (assoc 'INS_PT beam))
          len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
    (if pt
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      (cons 11 (list (+ (car pt) len) (cadr pt) 0.0))
                      '(62 . 3))))))

(defun mk:draw-test-model (posts beams fills windows doors bounds / layer prev-layer)
  (setq layer *mk:layer-test*)
  (setq *mk:drawn* nil)
  (mk:ensure-layer layer 7)
  (setq prev-layer (getvar "CLAYER"))
  (setvar "CLAYER" layer)
  (mk:draw-bounds bounds)
  (mk:draw-grid posts beams)
  (foreach fill fills (mk:draw-fill-cross fill))
  (foreach win windows (mk:draw-window-label win))
  (foreach door doors (mk:draw-door-label door))
  (setvar "CLAYER" prev-layer)
  (prompt (strcat "\n[OK] Тестовая модель отрисована на слое: " layer))
  (mk:make-group *mk:group-test* (reverse *mk:drawn*)))

;;;=====================================================================
;;; 17. ВЫВОД ДАННЫХ В ФАЙЛ
;;;=====================================================================
(defun mk:dump-data (vitrage bounds posts beams fills windows doors / f fn)
  (setq fn *mk:out-file*)
  (setq f (open fn "w"))
  (if f
    (progn
      (write-line "=== ДАННЫЕ ВИТРАЖА (MarkZV) ===" f)
      (write-line "" f)
      (write-line "--- ГРАНИЦЫ ВИТРАЖА ---" f)
      (write-line (strcat "  MIN_X: " (mk:safe-num (cdr (assoc 'MIN_X bounds)))) f)
      (write-line (strcat "  MAX_X: " (mk:safe-num (cdr (assoc 'MAX_X bounds)))) f)
      (write-line (strcat "  MIN_Y: " (mk:safe-num (cdr (assoc 'MIN_Y bounds)))) f)
      (write-line (strcat "  MAX_Y: " (mk:safe-num (cdr (assoc 'MAX_Y bounds)))) f)
      (write-line "" f)
      (write-line "--- АТРИБУТЫ ВИТРАЖА ---" f)
      (if vitrage
        (progn
          (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT vitrage)))) f)
          (write-line "  АТРИБУТЫ:" f)
          (foreach attr (cdr (assoc 'ATTRS vitrage))
            (write-line (strcat "    <" (car attr) "> = \"" (cdr attr) "\"") f)))
        (write-line "  [НЕ НАЙДЕН]" f))
      (write-line "" f)
      (write-line "--- СТОЙКИ ---" f)
      (foreach post posts
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT post)))) f)
        (write-line (strcat "  LENGTH: " (vl-prin1-to-string (cdr (assoc 'LENGTH post)))
                            "  (источник: " (if (cdr (assoc 'SIZE_SRC post)) (cdr (assoc 'SIZE_SRC post)) "НЕТ") ")") f)
        (write-line (strcat "  АРТИКУЛ: " (if (cdr (assoc 'ARTICLE post)) (cdr (assoc 'ARTICLE post)) "НЕТ")) f)
        (write-line (strcat "  PROTRUDING: " (if (cdr (assoc 'PROTRUDING post)) "ДА" "НЕТ")) f)
        (write-line (strcat "  СЛЕВА:  " (if (cdr (assoc 'LEFT_TYPES post))  (cdr (assoc 'LEFT_TYPES post))  "?")) f)
        (write-line (strcat "  СПРАВА: " (if (cdr (assoc 'RIGHT_TYPES post)) (cdr (assoc 'RIGHT_TYPES post)) "?")) f)
        (write-line (strcat "  СЕЧЕНИЕ: " (vl-prin1-to-string (cdr (assoc 'CROSS post)))
                            "  (СИАЛ: " (if (mk:sial-size (cdr (assoc 'ARTICLE post)))
                                          (rtos (mk:sial-size (cdr (assoc 'ARTICLE post))) 2 1)
                                          "нет") ")") f)
        (write-line (strcat "  LEFT_CONN: " (itoa (length (cdr (assoc 'LEFT_CONN post))))) f)
        (write-line (strcat "  RIGHT_CONN: " (itoa (length (cdr (assoc 'RIGHT_CONN post))))) f)
        (write-line "" f))
      (write-line "--- РИГЕЛИ ---" f)
      (foreach beam beams
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT beam)))) f)
        (write-line (strcat "  LENGTH: " (vl-prin1-to-string (cdr (assoc 'LENGTH beam)))
                            "  (источник: " (if (cdr (assoc 'SIZE_SRC beam)) (cdr (assoc 'SIZE_SRC beam)) "НЕТ") ")"
                            "  ПРОФИЛЬ: " (if (cdr (assoc 'LENGTH beam))
                                            (rtos (mk:beam-cut-len-el beam) 2 1) "?")) f)
        (write-line (strcat "  АРТИКУЛ: " (if (cdr (assoc 'ARTICLE beam)) (cdr (assoc 'ARTICLE beam)) "НЕТ")) f)
        (write-line (strcat "  TOP_ELEM: " (if (cdr (assoc 'TOP_ELEM beam)) "Есть" "Нет")) f)
        (write-line (strcat "  BOT_ELEM: " (if (cdr (assoc 'BOT_ELEM beam)) "Есть" "Нет")) f)
        (write-line (strcat "  Т-СОЕДИНЕНИЕ: "
                            (if (mk:rec-get beam 'TJOINT) (mk:rec-get beam 'TJOINT) "нет")
                            "   ВЕРТИКАЛЬНЫЙ: "
                            (if (mk:rec-get beam 'VERT) "да" "нет")) f)
        (write-line (strcat "  СУФФИКС: " (if (and (cdr (assoc 'SUFFIX beam))
                                                   (> (strlen (cdr (assoc 'SUFFIX beam))) 0))
                                            (cdr (assoc 'SUFFIX beam)) "нет")) f)
        (write-line (strcat "  СЕЧЕНИЕ: " (vl-prin1-to-string (cdr (assoc 'CROSS beam)))
                            "  (СИАЛ: " (if (mk:sial-size (cdr (assoc 'ARTICLE beam)))
                                          (rtos (mk:sial-size (cdr (assoc 'ARTICLE beam))) 2 1)
                                          "нет") ")") f)
        (write-line (strcat "  ДЛИНА №: " (if (cdr (assoc 'LEN_IDX beam))
                                            (itoa (cdr (assoc 'LEN_IDX beam))) "?")) f)
        (write-line (strcat "  РЯД: " (if (cdr (assoc 'ROW beam))
                                        (itoa (cdr (assoc 'ROW beam))) "?")
                            "   СТОЛБЕЦ: " (if (cdr (assoc 'BAY beam))
                                             (itoa (cdr (assoc 'BAY beam))) "?")) f)
        (write-line (strcat "  РАЗМЕР: " (if (and (cdr (assoc 'SIZE_MARK beam))
                                                  (> (strlen (cdr (assoc 'SIZE_MARK beam))) 0))
                                           (cdr (assoc 'SIZE_MARK beam)) "нет")) f)
        (if (mk:rec-get beam 'SUBCELL_PAIR)
          (progn
            (write-line (strcat "  SUBCELL_1: " (mk:rec-get beam 'SUBCELL_1)) f)
            (write-line (strcat "  SUBCELL_2: " (mk:rec-get beam 'SUBCELL_2)) f)))
        (write-line "" f))
      (write-line "--- ЗАПОЛНЕНИЯ ---" f)
      (foreach fill fills
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT fill)))) f)
        (write-line (strcat "  HEIGHT: " (vl-prin1-to-string (cdr (assoc 'HEIGHT fill)))) f)
        (write-line (strcat "  WIDTH: " (vl-prin1-to-string (cdr (assoc 'WIDTH fill)))) f)
        (write-line (strcat "  THICKNESS: " (if (cdr (assoc 'THICKNESS fill)) (cdr (assoc 'THICKNESS fill)) "НЕТ")) f)
        (write-line (strcat "  АРТИКУЛ: " (if (cdr (assoc 'ARTICLE fill)) (cdr (assoc 'ARTICLE fill)) "НЕТ")) f)
        (write-line "" f))
      (write-line "--- ОКНА ---" f)
      (foreach win windows
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT win)))) f)
        (write-line (strcat "  HEIGHT: " (vl-prin1-to-string (cdr (assoc 'HEIGHT win)))) f)
        (write-line (strcat "  WIDTH: " (vl-prin1-to-string (cdr (assoc 'WIDTH win)))) f)
        (write-line (strcat "  NAME: " (if (cdr (assoc 'NAME win)) (cdr (assoc 'NAME win)) "НЕТ")) f)
        (write-line "" f))
      (write-line "--- ДВЕРИ ---" f)
      (foreach door doors
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT door)))) f)
        (write-line (strcat "  HEIGHT: " (vl-prin1-to-string (cdr (assoc 'HEIGHT door)))) f)
        (write-line (strcat "  WIDTH: " (vl-prin1-to-string (cdr (assoc 'WIDTH door)))) f)
        (write-line (strcat "  NAME: " (if (cdr (assoc 'NAME door)) (cdr (assoc 'NAME door)) "НЕТ")) f)
        (write-line "" f))
      (close f)
      (prompt (strcat "\n[OK] Данные сохранены в: " fn)))
    (prompt "\n[ERROR] Не удалось создать файл.")))

;;;=====================================================================
;;; 18. АВТОЗАПОЛНЕНИЕ АТРИБУТОВ
;;;=====================================================================
(defun mk:set-attr (ename tag newval / sub data done)
  (if (not (mk:has-attrs-insert? ename))
    (setq sub nil done nil)
    (setq sub (entnext ename) done nil))
  (while (and sub (not done))
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (if (mk:name= (cdr (assoc 2 data)) tag)
         (progn
           (entmod (subst (cons 1 newval) (assoc 1 data) data))
           (entupd ename)
           (setq done t))))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if (and sub (not done)) (setq sub (entnext sub))))
  done)

;; Ключ размера с допуском *mk:tol-size* (0.5 мм):
;; 1500.4 и 1500.6 должны попадать в одну группу.
(defun mk:size-key (v / step)
  (if (numberp v)
    (progn
      (setq step (* 2.0 *mk:tol-size*))
      (rtos (* step (fix (+ (/ (float v) step) 0.5))) 2 1))
    "0"))

;;;---------------------------------------------------------------------
;;; 18a. СЛУЖЕБНОЕ: сортировка, фильтры, уникальные ключи
;;;---------------------------------------------------------------------
;; Порядок обнаружения: слева направо, снизу вверх
(defun mk:xy< (a b / xa xb ya yb)
  (setq xa (car  (cdr (assoc 'INS_PT a)))
        xb (car  (cdr (assoc 'INS_PT b)))
        ya (cadr (cdr (assoc 'INS_PT a)))
        yb (cadr (cdr (assoc 'INS_PT b))))
  (cond
    ((null xa) nil)
    ((null xb) t)
    ((< xa (- xb *mk:tol-adjacency*)) t)
    ((> xa (+ xb *mk:tol-adjacency*)) nil)
    (t (< ya yb))))

(defun mk:insert-xy (sorted el / out done)
  (setq out nil done nil)
  (foreach x sorted
    (if (and (not done) (mk:xy< el x))
      (progn (setq out (cons el out)) (setq done t)))
    (setq out (cons x out)))
  (if (not done) (setq out (cons el out)))
  (reverse out))

(defun mk:sort-xy (lst / out el)
  (setq out nil)
  (foreach el lst (setq out (mk:insert-xy out el)))
  out)

(defun mk:filter-by (elements key val / out)
  (setq out nil)
  (foreach el elements
    (if (equal (mk:rec-get el key) val) (setq out (cons el out))))
  (reverse out))

(defun mk:reject-by (elements key val / out)
  (setq out nil)
  (foreach el elements
    (if (not (equal (mk:rec-get el key) val)) (setq out (cons el out))))
  (reverse out))

(defun mk:unique-keys (elements key / out v)
  (setq out nil)
  (foreach el elements
    (setq v (mk:rec-get el key))
    (if (not (member v out)) (setq out (cons v out))))
  (reverse out))

;; Дополнить строку пробелами справа / слева до ширины n
(defun mk:pad (s n)
  (if (not (mk:strp s)) (setq s ""))
  (while (< (strlen s) n) (setq s (strcat s " ")))
  s)

(defun mk:padl (s n)
  (if (not (mk:strp s)) (setq s ""))
  (while (< (strlen s) n) (setq s (strcat " " s)))
  s)

(defun mk:join (lst sep / out first)
  (setq out "" first t)
  (foreach s lst
    (if first (setq out s first nil) (setq out (strcat out sep s))))
  out)

;;;---------------------------------------------------------------------
;;; 18b. ТИП ПРИМЫКАНИЯ СЛЕВА / СПРАВА ОТ СТОЙКИ
;;;---------------------------------------------------------------------
;; Оси стоек: уникальные X с допуском кластеризации
(defun mk:post-axes (posts / xs sorted out last)
  (setq xs nil)
  (foreach p posts
    (if (cdr (assoc 'INS_PT p))
      (setq xs (cons (car (cdr (assoc 'INS_PT p))) xs))))
  (setq sorted (vl-sort xs '<) out nil last nil)
  (foreach v sorted
    (if (or (null out) (> (- v last) *mk:tol-adjacency*))
      (progn (setq out (cons v out)) (setq last v))))
  (reverse out))

(defun mk:axis-prev (axes x / out)
  (setq out nil)
  (foreach a axes
    (if (< a (- x *mk:tol-adjacency*)) (setq out a)))
  out)

(defun mk:axis-next (axes x / out)
  (setq out nil)
  (foreach a (reverse axes)
    (if (> a (+ x *mk:tol-adjacency*)) (setq out a)))
  out)

(defun mk:el-xrange (el / x w)
  (setq x (car (cdr (assoc 'INS_PT el))))
  (cond
    ((mk:rec-get el 'VERT) (setq w 0.0))            ; вертикальный ригель (импост)
    ((= (cdr (assoc 'TYPE el)) "РИГЕЛЬ")
     (setq w (if (cdr (assoc 'LENGTH el)) (cdr (assoc 'LENGTH el)) 0.0)))
    (t (setq w (if (cdr (assoc 'WIDTH el)) (cdr (assoc 'WIDTH el)) 0.0))))
  (list x (+ x w)))

(defun mk:el-yrange (el / y h)
  (setq y (cadr (cdr (assoc 'INS_PT el))))
  (cond
    ((mk:rec-get el 'VERT)
     (setq h (if (cdr (assoc 'LENGTH el)) (cdr (assoc 'LENGTH el)) 0.0)))
    ((= (cdr (assoc 'TYPE el)) "РИГЕЛЬ") (setq h 0.0))
    (t (setq h (if (cdr (assoc 'HEIGHT el)) (cdr (assoc 'HEIGHT el)) 0.0))))
  (list y (+ y h)))

(defun mk:overlap? (lo1 hi1 lo2 hi2)
  (and (<= lo1 (+ hi2 *mk:tol-adjacency*))
       (>= hi1 (- lo2 *mk:tol-adjacency*))))

;; Типы элементов в пролёте [lo .. hi], перекрывающихся со стойкой по высоте
(defun mk:side-types (post elements lo hi / out typ xr yr cx span)
  (setq span (mk:post-span post) out nil)
  (if (and lo hi)
    (foreach el elements
      (if (cdr (assoc 'INS_PT el))
        (progn
          (setq xr  (mk:el-xrange el)
                yr  (mk:el-yrange el)
                cx  (/ (+ (car xr) (cadr xr)) 2.0)
                typ (cdr (assoc 'TYPE el)))
          (if (and (> cx (- lo *mk:tol-adjacency*))
                   (< cx (+ hi *mk:tol-adjacency*))
                   (mk:overlap? (car yr) (cadr yr) (car span) (cadr span))
                   (not (member typ out)))
            (setq out (cons typ out)))))))
  (if out (mk:join (vl-sort out '<) "+") *mk:conn-none*))

;; Подробная «подпись» стороны: тип + отметки примыкания относительно низа
;; стойки. Две стойки зеркальны, только если подписи сторон совпадают
;; накрест (одинаковое число примыканий на тех же отметках).
(defun mk:side-sig (post elements lo hi / span base out typ xr yr cx)
  (setq span (mk:post-span post)
        base (car span)
        out  nil)
  (if (and lo hi)
    (foreach el elements
      (if (cdr (assoc 'INS_PT el))
        (progn
          (setq xr (mk:el-xrange el)
                yr (mk:el-yrange el)
                cx (/ (+ (car xr) (cadr xr)) 2.0)
                typ (cdr (assoc 'TYPE el)))
          (if (and (> cx (- lo *mk:tol-adjacency*))
                   (< cx (+ hi *mk:tol-adjacency*))
                   (mk:overlap? (car yr) (cadr yr) (car span) (cadr span)))
            (setq out (cons (strcat typ ":" (mk:size-key (- (car yr) base))
                                    "-" (mk:size-key (- (cadr yr) base)))
                            out)))))))
  (if out (mk:join (vl-sort out '<) "+") *mk:conn-none*))

;; Разметка стоек: LEFT_TYPES / RIGHT_TYPES / ORIENT / GROUP_KEY
(defun mk:annotate-posts (posts elements / axes out x lt rt ls rs art len canon)
  (setq axes (mk:post-axes posts) out nil)
  (foreach post posts
    (setq x   (car (cdr (assoc 'INS_PT post)))
          lt  (mk:side-types post elements (mk:axis-prev axes x) x)
          rt  (mk:side-types post elements x (mk:axis-next axes x))
          ls  (mk:side-sig   post elements (mk:axis-prev axes x) x)
          rs  (mk:side-sig   post elements x (mk:axis-next axes x))
          len (mk:size-key (mk:rec-get post 'LENGTH))
          art (if (mk:rec-get post 'ARTICLE) (mk:rec-get post 'ARTICLE) "БЕЗ_АРТИКУЛА"))
    ;; канонический ключ не зависит от того, зеркальна стойка или нет
    (setq canon (if (<= (strcase ls) (strcase rs))
                  (strcat ls ">" rs)
                  (strcat rs ">" ls)))
    (setq post (mk:rec-put post 'LEFT_TYPES  lt))
    (setq post (mk:rec-put post 'RIGHT_TYPES rt))
    (setq post (mk:rec-put post 'LEFT_SIG    ls))
    (setq post (mk:rec-put post 'RIGHT_SIG   rs))
    (setq post (mk:rec-put post 'ORIENT      (strcat ls ">" rs)))
    (setq post (mk:rec-put post 'GROUP_KEY   (strcat len "_" art "_" canon)))
    (setq out (cons post out)))
  (reverse out))

;;;---------------------------------------------------------------------
;;; 18c. СУФФИКС РИГЕЛЯ
;;;---------------------------------------------------------------------
(defun mk:get-beam-suffix (beam all-panels doors bounds /
                           top-elem bot-elem top-type bot-type
                           top-thick bot-thick suffix beam-y door-y)
  (setq top-elem (cdr (assoc 'TOP_ELEM beam))
        bot-elem (cdr (assoc 'BOT_ELEM beam))
        suffix "")
  (setq top-type (if top-elem (cdr (assoc 'TYPE top-elem)) "ПУСТО"))
  (setq bot-type (if bot-elem (cdr (assoc 'TYPE bot-elem)) "ПУСТО"))
  (setq top-thick nil bot-thick nil)
  (if (and top-elem (= top-type "ЗАПОЛНЕНИЕ"))
    (setq top-thick (mk:numval (cdr (assoc 'THICKNESS top-elem)))))
  (if (and bot-elem (= bot-type "ЗАПОЛНЕНИЕ"))
    (setq bot-thick (mk:numval (cdr (assoc 'THICKNESS bot-elem)))))
  (cond
    ((and (= top-type "ОКНО") (= bot-type "ОКНО"))
     (setq suffix *mk:suffix-window-both*))
    ((or (= top-type "ОКНО") (= bot-type "ОКНО"))
     (setq suffix *mk:suffix-window-one*))
    ((or (= top-type "ДВЕРЬ") (= bot-type "ДВЕРЬ"))
     (setq suffix *mk:suffix-door-one*))
    ((and bot-thick top-thick
          (>= bot-thick *mk:thick-warm-min*) (<= bot-thick *mk:thick-warm-max*)
          (>= top-thick *mk:thick-cold-min*) (<= top-thick *mk:thick-cold-max*))
     (setq suffix *mk:suffix-warm-cold*))
    ((and bot-thick top-thick
          (>= bot-thick *mk:thick-cold-min*) (<= bot-thick *mk:thick-cold-max*)
          (>= top-thick *mk:thick-warm-min*) (<= top-thick *mk:thick-warm-max*))
     (setq suffix *mk:suffix-cold-warm*))
    (t (setq suffix "")))
  (if (and (cdr (assoc 'INS_PT beam)) doors)
    (progn
      (setq beam-y (cadr (cdr (assoc 'INS_PT beam))))
      (foreach door doors
        (if (cdr (assoc 'INS_PT door))
          (progn
            (setq door-y (cadr (cdr (assoc 'INS_PT door))))
            (if (<= (abs (- beam-y door-y)) *mk:tol-threshold*)
              (if (not (vl-string-search *mk:suffix-threshold* suffix))
                (setq suffix (strcat suffix *mk:suffix-threshold*)))))))))
  suffix)

;;;---------------------------------------------------------------------
;;; 18c-1. ВЕРТИКАЛЬНЫЕ СТОЛБЦЫ (ПРОЛЁТЫ) И МАРКЕР РАЗМЕРА ПРОФИЛЯ
;;;---------------------------------------------------------------------
;; Номер пролёта (1..N-1), в который попадает X; 0 — вне сетки стоек
(defun mk:bay-index (x axes / i idx n)
  (setq i 0 idx 0 n (length axes))
  (while (< (1+ i) n)
    (if (and (= idx 0)
             (> x (- (nth i axes) *mk:tol-adjacency*))
             (< x (+ (nth (1+ i) axes) *mk:tol-adjacency*)))
      (setq idx (1+ i)))
    (setq i (1+ i)))
  idx)

(defun mk:el-center-x (el / r)
  (setq r (mk:el-xrange el))
  (/ (+ (car r) (cadr r)) 2.0))

;; Уникальные пары (артикул . габарит сечения) в порядке появления
;; Шкала размеров «в свету»: уникальные длины ригелей и ширины заполнений,
;; по возрастанию. Индекс в этой шкале и есть цифра марки ригеля —
;; так она совпадает с цифрой марки заполнения (в MarkZ: номер = ширина).
(defun mk:length-scale (beams panels / bvals pvals out extra v)
  ;; Шкала номеров = длины горизонтальных ригелей «в свету», по возрастанию.
  ;; Ред. 3.9: ширины заполнений шкалу больше НЕ расширяют — секция всегда
  ;; ограничена ригелями, а узкие подъячейки под Т-узлом в MarkZ получают
  ;; подындекс (.1/.2), а не собственный номер. Несовпавшие ширины
  ;; выводятся отдельной строкой — для сверки с «<dwg> Шкала.txt» MarkZ.
  (setq bvals nil)
  (foreach b beams
    (setq v (mk:rec-get b 'LENGTH))
    (if (and (numberp v) (null (mk:rec-get b 'VERT)))
      (setq bvals (cons (atof (mk:size-key v)) bvals))))
  (setq out nil)
  (foreach v (vl-sort bvals '<)
    (if (not (member v out)) (setq out (cons v out))))
  (setq out (reverse out))
  ;; диагностика: ширины заполнений без парного ригеля
  (setq pvals nil extra nil)
  (foreach pn panels
    (if (= (mk:rec-get pn 'TYPE) "ЗАПОЛНЕНИЕ")
      (progn
        (setq v (mk:rec-get pn 'WIDTH))
        (if (and (numberp v) (= 0 (mk:scale-index (atof (mk:size-key v)) out)))
          (setq pvals (cons (atof (mk:size-key v)) pvals))))))
  (foreach v (vl-sort pvals '<)
    (if (not (member v extra)) (setq extra (cons v extra))))
  (setq extra (reverse extra))
  (prompt (strcat "\n  [INFO] Шкала размеров в свету: " (itoa (length out))
                  " (по ригелям)"))
  (if extra
    (progn
      (prompt (strcat "\n  [WARN] Ширин заполнений без ригеля: "
                      (itoa (length extra))
                      " — в MarkZ это подъячейки Т-узла (подындекс .1/.2):"))
      (foreach v extra
        (prompt (strcat "\n           " (rtos v 2 0))))))
  out)

(defun mk:vert-scale (beams / vals v out)
  (setq vals nil)
  (foreach b beams
    (setq v (mk:rec-get b 'LENGTH))
    (if (and (numberp v) (mk:rec-get b 'VERT))
      (setq vals (cons (atof (mk:size-key v)) vals))))
  (setq out nil)
  (foreach v (vl-sort vals '<)
    (if (not (member v out)) (setq out (cons v out))))
  (reverse out))

(defun mk:scale-index (v scale / i idx)
  (setq i 1 idx 0)
  (foreach s scale
    (if (and (= idx 0) (equal s v *mk:tol-size*)) (setq idx i))
    (setq i (1+ i)))
  idx)

;; Уровни ригелей (горизонтальные ряды) снизу вверх
(defun mk:row-levels (beams / ys sorted out last)
  (setq ys nil)
  (foreach b beams
    (if (cdr (assoc 'INS_PT b))
      (setq ys (cons (cadr (cdr (assoc 'INS_PT b))) ys))))
  (setq sorted (vl-sort ys '<) out nil last nil)
  (foreach v sorted
    (if (or (null out) (> (- v last) *mk:tol-adjacency*))
      (progn (setq out (cons v out)) (setq last v))))
  (reverse out))

;; Номер ряда (1 — самый нижний); 0 — уровень не распознан
(defun mk:row-index (y levels / i idx)
  (setq i 1 idx 0)
  (foreach lv levels
    (if (and (= idx 0) (<= (abs (- y lv)) *mk:tol-adjacency*)) (setq idx i))
    (setq i (1+ i)))
  idx)

;; Пары «артикул -> габарит». Элементы без артикула не участвуют:
;; маркер м/б ставится только там, где артикул известен.
(defun mk:article-cross-pairs (elements / out art cross)
  (setq out nil)
  (foreach el elements
    (setq art   (mk:rec-get el 'ARTICLE)
          cross (mk:cross-size el))
    (if (and (mk:strp art) (> (strlen (mk:art-key art)) 0)
             (null (assoc art out)))
      (setq out (cons (cons art cross) out))))
  (reverse out))

;; Лестница маркеров размера профиля (по возрастанию габарита сечения):
;;   1 артикул  -> без маркера
;;   2          -> м, б
;;   3          -> м, «» (средний массовый), б
;;   4          -> м, ср, «», б
;;   5 и более  -> м, м2, м3 … ср, «», б
(defun mk:size-labels (n / out i)
  (cond
    ((< n 2) (list ""))
    ((= n 2) (list *mk:suffix-small* *mk:suffix-big*))
    ((= n 3) (list *mk:suffix-small* "" *mk:suffix-big*))
    ((= n 4) (list *mk:suffix-small* *mk:suffix-mid* "" *mk:suffix-big*))
    (t
     (setq out (list *mk:suffix-small*) i 2)
     (repeat (- n 4)
       (setq out (cons (strcat *mk:suffix-small* (itoa i)) out))
       (setq i (1+ i)))
     (append (reverse out) (list *mk:suffix-mid* "" *mk:suffix-big*)))))

;; Таблица «артикул -> маркер» по габариту сечения.
;; Артикул один — таблица пустая (маркер не нужен); средний массовый
;; артикул маркера не получает.
(defun mk:size-mark-table (elements / pairs sorted out labels info)
  (setq pairs (mk:article-cross-pairs elements) out nil)
  (if (> (length pairs) 1)
    (progn
      (setq sorted (vl-sort pairs '(lambda (a b) (< (cdr a) (cdr b)))))
      (if (equal (cdr (car sorted)) (cdr (last sorted)) *mk:tol-size*)
        (prompt (strcat "\n  [WARN] Габариты сечений артикулов ригелей одинаковы"
                        " — маркеры размера не присвоены. Задайте *mk:article-size*."))
        (progn
          (setq labels (mk:size-labels (length sorted)) info "")
          (foreach pr sorted
            (if (> (strlen (car labels)) 0)
              (setq out (cons (cons (car pr) (car labels)) out)))
            (setq info (strcat (if (> (strlen (car labels)) 0) (car labels) "«»")
                               (if (= (strlen info) 0) "" " < ") info))
            (setq labels (cdr labels)))
          (prompt (strcat "\n  [INFO] Артикулов ригелей: " (itoa (length pairs))
                          " — маркеры по убыванию: " info))))))
  out)

(defun mk:size-mark (art table)
  (cond
    ((or (not (mk:strp art)) (= art "БЕЗ_АРТИКУЛА")
         (= (strlen (mk:art-key art)) 0)) "")
    ((cdr (assoc art *mk:article-size*)) (cdr (assoc art *mk:article-size*)))
    ((cdr (assoc art table))             (cdr (assoc art table)))
    (t "")))

;; Разметка ригелей: TOP_ELEM / BOT_ELEM / SUFFIX / BAY / SIZE_MARK / GROUP_KEY
(defun mk:annotate-beams (beams all-panels doors bounds posts /
                          out suffix len art axes table mark bay rows row
                          scale vscale lq pairs)
  (setq out   nil
        axes  (mk:post-axes posts)
        rows  (mk:row-levels beams)
        scale (mk:length-scale beams all-panels)
        vscale (mk:vert-scale beams)
        table (mk:size-mark-table beams)
        pairs (mk:subcell-pairs (mk:panel-width-scale all-panels) all-panels beams))
  (foreach beam beams
    (setq beam (mk:rec-put beam 'TOP_ELEM (mk:find-top-element beam all-panels)))
    (setq beam (mk:rec-put beam 'BOT_ELEM (mk:find-bot-element beam all-panels)))
    (setq suffix (mk:get-beam-suffix beam all-panels doors bounds))
    (setq beam (mk:rec-put beam 'SUFFIX suffix))
    (setq art  (if (mk:rec-get beam 'ARTICLE) (mk:rec-get beam 'ARTICLE) "БЕЗ_АРТИКУЛА")
          mark (mk:size-mark art table)
          bay  (mk:bay-index (mk:el-center-x beam) axes)
          len  (mk:size-key (mk:rec-get beam 'LENGTH)))
    (setq row (mk:row-index (cadr (cdr (assoc 'INS_PT beam))) rows))
    (setq beam (mk:rec-put beam 'BAY bay))
    (setq beam (mk:rec-put beam 'ROW row))
    (setq lq (atof len))
    (setq beam (mk:rec-put beam 'LEN_Q lq))
    (foreach pv pairs
      (if (equal lq (nth 2 pv) *mk:tol-size*)
        (progn
          (setq beam (mk:rec-put beam 'SUBCELL_PAIR pv))
          (setq beam (mk:rec-put beam 'SUBCELL_BASE_IDX (mk:scale-index lq scale)))
          (setq beam (mk:rec-put beam 'SUBCELL_1 (strcat (itoa (mk:scale-index lq scale)) ".1=" (rtos (nth 0 pv) 2 1))))
          (setq beam (mk:rec-put beam 'SUBCELL_2 (strcat (itoa (mk:scale-index lq scale)) ".2=" (rtos (nth 1 pv) 2 1)))))))
    (setq beam (mk:rec-put beam 'LEN_IDX
                 (if (mk:rec-get beam 'VERT)
                   (+ (length scale) (mk:scale-index lq vscale))
                   (mk:scale-index lq scale))))
    (setq beam (mk:rec-put beam 'SIZE_MARK mark))
    (setq beam (mk:rec-put beam 'GROUP_KEY
                 (strcat len "_" art "_" suffix
                         "_" (if (mk:rec-get beam 'TJOINT) (mk:rec-get beam 'TJOINT) "")
                         "_" (if (mk:rec-get beam 'VERT) *mk:suffix-vert-beam* ""))))
    (setq out (cons beam out)))
  (reverse out))

;;;---------------------------------------------------------------------
;;; 18d. ПЛАН МАРОК (марка -> список элементов)
;;;---------------------------------------------------------------------
;; Стойки: номер по канонической группе, зеркальная ориентация — суффикс "зерк"
(defun mk:plan-posts (posts prefix / plan idx grp base-or base mirror k)
  (setq posts (mk:sort-xy posts) plan nil idx 1)
  (foreach k (mk:unique-keys posts 'GROUP_KEY)
    (setq grp     (mk:filter-by posts 'GROUP_KEY k)
          base-or (mk:rec-get (car grp) 'ORIENT)
          base    (mk:filter-by grp 'ORIENT base-or)
          mirror  (mk:reject-by grp 'ORIENT base-or))
    ;; есть зеркальная пара -> Ст{N}.1 и Ст{N}.2, иначе просто Ст{N}
    (if mirror
      (progn
        (setq plan (cons (list (strcat prefix " Ст" (itoa idx)
                                       (nth 0 *mk:mirror-suffixes*)) base) plan))
        (setq plan (cons (list (strcat prefix " Ст" (itoa idx)
                                       (nth 1 *mk:mirror-suffixes*)) mirror) plan)))
      (setq plan (cons (list (strcat prefix " Ст" (itoa idx)) base) plan)))
    (setq idx (1+ idx)))
  (mk:sort-plan (reverse plan)))


;; Ригели: цифра марки = индекс длины ригеля в шкале размеров «в свету»
;; (от минимальной к максимальной) — та же цифра, что у заполнения в MarkZ
;; ("номер = ширина"). Далее маркер размера профиля (м/б) и суффикс окружения.
;; План марок по возрастанию номера: Рг1, Рг2 … Рг9, Рг10 (натуральный ключ)
(defun mk:sort-plan (plan)
  (vl-sort plan '(lambda (a b) (< (mk:nat-key (car a)) (mk:nat-key (car b))))))

(defun mk:plan-beams (beams prefix / plan idxs idx grp k sub mark hit)
  (setq beams (mk:sort-xy beams) plan nil used nil)
  (setq idxs (vl-sort (mk:unique-keys beams 'LEN_IDX) '<))
  (if (member 0 idxs)
    (setq idxs (append (vl-remove 0 idxs) (list 0))))
  (foreach idx idxs
    (setq grp (mk:filter-by beams 'LEN_IDX idx))
    (foreach k (mk:unique-keys grp 'GROUP_KEY)
      (setq sub  (mk:filter-by grp 'GROUP_KEY k)
            mark (strcat prefix " Рг" (itoa idx)
                         (if (mk:rec-get (car sub) 'VERT) *mk:suffix-vert-beam* "")
                         (if (mk:rec-get (car sub) 'TJOINT)
                           (mk:rec-get (car sub) 'TJOINT) "")
                         (if (mk:rec-get (car sub) 'SIZE_MARK)
                           (mk:rec-get (car sub) 'SIZE_MARK) "")
                         (if (mk:rec-get (car sub) 'SUFFIX)
                           (mk:rec-get (car sub) 'SUFFIX) "")))
      ;; одинаковая марка разных групп -> это одна позиция, элементы сливаются
      (setq hit (assoc mark plan))
      (if hit
        (setq plan (subst (list mark (append (cadr hit) sub)) hit plan))
        (setq plan (cons (list mark sub) plan)))))
  (mk:sort-plan (reverse plan)))

;;;---------------------------------------------------------------------
;;; 18e. ОБЛАСТЬ ДЕЙСТВИЯ, UNDO, ЗАПИСЬ
;;;---------------------------------------------------------------------
(defun mk:ss-all-inserts ()
  (ssget "_X" (list (cons 0 "INSERT"))))

;; Область действия команды: своя выборка -> выборка последней МАРКАВГЕОМЕТРИЯ ->
;; (только если ничего нет) весь чертёж.
;; Нормализация ключевого слова режима каркаса
(defun mk:norm-mode (kw)
  (cond
    ((null kw) "Все-типы")
    ((= kw "M") "Мультилинии")
    ((= kw "B") "Блоки")
    ((= kw "A") "Все-типы")
    (t kw)))

(defun mk:scope-ss (/ ss n)
  (if (and *mk:batch* *mk:last-ss*)
    (progn
      (prompt "\n  Область: выборка пакета МАРКАВ")
      (setq ss *mk:last-ss*))
    (progn
      (prompt "\nВыберите элементы витража (Enter — выборка последней МАРКАВГЕОМЕТРИЯ): ")
      (setq ss (if (= *mk:scope-mode* "Блоки")
                 (ssget (list (cons 0 "INSERT")))
                 (ssget)))))
  (cond
    (ss
     (setq *mk:last-ss* ss)
     (prompt (strcat "\n  Область: выборка, объектов " (itoa (sslength ss))))
     ss)
    (t
     (setq n (vl-catch-all-apply 'sslength (list *mk:last-ss*)))
     (if (and *mk:last-ss* (not (vl-catch-all-error-p n)) (> n 0))
       (progn
         (prompt (strcat "\n  Область: выборка последней МАРКАВГЕОМЕТРИЯ, объектов " (itoa n)))
         *mk:last-ss*)
       (progn
         (prompt "\n  [WARN] Выборки нет — обрабатывается ВЕСЬ чертёж.")
         (mk:ss-all-inserts))))))

(defun mk:undo-begin (/ doc)
  (if *mk:batch*
    (setq doc nil)                       ; в пакете UNDO ставит сама МАРКАВ
    (setq doc (mk:ax-get (vlax-get-acad-object) "ActiveDocument")))
  (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "StartUndoMark")))
  doc)

(defun mk:undo-end (doc)
  (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "EndUndoMark")))
  nil)

;; Запись марки по группе; возвращает (записано пропущено)
;;;---------------------------------------------------------------------
;;; Выноски марок и артикулов (для элементов без атрибута «Марка»)
;;; Слой «Обозначения», жёлтый, стиль «Основной стиль (надписи без наклона)»,
;;; высота 40, выравнивание вправо.
;;;   ригель — у правого конца, не доходя *mk:label-end-gap* мм, над профилем
;;;            на *mk:label-offset* мм;
;;;   стойка — у верхнего конца, не доходя *mk:label-end-gap* мм, сбоку
;;;            на *mk:label-offset* мм, текст повёрнут на *mk:label-rot-post*.
;;;---------------------------------------------------------------------
(setq *mk:labels* nil)
(setq *mk:style-warned* nil)

;; Имя текстового стиля, если он есть в чертеже
(defun mk:label-style-name ()
  (cond
    ((null (mk:strp *mk:label-style*)) nil)
    ((tblsearch "STYLE" *mk:label-style*) *mk:label-style*)
    (t
     (if (null *mk:style-warned*)
       (progn
         (setq *mk:style-warned* t)
         (prompt (strcat "\n  [WARN] Текстовый стиль «" *mk:label-style*
                         "» не найден — выноски текущим стилем."))))
     nil)))

;; Точка и поворот подписи: (точка поворот шаг-строки-по-X шаг-по-Y)
;; Окно над ригелем -> подпись зеркалится под ригель
(defun mk:beam-window-above? (el / top)
  (setq top (mk:rec-get el 'TOP_ELEM))
  (and top (= (cdr (assoc 'TYPE top)) "ОКНО")))

(defun mk:label-anchor (el / p len cross etype x y rot dx dy step just below ax)
  (setq p     (cdr (assoc 'INS_PT el))
        len   (if (numberp (cdr (assoc 'LENGTH el))) (cdr (assoc 'LENGTH el)) 0.0)
        cross (if (numberp (mk:rec-get el 'CROSS)) (mk:rec-get el 'CROSS) 0.0)
        etype (cdr (assoc 'TYPE el))
        step  (* *mk:label-line-gap* *mk:label-height*))
  (if (<= cross 1.0) (setq cross *mk:cross-fallback*))   ; отступ от грани, не от оси
  (if (null p)
    nil
    (progn
      (setq just 2)                                  ; по умолчанию — вправо
      (if (or (= etype "СТОЙКА") (mk:rec-get el 'VERT))
        (progn
          (setq x   (- (car p) (/ cross 2.0) *mk:label-offset*)
                rot *mk:label-rot-post*
                dx  (- step)
                dy  0.0)
          (if (< len *mk:label-short-len*)            ; короткий — по центру
            (setq y (+ (cadr p) (/ len 2.0)) just 1)
            (setq y (+ (cadr p) (- len *mk:label-end-gap*)))))
        (progn
          (setq below (mk:beam-window-above? el)      ; окно сверху -> текст снизу
                rot   0.0
                dx    0.0)
          (if below
            (setq y  (- (cadr p) (/ cross 2.0) *mk:label-offset* *mk:label-height*)
                  dy (- step))
            (setq y  (+ (cadr p) (/ cross 2.0) *mk:label-offset*)
                  dy step))
          (if (< len *mk:label-short-len*)            ; короткий — по центру
            (setq x (+ (car p) (/ len 2.0)) just 1)
            (setq x (+ (car p) (- len *mk:label-end-gap*))))))
      ;; точка на оси элемента: вдоль — как у марки, поперёк — по оси
      (if (or (= etype "СТОЙКА") (mk:rec-get el 'VERT))
        (setq ax (list (+ (car p) *mk:label-len-shift*) y 0.0))
        (setq ax (list x (+ (cadr p) *mk:label-len-shift*) 0.0)))
      (list (list x y 0.0) rot dx dy just ax))))

;; Одна строка текста с выравниванием вправо
;; just — по горизонтали (2 вправо, 1 по центру), vj — по вертикали
;; (0 базовая линия, 2 середина). «Середина вправо» = just 2, vj 2.
(defun mk:label-text (pt rot str just h vj / dxf sty)
  (setq dxf (list '(0 . "TEXT")
                  (cons 8 *mk:layer-label*)
                  (cons 10 pt)
                  (cons 40 (if (numberp h) h *mk:label-height*))
                  (cons 1 str)
                  (cons 50 (* pi (/ rot 180.0)))
                  (cons 62 *mk:label-color*)
                  (cons 72 just)                   ; 2 - вправо, 1 - по центру
                  (cons 73 (if (numberp vj) vj 0))  ; 2 - середина по высоте
                  (cons 11 pt)))
  (if (setq sty (mk:label-style-name))
    (setq dxf (append dxf (list (cons 7 sty)))))
  (entmakex dxf))

;; Строка длины: L=xxx мм (с припуском для ригелей, у стоек припуска нет)
(defun mk:label-len-str (el / v)
  (setq v (if (= (cdr (assoc 'TYPE el)) "РИГЕЛЬ")
            (mk:beam-cut-len-el el)
            (mk:rec-get el 'LENGTH)))
  (if (numberp v)
    (strcat *mk:label-len-prefix* (itoa (fix (+ (float v) 0.5)))
            *mk:label-len-suffix*)
    nil))

;; Выноска: марка, следом артикул, и длина по оси элемента
(defun mk:label-mark (el mark-str / anc pt rot dx dy just ax art ls e n)
  (setq anc (mk:label-anchor el)
        art (mk:rec-get el 'ARTICLE)
        n   0)
  (if (null anc)
    nil
    (progn
      (mk:ensure-layer *mk:layer-label* *mk:label-color*)
      (setq pt   (nth 0 anc) rot (nth 1 anc)
            dx   (nth 2 anc) dy  (nth 3 anc)
            just (if (nth 4 anc) (nth 4 anc) 2)
            ax   (nth 5 anc))
      (if (setq e (mk:label-text pt rot mark-str just *mk:label-height* 0))
        (progn (setq *mk:labels* (cons e *mk:labels*)) (setq n (1+ n))))
      (if (and (mk:strp art) (> (strlen art) 0))
        (if (setq e (mk:label-text (list (+ (car pt) dx) (+ (cadr pt) dy) 0.0)
                                   rot art just *mk:label-height* 0))
          (progn (setq *mk:labels* (cons e *mk:labels*)) (setq n (1+ n)))))
      ;; длина по оси элемента
      (if (and *mk:label-len-show* ax (setq ls (mk:label-len-str el)))
        (if (setq e (mk:label-text ax rot ls just *mk:label-len-height* 2))
          (progn (setq *mk:labels* (cons e *mk:labels*)) (setq n (1+ n)))))
      (> n 0))))

;; Собрать все выноски чертежа в группу
(defun mk:group-labels (/ ss i lst e d txt prefix)
  (setq prefix (if (mk:strp *mk:last-prefix*)
                 *mk:last-prefix*
                 (if (mk:strp *mk:current-vitrage-prefix*)
                   *mk:current-vitrage-prefix*
                   ""))
        ss (ssget "_X" (list (cons 8 *mk:layer-label*) (cons 0 "TEXT")))
        lst nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i)
              d (entget e)
              txt (cdr (assoc 1 d)))
        (if (and (mk:strp txt)
                 (wcmatch txt (strcat prefix " *")))
          (setq lst (cons e lst)))
        (setq i (1+ i)))
      (mk:make-group (strcat "Маркировка_" prefix) (reverse lst))))
  (if lst (length lst) 0))

(defun mk:write-marks (elements mark-str / count skip-count lab-count e)
  (setq count 0 skip-count 0 lab-count 0)
  (foreach el elements
    (setq e (mk:rec-get el 'ENAME))
    (if (and e (mk:has-attr? e *mk:attr-mark*))
      (progn
        (mk:set-attr e *mk:attr-mark* mark-str)
        (setq count (1+ count)))
      (progn
        (if (mk:label-mark el mark-str)
          (setq lab-count (1+ lab-count))
          (setq skip-count (1+ skip-count))))))
  (list count skip-count lab-count))

;; Сбор всех типов элементов из выборки
;;;---------------------------------------------------------------------
;;; Т-СОЕДИНЕНИЯ
;;; Т-соединение — вертикальный элемент упирается торцом в ригель внутри его
;;; пролёта (не на конце). Такому ригелю даётся суффикс:
;;;   .1 — Т снизу (импост идёт вверх от ригеля);
;;;   .2 — Т сверху (импост идёт вниз к ригелю);
;;;   .3 — Т с обеих сторон.
;;; Частный случай (симметричный узел, *mk:tj-symmetric*): импост стоит строго
;;; по центру обоих ригелей (допуск *mk:tol-tcenter*), у ригелей одинаковые
;;; длина и артикул, в примыкающих ячейках нет окон и дверей, а толщины
;;; заполнений совпадают (если заданы) — тогда нижний и верхний ригели
;;; взаимозаменяемы и оба получают «.1».
;;; Вертикальный элемент с двумя Т-соединениями считается ригелем (суффикс «т»),
;;; с одним — остаётся стойкой.
;;;---------------------------------------------------------------------
(defun mk:beam-hit (x y beams / out p bx0 bx1 by len)
  (setq out nil)
  (foreach b beams
    (if (null out)
      (progn
        (setq p   (cdr (assoc 'INS_PT b))
              len (if (numberp (cdr (assoc 'LENGTH b))) (cdr (assoc 'LENGTH b)) 0.0))
        (if p
          (progn
            (setq bx0 (car p) bx1 (+ (car p) len) by (cadr p))
            (if (and (<= (abs (- y by)) *mk:tol-tjoint*)
                     (> x (+ bx0 *mk:tol-tjoint*))
                     (< x (- bx1 *mk:tol-tjoint*)))
              (setq out b)))))))
  out)

;; Карта суффиксов: ключ — ENAME ригеля; «.1» имеет приоритет над «.2»
;; Накопление ролей ригеля в Т-узлах: ключ ENAME -> (есть-снизу есть-сверху)
(defun mk:tj-put (map beam role / e hit lo hi)
  (setq e (mk:rec-get beam 'ENAME))
  (if (null e)
    map
    (progn
      (setq hit (assoc e map)
            lo  (if hit (nth 0 (cdr hit)) nil)
            hi  (if hit (nth 1 (cdr hit)) nil))
      (if (= role 'lo) (setq lo t) (setq hi t))
      (if hit
        (subst (cons e (list lo hi)) hit map)
        (cons (cons e (list lo hi)) map)))))

;; Т-точка по центру длины ригеля?
(defun mk:t-centered? (beam x / p len)
  (setq p   (cdr (assoc 'INS_PT beam))
        len (mk:len-or0 beam))
  (and p (> len 0.0)
       (<= (abs (- x (+ (car p) (/ len 2.0)))) *mk:tol-tcenter*)))

;; Длина элемента или 0.0
(defun mk:len-or0 (el / v)
  (setq v (mk:numval (cdr (assoc 'LENGTH el))))
  (if (numberp v) v 0.0))

;; Толщина заполнения как строка сравнения ("" -> nil)
(defun mk:th-key (pn / v)
  (setq v (cdr (assoc 'THICKNESS pn)))
  (cond ((numberp v) (rtos (float v) 2 2))
        ((and (mk:strp v) (> (strlen (mk:trim v)) 0)) (mk:trim v))
        (t nil)))

;; Центр панели (INS_PT + половина габарита)
(defun mk:panel-center (pn / p w h)
  (setq p (cdr (assoc 'INS_PT pn))
        w (mk:numval (cdr (assoc 'WIDTH pn)))
        h (mk:numval (cdr (assoc 'HEIGHT pn))))
  (if p
    (list (+ (car p)  (if (numberp w) (/ w 2.0) 0.0))
          (+ (cadr p) (if (numberp h) (/ h 2.0) 0.0)))))

;; Панели, попадающие в узел: по X — в пролёт ригеля, по Y — между ригелями
(defun mk:tj-panels (bb ba panels / out pb pa x0 x1 y0 y1 c)
  (setq pb (cdr (assoc 'INS_PT bb))
        pa (cdr (assoc 'INS_PT ba))
        x0 (min (car pb) (car pa))
        x1 (max (+ (car pb) (mk:len-or0 bb)) (+ (car pa) (mk:len-or0 ba)))
        y0 (min (cadr pb) (cadr pa))
        y1 (max (cadr pb) (cadr pa))
        out nil)
  (foreach pn panels
    (setq c (mk:panel-center pn))
    (if (and c
             (> (car c)  (- x0 *mk:tol-adjacency*))
             (< (car c)  (+ x1 *mk:tol-adjacency*))
             (> (cadr c) (- y0 *mk:tol-adjacency*))
             (< (cadr c) (+ y1 *mk:tol-adjacency*)))
      (setq out (cons pn out))))
  out)

;; Симметричный Т-узел: оба ригеля взаимозаменяемы -> обоим «.1»
(defun mk:tj-symmetric? (bb ba vx panels / lb la ab aa cells th bad)
  (setq lb (mk:numval (cdr (assoc 'LENGTH bb)))
        la (mk:numval (cdr (assoc 'LENGTH ba)))
        ab (strcase (if (mk:strp (cdr (assoc 'ARTICLE bb))) (cdr (assoc 'ARTICLE bb)) ""))
        aa (strcase (if (mk:strp (cdr (assoc 'ARTICLE ba))) (cdr (assoc 'ARTICLE ba)) "")))
  (if (not (and *mk:tj-symmetric*
                (mk:t-centered? bb vx) (mk:t-centered? ba vx)
                (numberp lb) (numberp la)
                (<= (abs (- lb la)) *mk:tol-size*)
                (= ab aa)))
    nil
    (progn
      (setq cells (mk:tj-panels bb ba panels) th nil bad nil)
      (foreach pn cells
        (cond
          ;; окно или дверь в ячейке — узел несимметричен
          ((not (= (cdr (assoc 'TYPE pn)) "ЗАПОЛНЕНИЕ")) (setq bad t))
          ;; толщина заполнения (если задана) должна совпадать
          ((mk:th-key pn)
           (if (null th)
             (setq th (mk:th-key pn))
             (if (not (= th (mk:th-key pn))) (setq bad t))))))
      (not bad))))

;; Роли -> суффикс
(defun mk:tj-suffix (roles)
  (cond
    ((and (nth 0 roles) (nth 1 roles)) *mk:suffix-tjoint-both*)
    ((nth 0 roles) *mk:suffix-tjoint-lo*)
    ((nth 1 roles) *mk:suffix-tjoint-hi*)
    (t nil)))

;; Разбор узлов: возвращает (стойки ригели)
(defun mk:split-tjoints (posts beams panels / map newposts newbeams p len vx vy0 vy1
                                              bb ba nT e hit nsym)
  (setq map nil newposts nil newbeams nil nsym 0)
  (foreach v posts
    (setq p   (cdr (assoc 'INS_PT v))
          len (if (numberp (cdr (assoc 'LENGTH v))) (cdr (assoc 'LENGTH v)) 0.0))
    (if (null p)
      (setq newposts (cons v newposts))
      (progn
        (setq vx  (car p) vy0 (cadr p) vy1 (+ (cadr p) len)
              bb  (mk:beam-hit vx vy0 beams)      ; ригель снизу от импоста
              ba  (mk:beam-hit vx vy1 beams))     ; ригель сверху от импоста
        ;; для нижнего ригеля Т приходит сверху -> роль lo, для верхнего -> hi;
        ;; симметричный узел -> обоим роль lo, то есть «.1»
        (cond
          ((and bb ba (mk:tj-symmetric? bb ba vx panels))
           (setq map  (mk:tj-put map bb 'lo)
                 map  (mk:tj-put map ba 'lo)
                 nsym (1+ nsym)))
          (t
           (if bb (setq map (mk:tj-put map bb 'lo)))
           (if ba (setq map (mk:tj-put map ba 'hi)))))
        (setq nT (+ (if bb 1 0) (if ba 1 0)))
        (if (= nT 2)
          ;; вертикальный элемент между двумя ригелями -> это ригель
          (setq newbeams (cons (mk:rec-put (mk:rec-put v 'TYPE "РИГЕЛЬ") 'VERT t)
                               newbeams))
          (setq newposts (cons v newposts))))))
  (if (> nsym 0)
    (prompt (strcat "\n  [INFO] Симметричных Т-узлов (оба ригеля «"
                    *mk:suffix-tjoint-lo* "»): " (itoa nsym))))
  ;; проставить суффиксы Т-соединений ригелям
  (setq beams (mapcar
                '(lambda (b / e hit)
                   (setq e   (mk:rec-get b 'ENAME)
                         hit (if e (assoc e map) nil))
                   (if hit (mk:rec-put b 'TJOINT (mk:tj-suffix (cdr hit))) b))
                beams))
  (list (reverse newposts) (append beams (reverse newbeams))))

(defun mk:collect-scope (ss / posts beams fills windows doors res mlines)
  (cond
    ;; режим последнего сбора: «Все-типы» — каркас из любой геометрии + блоков
    ((= *mk:scope-mode* "Все-типы")
     (setq res   (mk:extract-posts-beams-all ss)
           posts (nth 0 res)
           beams (nth 1 res)))
    ;; «Мультилинии» — каркас из мультилиний выборки
    ((= *mk:scope-mode* "Мультилинии")
     (setq mlines (mk:find-mlines-in-ss ss))
     (if mlines
       (progn
         (setq res   (mk:extract-posts-beams-from-mlines mlines)
               posts (nth 0 res)
               beams (nth 1 res)))
       (setq posts nil beams nil)))
    (t
     (setq posts (mapcar 'mk:collect-post
                   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*"))))
     (setq beams (mapcar 'mk:collect-beam
                   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam* "*"))))))
  (setq posts (mk:sanitize-frame posts "Стойки"))
  (setq beams (mk:sanitize-frame beams "Ригели"))
  ;; панели нужны до разбора узлов: по ним проверяется симметрия Т-соединения
  (setq fills   (mk:sanitize-panels
                  (mapcar 'mk:collect-fill (mk:find-blocks-in-ss ss (strcat "*" *mk:block-fill* "*")))
                  "Заполнения"))
  (setq windows (mk:sanitize-panels
                  (mapcar 'mk:collect-window
                    (mk:find-proems ss (append *mk:window-masks* (list *mk:block-window*))))
                  "Окна"))
  (setq doors   (mk:sanitize-panels
                  (mapcar 'mk:collect-door
                    (mk:find-proems ss (append *mk:door-masks* (list *mk:block-door*))))
                  "Двери"))
  (setq res   (mk:split-tjoints posts beams (append fills windows doors))
        posts (nth 0 res)
        beams (nth 1 res))
  ;; В многовитражном прогоне витраж уже известен (это группа), и повторный
  ;; поиск рамок по всему чертежу на каждый витраж был бы лишней работой:
  ;; префикс всё равно проставляет mk:tag-records.
  (if (null *mk:in-group*)
    (progn
      (setq frames (mk:frame-list))
      (setq posts (mk:assign-vitrage-records posts frames)
            beams (mk:assign-vitrage-records beams frames)
            fills (mk:assign-vitrage-records fills frames)
            windows (mk:assign-vitrage-records windows frames)
            doors (mk:assign-vitrage-records doors frames))))
  (list (cons 'POSTS posts) (cons 'BEAMS beams) (cons 'FILLS fills)
        (cons 'WINDOWS windows) (cons 'DOORS doors)
        (cons 'PANELS (append fills windows doors))))

;;;---------------------------------------------------------------------
;;; 18f. КОМАНДЫ МАРКИРОВКИ
;;;---------------------------------------------------------------------
;;;=====================================================================
;;; 20a. ДИАГНОСТИКА БЛОКА (почему LENGTH/WIDTH/АРТИКУЛ = nil)
;;;=====================================================================
(defun mk:fmt (v) (vl-princ-to-string v))

(defun mk:diag-block (ename / obj lines bb attrs pairs nm hit)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (vl-catch-all-error-p obj) (setq obj nil))
  (setq lines nil)
  (setq lines (cons (strcat "DXF 0        : " (mk:fmt (cdr (assoc 0 (entget ename))))) lines))
  (setq lines (cons (strcat "DXF 2 (имя)  : " (mk:fmt (cdr (assoc 2 (entget ename))))) lines))
  (setq lines (cons (strcat "Name         : " (mk:fmt (mk:ax-get obj "Name"))) lines))
  (setq lines (cons (strcat "EffectiveName: " (mk:fmt (mk:ax-get obj "EffectiveName"))) lines))
  (setq lines (cons (strcat "Слой         : " (mk:fmt (cdr (assoc 8 (entget ename))))) lines))
  (setq lines (cons (strcat "Динамический : " (if (mk:dynamic? obj) "ДА" "НЕТ")) lines))
  (setq lines (cons (strcat "InsertionPoint: " (mk:fmt (cdr (assoc 'INS_PT (mk:get-geom obj))))) lines))
  (setq lines (cons (strcat "Rotation     : " (mk:fmt (cdr (assoc 'ROTATION (mk:get-geom obj))))) lines))
  (setq lines (cons (strcat "XScale/YScale: "
                            (mk:fmt (cdr (assoc 'XSCALE (mk:get-geom obj)))) " / "
                            (mk:fmt (cdr (assoc 'YSCALE (mk:get-geom obj))))) lines))
  (setq bb (mk:e-bb obj))
  (setq lines (cons (strcat "Габарит BBOX : "
                            (if bb (strcat "Ш=" (rtos (mk:bb-w bb) 2 1)
                                           "  В=" (rtos (mk:bb-h bb) 2 1))
                              "не получен")) lines))
  (setq lines (cons "--- АТРИБУТЫ (тег = значение) ---" lines))
  (setq attrs (mk:get-all-attrs ename))
  (if attrs
    (foreach a attrs
      (setq lines (cons (strcat "   <" (mk:fmt (car a)) "> = \"" (mk:fmt (cdr a)) "\"") lines)))
    (setq lines (cons "   [нет атрибутов]" lines)))
  (setq lines (cons "--- ДИНАМИЧЕСКИЕ СВОЙСТВА (имя = значение) ---" lines))
  (setq pairs (mk:dyn-pairs-raw obj))
  (if pairs
    (foreach pr pairs
      (setq lines (cons (strcat "   <" (car pr) "> = " (mk:fmt (cdr pr))) lines)))
    (setq lines (cons "   [нет динамических свойств]" lines)))
  (setq lines (cons "--- ЧТО НАХОДЯТ ТЕКУЩИЕ МАСКИ ---" lines))
  (setq lines (cons (strcat "   " *mk:mask-length* " -> " (mk:fmt (mk:get-dyn obj *mk:mask-length*))) lines))
  (setq lines (cons (strcat "   " *mk:mask-height* " -> " (mk:fmt (mk:get-dyn obj *mk:mask-height*))) lines))
  (setq lines (cons (strcat "   " *mk:mask-width*  " -> " (mk:fmt (mk:get-dyn obj *mk:mask-width*)))  lines))
  (setq lines (cons (strcat "   Видимость -> " (mk:fmt (mk:get-vis obj))) lines))
  (setq lines (cons (strcat "   Атрибут «" *mk:attr-article* "» -> "
                            (mk:fmt (mk:get-attr ename *mk:attr-article*))) lines))
  (setq lines (cons (strcat "   Атрибут «" *mk:attr-mark* "» -> "
                            (if (mk:has-attr? ename *mk:attr-mark*) "ЕСТЬ" "НЕТ")) lines))
  (reverse lines))

(defun c:МАРКАВДИАГНОЗ (/ sel ename lines f)
  (prompt "\n[МАРКАВДИАГНОЗ] Диагностика блока.")
  (setq sel (entsel "\nУкажите блок (стойку, ригель, заполнение, окно, дверь): "))
  (if (null sel)
    (prompt "\n  [INFO] Блок не указан.")
    (progn
      (setq ename (car sel))
      (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)
      (setq lines (mk:diag-block ename))
      (prompt "\n---------------------------------------------")
      (foreach l lines (prompt (strcat "\n" l)))
      (prompt "\n---------------------------------------------")
      (setq f (open *mk:diag-file* "a"))
      (if f
        (progn
          (write-line "=== МАРКАВДИАГНОЗ ===" f)
          (foreach l lines (write-line l f))
          (write-line "" f)
          (close f)
          (prompt (strcat "\n[OK] Дописано в: " *mk:diag-file*)))
        (prompt "\n[WARN] Файл диагностики не создан."))))
  (princ))

;;;=====================================================================
;;; 20a. ВЕДОМОСТЬ ПРОФИЛЕЙ — МАРКАВТАБЛ
;;;   Таблица AutoCAD + XLS (SpreadsheetML 2003).
;;;   Шапка: № | Артикул | Марка | Длина, мм | Кол-во, шт. | Всего, м.п.
;;;   Стойки, затем ригели; сортировка по артикулу, затем по марке и длине.
;;;   С Ред. 7.0 команда строит ОДНУ общую ведомость по всем витражам
;;;   (см. раздел 20e); здесь — только движок таблицы и XLS.
;;;=====================================================================
;;;--- Приписка к имени файла ведомости --------------------------------
;; Один витраж — его марка; несколько — число витражей.
(defun mk:tab-prefix (/ groups)
  (setq groups (if *mk:last-groups* *mk:last-groups* nil))
  (cond
    ((null groups) (if (mk:strp *mk:last-prefix*) *mk:last-prefix* "витраж"))
    ((= (length groups) 1) (nth 0 (car groups)))
    (t (strcat (itoa (length groups)) " витражей"))))
(setq *mk:tab-title* "Ведомость профилей")

;; Число с двумя знаками и запятой-разделителем: 12.5 -> "12,5"
(defun mk:tab-num (x / s i c out)
  (setq s (rtos (float x) 2 2) out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq out (strcat out (if (= c ".") "," c)))
    (setq i (1+ i)))
  out)

;; Число для SpreadsheetML: разделитель — точка
(defun mk:tab-xnum (x / s i c out)
  (setq s (rtos (float x) 2 2) out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq out (strcat out (if (= c ",") "." c)))
    (setq i (1+ i)))
  out)

(defun mk:tab-int (x) (itoa (fix (+ (float x) 0.5))))

;; Припуск для конкретного артикула:
;; 1) явная маска из *mk:beam-allowance-by-article*;
;; 2) артикул системы КП50К -> *mk:allowance-kp50k* (25 мм);
;; 3) иначе (КП50 либо артикул неизвестен) -> *mk:beam-allowance* (0).
(defun mk:beam-allowance-for (art / k out)
  (setq k (mk:art-key art) out nil)
  (if (> (strlen k) 0)
    (progn
      (foreach pair *mk:beam-allowance-by-article*
        (if (and (null out) (wcmatch k (strcase (car pair))))
          (setq out (cdr pair))))
      (if (and (null out) (member k *mk:articles-kp50k*))
        (setq out *mk:allowance-kp50k*)))
    ;; артикул не распознан — общий припуск, заданный вопросом
    (if (numberp *mk:allowance-unknown*)
      (setq out *mk:allowance-unknown*)))
  (if (numberp out) out *mk:beam-allowance*))

;; Сколько ригелей без артикула (вертикальные не в счёт — у них припуск 0)
(defun mk:beams-wo-article (beams / n a)
  (setq n 0)
  (foreach b beams
    (setq a (mk:rec-get b 'ARTICLE))
    (if (and (null (mk:rec-get b 'VERT))
             (or (not (mk:strp a)) (= (strlen (mk:art-key a)) 0)))
      (setq n (1+ n))))
  n)

;; Вопрос «оптом»: припуск для ригелей с нераспознанным артикулом
(defun mk:ask-unknown-allowance (beams / n ans v)
  (setq n (mk:beams-wo-article beams))
  (if (and (> n 0) *mk:ask-unknown-allowance* (null *mk:allowance-unknown*))
    (progn
      (prompt (strcat "\n  Ригелей с нераспознанным артикулом: " (itoa n)))
      (initget "КП50 КП50К Ввод")
      (setq ans (getkword "\n  Припуск для них [КП50/КП50К/Ввод] <КП50>: "))
      (cond
        ((= ans "КП50К") (setq *mk:allowance-unknown* *mk:allowance-kp50k*))
        ((= ans "Ввод")
         (initget 4)
         (setq v (getreal "\n  Припуск на элемент, мм <0>: "))
         (setq *mk:allowance-unknown* (if (numberp v) v 0.0)))
        (t (setq *mk:allowance-unknown* 0.0)))
      (prompt (strcat "\n  Принято: +" (rtos *mk:allowance-unknown* 2 1)
                      " мм к " (itoa n) " ригел(ю/ям) без артикула."))))
  (if (numberp *mk:allowance-unknown*) *mk:allowance-unknown* 0.0))

;; Вертикальный ригель («т») считается как КП50 — без припуска
(defun mk:beam-cut-len-el (el / len)
  (setq len (mk:rec-get el 'LENGTH))
  (cond
    ((not (numberp len)) 0.0)
    ((mk:rec-get el 'VERT) (float len))
    (t (+ (float len) (mk:beam-allowance-for (mk:rec-get el 'ARTICLE))))))

(defun mk:tab-mp (len cnt) (/ (* (float len) cnt) 1000.0))

;; Записи: (ранг раздел артикул марка длина)
(defun mk:tab-rows (posts beams / out)
  (setq out nil)
  (foreach el posts
    (setq out (cons (list 1 "Стойки"
                          (if (mk:rec-get el 'ARTICLE) (mk:rec-get el 'ARTICLE) "—")
                          (mk:read-mark el)
                          (if (mk:rec-get el 'LENGTH) (mk:rec-get el 'LENGTH) 0.0))
                    out)))
  (foreach el beams
    (setq out (cons (list 2 "Ригели"
                          (if (mk:rec-get el 'ARTICLE) (mk:rec-get el 'ARTICLE) "—")
                          (mk:read-mark el)
                          (mk:beam-cut-len-el el))
                    out)))
  (reverse out))

;; Карта «ename -> расчётная марка» для элементов без атрибута «Марка»
(setq *mk:mark-map* nil)

(defun mk:mark-map-put (plan)
  (foreach item plan
    (foreach el (cadr item)
      (if (mk:rec-get el 'ENAME)
        (setq *mk:mark-map*
          (cons (cons (mk:rec-get el 'ENAME) (car item)) *mk:mark-map*)))))
  *mk:mark-map*)

(defun mk:read-mark (el / e v hit)
  (setq e (cdr (assoc 'ENAME el))
        v (if (and e (mk:has-attr? e *mk:attr-mark*))
            (mk:get-attr e *mk:attr-mark*) nil))
  (cond
    ((and v (> (strlen v) 0)) v)
    ((setq hit (assoc e *mk:mark-map*)) (cdr hit))
    (t "—")))

;; Натуральный ключ сортировки: числа дополняются нулями слева,
;; поэтому Ст2 < Ст10, Рг9 < Рг10.
(defun mk:nat-key (s / up n i c out num)
  (if (not (mk:strp s))
    ""
    (progn
      (setq up (strcase s) n (strlen up) i 1 out "" num "")
      (while (<= i n)
        (setq c (substr up i 1))
        (if (member c '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
          (setq num (strcat num c))
          (progn
            (if (> (strlen num) 0)
              (progn
                (while (< (strlen num) 6) (setq num (strcat "0" num)))
                (setq out (strcat out num) num "")))
            (setq out (strcat out c))))
        (setq i (1+ i)))
      (if (> (strlen num) 0)
        (progn
          (while (< (strlen num) 6) (setq num (strcat "0" num)))
          (setq out (strcat out num))))
      out)))

(defun mk:tab-less (a b / aa ab ma mb)
  (cond
    ((< (nth 0 a) (nth 0 b)) t)
    ((> (nth 0 a) (nth 0 b)) nil)
    (t
     ;; сначала по марке, затем по артикулу
     (setq aa (mk:nat-key (nth 3 a)) ab (mk:nat-key (nth 3 b)))
     (cond
       ((< aa ab) t)
       ((> aa ab) nil)
       (t
        (setq ma (mk:nat-key (nth 2 a)) mb (mk:nat-key (nth 2 b)))
        (cond
          ((< ma mb) t)
          ((> ma mb) nil)
          ((< (nth 4 a) (nth 4 b)) t)
          (t nil)))))))

;; Агрегация -> (ранг раздел артикул марка длина кол-во)
(defun mk:tab-aggregate (rows / acc key found out)
  (setq acc nil)
  (foreach r rows
    (setq key (strcat (itoa (nth 0 r)) "|" (strcase (nth 2 r)) "|"
                      (strcase (nth 3 r)) "|" (mk:tab-int (nth 4 r))))
    (setq found (assoc key acc))
    (if found
      (setq acc (subst (list key (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)
                             (nth 4 r) (1+ (nth 6 found)))
                       found acc))
      (setq acc (cons (list key (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)
                            (nth 4 r) 1)
                      acc))))
  (setq out (mapcar '(lambda (r) (cdr r)) acc))
  (vl-sort out 'mk:tab-less))

;;;--- Таблица AutoCAD ---------------------------------------------------
(defun mk:tab-merge (tbl r / res)
  (setq res (vl-catch-all-apply 'vlax-invoke-method
              (list tbl "MergeCells" r r 1 3)))
  (if (vl-catch-all-error-p res)
    (prompt "\n  [WARN] Объединение ячеек не выполнено."))
  res)

(defun mk:tab-set (tbl r c val)
  (vl-catch-all-apply 'vlax-invoke-method (list tbl "SetText" r c val)))

(defun mk:tab-align (tbl r c a)
  (vl-catch-all-apply 'vlax-invoke-method (list tbl "SetCellAlignment" r c a)))

(defun mk:tab-create (data / pt doc space tbl nRows nCols row n-row i hdr
                        cur cnt-sub mp-sub n-sec rec sect art mark len cnt mp
                        total-cnt total-mp old-echo)
  (if (null data)
    (progn (prompt "\n  [INFO] Нет данных для таблицы.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
      (if (null pt)
        (progn (prompt "\n  [INFO] Таблица пропущена.") nil)
        (progn
          (setq n-sec 0 cur nil)
          (foreach rec data
            (if (not (and cur (= (nth 1 rec) cur)))
              (progn (setq n-sec (1+ n-sec)) (setq cur (nth 1 rec)))))
          (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object))
                space    (vla-get-ModelSpace doc)
                pt       (trans pt 1 0)
                nCols    6
                nRows    (+ 3 (length data) n-sec)
                old-echo (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (mk:ensure-layer *mk:layer-table* 7)
          (setq tbl (vl-catch-all-apply 'vla-AddTable
                      (list space (vlax-3d-point pt) nRows nCols 10.0 35.0)))
          (if (not (vl-catch-all-error-p tbl))
            (mk:ax-put tbl "Layer" *mk:layer-table*))
          (if (vl-catch-all-error-p tbl)
            (progn
              (prompt (strcat "\n  [ERROR] AddTable: "
                              (vl-catch-all-error-message tbl)))
              (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
              nil)
            (progn
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 14.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 1 42.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 2 42.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 3 32.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 4 32.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 5 32.0))
              (mk:tab-set tbl 0 0 *mk:tab-title*)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 8.0))
              (setq i 0)
              (foreach hdr '("№" "Артикул" "Марка" "Длина, мм"
                             "Кол-во, шт." "Всего, м.п.")
                (mk:tab-set tbl 1 i hdr)
                (mk:tab-align tbl 1 i 5)
                (setq i (1+ i)))
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 1 8.0))
              (setq row 2 n-row 1 cur nil cnt-sub 0 mp-sub 0.0
                    total-cnt 0 total-mp 0.0)
              (foreach rec data
                (setq sect (nth 1 rec)
                      art  (nth 2 rec)
                      mark (nth 3 rec)
                      len  (nth 4 rec)
                      cnt  (nth 5 rec)
                      mp   (mk:tab-mp len cnt))
                (if (and cur (/= sect cur))
                  (progn
                    (mk:tab-set tbl row 0 "")
                    (mk:tab-set tbl row 1 (strcat "   {\\L" cur "}"))
                    (mk:tab-merge tbl row)
                    (mk:tab-align tbl row 1 4)
                    (mk:tab-set tbl row 4 (itoa cnt-sub))
                    (mk:tab-set tbl row 5 (mk:tab-num mp-sub))
                    (mk:tab-align tbl row 4 5)
                    (mk:tab-align tbl row 5 5)
                    (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                    (setq row (1+ row) cnt-sub 0 mp-sub 0.0)))
                (setq cur sect
                      cnt-sub (+ cnt-sub cnt)
                      mp-sub  (+ mp-sub mp)
                      total-cnt (+ total-cnt cnt)
                      total-mp  (+ total-mp mp))
                (mk:tab-set tbl row 0 (itoa n-row))
                (mk:tab-set tbl row 1 art)
                (mk:tab-set tbl row 2 mark)
                (mk:tab-set tbl row 3 (mk:tab-int len))
                (mk:tab-set tbl row 4 (itoa cnt))
                (mk:tab-set tbl row 5 (mk:tab-num mp))
                (mk:tab-align tbl row 0 5)
                (mk:tab-align tbl row 1 4)
                (mk:tab-align tbl row 2 4)
                (mk:tab-align tbl row 3 5)
                (mk:tab-align tbl row 4 5)
                (mk:tab-align tbl row 5 5)
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                (setq row (1+ row) n-row (1+ n-row)))
              (if cur
                (progn
                  (mk:tab-set tbl row 0 "")
                  (mk:tab-set tbl row 1 (strcat "   {\\L" cur "}"))
                  (mk:tab-merge tbl row)
                  (mk:tab-align tbl row 1 4)
                  (mk:tab-set tbl row 4 (itoa cnt-sub))
                  (mk:tab-set tbl row 5 (mk:tab-num mp-sub))
                  (mk:tab-align tbl row 4 5)
                  (mk:tab-align tbl row 5 5)
                  (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                  (setq row (1+ row))))
              (mk:tab-set tbl row 0 "")
              (mk:tab-set tbl row 1 "   {\\LИтого:}")
              (mk:tab-merge tbl row)
              (mk:tab-align tbl row 1 4)
              (mk:tab-set tbl row 4 (itoa total-cnt))
              (mk:tab-set tbl row 5 (mk:tab-num total-mp))
              (mk:tab-align tbl row 4 5)
              (mk:tab-align tbl row 5 5)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
              (vl-catch-all-apply 'vla-Update (list tbl))
              (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
              (prompt (strcat "\n  [OK] Таблица создана, строк данных: "
                              (itoa (length data))))
              (list tbl pt))))))))

;;;--- XLS (SpreadsheetML 2003) -----------------------------------------
(defun mk:xml-esc (str / out ch)
  (setq out "")
  (if (null str) (setq str ""))
  (while (/= str "")
    (setq ch (substr str 1 1))
    (cond
      ((= ch "&")  (setq out (strcat out "&amp;")))
      ((= ch "<")  (setq out (strcat out "&lt;")))
      ((= ch ">")  (setq out (strcat out "&gt;")))
      ((= ch "\"") (setq out (strcat out "&quot;")))
      ((= ch "'")  (setq out (strcat out "&apos;")))
      (t (setq out (strcat out ch))))
    (setq str (substr str 2)))
  out)

(defun mk:xcell (f style formula dtype val)
  (if formula
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:Formula=\"" (mk:xml-esc formula) "\">"
              "<Data ss:Type=\"" dtype "\">" (mk:xml-esc val)
              "</Data></Cell>") f)
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style "\">"
              "<Data ss:Type=\"" dtype "\">" (mk:xml-esc val)
              "</Data></Cell>") f)))

(defun mk:xcell-m (f style across dtype val)
  (write-line
    (strcat "    <Cell ss:StyleID=\"" style
            "\" ss:MergeAcross=\"" (itoa across) "\">"
            "<Data ss:Type=\"" dtype "\">" (mk:xml-esc val)
            "</Data></Cell>") f))

;; SUM по строкам r0..r1 в текущей колонке (формула пишется в строку r1+1)
(defun mk:xsum (r0 r1 / n0)
  (setq n0 (1- (- r0 r1)))
  (if (= n0 -1)
    "=R[-1]C"
    (strcat "=SUM(R[" (itoa n0) "]C:R[-1]C)")))

(defun mk:xsum-abs (rows col / out r)
  (if (null rows)
    "=0"
    (progn
      (setq out "")
      (foreach r rows
        (setq out (if (= out "")
                    (strcat "R" (itoa r) "C" (itoa col))
                    (strcat out ",R" (itoa r) "C" (itoa col)))))
      (strcat "=SUM(" out ")"))))

(defun mk:xstyle (f id bold fill align / )
  (mk:xstyle2 f id bold fill align nil))

(defun mk:xstyle2 (f id bold fill align uline / )
  (write-line (strcat "  <Style ss:ID=\"" id "\">") f)
  (if (or bold uline)
    (write-line (strcat "   <Font"
                        (if bold " ss:Bold=\"1\"" "")
                        (if uline " ss:Underline=\"Single\"" "") "/>") f))
  (if fill  (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f))
  (if align (write-line (strcat "   <Alignment ss:Horizontal=\"" align "\"/>") f))
  (write-line "   <Borders>" f)
  (foreach pos '("Bottom" "Left" "Right" "Top")
    (write-line (strcat "    <Border ss:Position=\"" pos
                        "\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>") f))
  (write-line "   </Borders>" f)
  (write-line "  </Style>" f))

(defun mk:tab-xls (data file acc / f rec sect art mark len cnt mp cur
                     xl-row grp-start sub-rows n-no cnt-sub mp-sub
                     total-cnt total-mp hdr w0)
  (setq f (open file "w"))
  (if (null f)
    (progn (prompt (strcat "\n  [ERROR] Нет доступа: " file)) nil)
    (progn
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:html=\"http://www.w3.org/TR/REC-html40\">" f)
      (write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\" ss:Name=\"Normal\">" f)
      (write-line "   <Font ss:FontName=\"Calibri\" ss:Size=\"11\"/>" f)
      (write-line "  </Style>" f)
      (mk:xstyle f "D"     nil nil nil)
      (mk:xstyle f "H"     t   t   "Center")
      (mk:xstyle f "TITLE" t   t   "Center")
      (mk:xstyle f "S"     t   t   "Left")
      (mk:xstyle f "SN"    t   t   "Right")
      (mk:xstyle2 f "SU" t t "Left" t)
      (write-line " </Styles>" f)
      (write-line " <Worksheet ss:Name=\"Профили\">" f)
      (write-line "  <Table>" f)
      (foreach w0 '("30" "110" "110" "80" "80" "80")
        (write-line (strcat "   <Column ss:Width=\"" w0 "\"/>") f))
      (write-line (strcat "   <Row><Cell ss:StyleID=\"TITLE\" ss:MergeAcross=\"5\">"
                          "<Data ss:Type=\"String\">" (mk:xml-esc *mk:tab-title*)
                          "</Data></Cell></Row>") f)
      (write-line "   <Row>" f)
      (foreach hdr '("№" "Артикул" "Марка" "Длина, мм" "Кол-во, шт." "Всего, м.п.")
        (mk:xcell f "H" nil "String" hdr))
      (write-line "   </Row>" f)
      (setq xl-row 3 n-no 0 cur nil cnt-sub 0 mp-sub 0.0
            total-cnt 0 total-mp 0.0 grp-start 3 sub-rows nil)
      (foreach rec data
        (setq sect (nth 1 rec)
              art  (nth 2 rec)
              mark (nth 3 rec)
              len  (nth 4 rec)
              cnt  (nth 5 rec)
              mp   (mk:tab-mp len cnt))
        (if (and cur (/= sect cur))
          (progn
            (write-line "   <Row>" f)
            (mk:xcell f "S" nil "String" "")
            (mk:xcell-m f "SU" 2 "String" (strcat "   " cur))
            (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (itoa cnt-sub))
            (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (mk:tab-xnum mp-sub))
            (write-line "   </Row>" f)
            (setq sub-rows (cons xl-row sub-rows)
                  cnt-sub 0 mp-sub 0.0
                  xl-row (1+ xl-row)
                  grp-start xl-row)))
        (setq cur sect
              cnt-sub (+ cnt-sub cnt)
              mp-sub  (+ mp-sub mp)
              total-cnt (+ total-cnt cnt)
              total-mp  (+ total-mp mp)
              n-no (1+ n-no))
        (write-line "   <Row>" f)
        (mk:xcell f "D" nil "Number" (itoa n-no))
        (mk:xcell f "D" nil "String" art)
        (mk:xcell f "D" nil "String" mark)
        (mk:xcell f "D" nil "Number" (mk:tab-int len))
        (mk:xcell f "D" nil "Number" (itoa cnt))
        (mk:xcell f "D" "=ROUND(RC[-2]*RC[-1]/1000,2)" "Number" (mk:tab-xnum mp))
        (write-line "   </Row>" f)
        (setq xl-row (1+ xl-row)))
      (if cur
        (progn
          (write-line "   <Row>" f)
          (mk:xcell f "S" nil "String" "")
          (mk:xcell-m f "SU" 2 "String" (strcat "   " cur))
          (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (itoa cnt-sub))
          (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (mk:tab-xnum mp-sub))
          (write-line "   </Row>" f)
          (setq sub-rows (cons xl-row sub-rows)
                xl-row (1+ xl-row))))
      (write-line "   <Row>" f)
      (mk:xcell f "S" nil "String" "")
      (mk:xcell-m f "S" 2 "String" "   Итого:")
      (mk:xcell f "SN" (mk:xsum-abs sub-rows 5) "Number" (itoa total-cnt))
      (mk:xcell f "SN" (mk:xsum-abs sub-rows 6) "Number" (mk:tab-xnum total-mp))
      (write-line "   </Row>" f)
      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (mk:acc-xls acc f)
      (mk:leg-xls *mk:leg-sfx* *mk:leg-arts* f)
      (write-line "</Workbook>" f)
      (close f)
      (prompt (strcat "\n  [OK] XLS: " file))
      t)))

;;;=====================================================================
;;; 20c. ЗАКЛАДНЫЕ: атрибуты «Пятка*» и «Кронштейн*» динамических блоков
;;;   Собираются только у вставок блоков (мультилинии игнорируются),
;;;   агрегируются по паре «атрибут + значение» и выводятся второй таблицей.
;;;=====================================================================
(setq *mk:acc-masks*    '("Пятка*" "Кронштейн*"))
(setq *mk:acc-gap*      30.0)   ; отступ второй таблицы от первой
(setq *mk:acc-title*    "Ведомость закладных")

;; Приведение тега атрибута к наименованию закладной: (маска . наименование).
;; Порядок списка задаёт порядок разделов в ведомости.
(setq *mk:acc-names*
  '(("КРОНШТЕЙН*"                    . "Кронштейн")
    ("ПЯТКА*НИЖ*"                    . "Пятка нижняя")
    ("ПЯТКА*НИЗ*"                    . "Пятка нижняя")
    ("ПЯТКА*Н"                       . "Пятка нижняя")
    ("ПЯТКА*ВЕРХ*"                   . "Пятка верхняя")
    ("ПЯТКА*В"                       . "Пятка верхняя")
    ("ПЯТКА*"                        . "Пятка")))

;; Наименование по тегу атрибута (если маска не подошла — сам тег)
(defun mk:acc-name (tag / out)
  (setq out nil)
  (foreach pair *mk:acc-names*
    (if (and (null out) (mk:strp tag) (wcmatch (strcase (mk:trim tag)) (car pair)))
      (setq out (cdr pair))))
  (if out out (mk:trim tag)))

;; Ранг наименования для сортировки (позиция в *mk:acc-names*)
(defun mk:acc-rank (name / i out)
  (setq i 0 out nil)
  (foreach pair *mk:acc-names*
    (if (and (null out) (= (strcase (cdr pair)) (strcase name)))
      (setq out i))
    (setq i (1+ i)))
  (if out out 99))
(setq *mk:acc-empty*    '("" "-" "—" "НЕТ" "НЕА" "0"))

;; Все атрибуты вставки: список (тег . значение)
(defun mk:attr-list (ename / sub data out)
  (setq out nil)
  (if (mk:has-attrs-insert? ename)
    (progn
      (setq sub (entnext ename))
      (while sub
        (setq data (entget sub))
        (cond
          ((null data) (setq sub nil))
          ((= (cdr (assoc 0 data)) "ATTRIB")
           (setq out (cons (cons (mk:trim (cdr (assoc 2 data)))
                                 (mk:trim (cdr (assoc 1 data)))) out)))
          ((= (cdr (assoc 0 data)) "SEQEND") (setq sub nil)))
        (if sub (setq sub (entnext sub))))))
  (reverse out))

(defun mk:acc-mask-hit? (tag / hit)
  (setq hit nil)
  (foreach m *mk:acc-masks*
    (if (and (null hit) (mk:strp tag) (wcmatch (strcase tag) (strcase m)))
      (setq hit t)))
  hit)

(defun mk:acc-empty? (v)
  (or (not (mk:strp v))
      (= (strlen (mk:trim v)) 0)
      (member (strcase (mk:trim v)) *mk:acc-empty*)))

;; Агрегация: ((тег значение кол-во) ...)
(defun mk:acc-rows (elements / out key hit tag val nm)
  (setq out nil)
  (foreach el elements
    (foreach pair (mk:attr-list (mk:rec-get el 'ENAME))
      (setq tag (car pair) val (cdr pair))
      (if (and (mk:acc-mask-hit? tag) (not (mk:acc-empty? val)))
        (progn
          (setq nm  (mk:acc-name tag)
                val (mk:trim val)
                key (strcat (strcase nm) "|" (strcase val))
                hit (assoc key out))
          (if hit
            (setq out (subst (list key nm val (1+ (nth 3 hit))) hit out))
            (setq out (cons (list key nm val 1) out)))))))
  (setq out (mapcar '(lambda (r) (list (nth 1 r) (nth 2 r) (nth 3 r))) out))
  ;; сортировка: наименование (порядок *mk:acc-names*) -> значение (натурально)
  (vl-sort out '(lambda (a b)
                  (cond
                    ((not (= (mk:acc-rank (nth 0 a)) (mk:acc-rank (nth 0 b))))
                     (< (mk:acc-rank (nth 0 a)) (mk:acc-rank (nth 0 b))))
                    ((not (= (mk:nat-key (nth 0 a)) (mk:nat-key (nth 0 b))))
                     (< (mk:nat-key (nth 0 a)) (mk:nat-key (nth 0 b))))
                    (t (< (mk:nat-key (nth 1 a)) (mk:nat-key (nth 1 b))))))))

;;;--- Таблица комплектующих в чертеже ----------------------------------
(defun mk:acc-table-create (data pt / doc space tbl nRows row n-row i hdr
                                 total-cnt old-echo)
  (if (null data)
    nil
    (progn
      (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object))
            space    (vla-get-ModelSpace doc)
            nRows    (+ 3 (length data))
            old-echo (getvar "CMDECHO"))
      (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
      (mk:ensure-layer *mk:layer-table* 7)
      (setq tbl (vl-catch-all-apply 'vla-AddTable
                  (list space (vlax-3d-point pt) nRows 4 10.0 35.0)))
      (if (vl-catch-all-error-p tbl)
        (progn
          (prompt (strcat "\n  [ERROR] AddTable: " (vl-catch-all-error-message tbl)))
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          nil)
        (progn
          (mk:ax-put tbl "Layer" *mk:layer-table*)
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 14.0))
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 1 60.0))
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 2 60.0))
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 3 32.0))
          (mk:tab-set tbl 0 0 *mk:acc-title*)
          (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 8.0))
          (setq i 0)
          (foreach hdr '("№" "Наименование" "Тип" "Кол-во, шт.")
            (mk:tab-set tbl 1 i hdr)
            (mk:tab-align tbl 1 i 5)
            (setq i (1+ i)))
          (vl-catch-all-apply 'vla-SetRowHeight (list tbl 1 8.0))
          (setq row 2 n-row 1 total-cnt 0)
          (foreach rec data
            (mk:tab-set tbl row 0 (itoa n-row))
            (mk:tab-set tbl row 1 (nth 0 rec))
            (mk:tab-set tbl row 2 (nth 1 rec))
            (mk:tab-set tbl row 3 (itoa (nth 2 rec)))
            (mk:tab-align tbl row 0 5)
            (mk:tab-align tbl row 1 4)
            (mk:tab-align tbl row 2 4)
            (mk:tab-align tbl row 3 5)
            (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
            (setq total-cnt (+ total-cnt (nth 2 rec))
                  row (1+ row) n-row (1+ n-row)))
          (mk:tab-set tbl row 0 "")
          (mk:tab-set tbl row 1 "   {\\LИтого:}")
          (mk:tab-set tbl row 3 (itoa total-cnt))
          (mk:tab-align tbl row 1 4)
          (mk:tab-align tbl row 3 5)
          (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
          (vl-catch-all-apply 'vla-Update (list tbl))
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          (prompt (strcat "\n  [OK] Таблица закладных, строк: " (itoa (length data))))
          tbl)))))

;;;--- Закладные в XLS (дополнительный лист) ----------------------------
(defun mk:acc-xls (data f / n total hdr)
  (if data
    (progn
      (write-line " <Worksheet ss:Name=\"Закладные\">" f)
      (write-line "  <Table>" f)
      (foreach hdr '("30" "180" "180" "80")
        (write-line (strcat "   <Column ss:Width=\"" hdr "\"/>") f))
      (write-line (strcat "   <Row><Cell ss:StyleID=\"TITLE\" ss:MergeAcross=\"3\">"
                          "<Data ss:Type=\"String\">" (mk:xml-esc *mk:acc-title*)
                          "</Data></Cell></Row>") f)
      (write-line "   <Row>" f)
      (foreach hdr '("№" "Наименование" "Тип" "Кол-во, шт.")
        (mk:xcell f "H" nil "String" hdr))
      (write-line "   </Row>" f)
      (setq n 0 total 0)
      (foreach rec data
        (setq n (1+ n) total (+ total (nth 2 rec)))
        (write-line "   <Row>" f)
        (mk:xcell f "D" nil "Number" (itoa n))
        (mk:xcell f "D" nil "String" (nth 0 rec))
        (mk:xcell f "D" nil "String" (nth 1 rec))
        (mk:xcell f "D" nil "Number" (itoa (nth 2 rec)))
        (write-line "   </Row>" f))
      (write-line "   <Row>" f)
      (mk:xcell f "SU" nil "String" "")
      (mk:xcell-m f "SU" 1 "String" "   Итого:")
      (mk:xcell f "SN" nil "Number" (itoa total))
      (write-line "   </Row>" f)
      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)))
  (length data))

;;;=====================================================================
;;; 20d. УСЛОВНЫЕ ОБОЗНАЧЕНИЯ
;;;   Расшифровка буквенных суффиксов марок и соответствие
;;;   «маркер размера -> артикул». В список попадают только те
;;;   обозначения, которые реально встретились в марках.
;;;=====================================================================
(setq *mk:leg-title*  "Условные обозначения")
(setq *mk:leg-h1*     "Условные обозначения ригелей:")
(setq *mk:leg-h2*     "Артикул профилей:")
(setq *mk:leg-none*   "без обозначения")

;; Расшифровка суффиксов: (обозначение . пояснение)
(defun mk:leg-desc-table ()
  (list (cons *mk:suffix-window-one*   "примыкание окна")
        (cons *mk:suffix-window-both*  "окна сверху и снизу")
        (cons *mk:suffix-door-one*     "примыкание двери")
        (cons *mk:suffix-threshold*    "накладной ригель")
        (cons *mk:suffix-warm-cold*    "переход тепло-холод")
        (cons *mk:suffix-cold-warm*    "переход холод-тепло")
        (cons *mk:suffix-vert-beam*    "вертикальный ригель (импост)")
        (cons *mk:suffix-tjoint-lo*
              "Т-соединение снизу (у стоек - левая из зеркальной пары)")
        (cons *mk:suffix-tjoint-hi*
              "Т-соединение сверху (у стоек - правая из зеркальной пары)")
        (cons *mk:suffix-tjoint-both*  "Т-соединения снизу и сверху")))

;; Хвост марки: всё после «Ст»/«Рг» и номера («Т-1 Рг9м2ок» -> «м2ок»)
(defun mk:mark-tail (mark / s i n c out)
  (setq out "")
  (if (mk:strp mark)
    (progn
      (setq s (mk:trim mark) i (strlen s))
      ;; обрезать по последнему пробелу
      (while (and (> i 0) (not (= (substr s i 1) " ")))
        (setq i (1- i)))
      (setq s (substr s (1+ i)) n (strlen s) i 1)
      ;; пропустить буквы обозначения типа (Ст / Рг)
      (while (and (<= i n) (not (wcmatch (substr s i 1) "#")))
        (setq i (1+ i)))
      ;; пропустить номер
      (while (and (<= i n) (wcmatch (substr s i 1) "#"))
        (setq i (1+ i)))
      (if (<= i n) (setq out (substr s i)))))
  out)

;; Суффиксы, встретившиеся в марках агрегата: ((обозначение . пояснение) ...)
(defun mk:leg-suffixes (agg / tails out hit)
  (setq tails nil out nil)
  (foreach rec agg
    (setq tails (cons (mk:mark-tail (nth 3 rec)) tails)))
  (foreach pair (mk:leg-desc-table)
    (if (and (> (strlen (car pair)) 0)
             (progn (setq hit nil)
                    (foreach t2 tails
                      (if (vl-string-search (car pair) t2) (setq hit t)))
                    hit))
      (setq out (cons pair out))))
  (reverse out))

;; Соответствие «маркер -> артикул» по лестнице размеров (только с артикулом)
(defun mk:leg-articles (elements / pairs sorted labels out)
  (setq pairs (mk:article-cross-pairs elements) out nil)
  (if pairs
    (progn
      (setq sorted (vl-sort pairs '(lambda (a b) (< (cdr a) (cdr b))))
            labels (mk:size-labels (length sorted)))
      (foreach pr sorted
        (setq out (cons (cons (car labels) (car pr)) out))
        (setq labels (cdr labels)))))     ; из возрастания -> убывание
  out)

;; Печать в командную строку
(defun mk:leg-print (sfx arts)
  (if sfx
    (progn
      (prompt (strcat "\n  " *mk:leg-h1*))
      (foreach p sfx
        (prompt (strcat "\n    " (mk:pad (car p) 6) " - " (cdr p))))))
  (if arts
    (progn
      (prompt (strcat "\n  " *mk:leg-h2*))
      (foreach p arts
        (prompt (strcat "\n    "
                        (mk:pad (if (> (strlen (car p)) 0) (car p) *mk:leg-none*) 16)
                        " - " (cdr p))))))
  nil)

;;;--- Таблица обозначений в чертеже ------------------------------------
(defun mk:leg-merge (tbl r)
  (vl-catch-all-apply 'vlax-invoke-method (list tbl "MergeCells" r r 0 1)))

(defun mk:leg-table-create (sfx arts pt / doc space tbl nRows row i hdr old-echo)
  (if (and (null sfx) (null arts))
    nil
    (progn
      (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object))
            space (vla-get-ModelSpace doc)
            nRows (+ 1
                     (if sfx  (1+ (length sfx))  0)
                     (if arts (1+ (length arts)) 0))
            old-echo (getvar "CMDECHO"))
      (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
      (mk:ensure-layer *mk:layer-table* 7)
      (setq tbl (vl-catch-all-apply 'vla-AddTable
                  (list space (vlax-3d-point pt) nRows 2 10.0 40.0)))
      (if (vl-catch-all-error-p tbl)
        (progn
          (prompt (strcat "\n  [ERROR] AddTable: " (vl-catch-all-error-message tbl)))
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          nil)
        (progn
          (mk:ax-put tbl "Layer" *mk:layer-table*)
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 40.0))
          (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 1 120.0))
          (mk:tab-set tbl 0 0 *mk:leg-title*)
          (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 8.0))
          (setq row 1)
          (if sfx
            (progn
              (mk:leg-merge tbl row)
              (mk:tab-set tbl row 0 (strcat "   {\\L" *mk:leg-h1* "}"))
              (mk:tab-align tbl row 0 4)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
              (setq row (1+ row))
              (foreach p sfx
                (mk:tab-set tbl row 0 (car p))
                (mk:tab-set tbl row 1 (cdr p))
                (mk:tab-align tbl row 0 5)
                (mk:tab-align tbl row 1 4)
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                (setq row (1+ row)))))
          (if arts
            (progn
              (mk:leg-merge tbl row)
              (mk:tab-set tbl row 0 (strcat "   {\\L" *mk:leg-h2* "}"))
              (mk:tab-align tbl row 0 4)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
              (setq row (1+ row))
              (foreach p arts
                (mk:tab-set tbl row 0
                  (if (> (strlen (car p)) 0) (car p) *mk:leg-none*))
                (mk:tab-set tbl row 1 (cdr p))
                (mk:tab-align tbl row 0 5)
                (mk:tab-align tbl row 1 4)
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                (setq row (1+ row)))))
          (vl-catch-all-apply 'vla-Update (list tbl))
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          (prompt (strcat "\n  [OK] Таблица обозначений, строк: "
                          (itoa (+ (length sfx) (length arts)))))
          tbl)))))

;;;--- Обозначения в XLS (дополнительный лист) --------------------------
(defun mk:leg-xls (sfx arts f)
  (if (or sfx arts)
    (progn
      (write-line " <Worksheet ss:Name=\"Обозначения\">" f)
      (write-line "  <Table>" f)
      (write-line "   <Column ss:Width=\"110\"/>" f)
      (write-line "   <Column ss:Width=\"320\"/>" f)
      (write-line (strcat "   <Row><Cell ss:StyleID=\"TITLE\" ss:MergeAcross=\"1\">"
                          "<Data ss:Type=\"String\">" (mk:xml-esc *mk:leg-title*)
                          "</Data></Cell></Row>") f)
      (if sfx
        (progn
          (write-line "   <Row>" f)
          (mk:xcell-m f "SU" 1 "String" *mk:leg-h1*)
          (write-line "   </Row>" f)
          (foreach p sfx
            (write-line "   <Row>" f)
            (mk:xcell f "H" nil "String" (car p))
            (mk:xcell f "D" nil "String" (cdr p))
            (write-line "   </Row>" f))))
      (if arts
        (progn
          (write-line "   <Row>" f)
          (mk:xcell-m f "SU" 1 "String" *mk:leg-h2*)
          (write-line "   </Row>" f)
          (foreach p arts
            (write-line "   <Row>" f)
            (mk:xcell f "H" nil "String"
              (if (> (strlen (car p)) 0) (car p) *mk:leg-none*))
            (mk:xcell f "D" nil "String" (cdr p))
            (write-line "   </Row>" f))))
      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)))
  nil)

;;;--- Команда -----------------------------------------------------------
(setq *mk:leg-sfx* nil)
(setq *mk:leg-arts* nil)

(defun mk:group-delete (name / doc groups g)
  (setq doc    (mk:ax-get (vlax-get-acad-object) "ActiveDocument")
        groups (if doc (mk:ax-get doc "Groups") nil))
  (if groups
    (progn
      (setq g (vl-catch-all-apply 'vlax-invoke-method (list groups "Item" name)))
      (if (and g (not (vl-catch-all-error-p g)))
        (vl-catch-all-apply 'vlax-invoke-method (list g "Delete")))))
  nil)

;; Удалить всю отрисовку модели: объекты слоя + одноимённую группу
(defun mk:erase-model (layer group / ss i n)
  (mk:group-delete group)
  (setq ss (ssget "_X" (list (cons 8 layer))))
  (setq n 0)
  (if ss
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i))
        (if (entdel (ssname ss i)) (setq n (1+ n))))))
  (prompt (strcat "\n  [OK] Сетка удалена, объектов: " (itoa n)))
  n)

;;;=====================================================================
;;; 20e. МНОГОВИТРАЖНЫЙ КОНВЕЙЕР (Ред. 7.0)
;;;   Раньше МАРКАВ брал один блок «Атрибуты витража» и навешивал его
;;;   префикс на всю выборку: при нескольких витражах вся выборка
;;;   получала префикс одной (ближайшей) рамки — например, ВН-7.
;;;   Теперь: рамки ищутся по всему чертежу, каждый объект получает марку
;;;   витража по максимальной площади пересечения BBOX с CaptureBBox
;;;   рамки, объекты делятся на группы, и каждая группа проходит цикл
;;;   (сбор -> стыки -> топология -> стойки -> ригели) отдельно, со своим
;;;   префиксом и своими локальными шкалами. Ведомость — одна, общая.
;;;   Выбор: выбраны объекты — только они; выбор отменён — весь чертёж.
;;;   Рамки всегда ищутся по всему чертежу, даже если не попали в выборку.
;;;=====================================================================
(setq *mk:last-groups*   nil)   ; группы прогона: (TAG FRAME ENANES RECORDS COUNTS)
(setq *mk:last-extra*    nil)   ; объекты вне рамок — не маркируются
(setq *mk:last-scope*    nil)   ; полная выборка прогона (список ename)
(setq *mk:last-frames*   nil)   ; рамки чертежа
(setq *mk:last-ss*       nil)   ; набор выборки прогона (для collect)
(setq *mk:last-prefix*   nil)   ; префикс текущей группы (выноски, группы)
(setq *mk:run-scope*     nil)   ; выборка пакета МАРКАВ (список ename)
(setq *mk:last-mode*     nil)   ; режим сбора каркаса прогона (nil — ещё не задан)
(setq *mk:vitrage-none*  "<ВНЕ РАМОК>")

;;;--- Служебное -------------------------------------------------------

;; ATTRIB / SEQEND / VERTEX — подобъекты, а не самостоятельные объекты
(defun mk:sub-entity? (ename / typ)
  (setq typ (if ename (cdr (assoc 0 (entget ename))) nil))
  (or (= typ "ATTRIB") (= typ "SEQEND") (= typ "VERTEX")))

;; Имя объекта: Name, затем EffectiveName
(defun mk:ent-name (ename / nm nm2 o)
  (setq nm (if ename (cdr (assoc 2 (entget ename))) nil))
  (setq o (if ename (vl-catch-all-apply 'vlax-ename->vla-object (list ename)) nil))
  (if (and o (not (vl-catch-all-error-p o)))
    (progn
      (setq nm2 (mk:ax-get o "EffectiveName"))
      (if (and (mk:strp nm2) (> (strlen nm2) 0)) (setq nm nm2))))
  nm)

;; Признак рамки: динамические «Ширина рамки» / «Высота рамки».
;; Работает и для не-INSERT (MLINE, LWPOLYLINE и т.п.) — динамики у них нет,
;; поэтому признак просто вернёт nil, без ошибок.
(defun mk:frame-like? (ename / o w h)
  (setq o (if ename (vl-catch-all-apply 'vlax-ename->vla-object (list ename)) nil))
  (if (or (null o) (vl-catch-all-error-p o))
    nil
    (progn
      (setq w (mk:get-dyn o "*Ширина рамки*")
            h (mk:get-dyn o "*Высота рамки*"))
      (and (numberp w) (numberp h) (> w 0.0) (> h 0.0)))))

;; ---------------------------------------------------------------- защита логов
;; В лог попадают значения из записей, которых может не быть (нет ключа, блок
;; без динамики, размер не определён). Любая такая строка раньше роняла прогон
;; на strcat/rtos с «неверный тип аргумента». Здесь все подстановки безопасны.
(defun mk:safe-str (v)
  (cond ((mk:strp v) v)
        ((numberp v) (rtos v 2 2))
        (t "—")))

(defun mk:safe-num (v / s)
  (setq s (vl-catch-all-apply 'rtos (list v 2 1)))
  (if (or (vl-catch-all-error-p s) (null s) (not (mk:strp s))) "—" s))

;; Совпадает ли объект хотя бы с одной маской списка
(defun mk:blk-match-any? (ename masks / hit)
  (setq hit nil)
  (foreach m masks
    (if (and (null hit) (mk:blk-match? ename m)) (setq hit t)))
  hit)

;; Маски-исключения: имя совпало с маской проёма, но это не проём
;; (глухое заполнение «Кассета» и т.п.) — как *mark:opening-skip-masks* в MarkZ.
(defun mk:opening-skipped? (ename / hit)
  (setq hit nil)
  (foreach m *mk:opening-skip-masks*
    (if (and (null hit) (mk:blk-match? ename m)) (setq hit t)))
  hit)

;; Тип объекта конвейера; nil — объект в конвейер не входит.
;; КАРКАС — геометрия, из которой в режиме «Все-типы»/«Мультилинии»
;; собираются стойки и ригели (MLINE и прочие типы *mk:seg-types*).
;; Без этой ветки выборка по рамке теряла бы саму геометрию каркаса: в группу
;; попадали только блоки, и на мультилиниях не было ни стоек, ни ригелей.
(defun mk:obj-kind (ename / n typ)
  (if (or (null ename) (mk:sub-entity? ename))
    nil
    (cond
      ((mk:blk-match? ename (strcat "*" *mk:block-fill* "*"))   "ЗАПОЛНЕНИЕ")
      ((mk:opening-skipped? ename) nil)   ; «Кассета» — не проём
      ((mk:blk-match-any? ename *mk:door-masks*)                "ДВЕРЬ")
      ((mk:blk-match-any? ename *mk:window-masks*)              "ОКНО")
      ((mk:blk-match? ename (strcat "*" *mk:block-window* "*")) "ОКНО")
      ((mk:blk-match? ename (strcat "*" *mk:block-door* "*"))   "ДВЕРЬ")
      ((mk:blk-match? ename (strcat "*" *mk:block-post* "*"))   "СТОЙКА")
      ((mk:blk-match? ename (strcat "*" *mk:block-beam* "*"))   "РИГЕЛЬ")
      (t
       (setq n (mk:ent-name ename))
       (setq typ (cdr (assoc 0 (entget ename))))
       (cond
         ((mk:name= n *mk:block-vitrage*) "АТРИБУТЫ_ВИТРАЖА")
         ((mk:frame-like? ename)          "РАМКА")
         ((member typ *mk:seg-types*)     "КАРКАС")
         ((and (mk:strp n) (wcmatch (strcase n) "*РЯД ЗАПОЛНЕНИ*")) "РЯД")
         (t nil))))))

;;;--- Этап 1. Выборка -------------------------------------------------

(defun mk:select-scope (/ ss out e i all)
  (prompt "\nВыберите элементы витражей (Enter — весь чертёж): ")
  (setq ss (ssget))
  (if ss
    (progn
      (setq out nil i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (not (mk:sub-entity? e)) (setq out (cons e out)))
        (setq i (1+ i)))
      (setq *mk:scope-source* "выборка" out (reverse out))
      (prompt (strcat "\n  Область: выборка, объектов " (itoa (length out)))))
    (progn
      (prompt "\n  [INFO] Выбор отменён — обрабатывается ВЕСЬ чертёж.")
      (setq all (ssget "_X" '((0 . "INSERT,MLINE,LINE,LWPOLYLINE,POLYLINE,ARC"))))
      (setq out nil i 0)
      (if all
        (repeat (sslength all)
          (setq e (ssname all i))
          (if (not (mk:sub-entity? e)) (setq out (cons e out)))
          (setq i (1+ i))))
      (setq *mk:scope-source* "чертёж" out (reverse out))
      (prompt (strcat "\n  Область: весь чертёж, объектов " (itoa (length out))))))
  out)

;; Список ename -> набор SSGET (подвыборка годится для всех collect-функций)
(defun mk:ss-of (enames / ss)
  (setq ss (ssadd))
  (foreach e enames (if e (ssadd e ss)))
  ss)

;;;--- Этап 2. Рамки (по всему чертежу) --------------------------------

;; Запись рамки: (ENAME INS_PT BBOX CAPTURE AX AY TAG ATTR)
;; AX: +1 — ширина растёт вправо, -1 — влево (Вертикаль);
;; AY: -1 — высота растёт вниз, +1 — вверх (Горизонталь);
;; ATTR = t для блока «Атрибуты витража» (он тоже несёт ширину/высоту рамки).
;; TAG — атрибут «Витраж» этой рамки: у блока «Атрибуты витража» он главный,
;; по нему и называется весь витраж.
(defun mk:frame-detail (ename / d o p w h vf hf cap tag attr)
  (setq d (entget ename) p (cdr (assoc 10 d)))
  (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (null o) (vl-catch-all-error-p o))
    nil
    (progn
      (setq w    (mk:get-dyn o "*Ширина рамки*")
            h    (mk:get-dyn o "*Высота рамки*")
            vf   (mk:get-dyn o "*Вертикаль*")
            hf   (mk:get-dyn o "*Горизонталь*")
            attr (mk:name= (mk:ent-name ename) *mk:block-vitrage*)
            cap  (mk:frame-capture-bb ename)
            tag  (mk:get-attr ename *mk:attr-vitrage*))
      (if (null cap) (setq cap (mk:e-bb o)))
      (if (null cap)
        nil
        (list ename p (mk:e-bb o) cap
              (if (= vf 1) -1 1)
              (if (= hf 1) 1 -1)
              (if (and (mk:strp tag) (> (strlen tag) 0)) tag nil)
              (if attr t nil))))))

;; Все рамки чертежа: слева-направо, сверху-вниз (сверху вниз — по Y).
;; Имя витража берётся из атрибута «Витраж» рамки или её блока «Атрибуты
;; витража». Рамка без имени получает ВИТРАЖ-N; одинаковые имена разводятся
;; суффиксом -2, -3, чтобы группы не сливались.
(defun mk:frame-table (/ ss i e rec frames lst n nm out seen base k)
  (setq ss (ssget "_X" '((0 . "INSERT"))) i 0 frames nil)
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i))
      (if (mk:frame-like? e)
        (progn
          (setq rec (mk:frame-detail e))
          (if rec (setq frames (cons rec frames)))))
      (setq i (1+ i))))
  (setq lst (vl-sort frames
              '(lambda (a b)
                 (if (equal (cadr (nth 1 a)) (cadr (nth 1 b)) 1.0)
                   (< (car (nth 1 a)) (car (nth 1 b)))
                   (> (cadr (nth 1 a)) (cadr (nth 1 b)))))))
  ;; имя: своё -> имя блока «Атрибуты витража» внутри -> ВИТРАЖ-N
  (setq n 0 seen nil out nil)
  (foreach fr lst
    (setq nm (nth 6 fr))
    (if (null nm) (setq nm (mk:frame-inner-tag fr lst)))
    (if (null nm)
      (progn (setq n (1+ n)) (setq nm (strcat "ВИТРАЖ-" (itoa n)))))
    (setq base nm k 1)
    (while (member nm seen) (setq k (1+ k) nm (strcat base "-" (itoa k))))
    (setq seen (cons nm seen))
    (setq out (cons (list (nth 0 fr) (nth 1 fr) (nth 2 fr) (nth 3 fr)
                          (nth 4 fr) (nth 5 fr) nm (nth 7 fr)) out)))
  (reverse out))

;;;--- Этап 3. Принадлежность -----------------------------------------

;; Индекс рамки с максимальным пересечением BBOX; 0 — вне рамок.
;; Рамка витража — это либо отдельный объект рамки, либо, если отдельных рамок
;; в чертеже нет, сам блок «Атрибуты витража» (у него те же динамические
;; «Ширина/Высота рамки»). Поэтому сначала ищем среди «настоящих» рамок, и
;; только если ни одна не подошла — среди блоков атрибутов. Иначе в чертеже,
;; где рамка одна и это и есть блок атрибутов, все объекты остались бы
;; «вне рамок».
(defun mk:frame-of (bb frames / i best besta a best2 besta2)
  (setq i 1 best nil besta 0.0 best2 nil besta2 0.0)
  (foreach fr frames
    (setq a (mk:bb-overlap-area bb (nth 3 fr)))
    (if (nth 7 fr)
      (if (> a besta2) (setq besta2 a best2 i))
      (if (> a besta) (setq besta a best i)))
    (setq i (1+ i)))
  (cond (best best)
        (best2 best2)
        (t 0)))

;; Рамка, внутри которой стоит блок «Атрибуты витража» (по точке вставки).
;; Из нескольких вложенных рамок берём минимальную — она и есть «своя»;
;; если настоящих рамок нет, годится и рамка-блок атрибутов.
(defun mk:frame-owner (pt frames / i best besta a cap best2 besta2)
  (setq i 1 best nil besta nil best2 nil besta2 nil)
  (foreach fr frames
    (setq cap (nth 3 fr))
    (if (and pt cap
             (>= (car pt) (nth 0 cap)) (<= (car pt) (nth 2 cap))
             (>= (cadr pt) (nth 1 cap)) (<= (cadr pt) (nth 3 cap)))
      (progn
        (setq a (* (- (nth 2 cap) (nth 0 cap)) (- (nth 3 cap) (nth 1 cap))))
        (if (nth 7 fr)
          (if (or (null besta2) (< a besta2)) (setq besta2 a best2 i))
          (if (or (null besta) (< a besta)) (setq besta a best i)))))
    (setq i (1+ i)))
  (cond (best best)
        (best2 best2)
        (t 0)))

;; Имя витража из блока «Атрибуты витража», стоящего внутри рамки FR.
;; Нужно, когда у самой рамки атрибут «Витраж» пуст.
(defun mk:frame-inner-tag (fr frames / cap best besta tag)
  (setq cap (nth 3 fr) best nil besta nil)
  (foreach f frames
    (if (and (nth 7 f) (nth 6 f) (nth 1 f)
             (>= (car (nth 1 f)) (nth 0 cap)) (<= (car (nth 1 f)) (nth 2 cap))
             (>= (cadr (nth 1 f)) (nth 1 cap)) (<= (cadr (nth 1 f)) (nth 3 cap)))
      (progn
        (setq tag (nth 6 f))
        (if (null besta)
          (setq besta 1.0 best tag)))))
  best)

;; Локальные координаты внутри витража: X слева-направо, Y сверху вниз,
;; с учётом отражений рамки (Вертикаль / Горизонталь).
(defun mk:local-xy (pt fr / cap)
  (setq cap (nth 3 fr))
  (list (if (> (nth 4 fr) 0) (- (car pt) (nth 0 cap)) (- (nth 2 cap) (car pt)))
        (if (> (nth 5 fr) 0) (- (cadr pt) (nth 1 cap)) (- (nth 3 cap) (cadr pt)))))

;;;--- Этап 3a. Единая запись объекта --------------------------------

(defun mk:obj-record (ename kind frames / o bb idx fr pt d)
  (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (setq bb (if (and o (not (vl-catch-all-error-p o))) (mk:e-bb o) nil))
  (setq d (entget ename) pt (cdr (assoc 10 d)))
  ;; «Атрибуты витража» принадлежат рамке по точке вставки, остальные —
  ;; по максимальной площади пересечения BBOX с CaptureBBox рамки
  (setq idx (cond
              ((= kind "АТРИБУТЫ_ВИТРАЖА") (mk:frame-owner pt frames))
              (bb (mk:frame-of bb frames))
              (t 0)))
  (setq fr (if (> idx 0) (nth (1- idx) frames) nil))
  (list (cons 'ENAME   ename)
        (cons 'HANDLE  (cdr (assoc 5 d)))
        (cons 'TYPE    kind)
        (cons 'NAME    (mk:ent-name ename))
        (cons 'BBOX    bb)
        (cons 'INS_PT  pt)
        (cons 'CENTER  (if bb (list (/ (+ (nth 0 bb) (nth 2 bb)) 2.0)
                                    (/ (+ (nth 1 bb) (nth 3 bb)) 2.0)) nil))
        (cons 'VITRAGE (if fr (nth 6 fr) *mk:vitrage-none*))
        (cons 'FRAME   idx)
        (cons 'LOCAL   (if (and fr pt) (mk:local-xy pt fr) nil))))

;;;--- Этап 4. Группы -------------------------------------------------

(defun mk:uniq-nums (lst / out v)
  (setq out nil)
  (foreach v lst (if (not (member v out)) (setq out (cons v out))))
  (reverse out))

(defun mk:filter-frame (records idx / out)
  (setq out nil)
  (foreach r records
    (if (= (cdr (assoc 'FRAME r)) idx) (setq out (cons r out))))
  (reverse out))

(defun mk:drop-kind (records kind / out)
  (setq out nil)
  (foreach r records
    (if (/= (cdr (assoc 'TYPE r)) kind) (setq out (cons r out))))
  (reverse out))

(defun mk:count-kind (records kind / n)
  (setq n 0)
  (foreach r records (if (= (cdr (assoc 'TYPE r)) kind) (setq n (1+ n))))
  n)


;; Делит выборку по витражам: (GROUPS EXTRA).
;; Группа: (TAG FRAME ENANES RECORDS COUNTS), COUNTS =
;; (стойки ригели заполнения окна двери всего)
(defun mk:split-by-vitrage (enames frames / records idxs out extra i sub tag k c unknown)
  (setq records nil unknown nil)
  (foreach e enames
    (setq k (mk:obj-kind e))
    (if k
      (setq records (cons (mk:obj-record e k frames) records))
      ;; вставка, не подошедшая ни под один тип, раньше терялась молча —
      ;; именно так «пропадали» окна с незнакомым именем блока
      (if (= (cdr (assoc 0 (entget e))) "INSERT")
        (setq unknown (cons e unknown)))))
  (setq records (reverse records)
        *mk:last-unknown* (reverse unknown))
  ;; группы — только по рамкам, в которых есть что обрабатывать: рамка без
  ;; единого объекта (или пустой «Атрибуты витража» в стороне) группу не создаёт
  (setq idxs (mk:uniq-nums
               (vl-sort
                 (mapcar '(lambda (r)
                            (if (member (cdr (assoc 'TYPE r)) '("АТРИБУТЫ_ВИТРАЖА" "РАМКА"))
                              0
                              (cdr (assoc 'FRAME r))))
                         records) '<)))
  (setq out nil extra nil)
  (foreach i idxs
    (setq sub (mk:filter-frame records i))
    (setq sub (mk:drop-kind sub "АТРИБУТЫ_ВИТРАЖА"))
    (setq sub (mk:drop-kind sub "РАМКА"))
    (if (= i 0)
      (setq extra sub)
      (progn
        (setq tag (nth 6 (nth (1- i) frames)))
        (setq c (list (mk:count-kind sub "СТОЙКА")
                      (mk:count-kind sub "РИГЕЛЬ")
                      (mk:count-kind sub "ЗАПОЛНЕНИЕ")
                      (mk:count-kind sub "ОКНО")
                      (mk:count-kind sub "ДВЕРЬ")
                      (length sub)))
        (setq out (cons (list tag (nth (1- i) frames)
                              (mapcar '(lambda (r) (cdr (assoc 'ENAME r))) sub)
                              sub c)
                        out)))))
  (list (reverse out) extra))

;;;--- Нераспознанные вставки ------------------------------------------

;; Сводка «имя блока -> сколько раз встретилось» по вставкам, которые не
;; попали ни в стойки/ригели, ни в заполнения/окна/двери, ни в рамки.
;; Нужна для разбора «окна = 0»: в логе сразу видно настоящее имя семейства.
(defun mk:unknown-table (/ out hit key)
  (setq out nil)
  (foreach e *mk:last-unknown*
    (setq key (mk:ent-name e))
    (setq key (if (mk:strp key) key "?"))
    (if (setq hit (assoc key out))
      (setq out (subst (cons key (1+ (cdr hit))) hit out))
      (setq out (cons (cons key 1) out))))
  (vl-sort out '(lambda (a b) (> (cdr a) (cdr b)))))

(defun mk:print-unknown (/ tb n shown tail)
  (setq tb (mk:unknown-table) n (length *mk:last-unknown*))
  (if (> n 0)
    (progn
      (prompt (strcat "\n  [INFO] Вставок вне типов MarkV: " (itoa n)
                      " — имена блоков ниже, полный список в MARKZV_MODEL.txt"))
      (setq shown 0 tail nil)
      (foreach pr tb
        (if (< shown 10)
          (progn
            (prompt (strcat "\n    " (mk:safe-str (car pr)) " x" (itoa (cdr pr))))
            (setq shown (1+ shown)))
          (setq tail t)))
      (if tail (prompt "\n    ... (остальные — в MARKZV_MODEL.txt)")))))

;;;--- Печать групп ----------------------------------------------------

(defun mk:print-groups (groups extra / n g c)
  (setq n 0)
  (prompt "\n=== ГРУППЫ ПО ВИТРАЖАМ ===")
  (foreach g groups
    (setq n (1+ n) c (nth 4 g))
    (prompt (strcat "\n  " (mk:pad (nth 0 g) 10)
                    " стойки " (itoa (nth 0 c))
                    ", ригели " (itoa (nth 1 c))
                    ", заполнения " (itoa (nth 2 c))
                    ", окна " (itoa (nth 3 c))
                    ", двери " (itoa (nth 4 c))
                    "   всего " (itoa (nth 5 c)))))
  (prompt (strcat "\n  " (mk:pad *mk:vitrage-none* 10)
                  " объектов: " (itoa (length extra))
                  " — в маркировку не входят"))
  n)

;;;--- Один витраж -----------------------------------------------------

;; Проставить марку витража в записи группы
(defun mk:tag-records (els tag / out)
  (setq out nil)
  (foreach el els (setq out (cons (mk:rec-put el 'VITRAGE tag) out)))
  (reverse out))

;; Сбор данных одного витража: подвыборка группы -> collect-scope.
;; Сбор дорогой (полный проход по объектам выборки), а нужен одним и тем же
;; в геометрии, стойках, ригелях и ведомости — поэтому кэшируется по витражу
;; на время прогона (кэш чистит mk:prepare-run).
(setq *mk:group-cache* nil)
(setq *mk:run-errors* nil)   ; ((витраж . сообщение) ...) за прогон
(setq *mk:last-unknown* nil) ; вставки, не подошедшие ни под один тип MarkV
;; t — идёт сбор внутри витража: префикс витража задан группой
(setq *mk:in-group* nil)

(defun mk:group-cache-clear () (setq *mk:group-cache* nil))

(defun mk:group-data (grp / tag enames sub data hit)
  (setq tag (nth 0 grp) enames (nth 2 grp))
  (setq *mk:last-prefix* tag)
  (setq hit (assoc tag *mk:group-cache*))
  (if hit
    (cdr hit)
    (progn
      (setq sub (mk:ss-of enames))
      (setq *mk:in-group* t)
      (setq data (mk:collect-scope sub))
      (setq *mk:in-group* nil)
      (setq data
        (list (cons 'POSTS   (mk:tag-records (cdr (assoc 'POSTS data))   tag))
              (cons 'BEAMS   (mk:tag-records (cdr (assoc 'BEAMS data))   tag))
              (cons 'FILLS   (mk:tag-records (cdr (assoc 'FILLS data))   tag))
              (cons 'WINDOWS (mk:tag-records (cdr (assoc 'WINDOWS data)) tag))
              (cons 'DOORS   (mk:tag-records (cdr (assoc 'DOORS data))   tag))))
      (setq *mk:group-cache* (cons (cons tag data) *mk:group-cache*))
      data)))

;; План марок стоек одного витража (локальные группы и нумерация).
;; Заполнения и двери одной ячейки стоят вплотную, и проверка перекрытий
;; на одном витраже давала ложные «перекрытия» — при равной площади пары
;; «заполнение + окно/дверь» считаются одной ячейкой и не сравниваются.
(defun mk:group-plan-posts (grp / data posts elements)
  (setq data (mk:group-data grp))
  (setq posts (cdr (assoc 'POSTS data)))
  (if posts
    (progn
      (setq elements (append (cdr (assoc 'FILLS data))
                             (cdr (assoc 'WINDOWS data))
                             (cdr (assoc 'DOORS data))
                             (cdr (assoc 'BEAMS data))))
      (mk:plan-posts (mk:annotate-posts posts elements) (nth 0 grp)))))

;; План марок ригелей одного витража
(defun mk:group-plan-beams (grp / data beams posts panels doors bounds)
  (setq data (mk:group-data grp))
  (setq beams  (cdr (assoc 'BEAMS data))
        posts  (cdr (assoc 'POSTS data))
        panels (append (cdr (assoc 'FILLS data))
                       (cdr (assoc 'WINDOWS data))
                       (cdr (assoc 'DOORS data)))
        doors  (cdr (assoc 'DOORS data)))
  (if beams
    (progn
      (setq bounds (mk:find-vitrage-bounds posts beams))
      (mk:plan-beams (mk:annotate-beams beams panels doors bounds posts)
                     (nth 0 grp)))))

;;;--- Подготовка прогона ---------------------------------------------

;; Область прогона: выборка пакета МАРКАВ либо вопрос пользователю
(defun mk:run-scope (/ enames)
  (if (and *mk:batch* *mk:run-scope*)
    (progn
      (prompt (strcat "\n  Область: выборка пакета МАРКАВ, объектов "
                      (itoa (length *mk:run-scope*))))
      *mk:run-scope*)
    (mk:select-scope)))

;; Выборка (или весь чертёж) + рамки всего чертежа + группы по витражам
(defun mk:prepare-run (/ enames frames res mode)
  (mk:group-cache-clear)
  (setq *mk:in-group* nil)   ; состояние предыдущего прогона не наследуем
  ;; режим сбора спрашивается один раз на прогон: в пакете его задаёт МАРКАВ,
  ;; при одиночном запуске этапа — сам mk:prepare-run
  (if (null *mk:last-mode*)
    (progn
      (initget "Блоки Мультилинии Все-типы B M A")
      (setq mode (getkword
                   "\nРежим сбора каркаса [Блоки/Мультилинии/Все-типы] <Все-типы>: "))
      (setq *mk:last-mode* (mk:norm-mode mode))
      (prompt (strcat "\n  Режим: " *mk:last-mode*))))
  (setq *mk:scope-mode* (mk:norm-mode *mk:last-mode*))
  (setq enames (mk:run-scope))
  (if (null enames)
    (progn (prompt "\n[INFO] Нечего обрабатывать — прогон прерван.") nil)
    (progn
      (setq frames (mk:frame-table))
      (prompt (strcat "\n  Рамок витражей в чертеже: " (itoa (length frames))))
      (if (null frames)
        (prompt "\n  [WARN] Рамки «Атрибуты витража» не найдены — все объекты вне рамок."))
      (setq res (mk:split-by-vitrage enames frames))
      (setq *mk:last-scope*  enames
            *mk:last-frames* frames
            *mk:last-groups* (car res)
            *mk:last-extra*  (cadr res)
            *mk:last-ss*     (mk:ss-of enames))
      (mk:print-groups *mk:last-groups* *mk:last-extra*)
      (mk:print-unknown)
      (if (null (car res))
        (progn (prompt "\n[WARN] Ни один объект не попал в рамки витражей.") nil)
        res))))

;; Группы для этапа: в пакете — уже собранные, иначе новый прогон
(defun mk:groups-or-prepare (/ res)
  (if (and *mk:batch* *mk:last-groups*)
    (progn (prompt "\n  Группы: выборка пакета МАРКАВ") *mk:last-groups*)
    (progn
      (setq res (mk:prepare-run))
      (if res (car res) nil))))

;; Имя группы на чертеже: только буквы/цифры, остальное — «_»
(defun mk:alnum-char? (c / n)
  ;; Только по коду символа: wcmatch с набором "А-Яа-я" внутри класса ненадёжен
  ;; (на кириллице давал совпадение и на буквы, и на цифры — имя группы
  ;; превращалось в «___-_»). Коды — CP1251: 168 Ё, 184 ё, 192..255 А..я.
  (setq n (if c (ascii c) nil))
  (and (numberp n)
       (or (and (>= n 48) (<= n 57))     ; 0-9
           (and (>= n 65) (<= n 90))     ; A-Z
           (and (>= n 97) (<= n 122))    ; a-z
           (= n 45) (= n 95)             ; - _
           (= n 168) (= n 184)           ; Ё ё
           (and (>= n 192) (<= n 255))))) ; А..я

;; Имя группы на чертеже: буквы/цифры/«_», остальное — «_».
;; Если после чистки не осталось ни одной буквы или цифры (нестандартная
;; кодировка, необычное имя), берём исходную строку и меняем только символы,
;; недопустимые в имени группы.
(defun mk:slug (s / i c out solid)
  (setq s (if (mk:strp s) s "") out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq out (strcat out (if (mk:alnum-char? c) c "_")))
    (setq i (1+ i)))
  (setq solid nil i 1)
  (while (<= i (strlen out))
    (setq c (substr out i 1))
    (if (/= c "_") (setq solid t))
    (setq i (1+ i)))
  (if (null solid)
    (progn
      (setq out "" i 1)
      (while (<= i (strlen s))
        (setq c (substr s i 1))
        (setq out (strcat out (if (wcmatch c "[\\/:*?\"<>|,; ]") "_" c)))
        (setq i (1+ i)))))
  (if (> (strlen out) 0) out "ВИТРАЖ"))

;; Базовое имя группы сетки — не меняется в течение прогона
(setq *mk:group-model-base* "Сетка_витража")

;; Имя группы сетки/модели текущего витража
;; (считается от базы: если брать текущее *mk:group-model*, имя накапливало бы
;;  суффиксы предыдущих витражей — Сетка_витража_ВН-1_ВН-2_…)
(defun mk:group-model-name (tag)
  (strcat *mk:group-model-base* "_" (mk:slug tag)))

;; Запись «Атрибуты витража» из рамки (для дампа)
(defun mk:vitrage-rec-from-frame (grp / e)
  (setq e (car (nth 1 grp)))
  (if e (mk:vitrage-rec e) nil))

;; HANDLE записи: у объектов конвейера он берётся из ENAME (в записях
;; mk:collect-post/beam ключа HANDLE нет — из-за этого дамп падал с
;; «неверный тип аргумента: stringp nil» в strcat).
(defun mk:rec-handle (rec / e)
  (setq e (if (mk:strp (mk:rec-get rec 'HANDLE))
            (mk:rec-get rec 'HANDLE)
            (if (mk:rec-get rec 'ENAME)
              (cdr (assoc 5 (entget (mk:rec-get rec 'ENAME))))
              nil)))
  (if (mk:strp e) e "—"))

;;;--- Дамп по витражам ----------------------------------------------

(defun mk:dump-groups (/ f fn grp data posts beams fills windows doors bounds)
  (setq fn (strcat (vl-filename-directory *mk:out-file*)
                   "/MARKZV_MODEL.txt"))
  (setq f (open fn "w"))
  (if (null f)
    (prompt (strcat "\n  [WARN] Дамп не создан: " fn))
    (progn
      (write-line "=== ДАННЫЕ ВИТРАЖЕЙ (MarkV, многовитражный прогон) ===" f)
      (write-line (strcat "  " *mk:ver* " / " *mk:build*) f)
      (write-line (strcat "  Область: " (if *mk:scope-source* *mk:scope-source* "?")) f)
      (write-line (strcat "  Рамок: " (itoa (length *mk:last-frames*))) f)
      (write-line "" f)
      (foreach grp *mk:last-groups*
        (setq data (mk:group-data grp)
              posts (cdr (assoc 'POSTS data)) beams (cdr (assoc 'BEAMS data))
              fills (cdr (assoc 'FILLS data)) windows (cdr (assoc 'WINDOWS data))
              doors (cdr (assoc 'DOORS data)))
        (setq bounds (mk:find-vitrage-bounds posts beams))
        (write-line (strcat "=== ВИТРАЖ " (nth 0 grp) " ===") f)
        (write-line (strcat "  Границы: X=" (mk:safe-num (cdr (assoc 'MIN_X bounds)))
                            ".." (mk:safe-num (cdr (assoc 'MAX_X bounds)))
                            "  Y=" (mk:safe-num (cdr (assoc 'MIN_Y bounds)))
                            ".." (mk:safe-num (cdr (assoc 'MAX_Y bounds)))) f)
        (write-line (strcat "  Стойки: " (itoa (length posts))
                            "  Ригели: " (itoa (length beams))
                            "  Заполнения: " (itoa (length fills))
                            "  Окна: " (itoa (length windows))
                            "  Двери: " (itoa (length doors))) f)
        (foreach post posts
          (write-line (strcat "  СТОЙКА HANDLE=" (mk:rec-handle post)
                              " X=" (mk:safe-num (car (cdr (assoc 'INS_PT post))))
                              " Y=" (mk:safe-num (cadr (cdr (assoc 'INS_PT post))))
                              " L=" (mk:safe-num (mk:rec-get post 'LENGTH))
                              " АРТ=" (if (mk:rec-get post 'ARTICLE)
                                        (mk:rec-get post 'ARTICLE) "—")) f))
        (foreach beam beams
          (write-line (strcat "  РИГЕЛЬ HANDLE=" (mk:rec-handle beam)
                              " X=" (mk:safe-num (car (cdr (assoc 'INS_PT beam))))
                              " Y=" (mk:safe-num (cadr (cdr (assoc 'INS_PT beam))))
                              " L=" (mk:safe-num (mk:rec-get beam 'LENGTH))
                              " АРТ=" (if (mk:rec-get beam 'ARTICLE)
                                        (mk:rec-get beam 'ARTICLE) "—")) f))
        (foreach pn (append fills windows doors)
          (write-line (strcat "  " (mk:safe-str (mk:rec-get pn 'TYPE))
                              " HANDLE=" (mk:rec-handle pn)
                              " X=" (mk:safe-num (car (cdr (assoc 'INS_PT pn))))
                              " Y=" (mk:safe-num (cadr (cdr (assoc 'INS_PT pn))))
                              " W=" (mk:safe-num (mk:rec-get pn 'WIDTH))
                              " H=" (mk:safe-num (mk:rec-get pn 'HEIGHT))) f))
        (write-line "" f))
      (write-line "--- НЕ РАСПОЗНАННЫЕ ВСТАВКИ (имя блока x количество) ---" f)
      (foreach pr (mk:unknown-table)
        (write-line (strcat "  " (car pr) " x" (itoa (cdr pr))) f))
      (write-line "" f)
      (write-line "--- ВНЕ РАМОК ---" f)
      (foreach r *mk:last-extra*
        (write-line (strcat "  " (mk:safe-str (cdr (assoc 'TYPE r)))
                            " HANDLE=" (mk:safe-str (cdr (assoc 'HANDLE r)))) f))
      (close f)
      (prompt (strcat "\n  [OK] Дамп по витражам: " fn)))))

;;;--- ЭТАП 1/4. МАРКАВГЕОМЕТРИЯ ------------------------------------

;; Один витраж: сбор -> проверки -> топология -> разметка -> 2D модель.
;; Возвращает (DATA POSTS BEAMS FILLS WINDOWS DOORS BOUNDS DUP OVERLAP OUTJ).
;; Вынесено отдельной функцией, чтобы сбой одного витража не прерывал прогон
;; (МАРКАВГЕОМЕТРИЯ вызывает её через vl-catch-all-apply).
(defun mk:process-group (grp / data posts beams fills windows doors bounds
                             all-panels dup overlap outj topo res2)
  (setq data (mk:group-data grp))
  (setq posts   (cdr (assoc 'POSTS data))
        beams   (cdr (assoc 'BEAMS data))
        fills   (cdr (assoc 'FILLS data))
        windows (cdr (assoc 'WINDOWS data))
        doors   (cdr (assoc 'DOORS data)))
  (prompt (strcat "\n  Стойки: " (itoa (length posts))
                  ", ригели: " (itoa (length beams))
                  ", заполнения: " (itoa (length fills))
                  ", окна: " (itoa (length windows))
                  ", двери: " (itoa (length doors))))
  (if (or posts beams fills)
    (progn
      (setq bounds (mk:find-vitrage-bounds posts beams))
      (prompt (strcat "\n  Границы: X=" (mk:safe-num (cdr (assoc 'MIN_X bounds)))
                      ".." (mk:safe-num (cdr (assoc 'MAX_X bounds)))
                      ", Y=" (mk:safe-num (cdr (assoc 'MIN_Y bounds)))
                      ".." (mk:safe-num (cdr (assoc 'MAX_Y bounds)))))
      ;; проверки — тоже внутри витража, чтобы не ловить соседей
      (setq all-panels (append fills windows doors))
      (setq dup (mk:check-duplicates (append posts beams all-panels)))
      (setq overlap (mk:check-overlaps all-panels))
      (setq outj (mk:check-out-of-grid (append posts beams all-panels) bounds))
      (setq res2 (mk:identify-protruding-posts posts bounds))
      (setq posts (nth 0 res2))
      ;; топология и разметка внутри витража
      (setq topo (mk:build-topology posts beams fills windows doors bounds))
      (setq posts   (nth 0 topo) beams (nth 1 topo) fills (nth 2 topo)
            windows (nth 3 topo) doors (nth 4 topo))
      (setq all-panels (append fills windows doors))
      (setq posts (mk:annotate-posts posts (append all-panels beams)))
      (setq beams (mk:annotate-beams beams all-panels doors bounds posts))
      ;; 2D модель витража — своя группа
      (mk:draw-model posts beams fills windows doors bounds)
      (prompt (strcat "\n  Дубликатов: " (itoa dup)
                      ", перекрытий: " (itoa overlap)
                      ", вне сетки: " (itoa outj))))
    (progn
      (setq bounds (list (cons 'MIN_X nil) (cons 'MAX_X nil)
                         (cons 'MIN_Y nil) (cons 'MAX_Y nil))
            dup 0 overlap 0 outj 0)
      (prompt "\n  [WARN] В витраже нет элементов каркаса и заполнений.")))
  (list data posts beams fills windows doors bounds dup overlap outj))

;; Итог по сбоям витражей за прогон (нет сбоев — молчит)
(defun mk:print-run-errors (/ n e)
  (setq n 0)
  (foreach e *mk:run-errors* (if (cadr e) (setq n (1+ n))))
  (if (> n 0)
    (progn
      (prompt (strcat "\n[WARN] Витражей со сбоем: " (itoa n) " из "
                      (itoa (length *mk:run-errors*))))
      (foreach e *mk:run-errors*
        (if (cadr e)
          (prompt (strcat "\n   " (car e) ": " (cadr e))))))))

(defun c:МАРКАВГЕОМЕТРИЯ (/ res groups grp data posts beams fills windows doors
                            dup overlap outj r err)
  (prompt "\n[МАРКАВГЕОМЕТРИЯ] Сбор данных и 2D модель — по каждому витражу отдельно...")
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)
  (setq *mk:cached-data* nil *mk:geo-summary* nil *mk:run-errors* nil)
  (if (setq res (mk:prepare-run))
    (progn
      (setq groups (car res))
      (foreach grp groups
        (setq *mk:last-prefix* (nth 0 grp))
        (setq *mk:group-model* (mk:group-model-name (nth 0 grp)))
        (setq *mk:scope-mode*  *mk:last-mode*)
        (prompt (strcat "\n\n--- Витраж " (nth 0 grp) " ---"))
        ;; один витраж не должен ронять весь прогон: сбой фиксируется и
        ;; прогон идёт дальше, остальные витражи обрабатываются
        (setq err nil)
        (setq r (vl-catch-all-apply 'mk:process-group (list grp)))
        (if (vl-catch-all-error-p r)
          (progn
            (setq err (vl-catch-all-error-message r))
            (prompt (strcat "\n  [ERROR] Витраж " (nth 0 grp)
                            ": сбой обработки — " err))
            (prompt "\n  [INFO] Прогон продолжается со следующего витража."))
          (progn
            (setq data    (nth 0 r) posts   (nth 1 r) beams  (nth 2 r)
                  fills   (nth 3 r) windows (nth 4 r) doors  (nth 5 r)
                  bounds  (nth 6 r) dup     (nth 7 r) overlap (nth 8 r)
                  outj    (nth 9 r))
            (setq *mk:geo-summary*
              (cons (list (nth 0 grp) (length posts) (length beams)
                          (length fills) (length windows) (length doors)
                          dup overlap outj)
                    *mk:geo-summary*))))
        ;; *mk:in-group* ставится внутри mk:group-data; если сбор упал, флаг
        ;; мог остаться поднятым и следующий витраж потерял бы привязку к рамке
        (setq *mk:in-group* nil)
        (setq *mk:run-errors* (cons (list (nth 0 grp) err) *mk:run-errors*)))
      (setq *mk:in-group* nil)
      (mk:dump-groups)
      (setq *mk:run-errors* (reverse *mk:run-errors*))
      (mk:print-run-errors)
      (prompt (strcat "\n[ГОТОВО] Витражей обработано: "
                      (itoa (length groups))
                      ". Модели: слой «" *mk:layer-model* "», группы «"
                      *mk:group-model-base* "_<витраж>».")))
    (prompt "\n[INFO] Сбор данных не выполнен."))
  (princ))

;;;--- ЭТАП 2/4. МАРКАВСТ --------------------------------------------

(defun c:МАРКАВСТ (/ groups grp plan item res n cnt sk lab doc tag tot data npost)
  (prompt "\n[МАРКАВСТ] Марки стоек по витражам...")
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)
  (if (setq groups (mk:groups-or-prepare))
    (progn
      (setq doc (mk:undo-begin) tot 0 sk 0 lab 0)
      (foreach grp groups
        (setq tag  (nth 0 grp)
              data (mk:group-data grp)
              npost (length (cdr (assoc 'POSTS data)))
              plan (mk:group-plan-posts grp))
        (if (null plan)
          (prompt (strcat "\n  " (mk:pad tag 10) " [INFO] стоек нет"))
          (progn
            (setq *mk:last-prefix* tag cnt 0)
            (prompt (strcat "\n  " (mk:pad tag 10)
                            " стоек: " (itoa npost)
                            ", марок: " (itoa (length plan))))
            (foreach item plan
              (setq res (mk:write-marks (cadr item) (car item)))
              (prompt (strcat "\n    " (mk:pad (car item) 18)
                              " шт.: " (mk:padl (itoa (length (cadr item))) 3)))
              (setq cnt (+ cnt (car res))
                    sk  (+ sk  (cadr res))
                    lab (+ lab (caddr res))))
            (setq tot (+ tot cnt))
            ;; выноски этого витража — в свою группу
            (if (> (mk:group-labels-tag tag) 0)
              (prompt (strcat "\n    выноски: группа «Маркировка_"
                              (mk:slug tag) "»"))))))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Стоек промаркировано: " (itoa tot)
                      ", выносок: " (itoa lab) ", пропущено: " (itoa sk))))
    (prompt "\n[INFO] Группы не собраны — маркировка стоек не выполнена."))
  (princ))

;;;--- ЭТАП 3/4. МАРКАВРГ --------------------------------------------

(defun c:МАРКАВРГ (/ groups grp plan item res n cnt sk lab doc tag tot data nbeam)
  (prompt "\n[МАРКАВРГ] Марки ригелей по витражам...")
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)
  (if (null *mk:batch*) (setq *mk:allowance-unknown* nil))
  (if (setq groups (mk:groups-or-prepare))
    (progn
      (setq doc (mk:undo-begin) tot 0 sk 0 lab 0)
      (foreach grp groups
        (setq tag  (nth 0 grp)
              data (mk:group-data grp)
              nbeam (length (cdr (assoc 'BEAMS data)))
              plan (mk:group-plan-beams grp))
        (if (null plan)
          (prompt (strcat "\n  " (mk:pad tag 10) " [INFO] ригелей нет"))
          (progn
            (setq *mk:last-prefix* tag cnt 0)
            (prompt (strcat "\n  " (mk:pad tag 10)
                            " ригелей: " (itoa nbeam)
                            ", марок: " (itoa (length plan))))
            (foreach item plan
              (setq res (mk:write-marks (cadr item) (car item)))
              (prompt (strcat "\n    " (mk:pad (car item) 18)
                              " шт.: " (mk:padl (itoa (length (cadr item))) 3)))
              (setq cnt (+ cnt (car res))
                    sk  (+ sk  (cadr res))
                    lab (+ lab (caddr res))))
            (setq tot (+ tot cnt))
            (if (> (mk:group-labels-tag tag) 0)
              (prompt (strcat "\n    выноски: группа «Маркировка_"
                              (mk:slug tag) "»"))))))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Ригелей промаркировано: " (itoa tot)
                      ", выносок: " (itoa lab) ", пропущено: " (itoa sk))))
    (prompt "\n[INFO] Группы не собраны — маркировка ригелей не выполнена."))
  (princ))

;;;--- Выноски одного витража -----------------------------------------

(defun mk:group-labels-tag (tag / ss i lst e d txt)
  (setq lst nil)
  (if (and (mk:strp tag) (> (strlen tag) 0))
    (progn
      (setq ss (ssget "_X" (list (cons 8 *mk:layer-label*) (cons 0 "TEXT"))) i 0)
      (if ss
        (repeat (sslength ss)
          (setq e (ssname ss i) d (entget e) txt (cdr (assoc 1 d)))
          (if (and (mk:strp txt) (wcmatch txt (strcat tag " *")))
            (setq lst (cons e lst)))
          (setq i (1+ i))))
      (if lst (mk:make-group (strcat "Маркировка_" (mk:slug tag)) (reverse lst)))))
  (if lst (length lst) 0))

;;;--- ЭТАП 4/4. МАРКАВТАБЛ (общая ведомость) -------------------------

(defun c:МАРКАВТАБЛ (/ groups grp data rows agg acc all-posts all-beams
                      doc do-tbl do-xls file res pt2 pt3 h h2 tbl2 suffix)
  (prompt "\n[МАРКАВТАБЛ] Ведомость профилей — общая по всем витражам...")
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)
  (if (null *mk:batch*) (setq *mk:allowance-unknown* nil))
  (if (setq groups (mk:groups-or-prepare))
    (progn
      (mk:group-cache-clear)
      (setq all-posts nil all-beams nil)
      (foreach grp groups
        (setq data (mk:group-data grp))
        (setq all-posts (append all-posts (cdr (assoc 'POSTS data))))
        (setq all-beams (append all-beams (cdr (assoc 'BEAMS data)))))
      (mk:ask-unknown-allowance all-beams)
      (setq rows (mk:tab-rows all-posts all-beams)
            agg  (mk:tab-aggregate rows))
      (prompt (strcat "\n  Стойки: " (itoa (length all-posts))
                      ", ригели: " (itoa (length all-beams))
                      ", витражей: " (itoa (length groups))))
      (prompt (strcat "\n  Уникальных позиций: " (itoa (length agg))))
      (setq acc (mk:acc-rows (append all-posts all-beams)))
      (if acc
        (prompt (strcat "\n  Закладных (позиций): " (itoa (length acc))))
        (prompt "\n  Закладные (Пятка/Кронштейн) не найдены."))
      (setq *mk:leg-sfx*  (mk:leg-suffixes agg)
            *mk:leg-arts* (mk:leg-articles all-beams))
      (mk:leg-print *mk:leg-sfx* *mk:leg-arts*)
      (initget "Да Нет")
      (setq do-tbl (getkword "\nТаблица в чертеже? [Да/Нет] <Да>: "))
      (setq do-tbl (if (= do-tbl "Нет") nil t))
      (initget "Да Нет")
      (setq do-xls (getkword "\nЭкспорт в XLS? [Да/Нет] <Да>: "))
      (setq do-xls (if (= do-xls "Нет") nil t))
      (setq suffix (if (= (length groups) 1)
                     (nth 0 (car groups))
                     (strcat (itoa (length groups)) " витражей")))
      (setq doc (mk:undo-begin))
      (if do-xls
        (progn
          (setq file (strcat (getvar "dwgprefix")
                             (vl-filename-base (getvar "dwgname"))
                             " Стойки ригели витража "
                             (mk:slug suffix) ".xls"))
          (mk:tab-xls agg file acc)))
      (if do-tbl
        (progn
          (setq res (mk:tab-create agg))
          (if res
            (progn
              (setq h (vl-catch-all-apply 'vlax-get-property (list (car res) 'Height)))
              (if (vl-catch-all-error-p h) (setq h (* 12.0 (+ 3 (length agg)))))
              (setq pt2 (list (car (cadr res))
                              (- (cadr (cadr res)) h *mk:acc-gap*)
                              0.0))
              (setq tbl2 (if acc (mk:acc-table-create acc pt2) nil))
              (setq pt3 pt2)
              (if tbl2
                (progn
                  (setq h2 (vl-catch-all-apply 'vlax-get-property (list tbl2 'Height)))
                  (if (vl-catch-all-error-p h2)
                    (setq h2 (* 12.0 (+ 3 (length acc)))))
                  (setq pt3 (list (car pt2) (- (cadr pt2) h2 *mk:acc-gap*) 0.0))))
              (mk:leg-table-create *mk:leg-sfx* *mk:leg-arts* pt3)))))
      (mk:undo-end doc)
      (prompt "\n[ГОТОВО] Ведомость сформирована (общая по всем витражам)."))
    (prompt "\n[INFO] Данных нет — ведомость не создана."))
  (princ))

;;;--- ПАКЕТ. МАРКАВ --------------------------------------------------
;;   Одна выборка -> по каждому витражу: сбор и модель, стойки, ригели ->
;;   удаление сеток -> одна общая ведомость. Один UNDO на весь пакет.

(defun c:МАРКАВ (/ ss keep doc n grp)
  (prompt (strcat "\n[МАРКАВ] Пакетный прогон, " *mk:ver* " / " *mk:build*))
  (prompt "\n  Порядок: выборка -> витражи -> (сбор, стойки, ригели) по каждому -> ведомость.")
  (setq *mk:batch* nil *mk:batch-ss* nil *mk:batch-mode* nil
        *mk:last-groups* nil *mk:last-extra* nil *mk:last-frames* nil)
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil *mk:no-vitrage-prompted* nil
        *mk:manual-vitrage-prefix* nil *mk:allowance-unknown* nil)
  (setq ss (mk:select-scope))
  (if (null ss)
    (prompt "\n[INFO] Ничего не выбрано — пакет прерван.")
    (progn
      (setq *mk:run-scope* ss)
      ;; режим каркаса и судьбу сетки спрашиваем один раз
      (initget "Блоки Мультилинии Все-типы B M A")
      (setq *mk:batch-mode*
        (getkword "\nРежим сбора каркаса [Блоки/Мультилинии/Все-типы] <Все-типы>: "))
      (if (null *mk:batch-mode*) (setq *mk:batch-mode* "Все-типы"))
      (setq *mk:batch-mode* (mk:norm-mode *mk:batch-mode*)
            *mk:last-mode*  *mk:batch-mode*)
      (initget "Да Нет")
      (setq keep (getkword "\nОставить сетки витражей в чертеже? [Да/Нет] <Нет>: "))
      (setq *mk:batch-keep-grid* (= keep "Да"))
      ;; единая транзакция отката
      (setq doc (mk:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "StartUndoMark")))
      (setq *mk:batch* t)
      (prompt "\n\n===== [ЭТАП 1/4] МАРКАВГЕОМЕТРИЯ =====")
      (c:МАРКАВГЕОМЕТРИЯ)
      (prompt "\n\n===== [ЭТАП 2/4] МАРКАВСТ =====")
      (c:МАРКАВСТ)
      (prompt "\n\n===== [ЭТАП 3/4] МАРКАВРГ =====")
      (c:МАРКАВРГ)
      (if *mk:batch-keep-grid*
        (prompt (strcat "\n\n  [INFO] Сетки витражей оставлены на слое «"
                        *mk:layer-model* "» (группы «" *mk:group-model-base*
                        "_<витраж>»)."))
        (progn
          (prompt "\n\n  Удаление сеток витражей перед вставкой таблицы...")
          (foreach grp *mk:last-groups*
            (mk:erase-model *mk:layer-model*
                            (mk:group-model-name (nth 0 grp))))
          (mk:erase-model *mk:layer-test* *mk:group-test*)))
      (prompt "\n\n===== [ЭТАП 4/4] МАРКАВТАБЛ =====")
      (c:МАРКАВТАБЛ)
      (setq *mk:batch* nil *mk:batch-ss* nil *mk:run-scope* nil)
      (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "EndUndoMark")))
      (setq n (length *mk:last-groups*))
      (prompt (strcat "\n\n[ГОТОВО] Пакет МАРКАВ завершён: витражей "
                      (itoa n) ", объектов вне рамок "
                      (itoa (length *mk:last-extra*)) "."))
      (prompt "\n  Откат всего пакета — один U.")))
  (setq *mk:batch* nil)
  (princ))
;;;=====================================================================
;;; 20b. ВНЕШНИЕ БАЗЫ ПРОФИЛЬНЫХ СИСТЕМ
;;;=====================================================================
;; Подхват файла MarkV-bases.lsp (рядом с модулем или в путях поиска AutoCAD)
(defun mk:load-bases (/ f res)
  (setq f (findfile *mk:bases-file*))
  (if f
    (progn
      (setq res (vl-catch-all-apply 'load (list f)))
      (if (vl-catch-all-error-p res)
        (prompt (strcat "\n  [WARN] Файл баз не загружен: " f))
        (prompt (strcat "\n  [OK] Базы профилей: " f)))))
  f)

(defun c:МАРКАВБАЗЫ (/ n)
  (prompt "\n[МАРКАВБАЗЫ] Зарегистрированные базы профилей:")
  (if (null *mk:size-bases*)
    (prompt "\n  (пусто)")
    (foreach b *mk:size-bases*
      (prompt (strcat "\n  " (car b) " — записей: " (itoa (length (cdr b)))))))
  (prompt (strcat "\n  Префиксы артикулов: "
                  (mk:join *mk:article-prefixes* ", ")))
  (if *mk:art-guess-log*
    (progn
      (prompt "\n  Артикулы, подобранные по похожести имени:")
      (foreach r (reverse *mk:art-guess-log*) (prompt (strcat "\n    " r)))))
  (prompt (strcat "\n  Файл внешних баз: " *mk:bases-file*
                  (if (findfile *mk:bases-file*) " (найден)" " (не найден)")))
  (princ))

;;;=====================================================================
;;; 20c. MarkZ/MarkV scale diagnostic
(defun mk:panel-width-scale (panels / vals p w out)
  (setq vals nil out nil)
  (foreach p panels
    (setq w (mk:rec-get p 'WIDTH))
    (if (and (numberp w) (> w 0.0)) (setq vals (cons w vals))))
  (foreach w (vl-sort vals '<)
    (if (or (null out)
            (not (equal w (car out) *mk:tol-size*)))
      (setq out (cons w out))))
  (reverse out))

(defun mk:panel-pair-evidence (panels beams wa wb / pa pb ia ib xa ya xb yb ha hb hit br bp bx by bl)
  (setq hit nil)
  (foreach pa panels
    (if (and (numberp (mk:rec-get pa 'WIDTH))
             (equal (mk:rec-get pa 'WIDTH) wa *mk:tol-size*))
      (foreach pb panels
        (if (and (numberp (mk:rec-get pb 'WIDTH))
                 (equal (mk:rec-get pb 'WIDTH) wb *mk:tol-size*))
          (progn
            (setq ia (cdr (assoc 'INS_PT pa)) ib (cdr (assoc 'INS_PT pb))
                  xa (if ia (car ia) nil) ya (if ia (cadr ia) nil)
                  xb (if ib (car ib) nil) yb (if ib (cadr ib) nil)
                  ha (mk:rec-get pa 'HEIGHT) hb (mk:rec-get pb 'HEIGHT))
            (if (and xa ya xb yb (numberp ha) (numberp hb)
                     (vl-some '(lambda (br / bp)
                       (setq bp (cdr (assoc 'INS_PT br)))
                       (and bp (numberp (mk:rec-get br 'LENGTH))
                            (equal (mk:rec-get br 'LENGTH) (+ wa wb 50.0) *mk:tol-size*)
                            (or (<= (abs (- (car bp) xa)) 2.0)
                                (<= (abs (- (car bp) xb)) 2.0)))) beams)
                     (or
                       (or (<= (abs (- (+ xa wa 50.0) xb)) 2.0)
                            (<= (abs (- (+ xb wb 50.0) xa)) 2.0))
                       (or (<= (abs (- (+ xa (/ wa 2.0) 50.0)
                                           (- xb (/ wb 2.0)))) 2.0)
                            (<= (abs (- (+ xb (/ wb 2.0) 50.0)
                                           (- xa (/ wa 2.0)))) 2.0)))
                     (> (min (+ ya ha) (+ yb hb)) (max ya yb))
                     (or (<= (abs (- ya yb)) 2.0)
                         (<= (abs (- (+ ya ha) (+ yb hb))) 2.0)))
              (setq hit t)))))))
  hit)

(defun mk:subcell-pairs (scale panels beams / out a rest c)
  (setq out nil)
  (foreach a scale
    (foreach c scale
      (if (and (< a c)
               (vl-some '(lambda (x) (= x (+ a c 50.0))) scale)
               (mk:panel-pair-evidence panels beams a c))
        (setq out (cons (list a c (+ a c 50.0)) out)))) )
  (reverse out))

(defun c:MARKVGEOMDIAG (/ ss data panels p pt w h)
  (prompt "\n[MARKVGEOMDIAG] Panel geometry diagnostic.")
  (initget "M B A")
  (setq *mk:scope-mode* (mk:norm-mode
                           (getkword "\nMode [M=MLINE/B=BLOCK/A=ALL] <A>: ")))
  (if (null *mk:scope-mode*) (setq *mk:scope-mode* (mk:norm-mode "A")))
  (setq ss (ssget))
  (if ss
    (progn
      (setq data (mk:collect-scope ss)
            panels (cdr (assoc 'PANELS data)))
      (prompt (strcat "\n  Panels: " (itoa (length panels))))
      (foreach p panels
        (setq pt (cdr (assoc 'INS_PT p))
              w (mk:rec-get p 'WIDTH)
              h (mk:rec-get p 'HEIGHT))
        (prompt (strcat "\n  " (if pt (strcat "X=" (rtos (car pt) 2 1)
                                  " Y=" (rtos (cadr pt) 2 1)) "X/Y=nil")
                        " W=" (rtos (if (numberp w) w 0.0) 2 1)
                        " H=" (rtos (if (numberp h) h 0.0) 2 1))))
      (prompt "\n[OK] Panel geometry diagnostic complete."))
    (prompt "\n[INFO] Selection cancelled."))
  (princ))

(defun c:MARKVSUBCELLS (/ ss data beams panels scale pairs v idx)
  (prompt "\n[MARKVSUBCELLS] Subcell index assignment.")
  (setq *mk:scope-mode* (mk:norm-mode "A"))
  (setq ss (ssget))
  (if ss
    (progn
      (setq data (mk:collect-scope ss)
            beams (cdr (assoc 'BEAMS data))
            panels (cdr (assoc 'PANELS data))
            scale (mk:length-scale beams panels)
            pairs (mk:subcell-pairs (mk:panel-width-scale panels) panels beams))
      (if pairs
        (foreach v pairs
          (setq idx (mk:scale-index (nth 2 v) scale))
          (prompt (strcat "\n  " (itoa idx) ".1=" (rtos (nth 0 v) 2 1)
                          ", " (itoa idx) ".2=" (rtos (nth 1 v) 2 1)
                          " [base=" (rtos (nth 2 v) 2 1) "]")))
        (prompt "\n  [INFO] Subcell pairs not found."))
      (prompt "\n[OK] Subcell assignment complete."))
    (prompt "\n[INFO] Selection cancelled."))
  (princ))

(defun c:MARKVFRAME (/ ss data posts beams r pt n)
  (prompt "\n[MARKVFRAME] Frame geometry diagnostic.")
  (initget "M B A")
  (setq *mk:scope-mode* (mk:norm-mode
                           (getkword "\nMode [M=MLINE/B=BLOCK/A=ALL] <A>: ")))
  (if (null *mk:scope-mode*) (setq *mk:scope-mode* (mk:norm-mode "A")))
  (setq ss (ssget))
  (if ss
    (progn
      (setq data (mk:collect-scope ss)
            posts (cdr (assoc 'POSTS data))
            beams (cdr (assoc 'BEAMS data)))
      (prompt (strcat "\n  Posts: " (itoa (length posts))))
      (foreach r posts
        (setq pt (cdr (assoc 'INS_PT r)))
        (prompt (strcat "\n  POST X=" (rtos (if pt (car pt) 0.0) 2 1)
                        " Y=" (rtos (if pt (cadr pt) 0.0) 2 1)
                        " L=" (rtos (if (numberp (mk:rec-get r 'LENGTH))
                                         (mk:rec-get r 'LENGTH) 0.0) 2 1))) )
      (prompt (strcat "\n  Beams: " (itoa (length beams))))
      (foreach r beams
        (setq pt (cdr (assoc 'INS_PT r)))
        (prompt (strcat "\n  BEAM X=" (rtos (if pt (car pt) 0.0) 2 1)
                        " Y=" (rtos (if pt (cadr pt) 0.0) 2 1)
                        " L=" (rtos (if (numberp (mk:rec-get r 'LENGTH))
                                         (mk:rec-get r 'LENGTH) 0.0) 2 1)
                        " H=" (rtos (if (numberp (mk:rec-get r 'CROSS))
                                         (mk:rec-get r 'CROSS) 0.0) 2 1))))
      (prompt "\n[OK] Frame geometry diagnostic complete."))
    (prompt "\n[INFO] Selection cancelled."))
  (princ))

(defun c:MARKVSYNC (/ ss data beams panels scale pscale pairs v idx mode-kw)
  (prompt "\n[MARKVSYNC] Scale diagnostic.")
  (initget "M B A")
  (setq mode-kw (getkword "\nMode [M=MLINE/B=BLOCK/A=ALL] <A>: "))
  (setq *mk:scope-mode* (mk:norm-mode (if mode-kw mode-kw "A")))
  (setq ss (ssget))
  (if ss
    (progn
      (setq data (mk:collect-scope ss)
            beams (cdr (assoc 'BEAMS data))
            panels (cdr (assoc 'PANELS data))
            scale (mk:length-scale beams panels)
            pscale (mk:panel-width-scale panels)
            pairs (mk:subcell-pairs pscale panels beams))
      (prompt (strcat "\n  Beam scale count: " (itoa (length scale))))
      (prompt (strcat "\n  Panel width scale count: " (itoa (length pscale))))
      (foreach v pscale (prompt (strcat "\n    panel = " (rtos v 2 1))))
      (foreach v pairs
        (prompt (strcat "\n    candidate subcells: " (rtos (nth 0 v) 2 1)
                        " + " (rtos (nth 1 v) 2 1)
                        " + 50 = " (rtos (nth 2 v) 2 1))))
      (foreach v pairs
        (prompt (strcat "\n    proposed: " (rtos (nth 2 v) 2 1)
                        ".1=" (rtos (nth 0 v) 2 1)
                        ", .2=" (rtos (nth 1 v) 2 1))))
      (foreach v pairs
        (prompt (strcat "\n    index assignment: "
                        (itoa (mk:scale-index (nth 2 v) scale))
                        ".1=" (rtos (nth 0 v) 2 1)
                        ", .2=" (rtos (nth 1 v) 2 1))))
      (setq idx 1)
      (foreach v scale
        (prompt (strcat "\n    " (itoa idx) " = " (rtos v 2 1)))
        (setq idx (1+ idx)))
      (prompt "\n[OK] MarkV scale diagnostic complete."))
    (prompt "\n[INFO] Selection cancelled."))
  (princ))

(defun mk:diag-bulk-line (f text)
  (if f (write-line text f)))

(defun mk:diag-inc (key alist / hit)
  (if (null key) alist
    (if (setq hit (assoc key alist))
      (subst (cons key (1+ (cdr hit))) hit alist)
      (cons (cons key 1) alist))))

(defun mk:diag-vltext (v / z)
  ;; VARIANT может быть и вложенным — раскрываем до двух уровней
  (while (and (= (type v) 'VARIANT)
              (not (vl-catch-all-error-p
                     (setq z (vl-catch-all-apply 'vlax-variant-value (list v))))))
    (setq v z))
  (if (= (type v) 'SAFEARRAY)
    (setq v (vl-catch-all-apply 'vlax-safearray->list (list v))))
  (vl-princ-to-string v))

(defun mk:diag-dynamic (f e / o n ps pr pn pv)
  (if (= (type e) 'ENAME)
    (progn
      (setq o (vlax-ename->vla-object e))
      (if (= :vlax-true (vla-get-IsDynamicBlock o))
        (progn
          (setq n (vla-get-EffectiveName o))
          (mk:diag-bulk-line f (strcat "  EffectiveName: " n))
          (setq ps (vlax-invoke o 'GetDynamicBlockProperties))
          (foreach pr ps
            (setq pn (vla-get-PropertyName pr)
                  pv (vla-get-Value pr))
            (mk:diag-bulk-line f (strcat "  Dynamic " pn " = " (mk:diag-vltext pv)))))))))

(defun mk:diag-bulk-object (f e d idx / q qd tag val p)
  (mk:diag-bulk-line f (strcat "Object #" (itoa idx) " HANDLE=" (if (cdr (assoc 5 d)) (cdr (assoc 5 d)) "<none>")))
  (mk:diag-bulk-line f (strcat "  Type: " (if (cdr (assoc 0 d)) (cdr (assoc 0 d)) "<none>")))
  (mk:diag-bulk-line f (strcat "  Layer: " (if (cdr (assoc 8 d)) (cdr (assoc 8 d)) "<none>")))
  (if (assoc 2 d) (mk:diag-bulk-line f (strcat "  Name: " (cdr (assoc 2 d)))))
  (if (= (cdr (assoc 0 d)) "INSERT") (mk:diag-dynamic f e))
  (if (assoc 10 d)
    (progn
      (setq p (cdr (assoc 10 d)))
      (mk:diag-bulk-line f (strcat "  Insert: " (rtos (car p) 2 3) ", " (rtos (cadr p) 2 3) ", " (rtos (if (caddr p) (caddr p) 0.0) 2 3)))))
  (if (assoc 50 d) (mk:diag-bulk-line f (strcat "  Rotation: " (rtos (cdr (assoc 50 d)) 2 4))))
  (if (assoc 41 d) (mk:diag-bulk-line f (strcat "  ScaleX: " (rtos (cdr (assoc 41 d)) 2 4))))
  (if (= (cdr (assoc 0 d)) "INSERT")
    (progn
      (setq q (if (= (cdr (assoc 66 d)) 1) (entnext e)))
      (while q
        (setq qd (entget q))
        (if (= (cdr (assoc 0 qd)) "SEQEND")
          (setq q nil)
          (progn
            (if (= (cdr (assoc 0 qd)) "ATTRIB")
              (progn
                (setq tag (cdr (assoc 2 qd)) val (cdr (assoc 1 qd)))
                (mk:diag-bulk-line f (strcat "  Attribute " (if tag tag "<no tag>") " = " (if val val "<empty>")))))
            (setq q (entnext q))))))))

(defun mk:diag-unique-add (v vals / hit)
  (setq hit nil)
  (foreach x vals
    (if (< (abs (- v x)) 0.01) (setq hit T)))
  (if hit vals (cons v vals)))

(defun mk:diag-layout (f ss / i e d o n p typ xs ys x y sx sy)
  (mk:diag-bulk-line f "")
  (mk:diag-bulk-line f "=== РАСКЛАДКА ВИТРАЖЕЙ ПО КООРДИНАТАМ ===")
  (setq i 0 xs nil ys nil)
  (while (< i (sslength ss))
    (progn
      (setq e (ssname ss i) d (entget e) typ (cdr (assoc 0 d)))
      (if (= typ "INSERT")
        (progn
          (setq p (cdr (assoc 10 d)) o (vlax-ename->vla-object e))
          (if p
            (progn
              (setq n (if (= :vlax-true (vla-get-IsDynamicBlock o)) (vla-get-EffectiveName o) ""))
              (if (= n "Стойка (вертикальный профиль)") (setq xs (mk:diag-unique-add (car p) xs)))
              (if (= n "Ригель (горизонтальный профиль)") (setq ys (mk:diag-unique-add (cadr p) ys)))))))
      (setq i (1+ i))))
  (setq sx (vl-sort xs '<) sy (vl-sort ys '<))
  (mk:diag-bulk-line f (strcat "Вертикальные оси стоек: " (itoa (length sx))))
  (setq i 1)
  (foreach x sx
    (mk:diag-bulk-line f (strcat "  S" (if (< i 10) "0" "") (itoa i) "  X=" (rtos x 2 2)))
    (setq i (1+ i)))
  (mk:diag-bulk-line f (strcat "Горизонтальные ряды ригелей: " (itoa (length sy))))
  (setq i 1)
  (foreach y sy
    (mk:diag-bulk-line f (strcat "  R" (if (< i 10) "0" "") (itoa i) "  Y=" (rtos y 2 2)))
    (setq i (1+ i)))
  (mk:diag-grid f ss sx sy)
  (mk:diag-bulk-line f "Примечание: ячейки строятся между соседними уникальными осями."))

(defun mk:diag-attr-value (e wanted / q qd out)
  (setq q (entnext e) out nil)
  (while (and q (null out))
    (setq qd (entget q))
    (if (= (cdr (assoc 0 qd)) "SEQEND") (setq q nil)
      (progn
        (if (and (= (cdr (assoc 0 qd)) "ATTRIB") (= (strcase (cdr (assoc 2 qd))) (strcase wanted)))
          (setq out (cdr (assoc 1 qd))))
        (if q (setq q (entnext q))))))
  out)

(defun mk:diag-dynamic-value (e wanted / o ps pr out)
  (setq o (vlax-ename->vla-object e) out nil)
  (if (= :vlax-true (vla-get-IsDynamicBlock o))
    (progn
      (setq ps (vlax-invoke o 'GetDynamicBlockProperties))
      (foreach pr ps
        (if (= (strcase (vla-get-PropertyName pr)) (strcase wanted))
          (setq out (mk:diag-vltext (vla-get-Value pr)))))))
  out)

(defun mk:diag-grid-cell (f ss x1 x2 y1 y2 / door kind article state fill d p o n i e mn mx q1 q2 ax1 ax2 ay1 ay2)
  (setq door nil kind "нет" article "не определён" state "не определено" fill "не определено")
  (setq i 0)
  (repeat (sslength ss)
    (progn
      (setq e (ssname ss i) d (entget e) p (cdr (assoc 10 d)))
      (if (= (cdr (assoc 0 d)) "INSERT")
        (progn
          (setq o (vlax-ename->vla-object e))
          (vla-GetBoundingBox o 'mn 'mx)
          (setq q1 (vlax-safearray->list (if (= (type mn) 'VARIANT) (vlax-variant-value mn) mn))
                q2 (vlax-safearray->list (if (= (type mx) 'VARIANT) (vlax-variant-value mx) mx))
                ax1 (car q1) ax2 (car q2) ay1 (cadr q1) ay2 (cadr q2))
          (if (and (< ax1 x2) (> ax2 x1) (< ay1 y2) (> ay2 y1))
            (progn
              (if (= :vlax-true (vla-get-IsDynamicBlock o))
            (setq n (vla-get-EffectiveName o))
            (setq n (cdr (assoc 2 d))))
          (if (wcmatch n "*Двер*") (setq door n kind "Дверь"))
          (if (wcmatch n "*Створ*") (setq door n kind "Створка"))
          (if (wcmatch n "*Окн*") (setq door n kind "Оконный блок"))
          (if door (setq fill (strcase kind)))
          (if (wcmatch n "*Ригель*")
            (progn
              (setq article (mk:diag-attr-value e "Артикул")
                    state (mk:diag-dynamic-value e "Видимость1"))
              (if (null article) (setq article "не определён"))
              (if (null state) (setq state "не определено"))
              (if (wcmatch state "*с заливкой*") (setq fill "заполнение с заливкой"))
              (if (wcmatch state "*без заливки*") (setq fill "заполнение без заливки"))))))))
      (setq i (1+ i))))
  (mk:diag-bulk-line f (strcat "  ВН-" (if (< x1 10000.0) "4" "6")
    " Ячейка X=" (rtos x1 2 2) ".." (rtos x2 2 2)
    " Y=" (rtos y1 2 2) ".." (rtos y2 2 2)
    " Ширина=" (rtos (- x2 x1) 2 2)
    " Высота=" (rtos (- y2 y1) 2 2)
    " Элемент=" (if door (strcat kind ": " door) "нет")
    " Заполнение=" fill " Артикул=" article " Состояние=" state)))

(defun mk:diag-grid-row (f ss x1 x2 ys)
  (if (cdr ys)
    (progn
      (mk:diag-grid-cell f ss x1 x2 (car ys) (cadr ys))
      (mk:diag-grid-row f ss x1 x2 (cdr ys)))))

(defun mk:diag-grid-cols (f ss xs sy)
  (if (cdr xs)
    (progn
      (if (< (- (cadr xs) (car xs)) 5000.0)
        (mk:diag-grid-row f ss (car xs) (cadr xs) sy))
      (mk:diag-grid-cols f ss (cdr xs) sy))))

(defun mk:diag-grid (f ss sx sy)
  (mk:diag-bulk-line f "")
  (mk:diag-bulk-line f "=== СЕТКА ЯЧЕЕК ВИТРАЖЕЙ ===")
  (mk:diag-grid-cols f ss sx sy)
  (mk:diag-bulk-line f "Сетка построена по соседним уникальным осям X и уровням Y."))

(defun mk:frame-point (p dx dy ang / c z)
  (setq c (cos ang) z (sin ang))
  (list (+ (car p) (- (* dx c) (* dy z)))
        (+ (cadr p) (+ (* dx z) (* dy c)))))

(defun mk:frame-capture-bb (e / d p o w h vf hf sx sy ang pts q xlo xhi ylo yhi)
  (setq d (entget e) p (cdr (assoc 10 d)) o (vlax-ename->vla-object e)
        w (mk:get-dyn o "*Ширина рамки*")
        h (mk:get-dyn o "*Высота рамки*")
        vf (mk:get-dyn o "*Вертикаль*")
        hf (mk:get-dyn o "*Горизонталь*")
        sx (if (cdr (assoc 41 d)) (cdr (assoc 41 d)) 1.0)
        sy (if (cdr (assoc 42 d)) (cdr (assoc 42 d)) sx)
        ang (if (cdr (assoc 50 d)) (cdr (assoc 50 d)) 0.0))
  (if (and p (numberp w) (numberp h))
    (progn
      ;; Базовая точка — верхний левый угол. Вертикаль отражает X,
      ;; Горизонталь отражает Y. Поворот применяется после отражения.
      (setq pts
        (list
          (mk:frame-point p 0.0 0.0 ang)
          (mk:frame-point p (if (= vf 1) (- (* w sx)) (* w sx)) 0.0 ang)
          (mk:frame-point p (if (= vf 1) (- (* w sx)) (* w sx))
                          (if (= hf 1) (* h sy) (- (* h sy))) ang)
          (mk:frame-point p 0.0 (if (= hf 1) (* h sy) (- (* h sy))) ang)))
      (setq xlo (apply 'min (mapcar 'car pts))
            xhi (apply 'max (mapcar 'car pts))
            ylo (apply 'min (mapcar 'cadr pts))
            yhi (apply 'max (mapcar 'cadr pts)))
      (list xlo ylo xhi yhi))))

(defun mk:bb-overlap-area (a b / x1 x2 y1 y2)
  (if (and a b)
    (progn
      (setq x1 (max (nth 0 a) (nth 0 b))
            x2 (min (nth 2 a) (nth 2 b))
            y1 (max (nth 1 a) (nth 1 b))
            y2 (min (nth 3 a) (nth 3 b)))
      (if (and (> x2 x1) (> y2 y1)) (* (- x2 x1) (- y2 y1)) 0.0))
    0.0))

(defun mk:frame-list (/ ss i e d o n bb mark out)
  (setq ss (ssget "_X" '((0 . "INSERT"))) i 0 out nil)
  (if ss
    (while (< i (sslength ss))
      (progn
        (setq e (ssname ss i) d (entget e) o (vlax-ename->vla-object e)
              n (mk:ax-get o "EffectiveName") mark (mk:get-attr e "Витраж"))
        (if (and (= (cdr (assoc 0 d)) "INSERT") mark)
          (progn
            (setq bb (mk:frame-capture-bb e))
            (if (null bb) (setq bb (mk:e-bb o)))
            (if bb (setq out (cons (list e bb mark) out))))))
        (setq i (1+ i))))
  out)

(defun c:MARKVFRAMEASSIGN (/ ss frames f i e d o n typ bb fr ar best besta mark)
  (vl-load-com)
  (prompt "\n[MARKVFRAMEASSIGN] Проверка принадлежности объектов рамкам...")
  (setq frames (mk:frame-list)
        ss (ssget)
        ss (if ss ss (ssget "_X" '((0 . "INSERT"))))
        i 0 f (open "D:/MARKV_FRAME_ASSIGN.txt" "w"))
  (if f
    (progn
      (write-line "=== MARKV FRAME ASSIGNMENT ===" f)
      (write-line (strcat "Objects processed: " (if ss (itoa (sslength ss)) "0")) f)
      (if ss
        (repeat (sslength ss)
          (setq e (ssname ss i) d (entget e) o (vlax-ename->vla-object e)
                n (mk:ax-get o "EffectiveName") typ (cdr (assoc 0 d)) best nil besta 0.0)
          (if (and (= typ "INSERT") (/= n "Атрибуты витража"))
            (progn
              (setq bb (mk:e-bb o))
              (foreach fr frames
                (setq ar (mk:bb-overlap-area bb (cadr fr)))
                (if (> ar besta) (setq besta ar best fr)))
              (setq mark (if best (if (caddr best) (caddr best) "<без марки>") "<ВНЕ РАМОК>"))
              (write-line (strcat "HANDLE=" (if (cdr (assoc 5 d)) (cdr (assoc 5 d)) "<none>")
                                  " NAME=" (if n n "<noname>") " FRAME=" (if mark mark "<no-frame>") " AREA=" (rtos besta 2 2)
                                  " BBOX=" (if bb (mk:diag-vltext bb) "<none>")) f)))
          (setq i (1+ i)))
        (write-line "NO INSERTS FOUND" f))
      (close f)
      (prompt "\n[OK] Привязка: D:/MARKV_FRAME_ASSIGN.txt"))
    (prompt "\n[ERROR] Не удалось открыть отчёт."))
  (princ))

(defun c:MARKVFRAMEINFO (/ ss f i e d o n bb cbb pr)
  (vl-load-com)
  (prompt "\n[MARKVFRAMEINFO] Сбор рамок Атрибуты витража...")
  (setq f (open "D:/MARKV_FRAMES.txt" "w") ss (ssget "_X" '((0 . "INSERT"))) i 0)
  (if f
    (progn
      (write-line "=== MARKV FRAME INFO ===" f)
      (if ss
        (repeat (sslength ss)
          (setq e (ssname ss i) d (entget e) o (vlax-ename->vla-object e)
                n (mk:ax-get o "EffectiveName"))
          (if (= n "Атрибуты витража")
            (progn
              (write-line (strcat "FRAME HANDLE=" (if (cdr (assoc 5 d)) (cdr (assoc 5 d)) "<none>")) f)
              (write-line (strcat "  Insert=" (mk:diag-vltext (cdr (assoc 10 d)))) f)
              (write-line (strcat "  ScaleX=" (mk:diag-vltext (cdr (assoc 41 d))) " ScaleY=" (mk:diag-vltext (cdr (assoc 42 d)))) f)
              (write-line (strcat "  Rotation=" (mk:diag-vltext (cdr (assoc 50 d)))) f)
              (setq bb (mk:e-bb o) cbb (mk:frame-capture-bb e))
              (if bb (write-line (strcat "  WorldBBox=" (mk:diag-vltext bb)) f))
              (if cbb (write-line (strcat "  CaptureBBox=" (mk:diag-vltext cbb)) f))
              (write-line (strcat "  Объект=" (if (mk:get-attr e "Объект") (mk:get-attr e "Объект") "")) f)
              (write-line (strcat "  Витраж=" (if (mk:get-attr e "Витраж") (mk:get-attr e "Витраж") "")) f)
              (foreach pr (mk:dyn-pairs o)
                (write-line (strcat "  Dynamic " (car pr) " = " (mk:diag-vltext (cdr pr))) f))
              (write-line "" f)))
          (setq i (1+ i)))
        (write-line "NO INSERTS FOUND" f))
      (close f)
      (prompt "\n[OK] Рамки: D:/MARKV_FRAMES.txt"))
    (prompt "\n[ERROR] Не удалось открыть D:/MARKV_FRAMES.txt"))
  (princ))

(defun c:MARKVBULKDIAG (/ ss f i e d typ counts styles blocks layers x)
  (prompt "\n[МАРКАВДИАГНОЗОПТОМ] Массовая диагностика выбранных объектов...")
  (vl-load-com)
  (setq ss (ssget))
  (if ss
    (progn
      (setq f (open *mk:diag-all-file* "w")
            i 0 counts nil styles nil blocks nil layers nil)
      (mk:diag-bulk-line f "=== MARKV BULK DIAGNOSTICS ===")
      (mk:diag-bulk-line f (strcat "Selected objects: " (itoa (sslength ss))))
      (while (< i (sslength ss))
        (setq e (ssname ss i)
              d (entget e)
              typ (cdr (assoc 0 d)))
        (mk:diag-bulk-object f e d (1+ i))
        (setq counts (mk:diag-inc typ counts)
              layers (mk:diag-inc (cdr (assoc 8 d)) layers))
        (if (= typ "MLINE")
          (setq styles (mk:diag-inc (cdr (assoc 2 d)) styles)))
        (if (= typ "INSERT")
          (setq blocks (mk:diag-inc (cdr (assoc 2 d)) blocks)))
        (setq i (1+ i)))
      (mk:diag-bulk-line f "")
      (mk:diag-bulk-line f "Object types:")
      (foreach x (reverse counts)
        (mk:diag-bulk-line f (strcat "  " (car x) " = " (itoa (cdr x)))))
      (mk:diag-bulk-line f "")
      (mk:diag-bulk-line f "MLINE styles:")
      (if styles
        (foreach x (reverse styles)
          (mk:diag-bulk-line f (strcat "  " (car x) " = " (itoa (cdr x)))))
        (mk:diag-bulk-line f "  <none>"))
      (mk:diag-bulk-line f "")
      (mk:diag-bulk-line f "Block names:")
      (if blocks
        (foreach x (reverse blocks)
          (mk:diag-bulk-line f (strcat "  " (car x) " = " (itoa (cdr x)))))
        (mk:diag-bulk-line f "  <none>"))
      (mk:diag-bulk-line f "")
      (mk:diag-bulk-line f "Layers:")
      (foreach x (reverse layers)
        (mk:diag-bulk-line f (strcat "  " (car x) " = " (itoa (cdr x)))))
      (mk:diag-layout f ss)
      (if f (close f))
      (prompt (strcat "\n[OK] Отчёт: " *mk:diag-all-file*)))
    (prompt "\n[INFO] Объекты не выбраны."))
  (princ))

(defun c:МАРКАВДИАГНОЗОПТОМ () (c:MARKVBULKDIAG))

;;;=====================================================================
;;; 21. ИНИЦИАЛИЗАЦИЯ С АВТОПРОВЕРКОЙ
;;;=====================================================================
(defun mk:repeat (ch n / out)
  (setq out "")
  (repeat n (setq out (strcat out ch)))
  out)
(defun mk:ljust (s w / d)
  ;; Return exactly w characters; do not append an extra space.
  (setq s (if (mk:strp s) s "") d (- w (strlen s)))
  (if (> d 0) (strcat s (mk:repeat " " d)) s))
(defun mk:help-row (cmd desc)
  (prompt (strcat "\n  | " (mk:ljust cmd 25) "| " (mk:ljust desc 34) " |")))
(defun mk:help-border ()
  (prompt (strcat "\n  +" (mk:repeat "-" 26) "+" (mk:repeat "-" 36) "+")))
(defun mk:help-header ()
  (prompt (strcat "\n  | " (mk:ljust "КОМАНДА" 25) "| " (mk:ljust "ЧТО ДЕЛАЕТ" 34) " |")))

;; (прежние mk:local-kind / mk:local-less удалены: не вызывались ниоткуда,
;;  а mk:local-kind к тому же опирался на кириллические маски в wcmatch —
;;  такой разбор ненадёжен, тип объекта даёт mk:obj-kind)

(prompt (strcat "\n[MarkV] Загружен " *mk:ver* " / " *mk:build*))
(mk:help-border)
(mk:help-header)
(mk:help-border)
(mk:help-row "МАРКАВ" "Пакет: витражи -> стойки, ригели, ведомость")
(mk:help-row "МАРКАВГЕОМЕТРИЯ" "Сбор по витражам: модель, проверки")
(mk:help-row "МАРКАВСТ" "Марки стоек (свой номер в каждом витраже)")
(mk:help-row "МАРКАВРГ" "Марки ригелей (своя шкала в каждом витраже)")
(mk:help-row "МАРКАВТАБЛ" "Общая ведомость профилей + XLS")
(mk:help-row "МАРКАВДИАГНОЗ" "Диагностика блока и атрибутов")
(mk:help-row "МАРКАВДИАГНОЗОПТОМ" "Сводка по выбранным объектам в файл")
(mk:help-row "МАРКАВБАЗЫ" "Список баз профильных систем")
(mk:help-row "МАРКАВСКОБКИ" "Скобки: CHKALL или свой сканер")
(mk:help-border)
(mk:load-bases)
(princ)
