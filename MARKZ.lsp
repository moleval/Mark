;;;=====================================================================
;;;  MARKZ.lsp  —  ЗАПОЛНЕНИЕ АТРИБУТОВ МАРКИ ЗАПОЛНЕНИЙ
;;;  Файл: D:\MARKZ.lsp
;;;  Команды: МАРКАЗАП, МАРКАРОВКАЗАП, МАРКАЗАПРЯД, МАРКАЗАПТАБЛ, МАРКАЗАПБЛОК
;;;
;;;  Режим самодиагностики TEST 01 ... TEST 16.
;;;  Проверка скобок: AutoExtraction, CHKALL / CHKFILE (вручную).
;;;  При ошибке RELOAD: диагностика упавшего файла + CHKLOAD.
;;;  Этап A: выбор > чтение > диагностика > расчёт > PRE-CHECK
;;;  Этап B: запись атрибута "Марка" только после PRE-CHECK = OK
;;;
;;;  Загрузка:
;;;    (load "D:/MARKZ.lsp")
;;;    МАРКАРОВКАЗАП
;;;
;;;  ВАЖНО: имена блоков, атрибутов и Dynamic Properties должны
;;;  точно совпадать с чертежом. Если Visibility называется иначе —
;;;  добавьте имя в *mark:prop-vis-candidates* (см. TEST 07).
;;;=====================================================================

(vl-load-com)

;;;--------------------- Шапка: имена и допуски (править здесь) ------

;; Точное имя или маска wcmatch, напр.: "*аполнение*витраж*"
(setq *mark:block-fill*    "Заполнение в витраж")
(setq *mark:block-glazing* "Атрибуты витража")
(setq *mark:attr-vitrage*  "Витраж")
(setq *mark:attr-mark*     "Марка")
(setq *mark:prop-width*    "Ширина в свету")
(setq *mark:prop-height*   "Высота в свету")

;; Кандидаты имени Dynamic Property состояния Visibility
(setq *mark:prop-vis-candidates*
  '("Видимость1"
    "Видимость"
    "Visibility"
    "Visibility1"
    "Состояние видимости"
    "Visibility State"
    "Вид"
    "Видимость2"))

;; 29 букв: нет Ё, Й, З, О. После Я — АА, АБ, … БА, ББ. Не останавливаемся.
(setq *mark:letters*
  '("А" "Б" "В" "Г" "Д" "Е" "Ж" "И"
    "К" "Л" "М" "Н" "П" "Р" "С" "Т" "У" "Ф"
    "Х" "Ц" "Ч" "Ш" "Щ" "Ъ" "Ы" "Ь" "Э" "Ю" "Я"))

(defun mark:letter-at (n / base len span start off digits d ch)
  (setq base (length *mark:letters*))
  (cond
    ((or (null n) (not (numberp n)) (< n 0) (<= base 0))
     nil)
    ((< n base)
     (nth (fix n) *mark:letters*))
    (t
     (setq len   1
           span  base
           start 0)
     (while (and (< len 4) (>= n (+ start span)))
       (setq start (+ start span)
             len   (1+ len)
             span  (* span base)))
     (setq off    (fix (- n start))
           digits nil)
     (repeat len
       (setq d      (rem off base)
             off    (/ off base)
             ch     (nth d *mark:letters*)
             digits (cons (if ch ch "") digits)))
     (apply 'strcat digits))))

;; Специальные Visibility > приписка (без дефиса, нижний регистр)
(setq *mark:specials*
  '(("Стемалит" . "стм")
    ("Сэндвич" . "снд")))

;; Ред. 48.0: секции/ярусы и шкала «в свету» (зеркало MarkZV)
(setq *mark:fill-geo*    nil)     ;; (ename . (ширина секции высота яруса))
(setq *mark:cell-hs*     nil)     ;; Ред. 48.8: высоты всех ячеек каркаса (З-1)
(setq *mark:scale1*      nil)     ;; Блок 1: ключи длин ригелей, мм
(setq *mark:scale-final* nil)     ;; ((размер источник) ...), Р/З/РЗ
(setq *mark:sec-diag*    nil)     ;; Ред. 48.14: [DIAG] печать геометрии секций
;; Сколько блоков показывать в TEST 13
(setq *mark:test13-show*  10)
;; Сколько строк ошибок показывать в одном тесте (остальное — сводка)
(setq *mark:max-err-lines* 10)

;; Рядовка
(setq *mark:ar-block*   "Ряд заполнений")
(setq *mark:ar-attr*    "Ряд")
(setq *mark:ar-group-h* "Рядовка горизонтальная")
(setq *mark:ar-group-v* "Рядовка вертикальная")
(setq *mark:ar-offset*  1000.0)
(setq *mark:ar-gap*     1000.0)
(setq *mark:ar-line*    150.0)   ; шаг линий 1+, мм
(setq *mark:ar-tol*     1.0)
;; Ред. 48.29: порог разрыва между витражами, мм — меньше порога
;; один витраж (проёмные ячейки не разрывают); у ТБ-1/В-7 ~9.9 м
(setq *mark:vz-gap*     2500.0)
;; Ред. 48.30: карта (ename . префикс витража) — совместная маркировка;
;; режим: "joint" (общие шкалы) / "separate" (свои per витраж) / nil
(setq *mark:vz-map*     nil)
(setq *mark:vz-mode*    nil)

;; Сетка заполнений
(setq *mark:fill-tol*    30.0)    ; мм — выравнивание осей
(setq *mark:fill-slope*  0.0033)  ; уклон стойки, не ослаблять без чертежа
(setq *mark:fill-window* 5000.0)  ; мм — окно поиска для режима «Точка»
(setq *mark:fill-max-w*  3000.0)  ; шире — не ячейка, а разрыв между участками
(setq *mark:fill-max-h*  4500.0)

;; Ведомость
(setq *mtab:allowance* 26)
(setq *mtab:h-keys* '("Высота в свету" "ВЫСОТА В СВЕТУ" "ВЫСОТА" "Height"))
(setq *mtab:w-keys* '("Ширина в свету" "ШИРИНА В СВЕТУ" "ШИРИНА" "ДЛИНА" "Width"))

;;;--------------------- Состояние сеанса -----------------------------

;; Редакция модуля — видно в консоли при загрузке и в баннерах
(setq *mark:rev*    "Ред. 48.41")
(setq *mark:build*  "Ред. 36.6")

;; МАРКАЗАП: один выбор; один UNDO на весь пакет
(setq *mark:reuse-sel* nil)
(setq *mark:batch-undo* nil)
(setq *mark:seg-cache* nil)
(setq *mark:udef-cache* nil)
(setq *mark:mline-cache* nil)
(setq *mark:no-expl-undo* nil)
(setq *mark:expl-n* 0)
(setq *mark:def-n* 0)
(setq *mark:vis-n* 0)
(setq *mark:hid-n* 0)
(setq *mark:vis-open* 0)
(setq *mark:dyn-open-skip* 0)

(defun mark:reset-state ()
  (setq *mark:dyn-cache*    nil
        *mark:seg-cache*    nil
        *mark:no-expl-undo* nil
        *mark:errors*       0
        *mark:warnings*     0
        *mark:records*      nil
        *mark:cnt-stem*     0
        *mark:cnt-sand*     0
        *mark:written*      0
        *mark:precheck-ok*  nil
        *mark:apply-failed* 0)
  ;; выбор и таблицы индексов не трогаем, если МАРКАЗАП переиспользует
  (if (not *mark:reuse-sel*)
    (setq *mark:found-names*   nil
          *mark:sel-total*     0
          *mark:others*        0
          *mark:fills*         nil
          *mark:glazings*      nil
          *mark:prefix*        ""
          *mark:prefix-found*  nil
          *mark:widths*        nil
          *mark:heights*       nil
          *mark:fill-geo*      nil
          *mark:scale1*        nil
          *mark:scale-final*   nil
          *mark:beam-scale*    nil
          *mark:scale-shift*   0
          *mark:num-was*       nil))
  ;; Ред. 48.23: кэш проёмов сбрасывается здесь, а не только в
  ;; fill-main — МАРКАЗАП (a-all) вызывает режимную функцию
  ;; напрямую, и после переноса чертежа в той же сессии кэш
  ;; держал СТАРЫЕ габариты окон: ячейки не признавались
  ;; проёмными и получали вставки (ТБ-1, прогон 48.21 — обе
  ;; 1150-ячейки). reset-state выполняется при LOAD и в начале
  ;; каждого пакета — кэш всегда свежий.
  (setq *mark:opening-bbs*     nil
        *mark:opening-loaded*  nil
        *mark:opening-partial* nil
        *mark:grid-zone*       nil))

(mark:reset-state)

;;;--------------------- Сообщения ------------------------------------

;; Ред. 36.5: сообщения дублируются в файл. Раньше mark:out был только
;; prompt, поэтому при закрытой/обрезанной консоли протокол терялся.
;; nil — автоматический путь: <папка текущего DWG>/MARKZ_LOG.txt.
(setq *mark:log-file*   nil)
(setq *mark:log-failed* nil)

(defun mark:log-path (/ dir ch)
  (if (and (mark:strp *mark:log-file*)
           (> (strlen *mark:log-file*) 0))
    *mark:log-file*
    (progn
      (setq dir (vl-catch-all-apply 'getvar (list "DWGPREFIX")))
      (if (or (vl-catch-all-error-p dir)
              (not (mark:strp dir)))
        (setq dir ""))
      (if (> (strlen dir) 0)
        (progn
          (setq ch (substr dir (strlen dir) 1))
          (if (and (/= ch "/") (/= ch "\\"))
            (setq dir (strcat dir "/")))))
      (strcat dir "MARKZ_LOG.txt"))))

;; Файл открывается на каждую строку: даже при Esc/сбое уже напечатанное
;; не остаётся в незакрытом буфере. Ошибка записи не должна ронять маркировку.
(defun mark:log-write (msg / path f r text)
  (if (not *mark:log-failed*)
    (progn
      (setq path (mark:log-path))
      (if (and (mark:strp path) (> (strlen path) 0)
               (mark:strp msg))
        (progn
          (setq f (vl-catch-all-apply 'open (list path "a")))
          (if (or (null f) (vl-catch-all-error-p f))
            (setq *mark:log-failed* t)
            (progn
              ;; Если сообщение уже заканчивается \n, не удваиваем пустую строку.
              (setq text msg)
              (if (and (> (strlen text) 0)
                       (= (substr text (strlen text) 1) "\n"))
                (setq text (substr text 1 (1- (strlen text)))))
              (setq r (vl-catch-all-apply 'write-line (list text f)))
              (vl-catch-all-apply 'close (list f))
              (if (vl-catch-all-error-p r)
                (setq *mark:log-failed* t))))))))
  nil)

;; Каждый пакет добавляет свой блок; одиночные команды дописывают в него.
(defun mark:log-start (/ path f)
  (setq *mark:log-failed* nil
        path (mark:log-path)
        f (vl-catch-all-apply 'open (list path "a")))
  (if (or (null f) (vl-catch-all-error-p f))
    (setq *mark:log-failed* t)
    (vl-catch-all-apply 'close (list f)))
  (if (not *mark:log-failed*)
    (mark:log-write
      (strcat "=== MARKZ " *mark:rev* " / " *mark:build* " ===")))
  path)

;; Единственная точка вывода: princ сохраняет сообщение в командной строке,
;; а log-write оставляет его в файле. Возврат nil — как у prompt.
(defun mark:emit (msg)
  (if (not (mark:strp msg))
    (setq msg (vl-princ-to-string msg)))
  (mark:log-write msg)
  (princ msg)
  nil)

(defun mark:out (msg)
  (mark:emit (strcat msg "\n")))

;; Одна строка в командной строке при старте. В пакете МАРКАЗАП не повторяем.
(defun mark:cmd-line (text)
  (if (not *mark:reuse-sel*)
    (mark:emit (strcat "\n" text "\n"))))

(defun mark:note-error ()
  (setq *mark:errors* (1+ *mark:errors*)))

(defun mark:note-warning ()
  (setq *mark:warnings* (1+ *mark:warnings*)))

(defun mark:banner ()
  (mark:out "========================================")
  (mark:out (strcat " ЗАПОЛНЕНИЕ АТРИБУТОВ МАРКИ  "
                    *mark:rev*))
  (mark:out "========================================"))

;;;--------------------- Утилиты --------------------------------------

;; Переносимая проверка строки (не зависит от `stringp` из Visual LISP)
(defun mark:strp (x)
  (and x (eq (type x) 'STR)))

(defun mark:trim (s)
  (if (mark:strp s)
    (vl-string-trim " \t\r\n" s)
    s))

(defun mark:name= (s1 s2)
  (and (mark:strp s1)
       (mark:strp s2)
       (= (strcase (mark:trim s1))
          (strcase (mark:trim s2)))))

(defun mark:fmt-raw (v / u)
  (setq u (mark:unwrap v))
  (if (or (numberp u) (mark:strp u))
    (setq v u))
  (cond
    ((null v) "NIL")
    ((numberp v)
     (if (= (float v) (fix (float v)))
       (itoa (fix (float v)))
       (rtos (float v) 2 8)))
    ((mark:strp v) (strcat "\"" v "\""))
    (t (vl-princ-to-string (mark:unwrap v)))))

(defun mark:mm-key (v / u)
  ;; Ред. 48.7: размер в целых мм (ключ сортировки); nil — не число.
  (setq u (mark:unwrap v))
  (if (numberp u)
    (mark:round1 u)
    nil))

(defun mark:fmt-mm (v / u)
  ;; Ред. 48.7: печать размеров в целых мм — без хвостов вида 409.99999847.
  (setq u (mark:unwrap v))
  (if (numberp u)
    (rtos (mark:round1 u) 2 0)
    (mark:fmt-raw v)))

(defun mark:numval (v / u)
  (setq u (mark:unwrap v))
  (cond
    ((numberp u) (float u))
    ((numberp v) (float v))
    ((and (mark:strp u) (distof u 2)) (distof u 2))
    ((and (mark:strp v) (distof v 2)) (distof v 2))
    (t nil)))

(defun mark:pad-line (label value)
  (strcat " "
          label
          (substr "                                  " 1
                  (max 1 (- 36 (strlen label))))
          value))

;; Выравнивание вправо: колонки лога «ячейка/вставка» стоят ровно.
(defun mark:rjust (s w / d)
  (setq d (- w (strlen s)))
  (if (> d 0)
    (strcat (substr "                            " 1 d) s)
    s))

;; Печать списка строк с ограничением *mark:max-err-lines*
(defun mark:print-limited (lines / total shown)
  (setq total (length lines)
        shown 0)
  (foreach ln lines
    (if (< shown *mark:max-err-lines*)
      (progn
        (setq shown (1+ shown))
        (mark:out ln))))
  (if (> total *mark:max-err-lines*)
    (mark:out (strcat "... и ещё "
                      (itoa (- total *mark:max-err-lines*))
                      " строк(и)."))))

(defun mark:insert-sorted (lst n / res done hit)
  (setq res nil
        done nil
        hit   nil)
  (foreach x lst
    (if (mark:same-num? n x)
      (setq hit t))
    (if (and (null done)
             (null hit)
             (< n x))
      (progn
        (setq res (cons n res))
        (setq done t)))
    (setq res (cons x res)))
  (if (and (not done) (not hit))
    (setq res (cons n res)))
  (reverse res))

(defun mark:same-num? (a b)
  ;; одинаковый размер: 0.5 мм — шум float/округление, не «другой типоразмер»
  (and (numberp a) (numberp b) (equal a b 0.5)))

;; округление размера для ключа индекса (0.5 мм)
(defun mark:dim-key (x)
  (if (and (numberp x) (> x 0.0))
    (fix (+ (* x 2.0) 0.5))  ; half-mm integer key
    nil))

(defun mark:sort-unique (nums / res n hit x)
  (setq res nil)
  (foreach n nums
    (setq hit nil)
    (foreach x res
      (if (and (null hit) (mark:same-num? x n))
        (setq hit t)))
    (if (not hit)
      (setq res (mark:insert-sorted res n))))
  res)

(defun mark:positives (lst / out)
  (setq out nil)
  (foreach n lst
    (if (and (numberp n) (> n 0.0))
      (setq out (cons (float n) out))))
  (reverse out))

(defun mark:index-of (lst v / i found)
  (setq i 0
        found nil)
  (foreach x lst
    (if (and (null found) (mark:same-num? x v))
      (setq found i))
    (setq i (1+ i)))
  found)

(defun mark:rec-get (r key)
  (cdr (assoc key r)))

(defun mark:rec-put (r key val)
  (if (assoc key r)
    (subst (cons key val) (assoc key r) r)
    (append r (list (cons key val)))))

(defun mark:vla (e)
  (vlax-ename->vla-object e))

;; Имя блока: DXF-код 2 + имя через ActiveX (для динамических блоков)
;; Безопасное чтение ActiveX-свойства через vlax-get-property
(defun mark:ax-get (obj prop / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (vl-catch-all-error-p r)
    nil
    r))

;; Безопасная запись ActiveX-свойства
(defun mark:ax-put (obj prop val / r)
  (setq r (vl-catch-all-apply 'vlax-put-property (list obj prop val)))
  (if (vl-catch-all-error-p r)
    nil
    t))

;; Вызов ActiveX-метода (аргументы после method)
(defun mark:ax-invoke (obj method arg / r)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (if arg
                                (list obj method arg)
                                (list obj method))))
  (if (vl-catch-all-error-p r)
    nil
    r))

;; T если метод вызван без ошибки (void-методы возвращают nil — это OK)
(defun mark:ax-invoke-ok (obj method arg / r)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (if arg
                                (list obj method arg)
                                (list obj method))))
  (not (vl-catch-all-error-p r)))

;; Все известные имена вставки: DXF-2, Name, EffectiveName
(defun mark:block-names (e / obj names nm)
  (setq names nil
        nm    (cdr (assoc 2 (entget e))))
  (if (mark:strp nm)
    (setq names (cons nm names)))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (and (not (vl-catch-all-error-p obj)) obj)
    (progn
      (setq nm (mark:ax-get obj "Name"))
      (if (and (mark:strp nm) (not (member nm names)))
        (setq names (cons nm names)))
      (setq nm (mark:ax-get obj "EffectiveName"))
      (if (and (mark:strp nm) (not (member nm names)))
        (setq names (cons nm names)))))
  (reverse names))

;; Точное имя ИЛИ маска wcmatch в target (* ?)
(defun mark:name-match? (actual target / a t*)
  (and (mark:strp actual)
       (mark:strp target)
       (progn
         (setq a   (strcase (mark:trim actual))
               t*  (strcase (mark:trim target)))
         (or (= a t*)
             (wcmatch a t*)))))

(defun mark:blk-match? (e target / names nm ok)
  (setq ok    nil
        names (mark:block-names e))
  (foreach nm names
    (if (and (null ok) (mark:name-match? nm target))
      (setq ok t)))
  ok)

;;;--------------------- Атрибуты -------------------------------------

;; Поиск ATTRIB по DXF-цепочке INSERT(-2) > ATTRIB(-2) > ...
;; Возвращает ename атрибута или nil.
;; Принимает ename вставки.
;; Универсальный поиск: tag передаём явно
(defun mark:find-attr-in-ename (e tag / head ent data result)
  ;; 1) классическая цепочка INSERT -2
  (setq head   e
        result nil
        ent    (cdr (assoc -2 (entget e))))
  (while (and ent (null result))
    (if (eq ent head)
      (setq ent nil)
      (progn
        (setq data (entget ent))
        (cond
          ((null data)
           (setq ent nil))
          ((and (cdr (assoc 2 data))
                (mark:name= (cdr (assoc 2 data)) tag))
           (setq result ent))
          (t
           (setq ent (cdr (assoc -2 data))))))))
  ;; 2) fallback: entnext — ATTRIB/SEQEND сразу после INSERT (66=1)
  (if (null result)
    (progn
      (setq ent (entnext e))
      (while (and ent (null result))
        (setq data (entget ent))
        (cond
          ((null data)
           (setq ent nil))
          ((mark:name= (cdr (assoc 0 data)) "ATTRIB")
           (if (mark:name= (cdr (assoc 2 data)) tag)
             (setq result ent)
             (setq ent (entnext ent))))
          ((mark:name= (cdr (assoc 0 data)) "SEQEND")
           (setq ent nil))
          (t
           ;; не ATTRIB — дальше не ищем в этой ветке
           (setq ent nil))))))
  result)

;; Через ActiveX-коллекцию Attributes (без vla-get-*)
(defun mark:find-attr-obj (obj tag / result attrs i cnt a tagv)
  (setq result nil)
  (if obj
    (progn
      (setq attrs (mark:ax-get obj "Attributes"))
      (if attrs
        (progn
          (setq cnt (mark:ax-get attrs "Count"))
          (if (and (numberp cnt) (> cnt 0))
            (progn
              (setq i 0)
              (while (and (< i cnt) (null result))
                (setq a    (mark:ax-invoke attrs "Item" i)
                      tagv (if a (mark:ax-get a "TagString") nil))
                (if (and tagv (mark:name= tagv tag))
                  (setq result a))
                (setq i (1+ i)))))))))
  result)

;; Чтение значения атрибута по ename вставки: (T значение) | (NIL NIL)
(defun mark:find-attr (e tag / a data val obj)
  (setq a (mark:find-attr-in-ename e tag))
  (cond
    (a
     (progn
       (setq data (entget a)
             val  (cdr (assoc 1 data)))
       (list t (if (mark:strp val) val ""))))
    (t
     (progn
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
       (if (or (vl-catch-all-error-p obj) (null obj))
         (list nil nil)
         (progn
           (setq a (mark:find-attr-obj obj tag))
           (if a
             (progn
               (setq val (mark:ax-get a "TextString"))
               (list t (if (mark:strp val) val "")))
             (list nil nil))))))))

;;;--------------------- Dynamic Properties ---------------------------

;; (T значение) | (NIL NIL), если свойство недоступно
;; Развернуть VARIANT > значение
(defun mark:unwrap (v / w)
  (setq w (vl-catch-all-apply 'vlax-variant-value (list v)))
  (if (vl-catch-all-error-p w)
    v
    w))

;; Имя свойства dynamic block: PropertyName | Name | Description
(defun mark:dyn-item-name (item / nm)
  (setq nm (mark:ax-get item "PropertyName"))
  (if (not (mark:strp nm))
    (setq nm (mark:ax-get item "Name")))
  (if (not (mark:strp nm))
    (setq nm (mark:ax-get item "Description")))
  nm)

(defun mark:dyn-item-value (item / val)
  (setq val (mark:ax-get item "Value"))
  (if (null val)
    (setq val (mark:ax-get item "PropertyValue")))
  (mark:unwrap val))

;; Разбор одного элемента коллекции свойств > cons или nil
(defun mark:dyn-item-pair (item / nm val)
  (setq nm  (mark:dyn-item-name item)
        val (mark:dyn-item-value item))
  (if (mark:strp nm)
    (cons nm val)
    nil))

;; Пары (имя . значение) из GetDynamicBlockProperties
(defun mark:load-dyn-pairs (obj / r v lst item pair out)
  (setq out nil)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (list obj "GetDynamicBlockProperties")))
  (if (or (vl-catch-all-error-p r) (null r))
    nil
    (progn
      (setq v (mark:unwrap r))
      ;; safearray > list (без get-ubound)
      (setq lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
      (cond
        ((and (not (vl-catch-all-error-p lst)) (listp lst))
         (progn
           (foreach item lst
             ;; item может быть variant
             (setq item (mark:unwrap item)
                   pair (mark:dyn-item-pair item))
             (if pair
               (setq out (cons pair out))))
           (reverse out)))
        (t
         (progn
           ;; не список — пробуем как одиночный объект
           (setq pair (mark:dyn-item-pair v))
           (if pair
             (list pair)
             (progn
               ;; последняя попытка: сам r как коллекция Count/Item
               (setq out nil
                     item (mark:ax-invoke r "Item" 0))
               (if item
                 (progn
                   (setq item (mark:unwrap item)
                         pair  (mark:dyn-item-pair item))
                   (if pair (setq out (list pair)))))
               out))))))))

;; Кэш: ename > alist
(defun mark:dyn-pairs (e / cache pair obj)
  (setq cache (assoc e *mark:dyn-cache*))
  (if cache
    (cdr cache)
    (progn
      (setq obj  (mark:vla e)
            pair (cons e (mark:load-dyn-pairs obj)))
      (setq *mark:dyn-cache* (cons pair *mark:dyn-cache*))
      (cdr pair))))

(defun mark:get-dyn (obj prop / r pr pairs v uv)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (not (vl-catch-all-error-p r))
    (list t (mark:unwrap r))
    (progn
      (setq pr (vl-catch-all-apply 'vlax-vla-object->ename (list obj))
            pairs nil)
      (if (and (not (vl-catch-all-error-p pr)) pr)
        (setq pairs (mark:dyn-pairs pr)))
      (if (null pairs)
        (setq pairs (mark:load-dyn-pairs obj)))
      (setq v  (if (assoc prop pairs) (cdr (assoc prop pairs)) nil)
            uv (mark:unwrap v))
      (if uv
        (list t uv)
        (list nil (vl-catch-all-error-message r))))))

(defun mark:get-visibility (obj / cand r res)
  (setq res nil)
  (foreach cand *mark:prop-vis-candidates*
    (if (null res)
      (progn
        (setq r (mark:get-dyn obj cand))
        (if (car r)
          (setq res (list t cand (cadr r)))))))
  res)

;;;--------------------- Выбор объектов -------------------------------

(defun mark:select (/ ss i e fills glazings others names nm lst)
  (if (and *mark:reuse-sel* *mark:fills*)
    (progn
      (mark:out "[INFO] Выбор уже выполнен (МАРКАЗАП) — без повторного выделения.")
      t)
    (progn
  (mark:out "")
  (mark:emit
    "\nВыберите блоки «Заполнение в витраж» (стойки не нужны): ")
  (setq ss (vl-catch-all-apply 'ssget nil))
  (cond
    ((vl-catch-all-error-p ss)
     (mark:out "[ERROR] Ошибка при выделении.")
     (mark:note-error)
     nil)
    ((null ss)
     (mark:out "[INFO] Выбор отменён. Изменений не выполнено.")
     nil)
    (t
     (setq fills     nil
           glazings  nil
           others    0
           names     nil
           i         (sslength ss))
     (repeat i
       (setq i (1- i)
             e (ssname ss i))
       (setq lst (mark:block-names e))
       (foreach nm lst
         (if (not (member nm names))
           (setq names (cons nm names))))
       (cond
         ((mark:blk-match? e *mark:block-fill*)
          (setq fills (cons e fills)))
         ((mark:blk-match? e *mark:block-glazing*)
          (setq glazings (cons e glazings)))
         (t (setq others (1+ others)))))
     (setq *mark:sel-total*   (sslength ss)
           *mark:fills*        (reverse fills)
           *mark:glazings*     (reverse glazings)
           *mark:others*       others
           *mark:found-names*  (reverse names))
     ;; Ред. 48.30: новый выбор — карта витражей недействительна
     ;; Ред. 48.32: и кэш каркаса тоже
     (setq *mark:vz-map*  nil
           *mark:vz-mode* nil
           *mark:carc-cache* nil)
     (mark:out "")
     (mark:out (strcat "[INFO] Выбрано объектов: " (itoa *mark:sel-total*)))
     (mark:out (strcat "[INFO] Заполнений: " (itoa (length *mark:fills*))))
     (mark:out
       (strcat "[INFO] \"Атрибуты витража\": "
               (itoa (length *mark:glazings*))))
     (if (> others 0)
       (mark:out
         (strcat "[INFO] Прочих объектов (игнорируются): " (itoa others))))
     (mark:out "[INFO] Имена вставок в выделении:")
     (if *mark:found-names*
       (foreach nm *mark:found-names*
         (mark:out (strcat "         - " nm)))
       (mark:out "         (имена не прочитаны)"))
     (if (null *mark:fills*)
       (progn
         (mark:out "[INFO] Имя/маска заполнения (*mark:block-fill*):")
         (mark:out (strcat "         " *mark:block-fill*))))
     t)))))

;;;--------------------- Сбор данных (только чтение) ------------------

;; Все теги ATTRIB вставки (DXF-цепочка -2)
(defun mark:attr-tags (e / head ent data tags tag)
  (setq head e
        tags nil
        ent  (cdr (assoc -2 (entget e))))
  (while (and ent (not (eq ent head)))
    (setq data (entget ent))
    (if (null data)
      (setq ent nil)
      (progn
        (setq tag (cdr (assoc 2 data)))
        (if (mark:strp tag)
          (setq tags (cons tag tags)))
        (setq ent (cdr (assoc -2 data))))))
  ;; fallback entnext
  (if (null tags)
    (progn
      (setq ent (entnext e))
      (while (and ent)
        (setq data (entget ent))
        (cond
          ((null data)
           (setq ent nil))
          ((mark:name= (cdr (assoc 0 data)) "ATTRIB")
           (setq tag (cdr (assoc 2 data)))
           (if (mark:strp tag)
             (setq tags (cons tag tags)))
           (setq ent (entnext ent)))
          ((mark:name= (cdr (assoc 0 data)) "SEQEND")
           (setq ent nil))
          (t
           (setq ent nil))))))
  (reverse tags))

;; Теги через ActiveX-коллекцию Attributes
(defun mark:attr-tags-ax (obj / attrs cnt i a tag tags)
  (setq tags nil
        attrs (mark:ax-get obj "Attributes"))
  (if attrs
    (progn
      (setq cnt (mark:ax-get attrs "Count"))
      (if (and (numberp cnt) (> cnt 0))
        (progn
          (setq i 0)
          (while (< i cnt)
            (setq a   (mark:ax-invoke attrs "Item" i)
                  tag (if a (mark:ax-get a "TagString") nil))
            (if (mark:strp tag)
              (setq tags (cons tag tags)))
            (setq i (1+ i)))))))
  (reverse tags))

(defun mark:collect-data (/ recs idx e obj wh hh vv mr)
  (setq recs     nil
        idx      0
        *mark:cnt-stem* 0
        *mark:cnt-sand* 0)
  (foreach e *mark:fills*
    (setq idx (1+ idx))
    (setq obj (mark:vla e))
    (setq wh (mark:get-dyn obj *mark:prop-width*))
    (setq hh (mark:get-dyn obj *mark:prop-height*))
    (setq vv (mark:get-visibility obj))
    (setq mr (mark:find-attr e *mark:attr-mark*))
    (if vv
      (progn
        (if (= (mark:vis-rank (caddr vv)) 1)
          (setq *mark:cnt-stem* (1+ *mark:cnt-stem*)))
        (if (= (mark:vis-rank (caddr vv)) 2)
          (setq *mark:cnt-sand* (1+ *mark:cnt-sand*)))))
    (setq recs
      (cons
        (list
          (cons 'idx        idx)
          (cons 'ename      e)
          (cons 'width-ok   (car wh))
          (cons 'width-err  (if (car wh) nil (cadr wh)))
          (cons 'width-raw  (cadr wh))
          (cons 'width      (mark:numval (cadr wh)))
          (cons 'height-ok  (car hh))
          (cons 'height-err (if (car hh) nil (cadr hh)))
          (cons 'height-raw (cadr hh))
          (cons 'height     (mark:numval (cadr hh)))
          (cons 'vis-ok     (if vv t nil))
          (cons 'vis-prop   (if vv (cadr vv) nil))
          (cons 'vis        (if vv (caddr vv) nil))
          (cons 'mark-ok    (car mr))
          (cons 'mark-old   (cadr mr))
          (cons 'letter     nil)
          (cons 'hnum       nil)
          (cons 'mark-new   nil))
        recs)))
  (setq *mark:records* (reverse recs))
  *mark:records*)

;;;--------------------- Исходник MARKZ.lsp (путь для снимка) ---------

(setq *mark:source-candidates*
  '("D:/MarkZ/MARKZ.lsp"
    "D:\\MarkZ\\MARKZ.lsp"
    "D:/MARKZ.lsp"
    "D:\\MARKZ.lsp"
    "MARKZ.lsp"))

(setq *mark:source-file* nil)

(defun mark:file-open? (path / f)
  (and (mark:strp path)
       (/= path "")
       (setq f (vl-catch-all-apply 'open (list path "r")))
       (not (vl-catch-all-error-p f))
       f
       (progn (close f) t)))

;; Не привязан к одному диску: findfile (пути поддержки) и явные кандидаты.
(defun mark:find-source (/ cand found ff)
  (setq found nil
        ff    (vl-catch-all-apply 'findfile (list "MARKZ.lsp")))
  (if (and (not (vl-catch-all-error-p ff)) (mark:file-open? ff))
    (setq found ff))
  (if (null found)
    (foreach cand *mark:source-candidates*
      (if (null found)
        (progn
          (setq ff (vl-catch-all-apply 'findfile (list cand)))
          (cond
            ((and (not (vl-catch-all-error-p ff)) (mark:file-open? ff))
             (setq found ff))
            ((mark:file-open? cand)
             (setq found cand)))))))
  (setq *mark:source-file* found)
  found)

;;;--------------------- TEST 01 — блоки заполнений -------------------

(defun mark:test-fillings ()
  (mark:out "")
  (if (> (length *mark:fills*) 0)
    (progn
      (mark:out "[TEST 01] Блоки \"Заполнение в витраж\" — OK")
      (mark:out (strcat "Найдено: " (itoa (length *mark:fills*)))))
    (progn
      (mark:out "[TEST 01] Блоки \"Заполнение в витраж\" — ERROR")
      (mark:out "Блоки заполнений не обнаружены.")
      (mark:out "[INFO] МАРКАРОВКАЗАП пишет марку только в «Заполнение в витраж».")
      (mark:out "[INFO] Стойки и линии сетки эта команда не маркирует.")
      (mark:out "[INFO] Сетка без заполнений — команда МАРКАЗАП, источник Сетка.")
      (mark:out "[INFO] Имена, найденные в выделении:")
      (if *mark:found-names*
        (foreach nm *mark:found-names*
          (mark:out (strcat "         - " nm))))
      (mark:out "[INFO] Ожидалось (*mark:block-fill*):")
      (mark:out (strcat "         " *mark:block-fill*))
      (mark:note-error))))

;;;--------------------- TEST 02 / 03 — Атрибуты витража --------------

(defun mark:test-glazing-attr (/ g obj res val)
  (mark:out "")
  (cond
    ;; не найден — не ошибка, работа без префикса
    ((null *mark:glazings*)
     (mark:out "[TEST 02] \"Атрибуты витража\" — NOT FOUND")
     (mark:out "Работа без префикса \"Витраж\".")
     (mark:out "[INFO] Блок \"Атрибуты витража\" не найден.")
     (mark:out "[INFO] Маркировка будет выполнена без префикса \"Витраж\".")
     (setq *mark:prefix* ""
           *mark:prefix-found* nil))
    (t
     (mark:out "[TEST 02] \"Атрибуты витража\" — FOUND")
     (mark:out (strcat "Количество: " (itoa (length *mark:glazings*))))
     (if (> (length *mark:glazings*) 1)
       (if *mark:vz-map*
         (mark:out
           "[INFO] Несколько блоков \"Атрибуты витража\" — совместная маркировка: префикс каждого витража в его марках.")
         (progn
           (mark:out
             "[WARN] Найдено несколько блоков \"Атрибуты витража\". Используется первый.")
           (mark:note-warning))))
     (setq g   (car *mark:glazings*)
           obj (mark:vla g)
           res (mark:find-attr g *mark:attr-vitrage*))
     (mark:out "")
     (cond
       ;; атрибута нет — критическая ошибка
       ((null (car res))
        (mark:out "[TEST 03] Атрибут \"Витраж\" — ERROR")
        (mark:out "В блоке \"Атрибуты витража\" атрибут не найден.")
        (mark:note-error)
        (setq *mark:prefix* ""
              *mark:prefix-found* nil))
       ;; пустое значение — WARNING, по умолчанию блокирует запись
       ((= (mark:trim (cadr res)) "")
        (mark:out "[TEST 03] Атрибут \"Витраж\" — WARNING")
        (mark:out "Атрибут найден, но значение пустое.")
        (mark:out
          "[ERROR] По умолчанию это критическая ошибка. Маркировка будет остановлена.")
        (mark:note-warning)
        (mark:note-error)
        (setq *mark:prefix* ""
              *mark:prefix-found* nil))
       ;; успех
       (t
        (setq val (mark:trim (cadr res)))
        (mark:out "[TEST 03] Атрибут \"Витраж\" — OK")
        (mark:out (strcat "Значение: " val))
        ;; Ред. 48.30: совместная маркировка — перечень витражей
        (if (and *mark:vz-map* (> (length (mark:vz-map-pfxs)) 1))
          (mark:out
            (strcat "Значения: " (mark:vz-join (mark:vz-map-pfxs) ", "))))
        (setq *mark:prefix* val
              *mark:prefix-found* t))))))

;;;--------------------- TEST 04/05/06 — размеры ----------------------

(defun mark:test-dynamic-properties (/ bad4 bad5 bad6 r idx lines)
  (setq bad4 nil
        bad5 nil
        bad6 nil)
  (foreach r *mark:records*
    (setq idx (mark:rec-get r 'idx))
    (if (null (mark:rec-get r 'width-ok))
      (setq bad4 (cons idx bad4)))
    (if (null (mark:rec-get r 'height-ok))
      (setq bad5 (cons idx bad5)))
    ;; TEST 06 — существуют, числовые, > 0
    (cond
      ((null (mark:rec-get r 'width-ok))
       (setq bad6 (cons (list idx "Ширина в свету" "NIL") bad6)))
      ((null (mark:rec-get r 'width))
       (setq bad6
         (cons (list idx "Ширина в свету"
                     (mark:fmt-raw (mark:rec-get r 'width-raw)))
               bad6)))
      ((not (> (mark:rec-get r 'width) 0.0))
       (setq bad6
         (cons (list idx "Ширина в свету"
                     (mark:fmt-raw (mark:rec-get r 'width)))
               bad6))))
    (cond
      ((null (mark:rec-get r 'height-ok))
       (setq bad6 (cons (list idx "Высота в свету" "NIL") bad6)))
      ((null (mark:rec-get r 'height))
       (setq bad6
         (cons (list idx "Высота в свету"
                     (mark:fmt-raw (mark:rec-get r 'height-raw)))
               bad6)))
      ((not (> (mark:rec-get r 'height) 0.0))
       (setq bad6
         (cons (list idx "Высота в свету"
                     (mark:fmt-raw (mark:rec-get r 'height)))
               bad6)))))
  (setq bad4 (reverse bad4)
        bad5 (reverse bad5)
        bad6 (reverse bad6))

  ;; TEST 04
  (mark:out "")
  (cond
    ((null bad4)
     (mark:out "[TEST 04] \"Ширина в свету\" — OK"))
    (t
     (mark:out "[TEST 04] \"Ширина в свету\" — ERROR")
     (setq lines nil)
     (foreach r *mark:records*
       (if (null (mark:rec-get r 'width-ok))
         (setq lines
           (cons (strcat "Блок №" (itoa (mark:rec-get r 'idx))
                         ": "
                         (if (mark:rec-get r 'width-err)
                           (mark:rec-get r 'width-err)
                           "свойство не найдено"))
                 lines))))
     (mark:print-limited (reverse lines))
     (mark:out "[INFO] Имя свойства должно точно совпадать: \"Ширина в свету\".")
     (mark:note-error)))

  ;; TEST 05
  (mark:out "")
  (cond
    ((null bad5)
     (mark:out "[TEST 05] \"Высота в свету\" — OK"))
    (t
     (mark:out "[TEST 05] \"Высота в свету\" — ERROR")
     (setq lines nil)
     (foreach r *mark:records*
       (if (null (mark:rec-get r 'height-ok))
         (setq lines
           (cons (strcat "Блок №" (itoa (mark:rec-get r 'idx))
                         ": "
                         (if (mark:rec-get r 'height-err)
                           (mark:rec-get r 'height-err)
                           "свойство не найдено"))
                 lines))))
     (mark:print-limited (reverse lines))
     (mark:out "[INFO] Имя свойства должно точно совпадать: \"Высота в свету\".")
     (mark:note-error)))

  ;; TEST 06
  (mark:out "")
  (cond
    ((null bad6)
     (progn
       (mark:out "[TEST 06] Значения размеров — OK")
       (mark:out (strcat "Обработано: " (itoa (length *mark:records*))))))
    (t
     (progn
       (mark:out "[TEST 06] Значения размеров — ERROR")
       (setq lines nil)
       (foreach r bad6
         (setq lines
           (cons (strcat "Блок №" (itoa (car r)) ": " (cadr r) " = " (caddr r))
                 lines)))
       (mark:print-limited (reverse lines))
       (mark:note-error)))))

;;;--------------------- TEST 07 — Visibility --------------------------

(defun mark:test-visibility (/ bad r lines)
  (setq bad nil)
  (foreach r *mark:records*
    (if (null (mark:rec-get r 'vis-ok))
      (setq bad (cons (mark:rec-get r 'idx) bad))))
  (setq bad (reverse bad))
  (mark:out "")
  (cond
    ((null bad)
     (mark:out "[TEST 07] Visibility State — OK"))
    (t
     (progn
       (mark:out "[TEST 07] Visibility State — ERROR")
       (setq lines nil)
       (foreach idx bad
         (setq lines
           (cons (strcat "Блок №" (itoa idx)
                         ": невозможно определить состояние Visibility.")
                 lines)))
       (mark:print-limited (reverse lines))
       (mark:note-error)
       (mark:out "[INFO] Проверьте имя Dynamic Property Visibility")
       (mark:out "       в списке *mark:prop-vis-candidates*.")))))

;;;--------------------- TEST 08 — атрибут "Марка" ---------------------

(defun mark:test-mark-attribute (/ bad r lines)
  (setq bad nil)
  (foreach r *mark:records*
    (if (null (mark:rec-get r 'mark-ok))
      (setq bad (cons (mark:rec-get r 'idx) bad))))
  (setq bad (reverse bad))
  (mark:out "")
  (cond
    ((null bad)
     (progn
       (mark:out "[TEST 08] Атрибут \"Марка\" — OK")
       (mark:out
         (strcat "Найден у всех " (itoa (length *mark:records*)) " блоков."))))
    (t
     (progn
       (mark:out "[TEST 08] Атрибут \"Марка\" — ERROR")
       (setq lines nil)
       (foreach idx bad
         (setq lines
           (cons (strcat "Блок №" (itoa idx) ": атрибут \"Марка\" отсутствует.")
                 lines)))
       (mark:print-limited (reverse lines))
       (mark:note-error)))))

;;;--------------------- TEST 09/10/11 — данные и нумерация ------------

(defun mark:test-data (/ wlist hlist geo th h)
  (if *mark:fill-geo*
    (progn
      ;; Ред. 48.20: номера — только фактические столбцы,
      ;; буквы — только фактические строки заполнений.
      (setq *mark:scale-final* (mark:sec-scale-full))
      ;; Ред. 48.37: номера секций — шкала ригелей (mark:num-scale),
      ;; как «Рг N» в MarkZV: цифра марки заполнения и цифра
      ;; горизонтальной рядовки совпадают с номером ригеля.
      ;; На Т-1/В-7 результат прежний (1430 = №10, 9 столбцов =
      ;; 9 Рг); шкала столбцов wlist остаётся только для сверки и
      ;; для столбцов без ригеля (продолжение нумерации, «З»).
      (setq wlist nil)
      (foreach geo *mark:fill-geo*
        (if (and (cdr geo) (numberp (cadr geo)))
          (setq wlist (cons (float (mark:sec-key (cadr geo))) wlist))))
      (setq th nil)
      (foreach geo *mark:fill-geo*
        (if (and (cdr geo) (numberp (caddr geo)))
          (setq th (cons (caddr geo) th))))
      ;; Ред. 48.20 (отмена 48.8/З-1): шкала строк — только
      ;; фактические строки заполнений (th = высоты строк-
      ;; родителей). Ячейки каркаса без заполнений (проёмы,
      ;; глухие зоны) шкалу не сдвигают: пропусков букв нет
      ;; (ТБ-1: Д = 1250, проёмные 1150 букву не резервируют).
      ;; Высоты подъячеек (750/1015) в шкалу не входят — они
      ;; получают литеры .а/.б внутри строки-родителя.
      (setq hlist (mark:sort-unique (mark:positives th)))
      ;; Ред. 48.37: scale1 (длины ригелей) собирается на этапе сетки 1
      ;; ещё до этой шкалы — он и задаёт номера секций.
      (if (and (null *mark:scale1*) *mark:fills*)
        (mark:out
          (strcat "[INFO] Длины ригелей не найдены (нет мультилиний и блоков "
                  "«ригель») — номера секций по порядку столбцов, как в 48.20.")))
      (setq *mark:num-was* (mark:sort-unique (mark:positives wlist)))
      (mark:num-scale)
      (if (and *mark:scale1* *mark:num-was*)
        (mark:num-shift-out))
      (setq *mark:heights* hlist))
    (progn
      (setq wlist
        (mark:sort-unique
          (mark:positives (mark:recs-widths))))
      (setq hlist
        (mark:sort-unique
          (mark:positives (mark:recs-heights))))
      (setq *mark:num-was* wlist)
      (mark:num-scale)
      (if (and *mark:scale1* *mark:num-was*)
        (mark:num-shift-out))
      (setq *mark:heights* hlist)))

  ;; TEST 09
  (mark:out "")
  (cond
    ((and wlist hlist)
     (progn
       (mark:out "[TEST 09] Уникальные размеры — OK")
       (mark:out "")
       (mark:out (strcat "Размеров шкалы (в свету): " (itoa (length wlist))))
       (mark:out (strcat "Шкала номеров (ригели MarkZV): "
                          (itoa (length *mark:beam-scale*))
                          ", столбцов: " (itoa (length *mark:widths*))))
       (mark:out (strcat "Строк (буквы): " (itoa (length hlist))))
       (mark:out "")
       (mark:out
         (strcat "Шкала: " (mark:fmt-raw (car *mark:widths*))
                 " ... " (mark:fmt-raw (car (reverse *mark:widths*)))))
       (mark:out
         (strcat "Строка: " (mark:fmt-raw (car hlist))
                 " ... " (mark:fmt-raw (car (reverse hlist)))))))
    (t
     (progn
       (mark:out "[TEST 09] Уникальные размеры — ERROR")
       (mark:out "Нет корректных значений ширины/высоты.")
       (mark:note-error))))

  ;; TEST 10 — буквы по ярусам. После списка продолжаем АА, АБ, … БА, ББ.
  (mark:out "")
  (mark:out "[TEST 10] Буквенная маркировка — OK")
  (mark:out (strcat "Строк: " (itoa (length hlist))))
  (mark:out
    (strcat "Букв в списке: " (itoa (length *mark:letters*))
            ". Дальше АА, АБ, … БА, ББ."))

  ;; TEST 11 — номера по шкале секций
  (mark:out "")
  (cond
    ((> (length wlist) 0)
     (progn
       (mark:out "[TEST 11] Нумерация секций — OK")
       (mark:out (strcat "Размеров шкалы: " (itoa (length wlist))))))
    (t
     (progn
       (mark:out "[TEST 11] Нумерация секций — ERROR")
       (mark:out "Размеров шкалы: 0")
       (mark:note-error)))))

;;;--------------------- TEST 12 — специальные Visibility --------------

(defun mark:test-specials ()
  (mark:out "")
  (mark:out "[TEST 12] Специальные состояния Visibility")
  (mark:out "")
  (if (> *mark:cnt-stem* 0)
    (mark:out (strcat "Стемалит — найдено: " (itoa *mark:cnt-stem*)))
    (mark:out "Стемалит — не найден"))
  (if (> *mark:cnt-sand* 0)
    (mark:out (strcat "Сэндвич   — найдено: " (itoa *mark:cnt-sand*)))
    (mark:out "Сэндвич   — не найден")))

;;;--------------------- Сортировка и сборка марок ---------------------

(defun mark:sort-widths ()
  *mark:widths*)

(defun mark:sort-heights ()
  *mark:heights*)

(defun mark:get-special-suffix (vis / result sp)
  (setq result "")
  (foreach sp *mark:specials*
    (if (and (mark:strp vis)
             (or (mark:name= vis (car sp))
                 (wcmatch (strcase (mark:trim vis))
                          (strcase (strcat (car sp) "*")))))
      (setq result (cdr sp))))
  result)

(defun mark:vis-rank (vis)
  (cond
    ((and (mark:strp vis)
          (or (mark:name= vis "Стемалит")
              (wcmatch (strcase (mark:trim vis)) "СТЕМАЛИТ*")))
     1)
    ((and (mark:strp vis)
          (or (mark:name= vis "Сэндвич")
              (wcmatch (strcase (mark:trim vis)) "СЭНДВИЧ*")))
     2)
    (t 0)))

(defun mark:compose (prefix letter subl hnum subw suffix)
  ;; Ред. 48.0: БУКВА [. литера подъяруса] НОМЕР [. подындекс ширины] суффикс
  (strcat
    (if (and prefix (/= prefix "") letter)
      (strcat prefix " ")
      "")
    (if letter letter "")
    (if (and subl (/= subl "")) (strcat "." subl) "")
    (if hnum (itoa hnum) "")
    (if (and subw (numberp subw) (> subw 0))
      (strcat "." (itoa subw))
      "")
    (if suffix suffix "")))

(defun mark:build-marks (/ out r w h geo secw tierh iw ih letter hnum suffix m
                            subw subl subh mapW mapH fullH lst pos v)
  ;; Ред. 48.0: марка от секции и яруса. Подындексы — глобально по
  ;; номеру секции (.1/.2/.3 по возрастанию ширины) и букве яруса
  ;; (.а/.б/.в по возрастанию высоты). Без гео — марка не формируется.
  ;; Ред. 48.38: литера по высоте нужна только для различения нескольких
  ;; фактических высот внутри одного геометрического яруса. fullH —
  ;; ярусы, где есть заполнение полной высоты tierh.
  (setq mapW  nil
        mapH  nil
        fullH nil
        out   nil)
  (foreach r *mark:records*
    (setq geo (cdr (assoc (mark:rec-get r 'ename) *mark:fill-geo*)))
    (if (and geo (numberp (nth 0 geo)) (numberp (nth 1 geo)))
      (progn
        (setq r (mark:rec-put r 'secw (nth 0 geo))
              r (mark:rec-put r 'tierh (nth 1 geo))
              iw (mark:index-of *mark:widths* (nth 0 geo))
              ih (mark:index-of *mark:heights* (nth 1 geo))
              r (mark:rec-put r 'hidx ih)
              w  (mark:rec-get r 'width)
              h  (mark:rec-get r 'height))
        (if (and ih (mark:same-num? h (nth 1 geo)))
          (setq fullH (cons ih fullH)))
        (if (and iw (not (mark:same-num? w (nth 0 geo))))
          (setq mapW (mark:map-add mapW iw w)))
        (if (and ih (not (mark:same-num? h (nth 1 geo))))
          (setq mapH (mark:map-add mapH ih h)))
        (setq out (cons r out)))
      (progn
        (setq r (mark:rec-put r 'secw nil)
              r (mark:rec-put r 'tierh nil)
              r (mark:rec-put r 'hidx nil))
        (setq out (cons r out)))))
  ;; Ред. 48.1: foreach не сохраняет изменённые записи (rec-put возвращает
  ;; новый список) — пишем *mark:records* обратно перед вторым проходом.
  (setq *mark:records* (reverse out)
        mapW            (mark:map-sort mapW)
        mapH            (mark:map-sort mapH)
        out             nil)
  (foreach r *mark:records*
    (setq secw (mark:rec-get r 'secw)
          tierh (mark:rec-get r 'tierh))
    (if (and (numberp secw) (numberp tierh))
      (progn
        (setq iw     (mark:index-of *mark:widths* secw)
              ih     (mark:index-of *mark:heights* tierh)
              letter (if ih (mark:letter-at ih) nil)
              hnum   (if iw (1+ iw) nil)
              w      (mark:rec-get r 'width)
              h      (mark:rec-get r 'height)
              subw   nil
              subl   nil
              subh   nil)
        (if (and iw (not (mark:same-num? w secw)))
          (progn
            (setq lst (cdr (assoc iw mapW))
                  pos 0)
            (foreach v lst
              (setq pos (1+ pos))
              (if (and (null subw) (mark:same-num? v w))
                (setq subw pos)))))
        (if (and ih (not (mark:same-num? h tierh)))
          (progn
            (setq lst (cdr (assoc ih mapH))
                  pos 0)
            (foreach v lst
              (setq pos (1+ pos))
              (if (and (null subh) (mark:same-num? v h))
                (setq subh pos)))
            ;; Одна фактическая высота во всём ярусе — отдельная литера
            ;; не нужна: базовой буквы достаточно. При полной высоте или
            ;; нескольких подъячейках литеры сохраняются (.а/.б/...).
            (if (and subh
                     (or (member ih fullH)
                         (> (length lst) 1)))
              (setq subl (strcase (mark:letter-at (- subh 1)) t)))))
        (setq r (mark:rec-put r 'letter letter)
              r (mark:rec-put r 'hnum hnum)
              r (mark:rec-put r 'subw subw)
              r (mark:rec-put r 'subl subl)
              r (mark:rec-put r 'subh subh)))
      (setq r (mark:rec-put r 'letter nil)
            r (mark:rec-put r 'hnum nil)
            r (mark:rec-put r 'subw nil)
            r (mark:rec-put r 'subl nil)
            r (mark:rec-put r 'subh nil)))
    (setq suffix (mark:get-special-suffix (mark:rec-get r 'vis))
          m      (if (and (mark:rec-get r 'letter)
                          (numberp (mark:rec-get r 'hnum)))
                   (mark:compose (mark:vz-rec-pfx r)
                                 (mark:rec-get r 'letter)
                                 (mark:rec-get r 'subl)
                                 (mark:rec-get r 'hnum)
                                 (mark:rec-get r 'subw)
                                 suffix)
                   nil)
          r      (mark:rec-put r 'mark-new m)
          out    (cons r out)))
  (setq *mark:records* (reverse out))
  *mark:records*)

;;;=====================================================================
;;; ШКАЛА НОМЕРОВ ПО РИГЕЛЯМ (Ред. 48.37)
;;; Цифра марки заполнения = номер ригеля «Рг N» в MarkZV.
;;; Правило (решение заказчика 2026-11-05): цифра ригеля и цифра
;;; горизонтальной рядовки заполнений должны совпадать, поэтому номер
;;; заполнения берётся из шкалы длин ригелей, а не из порядка столбцов.
;;;   *mark:beam-scale*     — номера ригелей: ((длина . N) ...) по возрастанию
;;;   *mark:widths*         — шкала номеров секций: длины ригелей, затем
;;;                           ширины столбцов без ригеля (Блок 2, «З»)
;;;   *mark:scale-shift*    — сдвиг сквозной нумерации (Блок 2 продолжает Блок 1)
;;;   *mark:num-was*        — номер при нумерации по столбцам (48.20), для сверки
;;;=====================================================================
(setq *mark:beam-scale*  nil)
(setq *mark:scale-shift* 0)
(setq *mark:num-was*     nil)

;; Длины ригелей, отсечённые порогом элемента каркаса (50 мм, как MarkZV)
(defun mark:scale-beam-keys (/ out k)
  (setq out nil)
  (foreach k *mark:scale1*
    (if (and (numberp k) (> k 50))
      (setq out (cons k out))))
  (reverse out))

;; Столбцы (ширины секций) из геометрии заполнений — по возрастанию.
;; Чистая функция: не зависит от *mark:widths* (её перезаписывает рядовка).
(defun mark:sec-ws (/ ws geo)
  (setq ws nil)
  (foreach geo *mark:fill-geo*
    (if (and (cdr geo) (numberp (cadr geo)))
      (setq ws (cons (float (mark:sec-key (cadr geo))) ws))))
  (mark:sort-unique (mark:positives ws)))

;; Шкала номеров секций, чистая: длины ригелей (как «Рг N» в MarkZV),
;; затем ширины столбцов, которым ригеля той же длины не нашлось (Блок 2).
(defun mark:num-keys (/ rk ws out hit)
  (setq rk  (mark:scale-beam-keys)
        ws  (mark:sec-ws)
        out rk)
  (foreach k ws
    (setq hit nil)
    (foreach ln rk
      (if (mark:same-num? ln k)
        (setq hit t)))
    (if (null hit)
      (setq out (cons k out))))
  ;; rk уже по возрастанию; добавленные столбцы — тоже, но хвост надо
  ;; отсортировать отдельно, поэтому сортируем целиком
  (mark:sort-unique out))

;; Шкала номеров: заполняет *mark:beam-scale* (длина . номер) и сдвиг,
;; *mark:widths* — итоговая шкала номеров (номера = позиция + 1).
(defun mark:num-scale (/ rk ws out k n)
  (setq ws (mark:num-keys)
        rk (mark:scale-beam-keys))
  (setq out nil
        n   0)
  (foreach k rk
    (setq n   (1+ n)
          out (cons (cons k n) out)))
  (setq *mark:beam-scale*  (reverse out)
        *mark:scale-shift* n
        *mark:widths*      ws)
  ws)

;; Номер по ширине — из той же шкалы, что у марок (ригели, затем Блок 2).
;; Нужен рядовке (блок без атрибута «Марка»): цифра заполнения и цифра
;; горизонтальной рядовки обязаны совпадать.
(defun mark:num-of-width (w / i)
  (setq i nil)
  (if (numberp w)
    (setq i (mark:index-of (mark:num-keys) w)))
  (if i (itoa (1+ i)) nil))

;; Совпадает ли номер секции (n) с номером ригеля той же ширины
(defun mark:num-match? (n w / i)
  (setq i nil)
  (if (and (numberp n) (numberp w))
    (setq i (mark:index-of (mark:num-keys) w)))
  (and i (= n (1+ i))))

;; Сдвиг номеров относительно нумерации по столбцам (Ред. 48.20).
;; Заказчику важно видеть, какие секции сменили номер при переходе на
;; шкалу ригелей: «550: №1 -> №2». Много строк — режем по лимиту.
(defun mark:num-shift-out (/ i old new lst cnt keys)
  (setq lst  nil
        keys (mark:num-keys)
        i    0)
  (foreach old *mark:num-was*
    (setq i   (1+ i)
          new (mark:index-of keys old))
    (if new
      (if (/= (1+ new) i)
        (setq lst (cons (strcat (rtos old 2 0) ": №" (itoa i)
                                " -> №" (itoa (1+ new)))
                        lst)))))
  (setq lst (reverse lst)
        cnt (length lst))
  (if (> cnt 0)
    (progn
      (mark:out "")
      (mark:out
        (strcat "[INFO] Смена номеров против нумерации по столбцам (48.20): "
                (itoa cnt)
                (if (> *mark:scale-shift* 0)
                  (strcat ", ригелей в шкале " (itoa *mark:scale-shift*))
                  "")))
      (setq i 0)
      (foreach k lst
        (setq i (1+ i))
        (if (<= i *mark:max-err-lines*)
          (mark:out (strcat "       " k))))
      (if (> cnt *mark:max-err-lines*)
        (mark:out (strcat "       ... далее "
                          (itoa (- cnt *mark:max-err-lines*)) " строк")))))
  cnt)

;; Таблица соответствия для лога: «Рг N = размер -> №N» по ригелям и
;; «№N = размер (З)» по столбцам без ригеля. Строк много — режем по
;; *mark:max-err-lines*, как TEST 13/16.
(defun mark:num-table-out (/ rows i cnt k beam)
  (setq rows nil
        i    0)
  (foreach k (mark:num-keys)
    (setq i    (1+ i)
          beam nil)
    (foreach e *mark:beam-scale*
      (if (mark:same-num? (car e) k)
        (setq beam (cdr e))))
    (setq rows
      (cons
        (if beam
          (strcat "       Рг" (itoa beam) " = " (rtos (float k) 2 0)
                  "  ->  №" (itoa i))
          (strcat "       №" (itoa i) " = " (rtos (float k) 2 0)
                  "  (З: столбец без ригеля)"))
        rows)))
  (setq rows (reverse rows)
        cnt  (length rows))
  (if (> cnt 0)
    (progn
      (mark:out "")
      (mark:out
        (strcat "[INFO] Соответствие номеров (MarkZV Рг  и  марка заполнения): "
                (itoa cnt)))
      (setq i 0)
      (foreach k rows
        (setq i (1+ i))
        (if (or (<= cnt *mark:max-err-lines*)
                (<= i *mark:max-err-lines*))
          (mark:out k)))
      (if (> cnt *mark:max-err-lines*)
        (mark:out (strcat "       ... далее "
                          (itoa (- cnt *mark:max-err-lines*)) " строк")))))
  rows)

;;;--------------------- TEST 13 — марки в памяти ----------------------

(defun mark:num-safe (x)
  (cond
    ((numberp x) (float x))
    (t
     (setq x (mark:numval x))
     (if (numberp x)
       (float x)
       0.0))))

(defun mark:rec-key (r / w h vr ix sw sh pw pl)
  ;; Ред. 48.0: секция -> подындекс -> ярус -> подъярус -> Visibility
  (setq w  (mark:num-safe (mark:rec-get r 'secw))
        h  (mark:num-safe (mark:rec-get r 'tierh))
        sw (mark:rec-get r 'subw)
        sh (mark:rec-get r 'subh)
        pw (mark:num-safe (mark:rec-get r 'width))
        pl (mark:num-safe (mark:rec-get r 'height))
        vr (mark:vis-rank (mark:rec-get r 'vis))
        ix (mark:rec-get r 'idx))
  (list (if (> w 0.0) w pw)
        (if (numberp sw) (float sw) 0.0)
        (if (> h 0.0) h pl)
        (if (numberp sh) (float sh) 0.0)
        (if (numberp vr) (float vr) 0.0)
        (if (numberp ix) (float ix) 0.0)))

(defun mark:key< (a b / i av bv)
  ;; поэлементное сравнение только чисел (никогда T/строки)
  (setq i 0)
  (while (< i 4)
    (setq av (nth i a)
          bv (nth i b))
    (if (not (and (numberp av) (numberp bv)))
      (progn
        (setq av (mark:num-safe av)
              bv (mark:num-safe bv))))
    (cond
      ((< av bv)
       (setq i 10)
       (setq mark:key<:result t))
      ((> av bv)
       (setq i 10)
       (setq mark:key<:result nil))
      (t
       (setq i (1+ i)))))
  (if (> i 4)
    mark:key<:result
    nil))

(defun mark:sort-records (recs / res r inserted lst)
  ;; сортировка для вывода: ширина ^, высота ^, Visibility (спец. — в конце)
  (setq res nil)
  (foreach r recs
    (setq inserted nil
          lst      nil)
    (foreach x res
      (if (and (null inserted)
               (mark:key< (mark:rec-key r) (mark:rec-key x)))
        (progn
          (setq lst      (cons r lst)
                inserted t)))
      (setq lst (cons x lst)))
    (if (null inserted)
      (setq lst (cons r lst)))
    (setq res (reverse lst)))
  res)

;;;--- DIAG: одинаковый размер — разный базис (стм/снд vs обычное) ----
;; Группирует записи по ключу (W,H) с допуском 0.5 мм.
;; В группе сравнивает (letter . hnum) — базис без суффикса.
(defun mark:diag-base (/ groups g gg ng key r w h base bases
                             lines shown total conflicts qcnt
                             letter hnum suffix
                             kinds kind)
  (mark:out "")
  (mark:out "[DIAG] Одинаковый размер > один базис (буква+литера+номер+подындекс)")
  (setq groups nil)
  (foreach r *mark:records*
    (setq w (mark:rec-get r 'width)
          h (mark:rec-get r 'height))
    (if (and (numberp w) (numberp h))
      (progn
        (setq key (list (mark:dim-key w) (mark:dim-key h))
              g   nil)
        (foreach gg groups
          (if (equal (car gg) key)
            (setq g gg)))
        (if g
          (progn
            (setq ng nil)
            (foreach gg groups
              (setq ng
                (cons (if (equal (car gg) key)
                        (append (list (car gg)) (list r) (cdr gg))
                        gg)
                      ng)))
            (setq groups (reverse ng)))
          (setq groups (cons (list key r) groups))))))
  (setq conflicts 0
        qcnt      0
        lines     nil)
  (foreach g groups
    (setq bases nil
          kinds  nil)
    (foreach r (cdr g)
      (setq letter (mark:rec-get r 'letter)
            hnum   (mark:rec-get r 'hnum)
            suffix (mark:get-special-suffix (mark:rec-get r 'vis))
            base   (list (if letter letter "?")
                         (if (mark:rec-get r 'subl)
                           (strcat "." (mark:rec-get r 'subl))
                           "")
                         (if hnum (itoa hnum) "?")
                         (if (mark:rec-get r 'subw)
                           (strcat "." (itoa (mark:rec-get r 'subw)))
                           "")))
      ;; Ред. 48.9: считаем базисы с «?» (буква/номер не определены).
      (if (or (null letter) (null hnum))
        (setq qcnt (1+ qcnt)))
      (if (not (member base bases))
        (setq bases (cons base bases)))
      (setq kind (if (= suffix "") "обычное" suffix))
      (if (not (member kind kinds))
        (setq kinds (cons kind kinds))))
    (if (> (length bases) 1)
      (progn
        (setq conflicts (1+ conflicts))
        (setq lines
          (cons (strcat
                  "  W=" (rtos (car (car g)) 2 0)
                  " H=" (rtos (cadr (car g)) 2 0)
                  " half-mm > базисы: "
                  (mark:diag-bases-str bases)
                  " | типы: " (mark:diag-join kinds))
                lines)))))
  (if (null lines)
    (mark:out "[DIAG] OK — расхождений базиса нет.")
    (progn
      (mark:out (strcat "[DIAG] КОНФЛИКТОВ: " (itoa conflicts)))
      (setq lines (reverse lines)
            shown 0
            total (length lines))
      (foreach ln lines
        (if (< shown 12)
          (progn
            (setq shown (1+ shown))
            (mark:out ln))))
      (if (> total shown)
        (mark:out (strcat "  ... ещё " (itoa (- total shown)) " строк"))
        nil)
      (mark:out "[DIAG] Причина: разные фактические W/H у блоков")
      (mark:out "        (сверьте «Ширина/Высота в свету» у конфликтов).")))
  ;; Ред. 48.9: подсказка — что значит «?» в базисе (только печать).
  (if (> qcnt 0)
    (progn
      (mark:out (strcat "[DIAG] «?» в базисе: " (itoa qcnt)
                        " — буква строки или номер секции не определены."))
      (mark:out "        Таким блокам марки не пишутся — подробности в TEST 13:")
      (mark:out "        нет гео секции / нет буквы строки / нет номера шкалы.")))
  conflicts)

(defun mark:diag-bases-str (bases / out b)
  (setq out "")
  (foreach b bases
    (setq out
      (if (= out "")
        (strcat (car b) (cadr b))
        (strcat out ", " (car b) (cadr b)))))
  out)

(defun mark:diag-join (ks / out k)
  (setq out "")
  (foreach k ks
    (setq out
      (if (= out "") k (strcat out "+" k))))
  out)

(defun mark:validate-marks (/ bad nogeo sorted r shown total lines vis)
  (mark:out "")
  ;; Ред. 48.5: без гео секции — предупреждение (блок уже помечен
  ;; «вне ячеек» в standalone), не блокирует запись остальных марок.
  (setq bad   nil
        nogeo nil)
  (foreach r *mark:records*
    (if (null (mark:rec-get r 'mark-new))
      (if (null (mark:rec-get r 'secw))
        (setq nogeo
          (cons
            (strcat "Блок №" (itoa (mark:rec-get r 'idx))
                    ": нет гео секции — марка не формируется.")
            nogeo))
        (setq bad
          (cons
            (strcat "Блок №" (itoa (mark:rec-get r 'idx)) ": марка не сформирована"
                    (cond
                      ((null (mark:rec-get r 'letter)) " — нет буквы строки.")
                      ((null (mark:rec-get r 'hnum)) " — нет номера шкалы.")
                      (t ".")))
            bad)))))
  (setq nogeo (reverse nogeo)
        bad   (reverse bad))
  (if nogeo
    (progn
      (mark:out
        (strcat "[TEST 13] Без гео секции: " (itoa (length nogeo))
                " — марки таким блокам не пишутся (предупреждение)."))
      (mark:print-limited nogeo)
      (mark:note-warning)))
  (cond
    (bad
     (progn
       (mark:out "[TEST 13] Проверка формирования марок — ERROR")
       (mark:print-limited bad)
       (mark:note-error)))
    (t
     (progn
       (mark:out "[TEST 13] Проверка формирования марок — OK")
       (mark:out "")
       (if (not (numberp *mark:test13-show*))
         (setq *mark:test13-show* 10))
       (setq sorted (vl-catch-all-apply 'mark:sort-records
                                        (list *mark:records*)))
       (if (or (vl-catch-all-error-p sorted) (null sorted))
         (setq sorted *mark:records*))
       (setq total (length sorted)
             shown 0)
       (foreach r sorted
         (if (and (< shown *mark:test13-show*)
                  (mark:rec-get r 'mark-new))
           (progn
             (setq shown (1+ shown))
             ;; Одна строка на блок, колонки выровнены по ширине.
             (setq vis (if (mark:rec-get r 'vis)
                         (mark:rec-get r 'vis)
                         "—"))
             (mark:out
               (strcat "Блок " (mark:rjust (itoa (mark:rec-get r 'idx)) 3) ": "
                       (mark:rjust (mark:fmt-mm (mark:rec-get r 'width)) 5)
                       " x "
                       (mark:rjust (mark:fmt-mm (mark:rec-get r 'height)) 5)
                       " / "
                       vis
                       (substr "              " 1
                               (max 1 (- 14 (strlen vis))))
                       " > "
                       (mark:rec-get r 'mark-new))))))
       (if (> total *mark:test13-show*)
         (mark:out
           (strcat "... (показаны первые " (itoa *mark:test13-show*)
                   " из " (itoa total) ")")))
       (mark:out (strcat "Всего блоков: " (itoa total)))
       (mark:out (strcat "Сформировано марок: "
                         (itoa (- total (length nogeo)))))
       (if nogeo
         (mark:out (strcat "Пропущено без гео: "
                           (itoa (length nogeo)))))))))

;;;--------------------- TEST 14 — PRE-CHECK ---------------------------

(defun mark:pre-check ()
  (mark:out "")
  (cond
    ((= *mark:errors* 0)
     (progn
       (setq *mark:precheck-ok* t)
       (mark:out "[TEST 14] PRE-CHECK — OK")
       (mark:out "Критических ошибок: 0")
       (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
       (mark:out "Готово к записи атрибутов.")))
    (t
     (progn
       (setq *mark:precheck-ok* nil)
       (mark:out "[TEST 14] PRE-CHECK — FAILED")
       (mark:out (strcat "Критических ошибок: " (itoa *mark:errors*)))
       (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
       (mark:out "Маркировка отменена.")
       (mark:out "Изменений в чертеже не выполнено.")))))



;;;--------------------- Глубокий разбор структуры --------------------

(defun mark:tags-str (tags)
  (if tags (vl-princ-to-string tags) "(нет)"))

;; Обычные свойства — проверка, что COM работает
(defun mark:debug-std-props (obj)
  (mark:out (strcat "  Layer: "
                    (vl-princ-to-string (mark:ax-get obj "Layer"))))
  (mark:out (strcat "  Color: "
                    (vl-princ-to-string (mark:ax-get obj "Color")))))

;; Проба списка имён Dynamic Properties
(defun mark:debug-try-names (obj names / cand r okn bad)
  (setq okn nil)
  (foreach cand names
    (setq r (mark:get-dyn obj cand))
    (if (car r)
      (setq okn (cons (strcat cand " = " (vl-princ-to-string (cadr r)))
                      okn))))
  (if okn
    (progn
      (mark:out "  Успешные имена Dynamic Properties:")
      (foreach x (reverse okn)
        (mark:out (strcat "    OK  " x))))
    (mark:out "  Успешных имён среди проб — нет")))

;; GetDynamicBlockProperties (метод есть не во всех версиях)
(defun mark:debug-enum-dyn (obj / r v lst item pair n)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (list obj "GetDynamicBlockProperties")))
  (cond
    ((vl-catch-all-error-p r)
     (mark:out (strcat "  GetDynamicBlockProperties: FAIL — "
                       (vl-catch-all-error-message r))))
    ((null r)
     (mark:out "  GetDynamicBlockProperties: nil"))
    (t
     (progn
       (mark:out "  GetDynamicBlockProperties: получен")
       (mark:out (strcat "    type = " (vl-princ-to-string (type r))))
       (setq v (mark:unwrap r))
       (mark:out (strcat "    unwrap type = " (vl-princ-to-string (type v))))
       ;; safearray > list (только эта функция)
       (setq lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
       (cond
         ((vl-catch-all-error-p lst)
          (mark:out (strcat "    safearray->list FAIL: "
                            (vl-catch-all-error-message lst))))
         ((null lst)
          (mark:out "    safearray->list: пустой список"))
         ((not (listp lst))
          (mark:out (strcat "    safearray->list: не список, type="
                            (vl-princ-to-string (type lst)))))
         (t
          (progn
            (mark:out (strcat "    элементов: "
                              (itoa (length lst))))
            (setq n 0)
            (foreach item lst
              (setq n (1+ n))
              (if (<= n 30)
                (progn
                  (setq item (mark:unwrap item)
                        pair (mark:dyn-item-pair item))
                  (if pair
                    (mark:out
                      (strcat "      * " (car pair)
                              " = " (vl-princ-to-string (cdr pair))))
                    (mark:out
                      (strcat "      ? item type="
                              (vl-princ-to-string (type item))
                              " PropertyName="
                              (vl-princ-to-string
                                (mark:ax-get item "PropertyName"))
                              " Name="
                              (vl-princ-to-string
                                (mark:ax-get item "Name"))))))))))
       ;; итог через общий загрузчик
       (setq pair (mark:load-dyn-pairs obj))
       (if pair
         (progn
           (mark:out "    --- mark:load-dyn-pairs ---")
           (foreach item pair
             (mark:out (strcat "      = " (car item)
                               " : " (vl-princ-to-string (cdr item))))))
         (mark:out "    load-dyn-pairs: nil")))))))


(defun mark:debug-entget (e / pair k v)
  (mark:out "  --- entget (ключевые группы) ---")
  (foreach pair (entget e)
    (setq k (car pair)
          v (cdr pair))
    (if (member k '(-2 -1 0 2 8 60 62 66 280 284 410 1 3 7 1000 1001 1070 1071 1040))
      (mark:out (strcat "    " (vl-princ-to-string k) " = "
                        (vl-princ-to-string v))))))

;; XDATA 1000–1071
(defun mark:debug-xdata (e / pair items)
  (mark:out "  --- XDATA 1000-1071 ---")
  (setq items nil)
  (foreach pair (entget e)
    (if (and (numberp (car pair))
             (>= (car pair) 1000)
             (<= (car pair) 1071))
      (setq items (cons pair items))))
  (if items
    (foreach pair (reverse items)
      (mark:out (strcat "    " (vl-princ-to-string pair))))
    (mark:out "    (нет)")))

;; Перечисление ATTDEF/ATTRIB/TEXT в определении блока (только имена типов)
(defun mark:debug-block-def (bname / doc blocks rec cnt i item tn en data)
  (cond
    ((null (mark:strp bname))
     (mark:out "  block-def: имя не строка"))
    (t
     (progn
       (mark:out (strcat "  block-def: " bname))
       (setq doc    (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
             blocks (mark:ax-get doc "Blocks")
             rec    (mark:ax-invoke blocks "Item" bname))
       (if (null rec)
         (mark:out "    Blocks.Item > nil")
         (progn
           (setq cnt (mark:ax-get rec "Count"))
           (mark:out (strcat "    Entities: " (vl-princ-to-string cnt)))
           (if (and (numberp cnt) (> cnt 0))
             (progn
               (setq i 0)
               (while (< i (min cnt 100))
                 (setq item (mark:ax-invoke rec "Item" i)
                       tn   (if item (mark:ax-get item "ObjectName") nil)
                       en   (if item
                              (vl-catch-all-apply 'vlax-vla-object->ename
                                                  (list item))
                              nil))
                 (if (vl-catch-all-error-p en) (setq en nil))
                 (setq data (if en (entget en) nil))
                 (mark:out
                   (strcat "    [" (itoa i) "] "
                           (vl-princ-to-string tn)
                           (if data
                             (strcat " DXF0="
                                     (vl-princ-to-string
                                       (cdr (assoc 0 data)))
                                     (if (cdr (assoc 2 data))
                                       (strcat " tag/name="
                                               (vl-princ-to-string
                                                 (cdr (assoc 2 data))))
                                       "")
                                     (if (cdr (assoc 1 data))
                                       (strcat " text="
                                               (vl-princ-to-string
                                                 (cdr (assoc 1 data))))
                                       ""))
                             " (нет ename/entget)")))
                 (setq i (1+ i)))))))))))

(defun mark:debug-entnext-chain (e / n data n15)
  (mark:out "  --- entnext после INSERT (66=1) ---")
  (setq n (entnext e)
        n15 0)
  (while (and n (< n15 30))
    (setq data (entget n)
          n15  (1+ n15))
    (if (null data)
      (setq n nil)
      (progn
        (mark:out (strcat "    "
                          (vl-princ-to-string (cdr (assoc 0 data)))
                          (if (cdr (assoc 2 data))
                            (strcat " tag=" (vl-princ-to-string (cdr (assoc 2 data))))
                            "")
                          (if (cdr (assoc 1 data))
                            (strcat " val=" (vl-princ-to-string (cdr (assoc 1 data))))
                            "")))
        (if (mark:name= (cdr (assoc 0 data)) "SEQEND")
          (setq n nil)
          (setq n (entnext n))))))
  (if (= n15 0)
    (mark:out "    (entnext дал другой объект/конец)")))

(defun mark:debug-attr-values (e / ent data tag val)
  (mark:out "  --- Значения ATTRIB (entnext) ---")
  (setq ent (entnext e))
  (while (and ent)
    (setq data (entget ent))
    (cond
      ((null data)
       (setq ent nil))
      ((mark:name= (cdr (assoc 0 data)) "ATTRIB")
       (progn
         (setq tag (cdr (assoc 2 data))
               val (cdr (assoc 1 data)))
         (mark:out (strcat "    " (vl-princ-to-string tag)
                           " = " (vl-princ-to-string val)))
         (setq ent (entnext ent))))
      ((mark:name= (cdr (assoc 0 data)) "SEQEND")
       (setq ent nil))
      (t
       (setq ent nil)))))

(defun mark:debug-one-fill (/ e obj tags tags2)
  (if *mark:fills*
    (progn
      (setq e   (car *mark:fills*)
            obj (mark:vla e))
      (mark:out "  --- Блок заполнения №1 ---")
      (mark:out (strcat "  DXF-2: "
                        (vl-princ-to-string (cdr (assoc 2 (entget e))))))
      (mark:out (strcat "  Name: "
                        (vl-princ-to-string (mark:ax-get obj "Name"))))
      (mark:out (strcat "  EffectiveName: "
                        (vl-princ-to-string (mark:ax-get obj "EffectiveName"))))
      (mark:debug-std-props obj)
      (mark:out (strcat "  INSERT -2: "
                        (vl-princ-to-string (cdr (assoc -2 (entget e))))))
      (setq tags  (mark:attr-tags e)
            tags2 (mark:attr-tags-ax obj))
      (mark:out (strcat "  ATTRIB DXF: " (mark:tags-str tags)))
      (mark:out (strcat "  ATTRIB AX:  " (mark:tags-str tags2)))
      (mark:debug-attr-values e)
      (mark:debug-try-names
        obj
        (list *mark:prop-width*
              *mark:prop-height*
              "Ширина_в_свету"
              "Ширина"
              "Высота"
              "Width"
              "Height"
              "Visibility"
              "Visibility1"
              "Видимость"
              "Марка"
              "Marka"))
      (mark:debug-enum-dyn obj)
      (mark:debug-entnext-chain e)
      (mark:debug-entget e)
      (mark:debug-xdata e)
      (mark:debug-block-def (mark:ax-get obj "EffectiveName")))))

(defun mark:debug-one-glazing (/ e obj tags tags2)
  (if *mark:glazings*
    (progn
      (setq e   (car *mark:glazings*)
            obj (mark:vla e))
      (mark:out "  --- Атрибуты витража ---")
      (mark:out (strcat "  DXF-2: "
                        (vl-princ-to-string (cdr (assoc 2 (entget e))))))
      (mark:out (strcat "  EffectiveName: "
                        (vl-princ-to-string (mark:ax-get obj "EffectiveName"))))
      (mark:debug-std-props obj)
      (mark:out (strcat "  INSERT -2: "
                        (vl-princ-to-string (cdr (assoc -2 (entget e))))))
      (setq tags  (mark:attr-tags e)
            tags2 (mark:attr-tags-ax obj))
      (mark:out (strcat "  ATTRIB DXF: " (mark:tags-str tags)))
      (mark:out (strcat "  ATTRIB AX:  " (mark:tags-str tags2)))
      (mark:debug-try-names obj (list "Витраж" "Vitrage" "Марка" "Visibility"))
      (mark:debug-enum-dyn obj)
      (mark:debug-entnext-chain e)
      (mark:debug-entget e)
      (mark:debug-xdata e)
      (mark:debug-block-def (mark:ax-get obj "EffectiveName")))))

(defun mark:debug-structure ()
  (mark:out "")
  (mark:out "[DEBUG] Разбор структуры (только чтение)...")
  (mark:debug-one-fill)
  (mark:debug-one-glazing))

;;;--------------------- Диагностика (безопасный запуск) ---------------

(defun mark:safe (fn label / r)
  (setq r (vl-catch-all-apply fn nil))
  (if (vl-catch-all-error-p r)
    (progn
      (mark:out (strcat "[ERROR] Сбой на этапе " label ":"))
      (mark:out (strcat "       " (vl-catch-all-error-message r)))
      (mark:note-error)
      nil)
    r))

(defun mark:diagnostics ()
  (mark:safe 'mark:test-fillings            "TEST 01")
  (mark:safe 'mark:test-glazing-attr        "TEST 02/03")
  (if *mark:records*
    (progn
      (mark:safe 'mark:test-dynamic-properties "TEST 04/05/06")
      (mark:safe 'mark:test-visibility         "TEST 07")
      (mark:safe 'mark:test-mark-attribute     "TEST 08")
      (mark:safe 'mark:test-data               "TEST 09/10/11")
      (mark:safe 'mark:test-specials           "TEST 12")
      (if (> *mark:errors* 0)
        (mark:safe 'mark:debug-structure        "DEBUG")))
    (progn
      (mark:out "")
      (mark:out "[TEST 04] \"Ширина в свету\" — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 05] \"Высота в свету\" — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 06] Значения размеров — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 07] Visibility State — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 08] Атрибут \"Марка\" — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 09] Уникальные размеры — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 10] Буквенная маркировка — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 11] Нумерация ширин — SKIPPED (нет блоков заполнений)")
      (mark:out "[TEST 12] Специальные состояния — SKIPPED (нет блоков заполнений)"))))

;;;--------------------- Этап B — запись атрибутов ---------------------

(defun mark:apply-impl (/ cnt failed r e obj aobj aent newm old)
  (setq cnt    0
        failed 0)
  (foreach r *mark:records*
    (setq newm (mark:rec-get r 'mark-new))
    (if newm
      (progn
        (setq e    (mark:rec-get r 'ename)
              obj  (mark:vla e)
              aent (mark:find-attr-in-ename e *mark:attr-mark*)
              aobj (if aent
                     nil
                     (mark:find-attr-obj obj *mark:attr-mark*)))
          (cond
            ;; запись через DXF (entmod) — без ActiveX
            (aent
             (progn
               (setq old (cdr (assoc 1 (entget aent))))
               (if (or (null old) (not (mark:strp old)) (/= old newm))
                 (progn
                   (entmod (subst (cons 1 newm) (assoc 1 (entget aent))
                                  (entget aent)))
                   (entupd aent)))
               (setq cnt (1+ cnt))))
            ;; фолбэк ActiveX
            (aobj
             (progn
               (setq old (mark:ax-get aobj "TextString"))
               (if (or (null old) (not (mark:strp old)) (/= old newm))
                 (mark:ax-put aobj "TextString" newm))
               (setq cnt (1+ cnt))))
            (t
             (setq failed (1+ failed)))))))
  (setq *mark:written*  cnt
        *mark:apply-failed* failed)
  cnt)

(defun mark:apply-marks (/ doc r1 r2)
  (mark:out "")
  (mark:out "[INFO] Начинается запись атрибутов...")
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
  ;; В МАРКАЗАП внешний маркер открыт — свой не открываем
  (if (and doc (not *mark:batch-undo*))
    (mark:ax-invoke-ok doc "StartUndoMark" nil))
  ;; EndUndoMark закрывается всегда — один шаг UNDO отменяет сеанс записи
  (setq r1 (vl-catch-all-apply 'mark:apply-impl nil))
  (setq r2 (if (and doc (not *mark:batch-undo*))
             (mark:ax-invoke-ok doc "EndUndoMark" nil)
             t))
  (cond
    ((vl-catch-all-error-p r1)
     (progn
       (mark:out "[ERROR] Сбой при записи атрибутов:")
       (mark:out (strcat "       " (vl-catch-all-error-message r1)))
       (mark:note-error)
       (mark:out (strcat "[INFO] Выполнено записей: " (itoa *mark:written*)))
       (mark:out "[INFO] При необходимости выполните UNDO (один шаг).")
       nil))
    ((null r2)
     (progn
       (mark:out "[WARN] Не удалось закрыть Undo-маркер.")
       (mark:note-warning)
       r1))
    (t
     (progn
       (if (> *mark:apply-failed* 0)
         (progn
           (mark:out
             (strcat "[ERROR] Не удалось записать: "
                     (itoa *mark:apply-failed*)))
           (mark:note-error)))
       r1))))

;;;--------------------- Итоговый отчёт --------------------------------

(defun mark:report (success)
  (mark:out "")
  (mark:out "========================================")
  (mark:out (strcat " ИТОГ  " *mark:rev*))
  (mark:out "========================================")
  (mark:out (mark:pad-line "Выбрано объектов:" (itoa *mark:sel-total*)))
  (mark:out
    (mark:pad-line "Заполнений найдено:" (itoa (length *mark:fills*))))
  (mark:out
    (mark:pad-line "\"Атрибуты витража\":"
                   (itoa (length *mark:glazings*))))
  ;; Ред. 48.30: совместная маркировка — перечень витражей
  (if (and *mark:vz-map* (> (length (mark:vz-map-pfxs)) 1))
    (mark:out
      (mark:pad-line "Витражи:"
                     (mark:vz-join (mark:vz-map-pfxs) ", ")))
    (mark:out
      (mark:pad-line "Витраж:"
                     (if *mark:prefix-found* *mark:prefix* "—"))))
  (mark:out
    (mark:pad-line "Шкала секций (номера):"
                   (itoa (length *mark:widths*))))
  (mark:out
    (mark:pad-line "Номера по ригелям (Рг):"
                   (itoa (length *mark:beam-scale*))))
  (mark:out
    (mark:pad-line "Шкала строк (буквы):"
                   (itoa (length *mark:heights*))))
  (mark:out (mark:pad-line "Стемалит:" (itoa *mark:cnt-stem*)))
  (mark:out (mark:pad-line "Сэндвич:"  (itoa *mark:cnt-sand*)))
  (mark:out
    (mark:pad-line "Марки успешно записаны:" (itoa *mark:written*)))
  (if (> *mark:apply-failed* 0)
    (mark:out
      (mark:pad-line "Ошибок записи:" (itoa *mark:apply-failed*))))
  (mark:out (mark:pad-line "Ошибок:" (itoa *mark:errors*)))
  (mark:out (mark:pad-line "Предупреждений:" (itoa *mark:warnings*)))
  (mark:out "========================================")
  (if success
    (mark:out "[INFO] Готово.")
    (progn
      (mark:out "[INFO] Маркировка отменена.")
      (mark:out "[INFO] Изменений в чертеже не выполнено."))))

;;;--------------------- Главная процедура -----------------------------

(defun mark:main (/ r)
  (mark:cmd-line
    "МАРКАРОВКАЗАП — марки блоков в «Заполнение в витраж».")
  (mark:reset-state)
  (mark:banner)

  ;; ===== Этап A: подготовка (без изменения чертежа) =====
  (if (null (mark:select))
    (princ)

    (progn
      ;; Ред. 48.0: без раскладки данных о секциях нет — просим каркас.
      (if (null *mark:fill-geo*)
        (setq r (vl-catch-all-apply 'mark:sec-standalone nil)))
      (if (and (null *mark:fill-geo*) *mark:fills*)
        (progn
          (mark:out
            "[ERROR] Нет данных о секциях: запустите МАРКАЗАПБЛОК")
          (mark:out
            "        или укажите каркас по запросу программы.")
          (mark:note-error)))
      ;; A1. чтение данных (read-only)
      (setq r (vl-catch-all-apply 'mark:collect-data nil))
      (if (vl-catch-all-error-p r)
        (progn
          (mark:out "[ERROR] Сбой чтения данных блоков:")
          (mark:out (strcat "       " (vl-catch-all-error-message r)))
          (mark:note-error)))

      ;; A2. самодиагностика TEST 01 ... TEST 12
      (mark:diagnostics)

      ;; A3. расчёт марок в памяти + TEST 13
      (mark:out "")
      (cond
        ((= *mark:errors* 0)
         (progn
           (mark:out
             "[INFO] Критических ошибок нет — формирование марок в памяти...")
           (setq r (vl-catch-all-apply 'mark:build-marks nil))
           (if (vl-catch-all-error-p r)
             (progn
               (mark:out "[TEST 13] Проверка формирования марок — ERROR")
               (mark:out (strcat "       " (vl-catch-all-error-message r)))
               (mark:note-error))
             (mark:safe 'mark:validate-marks "TEST 13"))))
        (t
         (progn
           (mark:out "[TEST 13] Проверка формирования марок — SKIPPED")
           (mark:out
             "[INFO] Есть критические ошибки диагностики — марки не формируются."))))

      ;; A3a. TEST 16 — секции и ярусы (Ред. 48.0)
      (if (and (= *mark:errors* 0) *mark:fill-geo* *mark:records*)
        (mark:safe 'mark:test-16 "TEST 16"))

      ;; A3a2. Ред. 48.37: соответствие «номер марки = номер ригеля»
      (if (and (= *mark:errors* 0) *mark:beam-scale*)
        (mark:safe 'mark:num-table-out "соответствие номеров"))

      ;; A3b. диагностика стм/снд vs обычное
      (if (and (= *mark:errors* 0) *mark:records*)
        (vl-catch-all-apply 'mark:diag-base nil))

      ;; A4. финальный контроль
      (mark:pre-check)

      ;; ===== Этап B: применение — только после PRE-CHECK OK =====
      (if *mark:precheck-ok*
        (progn
          (setq r (vl-catch-all-apply 'mark:apply-marks nil))
          (if (or (vl-catch-all-error-p r) (null r))
            (mark:report nil)
            (mark:report t)))
        (mark:report nil))))

  (princ))


;;;=====================================================================
;;;  MARKTABLE — обратное извлечение заполнений в таблицу / XLS / CSV
;;;  Команды: MARKTABLE / МАРКАЗАПТАБЛ
;;;  Столбец «Марка» читается из атрибута блока «Заполнение в витраж».
;;;  Размеры: припуск +26 мм (как в ZAPOLNENIE), округление fix().
;;;=====================================================================

;; *mtab:allowance* *mtab:h-keys* *mtab:w-keys* — в шапке

(defun mtab:round2 (x)
  (/ (fix (+ (* x 100.0) 0.5)) 100.0))

(defun mtab:format-area (area / int-part frac)
  (setq int-part (fix area)
        frac (fix (+ (* (- area int-part) 100.0) 0.5)))
  (cond
    ((= frac 0) (itoa int-part))
    ((= (rem frac 10) 0) (strcat (itoa int-part) "," (itoa (/ frac 10))))
    ((< frac 10) (strcat (itoa int-part) ",0" (itoa frac)))
    (t (strcat (itoa int-part) "," (itoa frac)))))

;; Поиск размера: список ключей через mark:get-dyn
(defun mtab:dim (obj keys / k r n)
  (setq r nil)
  (foreach k keys
    (if (null r)
      (progn
        (setq n (mark:get-dyn obj k))
        (if (car n)
          (progn
            (setq n (mark:numval (cadr n)))
            (if (and (numberp n) (> n 0.0))
              (setq r n)))))))
  r)

(defun mtab:select (/ ss i e fills glazings others names nm lst)
  (if (and *mark:reuse-sel* *mark:fills*)
    (progn
      (mark:out "[INFO] Выбор уже выполнен (МАРКАЗАП) — без повторного выделения.")
      t)
    (progn
  (mark:out "")
  (mark:emit "\nВыберите блоки «Заполнение в витраж» для ведомости: ")
  (setq ss (vl-catch-all-apply 'ssget nil))
  (cond
    ((vl-catch-all-error-p ss)
     (mark:out "[ERROR] Ошибка выделения.") nil)
    ((null ss)
     (mark:out "[INFO] Выбор отменён.") nil)
    (t
     (setq fills nil glazings nil others 0 names nil i (sslength ss))
     (repeat i
       (setq i (1- i)
             e (ssname ss i))
       (setq lst (mark:block-names e))
       (foreach nm lst
         (if (not (member nm names))
           (setq names (cons nm names))))
       (cond
         ((mark:blk-match? e *mark:block-fill*)
          (setq fills (cons e fills)))
         ((mark:blk-match? e *mark:block-glazing*)
          (setq glazings (cons e glazings)))
         (t (setq others (1+ others)))))
     (setq *mark:sel-total*  (sslength ss)
           *mark:fills*       (reverse fills)
           *mark:glazings*    (reverse glazings)
           *mark:others*      others
           *mark:found-names* (reverse names))
     (mark:out (strcat "[INFO] Заполнений: " (itoa (length *mark:fills*))))
     (mark:out (strcat "[INFO] \"Атрибуты витража\": " (itoa (length *mark:glazings*))))
     (if (null *mark:fills*)
       (progn (mark:out "[ERROR] Блоки заполнений не найдены.") nil)
       t))))))

;; Префикс Витраж (как в MARKZ)
(defun mtab:prefix (/ g res val names)
  (setq *mark:prefix* ""
        *mark:prefix-found* nil
        names               nil)
  (if *mark:glazings*
    (progn
      ;; Ред. 48.29: несколько витражей — перечень в лог (ведомость
      ;; одна на всех); *mark:prefix* — первый (марки уже с префиксами)
      (if (> (length *mark:glazings*) 1)
        (progn
          (foreach g *mark:glazings*
            (setq res (mark:find-attr g *mark:attr-vitrage*))
            (if (and (car res) (mark:strp (cadr res))
                     (/= (mark:trim (cadr res)) ""))
              (setq names (cons (mark:trim (cadr res)) names))))
          (setq names (reverse names))
          (if names
            (progn
              (setq *mark:prefix*       (car names)
                    *mark:prefix-found* t)
              (mark:out
                (strcat "[INFO] Витражи: " (mark:vz-join names ", "))))
            (mark:out "[INFO] Атрибут \"Витраж\" пуст — без префикса.")))
        (progn
          (setq g   (car *mark:glazings*)
                res (mark:find-attr g *mark:attr-vitrage*))
          (if (and (car res) (mark:strp (cadr res)) (/= (mark:trim (cadr res)) ""))
            (progn
              (setq *mark:prefix* (mark:trim (cadr res))
                    *mark:prefix-found* t)
              (mark:out (strcat "[INFO] Витраж: " *mark:prefix*)))
            (mark:out "[INFO] Атрибут \"Витраж\" пуст — без префикса.")))))
    (mark:out "[INFO] \"Атрибуты витража\" нет — без префикса."))
  *mark:prefix*)

;; Сбор по блокам: (type h w mark)
(defun mtab:collect-blocks (/ blocks e obj raw-h raw-w h w vv vis mres mval)
  (setq blocks nil)
  (foreach e *mark:fills*
    (setq obj (mark:vla e)
          raw-h (mtab:dim obj *mtab:h-keys*)
          raw-w (mtab:dim obj *mtab:w-keys*))
    (cond
      ((or (null raw-h) (null raw-w) (<= raw-h 0.0) (<= raw-w 0.0))
       (mark:out "[WARN] Пропуск блока: нет корректных размеров."))
      (t
       (progn
         (setq h   (fix (+ raw-h *mtab:allowance*))
               w   (fix (+ raw-w *mtab:allowance*))
               vv  (mark:get-visibility obj)
               vis (if vv (caddr vv) "")
               mres (mark:find-attr e *mark:attr-mark*)
               mval (if (car mres) (mark:trim (cadr mres)) ""))
         (if (or (null vis) (= vis ""))
           (setq vis "Без типа"))
         (setq blocks
           (cons (list vis h w mval) blocks))))))
  (reverse blocks))

;; Сортировка: Тип -> Высота -> Ширина -> Марка
;; Ред. 48.30: разделение марки — "ТБ-1 И3.2" даёт ("ТБ-1" "И3.2");
;; "Г1" даёт ("" "Г1") — витраж и маркировка
(defun mtab:mark-split (m / cl i sp)
  (setq cl (vl-string->list m)
        i  0
        sp nil)
  (foreach c cl
    (if (= c 32)
      (setq sp i))
    (setq i (1+ i)))
  (if sp
    (list (substr m 1 sp) (substr m (+ sp 2)))
    (list "" m)))

;; Ред. 48.30: сортировка ведомости — тип, витраж, маркировка
(defun mtab:sort-less (a b / ta tb pa pb)
  (setq ta (strcase (car a)) tb (strcase (car b)))
  (cond
    ((< ta tb) t)
    ((> ta tb) nil)
    (t
     (setq pa (mtab:mark-split (cadddr a))
           pb (mtab:mark-split (cadddr b)))
     (cond
       ((mark:nat-less (car pa) (car pb)) t)
       ((mark:nat-less (car pb) (car pa)) nil)
       ((mark:nat-less (cadr pa) (cadr pb)) t)
       ((mark:nat-less (cadr pb) (cadr pa)) nil)
       (t nil)))))

;; Агрегация (type h w mark) -> (type h w mark count)
(defun mtab:aggregate (blocks / acc rec key found)
  ;; acc: assoc, элемент (key type h w mark count)
  (setq acc nil)
  (foreach rec blocks
    (setq key (strcat (strcase (car rec)) "|"
                      (itoa (cadr rec)) "|"
                      (itoa (caddr rec)) "|"
                      (strcase (cadddr rec))))
    (setq found (assoc key acc))
    (if found
      ;; found = (key type h w mark count); cddddr = (mark count)
      (setq acc
        (subst (list key
                     (cadr found)
                     (caddr found)
                     (cadddr found)
                     (car (cddddr found))
                     (1+ (cadr (cddddr found))))
               found acc))
      (setq acc
        (cons (list key (car rec) (cadr rec)
                    (caddr rec) (cadddr rec) 1)
              acc))))
  ;; нормализация к (type h w mark count)
  (setq acc
    (mapcar
      '(lambda (r)
         (list (cadr r) (caddr r) (cadddr r)
               (car (cddddr r)) (cadr (cddddr r))))
      acc))
  (vl-sort acc 'mtab:sort-less))

;; ---------- ТАБЛИЦА AutoCAD ----------
(defun mtab:area (h w cnt)
  (mtab:round2 (/ (* h w cnt) 1000000.0)))

;; ---------- ТАБЛИЦА AutoCAD ----------
;; Порядок: № | Тип | Марка | Высота | Ширина | Кол-во | Площадь
;; Подитог по каждому типу + финальный итог
;; Объединение ячеек строки: столбцы 2-5 -> col 1..4
;; ActiveX: MergeCells(minRow, maxRow, minCol, maxCol)
(defun mtab:merge-2-5 (tbl r / res)
  (setq res
    (vl-catch-all-apply 'vla-MergeCells (list tbl r r 1 4)))
  (if (vl-catch-all-error-p res)
    (progn
      (setq res
        (vl-catch-all-apply 'vlax-invoke-method
                            (list tbl "MergeCells" r r 1 4)))
      (if (vl-catch-all-error-p res)
        (mark:out (strcat "[WARN] MergeCells: "
                          (vl-catch-all-error-message res))))))
  res)

;; ---------- ТАБЛИЦА AutoCAD ----------
;; Порядок: № | Тип | Марка | Высота | Ширина | Кол-во | Площадь
;; Подитог по типу (без слова «Подитог») + финальный итог
(defun mtab:create-table (data / pt doc space tbl row nRows nCols
    rec tip h w mark cnt area oldEcho i n-types
    cur cnt-sub area-sub n-row t0 t1 t2 t3)
  (if (null data)
    (progn (mark:out "[INFO] Нет данных для таблицы.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
      (if (null pt)
        (progn (mark:out "[INFO] Таблица пропущена.") nil)
        (progn
          (setq n-types 0
                cur nil)
          (foreach rec data
            (if (not (and cur (= (strcase (car rec)) (strcase cur))))
              (progn (setq n-types (1+ n-types)
                           cur (car rec)))))
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
                space (vla-get-ModelSpace doc)
                pt (trans pt 1 0)
                nCols 7
                ;; title + header + data + subtotals + total (без лишней строки)
                nRows (+ 3 (length data) n-types)
                oldEcho (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (if (not *mark:batch-undo*)
            (mark:ax-invoke-ok doc "StartUndoMark" nil))
          (setq t0  (getvar "MILLISECS")
                tbl (vl-catch-all-apply 'vla-AddTable
            (list space (vlax-3d-point pt) nRows nCols 10.0 30.0)))
          (if (vl-catch-all-error-p tbl)
            (progn
              (mark:out (strcat "[ERROR] AddTable: "
                                (vl-catch-all-error-message tbl)))
              (if (not *mark:batch-undo*)
                (mark:ax-invoke-ok doc "EndUndoMark" nil))
              (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
              nil)
            (progn
              ;; Ред. 48.32: подавить пересчёт таблицы на время заполнения
              ;; (документация Autodesk: RegenerateTableSuppressed — без
              ;; этого AutoCAD пересоздаёт таблицу на каждом изменении
              ;; ячейки, ~850 раз для ведомости из 54 позиций)
              (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
                                  (list tbl :vlax-true))
              ;; Ред. 48.33: замер этапов (создание / заполнение / сборка)
              (setq t1 (getvar "MILLISECS"))
              ;; ширины колонок: № | Тип | Марка | H | W | Кол-во | Площадь
              ;; Ред. 48.0: таблица ведомости — на слой «Размеры»
              (mark:tbl-layer doc tbl)
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 14.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 1 45.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 2 40.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 3 28.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 4 28.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 5 28.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 6 30.0))
              ;; title
              (vla-SetText tbl 0 0 "Заполнение")
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 8.0))
              ;; header
              (setq i 0)
              (foreach hdr '("№" "Тип" "Марка" "Высота, мм" "Ширина, мм"
                             "Кол-во, шт." "Площадь, м2")
                (vla-SetText tbl 1 i hdr)
                (vla-SetCellAlignment tbl 1 i 5)
                (setq i (1+ i)))
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 1 8.0))
              (setq row 2 n-row 1
                    cur nil
                    cnt-sub 0 area-sub 0.0)
              (foreach rec data
                (setq tip  (car rec)
                      h    (cadr rec)
                      w    (caddr rec)
                      mark (cadddr rec)
                      cnt  (car (cddddr rec))
                      area (mtab:area h w cnt))
                ;; смена типа > подитог предыдущего
                (if (and cur (/= (strcase tip) (strcase cur)))
                  (progn
                    (vla-SetText tbl row 0 "")
                    ;; 3 пробела + название типа, влево, без «Подитог»
                    (vla-SetText tbl row 1 (strcat "   " cur))
                    (mtab:merge-2-5 tbl row)
                    (vla-SetCellAlignment tbl row 1 4)
                    (vla-SetText tbl row 5 (itoa cnt-sub))
                    (vla-SetText tbl row 6 (mtab:format-area area-sub))
                    (vla-SetCellAlignment tbl row 5 5)
                    (vla-SetCellAlignment tbl row 6 5)
                    ;; высота как у строк данных
                    (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                    (setq row (1+ row)
                          cnt-sub 0 area-sub 0.0)))
                (setq cur tip
                      cnt-sub (+ cnt-sub cnt)
                      area-sub (+ area-sub area))
                (vla-SetText tbl row 0 (itoa n-row))
                (vla-SetText tbl row 1 tip)
                (vla-SetText tbl row 2 (if (= mark "") "—" mark))
                (vla-SetText tbl row 3 (itoa h))
                (vla-SetText tbl row 4 (itoa w))
                (vla-SetText tbl row 5 (itoa cnt))
                (vla-SetText tbl row 6 (mtab:format-area area))
                (vla-SetCellAlignment tbl row 0 5)
                (vla-SetCellAlignment tbl row 1 4)
                (vla-SetCellAlignment tbl row 2 4)
                (vla-SetCellAlignment tbl row 3 5)
                (vla-SetCellAlignment tbl row 4 5)
                (vla-SetCellAlignment tbl row 5 5)
                (vla-SetCellAlignment tbl row 6 5)
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                (setq row (1+ row)
                      n-row (1+ n-row)))
              ;; последний подитог
              (if cur
                (progn
                  (vla-SetText tbl row 0 "")
                  (vla-SetText tbl row 1 (strcat "   " cur))
                  (mtab:merge-2-5 tbl row)
                  (vla-SetCellAlignment tbl row 1 4)
                  (vla-SetText tbl row 5 (itoa cnt-sub))
                  (vla-SetText tbl row 6 (mtab:format-area area-sub))
                  (vla-SetCellAlignment tbl row 5 5)
                  (vla-SetCellAlignment tbl row 6 5)
                  (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                  (setq row (1+ row))))
              ;; финальный итог
              (setq cnt-sub 0 area-sub 0.0)
              (foreach rec data
                (setq h (cadr rec) w (caddr rec)
                      cnt (car (cddddr rec))
                      cnt-sub (+ cnt-sub cnt)
                      area-sub (+ area-sub (mtab:area h w cnt))))
              (vla-SetText tbl row 0 "")
              ;; 3 пробела + «Итого:», подчёркивание, влево
              (vla-SetText tbl row 1 "   {\\LИтого:}")
              (mtab:merge-2-5 tbl row)
              (vla-SetCellAlignment tbl row 1 4)
              (vla-SetText tbl row 5 (itoa cnt-sub))
              (vla-SetText tbl row 6 (mtab:format-area area-sub))
              (vla-SetCellAlignment tbl row 5 5)
              (vla-SetCellAlignment tbl row 6 5)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
              (setq t2 (getvar "MILLISECS"))
              ;; Ред. 48.34: vla-Update убран — включение регенерации
              ;; (:vlax-false) само пересобирает таблицовый блок
              ;; (официальный пример Autodesk его не вызывает), а Update
              ;; следом запускал пересчёт повторно — двойная «сборка».
              ;; Откат при проблемах с отображением: вернуть сюда строку
              ;; (vl-catch-all-apply 'vla-Update (list tbl)).
              (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed
                                  (list tbl :vlax-false))
              (setq t3 (getvar "MILLISECS"))
              (if (not *mark:batch-undo*)
                (mark:ax-invoke-ok doc "EndUndoMark" nil))
              (vl-catch-all-apply 'setvar (list "CMDECHO" oldEcho))
              (mark:out
                (strcat "[INFO] Таблица создана, строк данных: "
                        (itoa (length data))
                        " (время: "
                        (rtos (/ (- t3 t0) 1000.0) 2 2)
                        " с: создание "
                        (rtos (/ (- t1 t0) 1000.0) 2 2)
                        ", заполнение "
                        (rtos (/ (- t2 t1) 1000.0) 2 2)
                        ", сборка "
                        (rtos (/ (- t3 t2) 1000.0) 2 2)
                        ")"))
              t)))))))

;; ---------- XML escape / CSV quote ----------
(defun mtab:xml-escape (str / result ch)
  (setq result "")
  (if (null str) (setq str ""))
  (while (/= str "")
    (setq ch (substr str 1 1))
    (cond
      ((= ch "&") (setq result (strcat result "&amp;")))
      ((= ch "<") (setq result (strcat result "&lt;")))
      ((= ch ">") (setq result (strcat result "&gt;")))
      ((= ch "\"") (setq result (strcat result "&quot;")))
      ((= ch "'") (setq result (strcat result "&apos;")))
      (t (setq result (strcat result ch))))
    (setq str (substr str 2)))
  result)

(defun mtab:csv-quote (str)
  (if (null str) ""
    (if (or (vl-string-search ";" str)
            (vl-string-search "\"" str))
      (strcat "\"" (vl-string-subst "\"\"" "\"" str) "\"")
      str)))

;; ---------- XLS (XML Spreadsheet 2003) ----------
;; ТЗ: колонка площади и итоги — ФОРМУЛЫ Excel (не зашитые числа).
;; Примитив mtab:cell (аналог eu-cell): ss:Formula + XML-экранирование.
;; Синтаксис: ROUND(...,2) / SUM(...), английские имена, точка — дробь, запятая — разделитель.
;; A1: счёт строк/колонок ведётся здесь же; на ss:Index в формулах не полагаемся.

(defun mtab:rtos-xml (x / s)
  ;; число для SpreadsheetML: разделитель — точка
  (setq s (rtos (float x) 2 2))
  (while (vl-string-search "," s)
    (setq s (vl-string-subst "." "," s)))
  s)

;; Примитив записи ячейки (аналог eu-cell из excel-utils.lsp)
;; formula = nil > без ss:Formula; иначе — каноническая формула A1
(defun mtab:cell (f style formula dtype val)
  (if formula
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:Formula=\"" (mtab:xml-escape formula) "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)))

;; Ячейка с объединением столбцов (MergeAcross) + опциональная формула
(defun mtab:cell-m (f style merge-across formula dtype val)
  (if formula
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:MergeAcross=\"" (itoa merge-across)
              "\" ss:Formula=\"" (mtab:xml-escape formula) "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:MergeAcross=\"" (itoa merge-across) "\">"
              "<Data ss:Type=\"" dtype "\">"
              (mtab:xml-escape val)
              "</Data></Cell>")
      f)))

;; маркер идемпотентности патча формул
;; mtab:eu-cell-formula

;; Формула SUM в R1C1 для диапазона строк r0..r1 в ТЕКУЩЕЙ колонке
;; (SpreadsheetML — каноничный R1C1; одна строка > прямая ссылка R[n]C)
(defun mtab:sum-r1c1 (r0 r1 / n0)
  (setq n0 (- r0 r1))  ;; отрицательный сдвиг от строки формулы (r1 = строка над формулой)
  ;; формула пишется в строку (r1+1), сдвиг до r0 = r0-(r1+1) = n0-1
  (setq n0 (1- n0))
  (if (= n0 -1)
    (strcat "=R[" (itoa -1) "]C")
    (strcat "=SUM(R[" (itoa n0) "]C:R[" (itoa -1) "]C)")))

;; =SUM(R4C6,R6C6,…) — абсолютные строки подитогов, col 6=F / 7=G
(defun mtab:sum-abs-r1c1 (rows col / out r)
  (if (null rows)
    "=0"
    (progn
      (setq out "")
      (foreach r rows
        (setq out
          (if (= out "")
            (strcat "R" (itoa r) "C" (itoa col))
            (strcat out ",R" (itoa r) "C" (itoa col)))))
      (strcat "=SUM(" out ")"))))

(defun mtab:export-xls (data xlsfile / f tip h w mark cnt area
                          total-cnt total-area rec cur
                          cnt-sub area-sub
                          xl-row grp-start sub-rows n-no)
  (setq f (open xlsfile "w"))
  (if (null f)
    (progn (mark:out (strcat "[ERROR] Нет доступа: " xlsfile)) nil)
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
      (write-line "  <Style ss:ID=\"D\">" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line "  <Style ss:ID=\"H\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; TITLE — заголовок «Заполнение»: жирный + подчёркивание
      (write-line "  <Style ss:ID=\"TITLE\">" f)
      (write-line "   <Font ss:Bold=\"1\" ss:Underline=\"Single\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Center\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; S — подитог: подпись влево
      (write-line "  <Style ss:ID=\"S\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; SN — числа подитога вправо
      (write-line "  <Style ss:ID=\"SN\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Right\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; T — итог: подпись влево; без Underline на ячейке (пробелы не подчёркивать)
      (write-line "  <Style ss:ID=\"T\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Left\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      ;; TN — числа итога вправо
      (write-line "  <Style ss:ID=\"TN\">" f)
      (write-line "   <Font ss:Bold=\"1\"/>" f)
      (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f)
      (write-line "   <Alignment ss:Horizontal=\"Right\"/>" f)
      (write-line "   <Borders>" f)
      (write-line "    <Border ss:Position=\"Bottom\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Left\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Right\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "    <Border ss:Position=\"Top\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>" f)
      (write-line "   </Borders>" f)
      (write-line "  </Style>" f)
      (write-line " </Styles>" f)
      (write-line " <Worksheet ss:Name=\"Заполнение\">" f)
      (write-line "  <Table>" f)
      (foreach w0 '("30" "130" "90" "70" "70" "70" "70")
        (write-line (strcat "   <Column ss:Width=\"" w0 "\"/>") f))
      ;; row1 title, row2 header, data from 3
      ;; A № | B Тип | C Марка | D Высота | E Ширина | F Кол-во | G Площадь
      (write-line (strcat "   <Row><Cell ss:StyleID=\"TITLE\" ss:MergeAcross=\"6\">"
                          "<Data ss:Type=\"String\">Заполнение</Data></Cell></Row>") f)
      (write-line "   <Row>" f)
      (foreach hdr '("№" "Тип" "Марка" "Высота, мм" "Ширина, мм"
                      "Кол-во, шт." "Площадь, м2")
        (mtab:cell f "H" nil "String" hdr))
      (write-line "   </Row>" f)
      (setq xl-row 3 total-cnt 0 total-area 0.0
            cur nil cnt-sub 0 area-sub 0.0
            grp-start 3 sub-rows nil
            n-no 0)
      (foreach rec data
        (setq tip  (car rec)
              h    (cadr rec)
              w    (caddr rec)
              mark (cadddr rec)
              cnt  (car (cddddr rec))
              area (mtab:area h w cnt))
        ;; подитог: R1C1 SUM по группе (одна строка > прямая ссылка)
        (if (and cur (/= (strcase tip) (strcase cur)))
          (progn
            (write-line "   <Row>" f)
            (mtab:cell f "S" nil "String" "")
            (mtab:cell-m f "S" 3 nil "String" (strcat "   " cur))
            (mtab:cell f "SN"
                       (mtab:sum-r1c1 grp-start (1- xl-row))
                       "Number" (itoa cnt-sub))
            (mtab:cell f "SN"
                       (mtab:sum-r1c1 grp-start (1- xl-row))
                       "Number" (mtab:rtos-xml area-sub))
            (write-line "   </Row>" f)
            (setq sub-rows (cons xl-row sub-rows)
                  cnt-sub 0 area-sub 0.0
                  xl-row (1+ xl-row)
                  grp-start xl-row)))
        (setq cur tip
              total-cnt (+ total-cnt cnt)
              total-area (+ total-area area)
              cnt-sub (+ cnt-sub cnt)
              area-sub (+ area-sub area))
        (setq n-no (1+ n-no))
        (write-line "   <Row>" f)
        (mtab:cell f "D" nil "Number" (itoa n-no))
        (mtab:cell f "D" nil "String" tip)
        (mtab:cell f "D" nil "String" mark)
        (mtab:cell f "D" nil "Number" (itoa h))
        (mtab:cell f "D" nil "Number" (itoa w))
        (mtab:cell f "D" nil "Number" (itoa cnt))
        ;; площадь — R1C1: D*E*F той же строки / 1e6, округление до 2
        (mtab:cell f "D"
                   "=ROUND(RC[-3]*RC[-2]*RC[-1]/1000000,2)"
                   "Number" (mtab:rtos-xml area))
        (write-line "   </Row>" f)
        (setq xl-row (1+ xl-row)))
      ;; последний подитог
      (if cur
        (progn
          (write-line "   <Row>" f)
          (mtab:cell f "S" nil "String" "")
          (mtab:cell-m f "S" 3 nil "String" (strcat "   " cur))
          (mtab:cell f "SN"
                     (mtab:sum-r1c1 grp-start (1- xl-row))
                     "Number" (itoa cnt-sub))
          (mtab:cell f "SN"
                     (mtab:sum-r1c1 grp-start (1- xl-row))
                     "Number" (mtab:rtos-xml area-sub))
          (write-line "   </Row>" f)
          (setq sub-rows (cons xl-row sub-rows)
                xl-row (1+ xl-row))))
      ;; финальный итог: =SUM(R…C6,…) / =SUM(R…C7,…) по строкам подитогов
      (write-line "   <Row>" f)
      (mtab:cell f "T" nil "String" "")
      ;; 3 пробела + подчёркнутый только «Итого:»
      (write-line
        (strcat "    <Cell ss:StyleID=\"T\" ss:MergeAcross=\"3\">"
                "<Data ss:Type=\"String\">   "
                "<html:B><html:U>Итого:</html:U></html:B></Data></Cell>")
        f)
      (mtab:cell f "TN"
                 (mtab:sum-abs-r1c1 sub-rows 6)
                 "Number" (itoa total-cnt))
      (mtab:cell f "TN"
                 (mtab:sum-abs-r1c1 sub-rows 7)
                 "Number" (mtab:rtos-xml total-area))
      (write-line "   </Row>" f)
      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      (mark:out (strcat "[INFO] XLS: " xlsfile))
      t)))

;; ---------- CSV ----------
(defun mtab:export-csv (data csvfile / f rec tip h w mark cnt area
                         total-cnt total-area n cur
                         cnt-sub area-sub)
  (setq f (open csvfile "w"))
  (if (null f)
    (progn (mark:out (strcat "[ERROR] Нет доступа: " csvfile)) nil)
    (progn
      (write-line "Заполнение" f)
      (write-line "№;Тип;Марка;Высота, мм;Ширина, мм;Кол-во, шт.;Площадь, м2" f)
      (setq n 1 total-cnt 0 total-area 0.0
            cur nil cnt-sub 0 area-sub 0.0)
      (foreach rec data
        (setq tip  (car rec)
              h    (cadr rec)
              w    (caddr rec)
              mark (cadddr rec)
              cnt  (car (cddddr rec))
              area (mtab:area h w cnt))
        (if (and cur (/= (strcase tip) (strcase cur)))
          (progn
            (write-line
              (strcat ";Подитог: " (mtab:csv-quote cur) ";;;;"
                      (itoa cnt-sub) ";"
                      "\"" (mtab:format-area area-sub) "\"")
              f)
            (setq cnt-sub 0 area-sub 0.0)))
        (setq cur tip
              total-cnt (+ total-cnt cnt)
              total-area (+ total-area area)
              cnt-sub (+ cnt-sub cnt)
              area-sub (+ area-sub area))
        (write-line
          (strcat (itoa n) ";"
                  (mtab:csv-quote tip) ";"
                  (mtab:csv-quote mark) ";"
                  (itoa h) ";"
                  (itoa w) ";"
                  (itoa cnt) ";"
                  "\"" (mtab:format-area area) "\"")
          f)
        (setq n (1+ n)))
      (if cur
        (write-line
          (strcat ";Подитог: " (mtab:csv-quote cur) ";;;;"
                  (itoa cnt-sub) ";"
                  "\"" (mtab:format-area area-sub) "\"")
          f))
      (write-line
        (strcat ";Итого;;;;"
                (itoa total-cnt) ";"
                "\"" (mtab:format-area total-area) "\"")
        f)
      (close f)
      (mark:out (strcat "[INFO] CSV: " csvfile))
      t)))

;; ---------- Основная ----------
(defun mtab:main (/ data base do-table do-xls xls-ok)
  (mark:cmd-line "МАРКАЗАПТАБЛ — ведомость по блокам «Заполнение в витраж».")
  (mark:banner)
  (if (null (mtab:select))
    (princ)
    (progn
      (mtab:prefix)
      (setq data (mtab:aggregate (mtab:collect-blocks)))
      (if (null data)
        (mark:out "[ERROR] Нет данных (проверьте размеры блоков).")
        (progn
          (mark:out (strcat "[INFO] Уникальных позиций: " (itoa (length data))))
          ;; Ред. 48.27: русские ключи — ввод «Да»/«Нет» (или по
          ;; первой букве Д/Н); Y/N и Enter работают как раньше
          (initget "Да Нет Y N")
          (setq do-table
            (getkword "\nСоздать таблицу AutoCAD? [Да(Y)/Нет(N)] <Да>: "))
          (if (or (null do-table) (member do-table '("Y" "Да")))
            (setq do-table t) (setq do-table nil))
          (initget "Да Нет Y N")
          (setq do-xls
            (getkword "\nЭкспорт в XLS? [Да(Y)/Нет(N)] <Да>: "))
          (if (or (null do-xls) (member do-xls '("Y" "Да")))
            (setq do-xls t) (setq do-xls nil))
          (setq base
            (strcat (getvar "dwgprefix")
                    (vl-filename-base (getvar "dwgname"))
                    " Заполнение Марки"))
          (setq xls-ok nil)
          (if do-xls
            (progn
              (setq xls-ok (mtab:export-xls data (strcat base ".xls")))
              ;; CSV — только резерв, если XLS не создался
              (if (null xls-ok)
                (progn
                  (mark:out "[INFO] XLS не создан — резервный экспорт CSV.")
                  (mtab:export-csv data (strcat base ".csv"))))))
          (if do-table
            (mtab:create-table data))
          ;; Ред. 48.0: файл сверки шкал (SYNC_MARKZV.md, §7)
          (mark:scale-write-file)
          (mark:out "[INFO] Готово.")))))
  (princ))

(defun c:MARKTABLE () (mtab:main))
(defun c:МАРКАЗАПТАБЛ () (mtab:main))


;;;=====================================================================
;;;  MARKAR — рядовка условных обозначений (Ряд заполнений)
;;;  Команда: МАРКАЗАПРЯД
;;;  После МАРКАРОВКАЗАП: индекс от размера (буква = высота, номер = ширина).
;;;  Старые «Ряд заполнений» этой зоны удаляются и ставятся заново.
;;;  Горизонталь: номера 1,2,3… снизу на min(y0)-1000, соосно по X.
;;;  Вертикаль: буквы А,Б,В… справа на max(x1)+1000.
;;;  Разная W в столбце > этаж ниже; разная H в ряду > этаж правее.
;;;=====================================================================

;; *mark:ar-* — в шапке

(defun mark:ar-banner ()
  (mark:out "========================================")
  (mark:out (strcat " РЯДОВКА УСЛОВНЫХ ОБОЗНАЧЕНИЙ  "
                    *mark:rev*))
  (mark:out "========================================"))

;;;--- Разбор Марки: [Витраж] + " " + БУКВА + НОМЕР + [суффикс] ---------

;; Суффикс стм/снд не входит в индекс
(defun mark:ar-strip-suffix (m / out msc sp suf n list)
  (setq out (mark:trim (if m m ""))
        msc (strcase out)
        list nil)
  (foreach sp *mark:specials*
    (setq list (cons (strcase (cdr sp)) list)))
  (foreach suf list
    (setq n (strlen suf))
    (if (and (> (strlen msc) n)
             (= (substr msc (1+ (- (strlen msc) n))) suf))
      (setq out (substr out 1 (- (strlen out) n))
            msc (strcase out))))
  out)

(defun mark:ar-tail (m / i n)
  (setq n (strlen m)
        i n)
  (while (and (> i 0) (/= (substr m i 1) " "))
    (setq i (1- i)))
  (if (= i 0) m (substr m (1+ i))))

(defun mark:ar-letter? (ch)
  (and (mark:strp ch)
       (= (strlen ch) 1)
       (wcmatch ch "[А-Яа-я]")))

(defun mark:ar-digit? (ch)
  (and (mark:strp ch)
       (= (strlen ch) 1)
       (>= (ascii ch) 48)
       (<= (ascii ch) 57)))

;; > (letter num) | nil
(defun mark:ar-parse (m / tail i n letter num)
  (setq m (mark:ar-strip-suffix (mark:trim m)))
  (if (or (null m) (= m ""))
    nil
    (progn
      (setq tail  (mark:ar-tail m)
            i     1
            n     (strlen tail)
            letter ""
            num   "")
      (while (and (<= i n)
                  (mark:ar-letter? (substr tail i 1))
                  ;; Й, З не используем; strcase без T = ВЕРХНИЙ регистр
                  (not (member (strcase (substr tail i 1))
                               '("Й" "З"))))
        (setq letter (strcat letter (strcase (substr tail i 1)))
              i      (1+ i)))
      (while (and (<= i n) (mark:ar-digit? (substr tail i 1)))
        (setq num (strcat num (substr tail i 1))
              i   (1+ i)))
      (if (and (/= letter "") (/= num ""))
        (list (strcase letter) num)
        nil))))

;;;--- Сбор: rec = (letter num e cx cy x0 x1 y0 y1 w h) -----------------

(defun mark:ar-collect (fills / out e obj w h wh hh ins mres mval pa
                             x0 y0 x1 y1 cx cy)
  ;; 1) сбор: геометрия + W/H теми же ключами, что в МАРКАРОВКАЗАП;
  ;;    разбор Марки — только подсказка, не фильтр.
  ;;    Ред. 48.10: разбор — ar-parse2 (литера и подындекс), как в
  ;;    ar-reindex; ar-parse литер не знал — давал ложный WARN
  ;;    «Марка не разобрана» на марках вида «Ж.б10.2».
  (setq out nil)
  (foreach e fills
    (setq obj  (mark:vla e)
          wh   (mark:get-dyn obj *mark:prop-width*)
          hh   (mark:get-dyn obj *mark:prop-height*)
          w    (if (and (car wh) (mark:numval (cadr wh)))
                 (mark:numval (cadr wh))
                 (mtab:dim obj *mtab:w-keys*))
          h    (if (and (car hh) (mark:numval (cadr hh)))
                 (mark:numval (cadr hh))
                 (mtab:dim obj *mtab:h-keys*))
          ins  (cdr (assoc 10 (entget e)))
          mres (mark:find-attr e *mark:attr-mark*)
          mval (if (car mres) (cadr mres) "")
          pa   (mark:ar-parse2 mval))
    (cond
      ((or (null w) (null h) (<= w 0.0) (<= h 0.0))
       (progn
         (mark:out "[WARN] Нет W/H — пропуск.")
         (mark:note-warning)))
      ((null ins)
       (progn
         (mark:out "[WARN] Нет точки вставки — пропуск.")
         (mark:note-warning)))
      (t
       (progn
         ;; сообщения о марке — INFO/WARN, но не выбрасываем блок
         (cond
           ((null (car mres))
            (mark:out
              "[WARN] Нет атрибута \"Марка\" — индекс будет от размера."))
           ((or (null mval) (= (mark:trim mval) ""))
            (mark:out
              "[WARN] Атрибут \"Марка\" пуст — индекс будет от размера."))
           ((null pa)
            (mark:out
              (strcat "[WARN] Марка не разобрана (" mval
                      ") — индекс будет от размера."))))
         (setq x0 (float (car ins))
               y0 (float (cadr ins))
               x1 (+ x0 (float w))
               y1 (+ y0 (float h))
               cx (/ (+ x0 x1) 2.0)
               cy (/ (+ y0 y1) 2.0))
         (setq out
           (cons (list (if pa (car pa) "")
                       (if pa (cadr pa) "")
                       e cx cy x0 x1 y0 y1
                       (float w) (float h)
                       (if mval mval ""))
                 out))))))
  (setq out (reverse out))
  ;; 2) один знаменатель: индекс ТОЛЬКО от W/H — как МАРКАРОВКАЗАП
  (mark:ar-reindex out))

;; Переназначение: буква = индекс высоты, номер = индекс ширины + 1
;; rec = (letter num e cx cy x0 x1 y0 y1 w h)
;; Ред. 48.0: буква/номер — из Марки (подындексы сохраняются);
;; запас — индекс от W/H, как в Ред. 47.8.
;; rec = (letter num e cx cy x0 x1 y0 y1 w h mark)
(defun mark:ar-reindex (recs / ws hs ws1 hs1 r w h iw ih letter num out m pa
                              nums letters)
  (setq ws1 nil
        hs1 nil)
  (foreach r recs
    (setq w (nth 9 r)
          h (nth 10 r))
    (if (and (numberp w) (> w 0.0))
      (setq ws1 (cons w ws1)))
    (if (and (numberp h) (> h 0.0))
      (setq hs1 (cons h hs1))))
  (setq ws (mark:sort-unique (mark:positives ws1))
        hs (mark:sort-unique (mark:positives hs1))
        out nil)
  (foreach r recs
    (setq m  (nth 11 r)
          pa (if (mark:strp m) (mark:ar-parse2 m) nil))
    (cond
      (pa
        (setq letter (nth 0 pa)
              num    (nth 1 pa)))
      (t
        ;; запас: размер от блока — как в Ред. 47.8, но цифра из ТОЙ ЖЕ
        ;; шкалы, что у марок (Ред. 48.37: номер ригеля, затем Блок 2),
        ;; иначе рядовка блока без атрибута «Марка» разойдётся с маркировкой
        (setq w      (nth 9 r)
              h      (nth 10 r)
              iw     (mark:index-of ws w)
              ih     (mark:index-of hs h)
              letter ""
              num    "")
        (if ih
          (setq letter (mark:letter-at ih)))
        (if (mark:strp letter)
          (setq letter (strcase letter)))
        (setq num (mark:num-of-width w))
        (if (and (null num) iw)
          (setq num (itoa (1+ iw))))))
    (setq out (cons (list letter
                          num
                          (nth 2 r) (nth 3 r) (nth 4 r)
                          (nth 5 r) (nth 6 r) (nth 7 r) (nth 8 r)
                          (nth 9 r) (nth 10 r) (nth 11 r))
                    out)))
  (setq out (reverse out))
  ;; INFO-шкалы: уникальные обозначения
  (setq nums nil
        letters nil)
  (foreach r out
    (if (and (mark:strp (nth 1 r)) (/= (nth 1 r) "")
             (not (member (nth 1 r) nums)))
      (setq nums (cons (nth 1 r) nums)))
    (if (and (mark:strp (nth 0 r)) (/= (nth 0 r) "")
             (not (member (nth 0 r) letters)))
      (setq letters (cons (nth 0 r) letters))))
  (setq *mark:widths*  (reverse nums)
        *mark:heights* (reverse letters))
  out)

;;;--- Кластеризация ----------------------------------------------------

(defun mark:ar-clusters (vals tol / s g out v lastv)
  (setq s   (vl-sort (mapcar 'float vals) '<)
        out nil
        g   nil
        lastv nil)
  (foreach v s
    (if (and g (<= (abs (- v lastv)) tol))
      (setq g     (append g (list v))
            lastv v)
      (progn
        (if g (setq out (cons g out)))
        (setq g     (list v)
              lastv v))))
  (if g (setq out (cons g out)))
  (reverse out))

;; recs > ((cx0 rec...) (cx1 rec...) ...) по возрастанию ключа
;; idx — индекс числового поля в rec (2=cx нет, см. вызовы)
(defun mark:ar-group-idx (recs idx tol / keys clus out g r hit m)
  (setq keys nil)
  (foreach r recs
    (setq keys (cons (nth idx r) keys)))
  (setq clus (mark:ar-clusters keys tol)
        out  nil)
  (foreach g clus
    (setq m   (car g)
          hit nil)
    (foreach r recs
      (if (<= (abs (- (float (nth idx r)) (float m))) tol)
        (setq hit (cons r hit))))
    ;; порядок recs: как в исходном списке
    (setq hit (reverse hit))
    ;; в группе сортируем по idx (стабильно для кластера)
    (setq hit (vl-sort hit
                '(lambda (a b)
                   (< (nth idx a) (nth idx b)))))
    (if hit (setq out (cons hit out))))
  (reverse out))

;; внутри уже отсортированного по idx списка — подгруппы по другому idx
(defun mark:ar-subgroup (recs idx2 tol / keys clus out g r hit m)
  (setq keys nil)
  (foreach r recs
    (setq keys (cons (nth idx2 r) keys)))
  (setq clus (mark:ar-clusters keys tol)
        out  nil)
  (foreach g clus
    (setq m   (car g)
          hit nil)
    (foreach r recs
      (if (<= (abs (- (float (nth idx2 r)) (float m))) tol)
        (setq hit (cons r hit))))
    (setq hit (reverse hit))
    (if hit (setq out (cons hit out))))
  ;; порядок подгрупп: по ключу (clusters уже по возрастанию)
  out)

(defun mark:ar-sort-by (groups idx-desc / out g)
  ;; сортировка групп по (nth idx (car group)) по убыванию
  (setq out
    (vl-sort groups
      '(lambda (a b)
         (> (nth idx-desc (car a))
            (nth idx-desc (car b))))))
  out)

;;;--- Уже размещённые «Ряд заполнений» ---------------------------------

(defun mark:ar-exists (x y lst / p hit)
  (setq p nil)
  (foreach e lst
    (if (and (null p)
             (<= (distance (list x y 0.0)
                           (list (car e) (cadr e) 0.0))
                 *mark:ar-tol*))
      (setq p e)))
  p)

(defun mark:ar-placed-pts (/ ss i e lst p)
  (setq lst nil)
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list (cons 0 "INSERT")))))
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i))
        (if (mark:blk-match? e *mark:ar-block*)
          (progn
            (setq p (cdr (assoc 10 (entget e))))
            (if p
              (setq lst (cons (list (float (car p))
                                    (float (cadr p)))
                              lst))))))))
  lst)

;;;--- Вставка -----------------------------------------------------------

(defun mark:ar-set-attr (obj tag val / atts att cnt i tagv ok aent e
                            ent data)
  ;; 1) ActiveX: Attributes / GetAttributes
  (setq ok   nil
        atts (mark:ax-get obj "Attributes"))
  (if (null atts)
    (setq atts (mark:ax-invoke obj "GetAttributes" nil)))
  (if atts
    (progn
      (setq cnt (mark:ax-get atts "Count")
            i   0)
      (if (and (numberp cnt) (> cnt 0))
        (while (< i cnt)
          (setq att  (mark:ax-invoke atts "Item" i)
                tagv (if att (mark:ax-get att "TagString") nil))
          (if (and tagv (mark:name= tagv tag))
            (progn
              (if (mark:ax-put att "TextString" val)
                (setq ok t))
              (setq i cnt)))
          (setq i (1+ i))))))
  ;; 2) fallback: entmod по ename
  (if (null ok)
    (progn
      (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
      (if (and (not (vl-catch-all-error-p e)) e)
        (progn
          (setq aent (mark:find-attr-in-ename e tag))
          (if aent
            (progn
              (setq data (entget aent))
              (entmod (subst (cons 1 val) (assoc 1 data) data))
              (entupd aent)
              (setq ok t)))))))
  ;; 3) fallback: ActiveX-атрибут через find-attr-obj
  (if (null ok)
    (progn
      (setq att (mark:find-attr-obj obj tag))
      (if (and att (mark:ax-put att "TextString" val))
        (setq ok t))))
  ok)

;; Запись dynamic-свойства: GetDynamicBlockProperties > PropertyNumberValue
(defun mark:ar-put-dim (obj keys val / r v lst item nm ok k)
  (setq ok nil)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
            (list obj "GetDynamicBlockProperties")))
  (if (and (not (vl-catch-all-error-p r)) r)
    (progn
      (setq v   (mark:unwrap r)
            lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
      (if (and (not (vl-catch-all-error-p lst)) (listp lst))
        (foreach item lst
          (setq item (mark:unwrap item)
                nm   (mark:dyn-item-name item))
          (if (and (null ok) (mark:strp nm))
            (progn
              (setq k nil)
              (foreach key keys
                (if (and (null k) (mark:name= nm key))
                  (setq k key)))
              (if k
                (cond
                  ((mark:ax-put item "PropertyNumberValue" (float val))
                   (setq ok t))
                  ((mark:ax-put item "Value" (float val))
                   (setq ok t))
                  ((mark:ax-put item "PropertyValue" (float val))
                   (setq ok t))))))))))
  ;; фолбэк: прямая запись на объект
  (if (null ok)
    (foreach k keys
      (if (null ok)
        (if (mark:ax-put obj k (float val))
          (setq ok t)))))
  ok)

;; rot — в ГРАДУСАХ (270 — горизонтальный ряд, 0 — вертикаль)
(defun mark:ar-insert (space x y txt rot / obj)
  (setq obj
    (vl-catch-all-apply 'vla-InsertBlock
      (list space
            (vlax-3d-point (list (float x) (float y) 0.0))
            *mark:ar-block*
            1.0 1.0 1.0
            ;; InsertBlock принимает РАДИАНЫ
            (* (float rot) (/ pi 180.0)))))
  (cond
    ((vl-catch-all-error-p obj)
     (progn
       (mark:out (strcat "[ERROR] INSERT: "
                         (vl-catch-all-error-message obj)))
       (mark:note-error)
       nil))
    ((null obj)
     (progn
       (mark:out "[ERROR] INSERT nil.")
       (mark:note-error)
       nil))
    (t
     (progn
       ;; страховка: выставить Rotation в градусах через свойство
       (mark:ax-put obj "Rotation" (* (float rot) (/ pi 180.0)))
       (if (not (mark:ar-set-attr obj *mark:ar-attr* txt))
         (progn
           (mark:out
             (strcat "[WARN] Не записан атрибут \"" *mark:ar-attr*
                     "\" (ожидалось: " txt ")"))
           (mark:note-warning)))
       obj))))

;; Пересечение интервалов (строгое; касание = можно в одну линию)
(defun mark:ar-overlap? (tlist lo hi / hit iv)
  (setq hit nil)
  (foreach iv tlist
    (if (and (< lo (cadr iv)) (> hi (car iv)))
      (setq hit t)))
  hit)

(defun mark:ar-track-get (tracks k)
  (if (and (numberp k) (>= k 0) (< k (length tracks)))
    (nth k tracks)
    nil))

(defun mark:ar-track-set (tracks k iv / out i res t0)
  (setq out tracks
        i   (length out))
  (while (<= i k)
    (setq out (append out (list nil))
          i   (1+ i)))
  (setq i   0
        res nil)
  (foreach t0 out
    (setq res (cons (if (= i k) iv t0) res)
          i   (1+ i)))
  (reverse res))

;; Упаковка рядовки — items: ((pos size text) …)
;;  1) Все кандидаты: size v (максимальный первым).
;;  2) First-fit: в первую линию, где нет наложения
;;     (интервал [pos?size/2 ; pos+size/2], касание = можно).
;;  3) Линия 0 > база; линия k?1 > k * *mark:ar-line* (150 мм).
;; Пример: 1550,590,530,460,320 > ряд0:1550, ряд1:590+530+320, ряд2:460
;; > ((k pos size text) …)
(defun mark:ar-pack (items / sorted tracks out
                        item pos size lo hi k ok tlist)
  (setq sorted (vl-sort (append items nil)
                 '(lambda (a b)
                    (> (float (cadr a)) (float (cadr b)))))
        tracks nil
        out    nil)
  (foreach item sorted
    (setq pos  (float (car item))
          size (float (cadr item))
          lo   (- pos (/ size 2.0))
          hi   (+ pos (/ size 2.0))
          k    0
          ok   nil)
    (while (null ok)
      (setq tlist (mark:ar-track-get tracks k))
      (if (mark:ar-overlap? tlist lo hi)
        (setq k (1+ k))
        (progn
          (setq tracks (mark:ar-track-set tracks k
                         (cons (list lo hi) tlist))
                out    (cons (list k pos size (caddr item)) out)
                ok     t)))))
  (reverse out))

(defun mark:ar-vla->ename (obj / e)
  (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
  (if (vl-catch-all-error-p e) nil e))

;; Ред. 48.28: префикс витража для имени группы рядовки — без
;; лога. В МАРКАЗАП уже вычислен (МАРКАРОВКАЗАП, TEST 02/03); в
;; одиночном МАРКАЗАПРЯД — тихо из «Атрибутов витража» выделения.
(defun mark:ar-prefix (/ g res)
  (cond
    (*mark:prefix-found* *mark:prefix*)
    (*mark:glazings*
     (progn
       (setq g   (car *mark:glazings*)
             res (mark:find-attr g *mark:attr-vitrage*))
       (if (and (car res) (mark:strp (cadr res)))
         (mark:trim (cadr res))
         "")))
    (t "")))

;; Группа AutoCAD по списку enames; nil при <1 объекта
;; Ред. 48.28: существующую группу НЕ удаляем и НЕ пересоздаём —
;; новые вставки добавляются в неё (AppendItems): группировка
;; старых рядовок сохраняется. Имя вызов даёт с суффиксом витража
;; («Рядовка горизонтальная В-7»), поэтому прогоны разных витражей
;; друг другу не мешают; legacy-группы без суффикса (созданы до
;; 48.28) не трогаются вовсе.
(setq *mark:ar-group-seq* 0)
(defun mark:ar-new-group-name (base / n)
  (setq *mark:ar-group-seq* (1+ *mark:ar-group-seq*)
        n (getvar "MILLISECS"))
  (strcat base " [MARKZ-" (itoa n) "-" (itoa *mark:ar-group-seq*) "]"))

(defun mark:ar-make-group (name enames / doc groups old grp arr i n cnt)
  (if (and enames (> (length enames) 1))
    (progn
      (setq doc    (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
            groups (if doc (mark:ax-get doc "Groups") nil))
      (if (null groups)
        (mark:out "[WARN] Groups недоступны.")
        (progn
          ;; существующая группа — добавить в неё, не пересоздавать
          ;; Always create a fresh group; never append to a previous run.
          (setq name (mark:ar-new-group-name name)
                old  nil
                grp  (vl-catch-all-apply 'vla-Add (list groups name)))
          (cond
            ((vl-catch-all-error-p grp)
             (mark:out (strcat "[WARN] Группа \"" name "\": "
                               (vl-catch-all-error-message grp))))
            ((null grp)
             (mark:out (strcat "[WARN] Группа \"" name "\" не создана.")))
            (t
             (progn
               (setq n   (length enames)
                     arr (vlax-make-safearray vlax-vbObject
                            (cons 0 (1- n)))
                     i   0)
               (foreach e enames
                 (vlax-safearray-put-element arr i
                   (vlax-ename->vla-object e))
                 (setq i (1+ i)))
               (if (vl-catch-all-error-p
                     (vl-catch-all-apply 'vlax-invoke-method
                       (list grp "AppendItems" arr)))
                 (mark:out
                   (strcat "[WARN] Группа \"" name
                           "\": объекты не добавлены.")))
               (setq cnt (mark:ax-get grp "Count"))
               (if (not (numberp cnt))
                 (setq cnt n))
               (if old
                 (mark:out
                   (strcat "[INFO] Группа \"" name "\": добавлено "
                           (itoa n) ", всего " (itoa cnt)
                           " объект(ов)"))
                 (mark:out
                   (strcat "[INFO] Группа \"" name "\": "
                           (itoa cnt) " объект(ов)"))))))))))
  nil)

;;;--- Основной проход ---------------------------------------------------

(defun mark:ar-main (/ r recs doc space placed
                     y-base x-base cols rows
                     g letter number subs s0 k ref
                     x y w h obj keysW keysH
                     cnt-h cnt-v y0 x1
                     h-ents v-ents he ve
                     h-items v-items pl
                     ar-pfx ar-gh ar-gv)
  (mark:cmd-line
    "МАРКАЗАПРЯД — рядовка из блоков «Ряд заполнений»: номера снизу, буквы справа.")
  (mark:reset-state)
  (mark:ar-banner)

  (if (null (mark:select))
    (princ)
    (progn
      (if (null *mark:fills*)
        (progn
          (mark:out "[ERROR] Заполнения не выбраны.")
          (mark:note-error))
        (progn
          (setq recs (vl-catch-all-apply 'mark:ar-collect (list *mark:fills*)))
          (cond
            ((vl-catch-all-error-p recs)
             (progn
               (mark:out "[ERROR] Сбой чтения заполнений:")
               (mark:out
                 (strcat "       "
                         (vl-catch-all-error-message recs)))
               (mark:note-error)))
            ((null recs)
             (mark:out "[ERROR] Нет пригодных заполнений (Марка/W/H)."))
            (t
             (progn
               (mark:out
                 (strcat "[INFO] Пригодных заполнений: "
                         (itoa (length recs))))
               (mark:out
                 (strcat "[INFO] Индексы рядовки: номер=секция/ригель ("
                         (itoa (length *mark:widths*))
                         "), буква=строка ("
                         (itoa (length *mark:heights*))
                         ")."))
               (if *mark:beam-scale*
                 (mark:out
                   (strcat "       Цифра ряда = номер ригеля MarkZV (Рг): "
                           (itoa (length *mark:beam-scale*))
                           " поз.")))

               ;; ключи dynamic-свойств ряда: сначала как у заполнения,
               ;; затем универсальные
               ;; свойство «Ширина»/«Высота» блока Ряд — в приоритете
               (setq keysW (cons "Ширина" (cons "Width" *mtab:w-keys*))
                     keysH (cons "Высота" (cons "Height" *mtab:h-keys*)))

               (setq doc    (mark:ax-get (vlax-get-acad-object)
                                         "ActiveDocument")
                     space  (if doc (mark:ax-get doc "ModelSpace") nil)
                     placed (mark:ar-placed-pts)
                     cnt-h  0
                     cnt-v  0
                     h-ents nil
                     v-ents nil
                     ar-pfx nil
                     ar-gh  nil
                     ar-gv  nil)

               (if (null space)
                 (progn
                   (mark:out "[ERROR] ModelSpace недоступен.")
                   (mark:note-error)))

               (if space
                 (progn
                   (if (and doc (not *mark:batch-undo*))
                     (mark:ax-invoke-ok doc "StartUndoMark" nil))
                   ;; Ред. 48.25: старая рядовка НЕ трогается —
                   ;; снятие отключено полностью (решение пользователя).
                   ;; placed собран выше: занятые точки пропускает
                   ;; ar-exists, новые блоки — только на свободные места.

                   ;; ===== ГОРИЗОНТАЛЬ (номера по ширине) =====
                   (setq y-base nil)
                   (foreach r recs
                     (setq y0 (nth 7 r))
                     (if (or (null y-base) (< y0 y-base))
                       (setq y-base y0)))
                   (setq y-base (- (float y-base) *mark:ar-offset*))
                   (mark:out
                     (strcat "[INFO] Горизонтальный ряд Y = "
                             (rtos y-base 2 1)))

                   ;; кандидаты: (cx W НОМЕР) — столбцы по cx, подгруппы W
                   (setq cols (mark:ar-group-idx recs 3 *mark:ar-tol*)
                         h-items nil)
                   (foreach g cols
                     (setq subs (mark:ar-subgroup g 9 *mark:ar-tol*))
                     (foreach s0 subs
                       ;; номер — от заполнения ЭТОЙ подгруппы (своя ширина),
                       ;; не от первого в колонке
                       (setq ref    (car s0)
                             number (nth 1 ref))
                       (setq h-items
                         (cons (list (nth 3 ref) (nth 9 ref) number)
                               h-items))))
                   ;; упаковка: max W > ряд 0; наложения > ряд 1+ (по убыв.)
                   (foreach pl (mark:ar-pack h-items)
                     (setq k     (nth 0 pl)
                           x     (nth 1 pl)
                           w     (nth 2 pl)
                           number (nth 3 pl)
                           y     (- y-base (* k *mark:ar-line*)))
                     (if (not (mark:ar-exists x y placed))
                       (progn
                         (setq obj (mark:ar-insert space x y number 270.0))
                         (if obj
                           (progn
                             (setq he    (mark:ar-vla->ename obj)
                                   h-ents (if he (cons he h-ents) h-ents))
                             ;; (W/2 + 25): осевая + половина на сторону
                             (if (not (mark:ar-put-dim obj keysW
                                              (+ (/ w 2.0) 25.0)))
                               (progn
                                 (mark:out
                                   (strcat "[WARN] Ширина не задана ("
                                           (rtos (+ (/ w 2.0) 25.0) 2 1)
                                           ")"))
                                 (mark:note-warning)))
                             (setq placed (cons (list x y) placed)
                                   cnt-h  (1+ cnt-h)))))))

                   ;; ===== ВЕРТИКАЛЬ (буквы по высоте), справа =====
                   (setq x-base nil)
                   (foreach r recs
                     (setq x1 (nth 6 r))
                     (if (or (null x-base) (> x1 x-base))
                       (setq x-base x1)))
                   (setq x-base (+ (float x-base) *mark:ar-offset*))
                   (mark:out
                     (strcat "[INFO] Вертикальный ряд X = "
                             (rtos x-base 2 1)))

                   ;; кандидаты: (cy H БУКВА) — строки по cy, подгруппы H
                   (setq rows   (mark:ar-group-idx recs 4 *mark:ar-tol*)
                         v-items nil)
                   (foreach g rows
                     (setq subs (mark:ar-subgroup g 10 *mark:ar-tol*))
                     (foreach s0 subs
                       ;; буква — от заполнения ЭТОЙ подгруппы (своя высота),
                       ;; не от первого в строке
                       (setq ref    (car s0)
                             letter (if (mark:strp (nth 0 ref))
                                      (nth 0 ref)
                                      ""))
                       (setq v-items
                         (cons (list (nth 4 ref) (nth 10 ref) letter)
                               v-items))))
                   ;; упаковка: max H > колонка 0; наложения > 1+ (по убыв.)
                   (foreach pl (mark:ar-pack v-items)
                     (setq k      (nth 0 pl)
                           y      (nth 1 pl)
                           h      (nth 2 pl)
                           letter (nth 3 pl)
                           x      (+ x-base (* k *mark:ar-line*)))
                     (if (not (mark:ar-exists x y placed))
                       (progn
                         (setq obj (mark:ar-insert space x y letter 0.0))
                         (if obj
                           (progn
                             (setq ve    (mark:ar-vla->ename obj)
                                   v-ents (if ve (cons ve v-ents) v-ents))
                             ;; в «Ширина»: (H/2 + 25)
                             (if (not (mark:ar-put-dim obj keysW
                                              (+ (/ h 2.0) 25.0)))
                               (progn
                                 (mark:out
                                   (strcat "[WARN] Ширина не задана ("
                                           (rtos (+ (/ h 2.0) 25.0) 2 1)
                                           ")"))
                                 (mark:note-warning)))
                             (setq placed (cons (list x y) placed)
                                   cnt-v  (1+ cnt-v)))))))

                   ;; группы: горизонталь и вертикаль раздельно;
                   ;; Ред. 48.28: имя с суффиксом витража — прогоны
                   ;; разных витражей не расформировывают чужие группы
                   (setq ar-pfx (mark:ar-prefix)
                         ar-gh  (if (and (mark:strp ar-pfx) (/= ar-pfx ""))
                                  (strcat *mark:ar-group-h* " " ar-pfx)
                                  *mark:ar-group-h*)
                         ar-gv  (if (and (mark:strp ar-pfx) (/= ar-pfx ""))
                                  (strcat *mark:ar-group-v* " " ar-pfx)
                                  *mark:ar-group-v*))
                   (mark:ar-make-group ar-gh h-ents)
                   (mark:ar-make-group ar-gv v-ents)

                   (if (and doc (not *mark:batch-undo*))
                     (mark:ax-invoke-ok doc "EndUndoMark" nil))

                   (mark:out
                     (strcat "[INFO] Горизонтальных (номера): "
                             (itoa cnt-h)))
                   (mark:out
                     (strcat "[INFO] Вертикальных (буквы): "
                             (itoa cnt-v)))
                   (mark:out
                     (strcat "[INFO] Вставлено: "
                             (itoa (+ cnt-h cnt-v)))))))))))))

  (mark:out (strcat "Ошибок: " (itoa *mark:errors*)))
  (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
  (if (= *mark:errors* 0)
    (mark:out "[INFO] Готово."))
  (princ))


;;;=====================================================================
;;;  МАРКАЗАПБЛОК / MARKFILL — вставка «Заполнение в витраж» по ячейкам
;;;  Режимы, в этом порядке:
;;;    Сетка-мультилинии, Точка-мультилинии, Сетка-динамика,
;;;    Точка-динамика, Полилинии
;;;  Вставка: точный левый нижний угол. W/H блока — до миллиметра.
;;;  Атрибуты обнуляются — пользователь заполняет сам.
;;;=====================================================================

;; *mark:fill-tol* *mark:fill-slope* *mark:fill-window* — в шапке
(setq *mark:mline-warns* 0)          ; счётчик warn MLINE (макс 3)

;;; ---- габарит сущности > (x0 y0 x1 y1) | nil --------------------------

(defun mark:cell-bb (e / ed typ obj r mn mx L0 L1
                       x0 y0 x1 y1 xs ys p)
  (setq ed (entget e))
  (if (null ed)
    nil
    (progn
      (setq typ (cdr (assoc 0 ed)))
      (cond
        ((and (= typ "LWPOLYLINE")
              (cdr (assoc 70 ed))
              (= 1 (logand 1 (cdr (assoc 70 ed)))))
         (progn
           (setq xs nil
                 ys nil)
           (foreach p ed
             (if (= (car p) 10)
               (progn
                 (setq xs (cons (float (cadr p)) xs)
                       ys (cons (float (caddr p)) ys)))))
           (if (and xs ys)
             (progn
               (setq x0 (car xs)
                     x1 (car xs)
                     y0 (car ys)
                     y1 (car ys))
               (foreach p xs
                 (if (< p x0) (setq x0 p))
                 (if (> p x1) (setq x1 p)))
               (foreach p ys
                 (if (< p y0) (setq y0 p))
                 (if (> p y1) (setq y1 p)))
               (list x0 y0 x1 y1))
             nil)))
        (t
         (progn
           (setq obj (mark:ax-catch-vla e))
           (if (null obj)
             nil
             (progn
               (setq r (vl-catch-all-apply 'vla-GetBoundingBox
                         (list obj 'mn 'mx)))
               (if (or (vl-catch-all-error-p r) (null r))
                 nil
                 (progn
                   (setq L0 (vl-catch-all-apply 'vlax-safearray->list (list mn))
                         L1 (vl-catch-all-apply 'vlax-safearray->list (list mx)))
                   (if (or (vl-catch-all-error-p L0)
                           (vl-catch-all-error-p L1)
                           (null L0) (null L1)
                           (< (length L0) 2) (< (length L1) 2))
                     nil
                     (progn
                       (setq x0 (float (car L0))
                             y0 (float (cadr L0))
                             x1 (float (car L1))
                             y1 (float (cadr L1)))
                       (list (min x0 x1) (min y0 y1)
                             (max x0 x1) (max y0 y1))))))))))))))

(defun mark:ax-catch-vla (e / obj)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (vl-catch-all-error-p obj) (null obj)) nil obj))

;;; ---- атрибуты обнулить ------------------------------------------------

(defun mark:fill-clear-attrs-ename (e / ed cnt)
  (setq cnt 0)
  (if (and e (not (vl-catch-all-error-p e)))
    (progn
      (setq e (entnext e))
      (while e
        (setq ed (entget e))
        (if (and ed (= "ATTRIB" (cdr (assoc 0 ed))))
          (progn
            (if (assoc 1 ed)
              (entmod (subst (cons 1 "") (assoc 1 ed) ed))
              (entmod (append ed (list (cons 1 "")))))
            (setq cnt (1+ cnt))))
        (setq e (entnext e)))))
  cnt)

(defun mark:fill-clear-attrs (obj / e)
  (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
  (if (and (not (vl-catch-all-error-p e)) e)
    (mark:fill-clear-attrs-ename e)
    0))

;;; ---- вставка + W/H ----------------------------------------------------
(defun mark:fill-block-there ()
  (if (tblsearch "BLOCK" *mark:block-fill*)
    t
    (progn
      (if (null *mark:fill-miss*)
        (progn
          (setq *mark:fill-miss* t)
          (mark:out
            (strcat "[ERROR] В чертеже нет блока «" *mark:block-fill* "»."))))
      nil)))

(defun mark:fill-attdefs (bname / rec e ed out)
  (setq rec (tblsearch "BLOCK" bname)
        e   (if rec (cdr (assoc -2 rec)) nil)
        out nil)
  (while (and e (setq ed (entget e)) (/= "ENDBLK" (cdr (assoc 0 ed))))
    (if (= "ATTDEF" (cdr (assoc 0 ed)))
      (setq out (cons ed out)))
    (setq e (entnext e)))
  (reverse out))

(defun mark:fill-insert-ent (x y / e defs d tag pt h ang flags)
  (if (null (tblsearch "BLOCK" *mark:block-fill*))
    nil
    (progn
      (setq defs (mark:fill-attdefs *mark:block-fill*)
            e (vl-catch-all-apply 'entmakex
                (list (list '(0 . "INSERT")
                            (cons 2 *mark:block-fill*)
                            (list 10 (float x) (float y) 0.0)
                            '(41 . 1.0)
                            '(42 . 1.0)
                            '(43 . 1.0)
                            '(50 . 0.0)
                            (cons 66 (if defs 1 0))))))
      (if (or (vl-catch-all-error-p e) (null e))
        nil
        (progn
          (if defs
            (progn
              (foreach d defs
                (setq tag (cdr (assoc 2 d))
                      pt  (cdr (assoc 10 d))
                      h   (cdr (assoc 40 d))
                      ang (cdr (assoc 50 d))
                      flags (cdr (assoc 70 d)))
                (entmakex
                  (list '(0 . "ATTRIB")
                        (list 10
                              (+ (float x) (if (and pt (car pt)) (float (car pt)) 0.0))
                              (+ (float y) (if (and pt (cadr pt)) (float (cadr pt)) 0.0))
                              0.0)
                        (cons 2 (if tag tag "Марка"))
                        '(1 . "")
                        (cons 40 (if (numberp h) h 2.5))
                        (cons 70 (if (numberp flags) flags 0))
                        (cons 50 (if (numberp ang) ang 0.0)))))
              (entmakex '((0 . "SEQEND")))))
          (mark:out "[INFO] Вставка через entmakex.")
          (vl-catch-all-apply 'vlax-ename->vla-object (list e)))))))

(defun mark:fill-find-fill (/ ss i e nm best)
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list '(0 . "INSERT"))))
        best nil)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (while (and (> i 0) (null best))
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (if (mark:name= nm *mark:block-fill*)
          (setq best e)))))
  best)

(defun mark:fill-insert-copy (x y / src obj copy)
  (setq src (mark:fill-find-fill)
        obj (if src (mark:ax-catch-vla src) nil)
        copy (if obj (vl-catch-all-apply 'vla-Copy (list obj)) nil))
  (if (or (null copy) (vl-catch-all-error-p copy))
    nil
    (progn
      (vl-catch-all-apply 'vla-Move
        (list copy
              (vl-catch-all-apply 'vla-get-InsertionPoint (list copy))
              (vlax-3d-point (list (float x) (float y) 0.0))))
      (mark:out "[INFO] Вставка копией блока, который уже есть в чертеже.")
      copy)))

(defun mark:fill-try-vla (space x y)
  (vl-catch-all-apply 'vla-InsertBlock
    (list space
          (vlax-3d-point (list (float x) (float y) 0.0))
          *mark:block-fill*
          1.0 1.0 1.0
          0.0)))

(defun mark:fill-end-undo (/ doc)
  ;; Группу режима «точка» не закрываем: её закроет конец захода.
  (if *mark:pt-undo*
    (setq *mark:fill-undo-off* t)
    (progn
      (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))
      (setq *mark:fill-undo-off* t))))

(defun mark:fill-insert (space x y / obj err alt msg)
  (cond
    ((null (mark:fill-block-there)) nil)
    (t
     (progn
       (setq obj (mark:fill-try-vla space x y)
             err (if (vl-catch-all-error-p obj)
                   (vl-catch-all-error-message obj)
                   nil))
       (if (and err (null *mark:fill-undo-off*)
                (wcmatch (strcase err) "*FILER*,*ФАЙЛЕР*"))
         (progn
           (mark:out "[INFO] Ошибка файлера внутри UNDO — повторяю вставку без группы.")
           (mark:fill-end-undo)
           (setq obj (mark:fill-try-vla space x y)
                 err (if (vl-catch-all-error-p obj)
                       (vl-catch-all-error-message obj)
                       nil))))
       (if (or err (null obj))
         (setq alt (mark:fill-insert-copy x y)
               obj (if (and alt (not (vl-catch-all-error-p alt))) alt obj)))
       (if (or (null obj) (vl-catch-all-error-p obj))
         (setq alt (mark:fill-insert-ent x y)
               obj (if (and alt (not (vl-catch-all-error-p alt))) alt nil)))
       (if (and obj (not (vl-catch-all-error-p obj)))
         obj
         (progn
           (setq msg (if err err "блок не вставился"))
           (mark:out (strcat "[ERROR] INSERT: " msg))
           (mark:note-error)
           nil))))))

(defun mark:fill-set-dims (obj w h / keysW keysH okw okh)
  (setq keysW (cons *mark:prop-width* *mtab:w-keys*)
        keysH (cons *mark:prop-height* *mtab:h-keys*)
        okw (mark:ar-put-dim obj keysW (float w))
        okh (mark:ar-put-dim obj keysH (float h)))
  (if (not okw)
    (progn
      (mark:out (strcat "[WARN] Ширина в свету не задана ("
                        (rtos w 2 1) ")"))
      (mark:note-warning)))
  (if (not okh)
    (progn
      (mark:out (strcat "[WARN] Высота в свету не задана ("
                        (rtos h 2 1) ")"))
      (mark:note-warning)))
  (and okw okh))

;;; ---- список ячеек + дедуп --------------------------------------------


;; Маски имён проёмов (окна/двери) по EffectiveName блока.
;; По умолчанию: «ОКНО», «створка», «ДВЕРЬ». Профильные заполнения
;; («Кассета» и т.п.) под эти маски не попадают. Список можно дополнять
;; прямо в чертеже, без правки кода:
;;   (setq *mark:opening-masks* (append *mark:opening-masks* (list "*КПТ*")))
(if (null *mark:opening-masks*)
  (setq *mark:opening-masks*
    (list "*ОКН*" "*СТВОР*" "*ДВЕР*")))

;; Маски-исключения: имена, которые НЕ считать проёмом, даже если они
;; случайно совпали с маской выше (например, глухие заполнения).
(if (null *mark:opening-skip-masks*)
  (setq *mark:opening-skip-masks* (list "*КАССЕТ*")))

;; Допуск (мм) на «неполное заполнение» проёма: разница габарита проёма и
;; ячейки меньше него считается погрешностью и НЕ вызывает предупреждение
;; и подсветку. Можно менять в чертеже: (setq *mark:opening-fit-tol* 30.0)
(if (null *mark:opening-fit-tol*)
  (setq *mark:opening-fit-tol* 20.0))

(defun mark:fill-opening-block? (name / u hit m)
  (setq u (strcase (if name name "")) hit nil)
  (if (= u "")
    nil
    (progn
      ;; сначала явные исключения
      (foreach m *mark:opening-skip-masks*
        (if (wcmatch u (strcase m)) (setq hit 'skip)))
      (if (eq hit 'skip)
        nil
        (progn
          (foreach m *mark:opening-masks*
            (if (and (null hit) (wcmatch u (strcase m)))
              (setq hit t)))
          hit)))))

(defun mark:fill-dyn-number (pairs keys / pair nm val found key)
  ;; ????? ???????? ?? ?????????? ?????; ??????? ??????????.
  (setq found nil)
  (foreach pair pairs
    (if (and (null found) (mark:strp (car pair)))
      (progn
        (setq nm (strcase (car pair)))
        (foreach key keys
          (if (and (null found) (wcmatch nm (strcat "*" key "*")))
            (progn
              (setq val (mark:unwrap (cdr pair)))
              (if (and (numberp val) (> (float val) 0.0))
                (setq found (float val)))))))))
  found)

(defun mark:fill-vla-bb (e / obj mn mx r a b)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e))
        mn nil mx nil)
  (if (and (not (vl-catch-all-error-p obj)) obj)
    (progn
      ;; GetBoundingBox является void-методом: успешный результат может быть nil.
      (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx)))
      (if (and (not (vl-catch-all-error-p r)) mn mx)
        (progn
          (setq a (vl-catch-all-apply 'vlax-safearray->list (list mn))
                b (vl-catch-all-apply 'vlax-safearray->list (list mx)))
          (if (and (not (vl-catch-all-error-p a))
                   (not (vl-catch-all-error-p b))
                   (>= (length a) 2) (>= (length b) 2))
            (list (float (car a)) (float (cadr a))
                  (float (car b)) (float (cadr b)))
            nil))
        nil))
    nil))

(defun mark:fill-opening-dim-bb (e / obj ip pairs w h a b x y)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (vl-catch-all-error-p obj) (null obj)) nil
    (progn
      (setq ip (mark:ax-get obj "InsertionPoint")
            pairs (mark:load-dyn-pairs obj)
            w (mark:fill-dyn-number pairs (list "ШИРИН" "ДЛИН"))
            h (mark:fill-dyn-number pairs (list "ВЫСОТ" "ДЛИН"))
            ip (mark:unwrap ip))
      (if (and (listp ip) (>= (length ip) 2) w h)
        (progn
          (setq x (float (car ip)) y (float (cadr ip))
                a (list (- x (/ w 2.0)) (- y (/ h 2.0)))
                b (list (+ x (/ w 2.0)) (+ y (/ h 2.0))))
          (list (car a) (cadr a) (car b) (cadr b)))
        nil))))

(defun mark:fill-bb-in-zone? (eb z)
  ;; Пересекается ли габарит проёма с зоной сетки z (xmin ymin xmax ymax).
  (or (null z)
      (and eb
           (<= (nth 0 eb) (nth 2 z)) (>= (nth 2 eb) (nth 0 z))
           (<= (nth 1 eb) (nth 3 z)) (>= (nth 3 eb) (nth 1 z)))))

(defun mark:fill-load-openings (/ ss i e ed nm eb out)
  ;; Реальные габариты проёмов получаем напрямую через VLA. Если задана зона
  ;; сетки (*mark:grid-zone*) — берём только проёмы, попадающие в неё
  ;; (быстрее и без «мусора» из других видов). Подробный список НЕ печатаем.
  (setq out nil
        ss (vl-catch-all-apply 'ssget (list "X" (list (cons 0 "INSERT")))))
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (while (> i 0)
        (setq i (1- i)
              e (ssname ss i)
              ed (entget e)
              nm (if ed (mark:fill-eff-name e) nil))
        (if (mark:fill-opening-block? nm)
          (progn
            (setq eb (mark:fill-vla-bb e))
            (if (null eb) (setq eb (mark:cell-bb e)))
            ;; Если Bounding Box отсутствует — размеры из динамики.
            (if (null eb) (setq eb (mark:fill-opening-dim-bb e)))
            (if (and eb (mark:fill-bb-in-zone? eb *mark:grid-zone*))
              (progn
                ;; Тройка: имя проёма, габарит, ename блока (для подсветки).
                (setq out (cons (list nm eb e) out))
                ;; Полный список проёмов печатаем только в режимах сетки,
                ;; где известна зона; в «Точка/Полилиния» хватает счётчика.
                (if *mark:grid-zone*
                  (mark:out
                    (strcat "[INFO] Проём: " (if nm nm "?")
                            "  X " (rtos (nth 0 eb) 2 1)
                            ".." (rtos (nth 2 eb) 2 1)
                            "  Y " (rtos (nth 1 eb) 2 1)
                            ".." (rtos (nth 3 eb) 2 1)
                            "  W=" (rtos (- (nth 2 eb) (nth 0 eb)) 2 0)
                            "  H=" (rtos (- (nth 3 eb) (nth 1 eb)) 2 0)))))))))))
  (setq *mark:opening-bbs* out
        *mark:opening-loaded* t)
  (mark:out (strcat "[INFO] Проёмов для фильтра: " (itoa (length out))))
  out)


(defun mark:fill-cell-opening? (bb / hit hit-eb hit-e item eb cx cy tol)
  ;; Ячейка считается занятой проёмом, если ЦЕНТР ячейки попадает внутрь
  ;; габарита окна/двери. Так проём захватывает только ту ячейку, в которой
  ;; он реально стоит, а не соседние из-за небольшого «залезания» рамки.
  (if (and bb (not *mark:opening-loaded*))
    (mark:fill-load-openings))
  (setq hit nil hit-eb nil hit-e nil)
  (if bb
    (progn
      (setq cx (/ (+ (nth 0 bb) (nth 2 bb)) 2.0)
            cy (/ (+ (nth 1 bb) (nth 3 bb)) 2.0))
      (foreach item *mark:opening-bbs*
        (setq eb (cadr item))
        (if (and (null hit) eb
                 (>= cx (nth 0 eb)) (<= cx (nth 2 eb))
                 (>= cy (nth 1 eb)) (<= cy (nth 3 eb)))
          (setq hit (car item) hit-eb eb hit-e (caddr item))))))
  ;; Проём найден, но НЕ полностью накрывает ячейку (с учётом допуска на
  ;; небольшие погрешности в размерах). Тогда: предупреждение + счётчик +
  ;; запоминаем блок проёма, чтобы подсветить его после заполнения.
  (if (and hit hit-eb)
    (progn
      (setq tol (if (numberp *mark:opening-fit-tol*) *mark:opening-fit-tol* 20.0))
      (if (not (and (<= (nth 0 hit-eb) (+ (nth 0 bb) tol))
                    (<= (nth 1 hit-eb) (+ (nth 1 bb) tol))
                    (>= (nth 2 hit-eb) (- (nth 2 bb) tol))
                    (>= (nth 3 hit-eb) (- (nth 3 bb) tol))))
        (progn
          (mark:out
            (strcat "[WARN] Проём «" hit
                    "» не полностью заполняет ячейку "
                    (rtos (nth 0 bb) 2 1) "," (rtos (nth 1 bb) 2 1)
                    " (W=" (rtos (- (nth 2 bb) (nth 0 bb)) 2 0)
                    " H=" (rtos (- (nth 3 bb) (nth 1 bb)) 2 0)
                    "). Ячейка пропущена как проёмная — проверьте вручную."))
          (mark:note-warning)
          (if (and hit-e (not (member hit-e *mark:opening-partial*)))
            (setq *mark:opening-partial* (cons hit-e *mark:opening-partial*)))))))
  hit)


(defun mark:fill-seg-touch? (a b tol / ax ay az bx by bz)
  (setq ax (list (nth 0 a) (nth 1 a))
        ay (list (nth 2 a) (nth 3 a))
        bx (list (nth 0 b) (nth 1 b))
        by (list (nth 2 b) (nth 3 b)))
  (or (<= (distance (list (car ax) (cadr ax) 0.0)
                    (list (car bx) (cadr bx) 0.0)) tol)
      (<= (distance (list (car ax) (cadr ax) 0.0)
                    (list (car by) (cadr by) 0.0)) tol)
      (<= (distance (list (car ay) (cadr ay) 0.0)
                    (list (car bx) (cadr bx) 0.0)) tol)
      (<= (distance (list (car ay) (cadr ay) 0.0)
                    (list (car by) (cadr by) 0.0)) tol)))

(defun mark:fill-seg-components (segs / rest comps seed group changed out s g hit)
  ;; Разделяем разорванные витражи до построения осей.
  (setq rest segs comps nil)
  (while rest
    (setq seed (car rest)
          rest (cdr rest)
          group (list seed)
          changed t)
    (while changed
      (setq changed nil out nil)
      (foreach s rest
        (setq hit nil)
        (foreach g group
          (if (mark:fill-seg-touch? s g *mark:fill-tol*)
            (setq hit t)))
        (if hit
          (progn (setq group (cons s group) changed t))
          (setq out (cons s out))))
      (setq rest (reverse out)))
    (setq comps (cons group comps)))
  (reverse comps))


(defun mark:fill-add (cells pts bb / x0 y0 x1 y1 w h dup p opening)
  (if (null bb)
    (list cells pts)
    (progn
      (setq opening (mark:fill-cell-opening? bb))
      (if opening
        (progn
          (mark:out (strcat "[INFO] Ячейка с блоком проёма " opening " — пропуск."))
          (list cells pts))
        (progn
          (setq x0 (nth 0 bb)
                y0 (nth 1 bb)
                x1 (nth 2 bb)
                y1 (nth 3 bb)
                w  (- x1 x0)
                h  (- y1 y0)
                dup nil)
          (foreach p pts
            (if (and (null dup)
                     (<= (distance (list x0 y0 0.0)
                                   (list (car p) (cadr p) 0.0))
                         *mark:ar-tol*))
              (setq dup t)))
          (if dup
            (progn
              (mark:out "[INFO] Ячейка уже в списке — пропуск.")
              (list cells pts))
            (progn
              (setq cells (cons bb cells)
                    pts   (cons (list x0 y0) pts))
              (list cells pts))))))))

;;; ---- вершины / точка в полигоне --------------------------------------

(defun mark:fill-poly-verts (e / ed verts p)
  (setq ed (entget e))
  (if (and ed
           (= "LWPOLYLINE" (cdr (assoc 0 ed)))
           (cdr (assoc 70 ed))
           (= 1 (logand 1 (cdr (assoc 70 ed)))))
    (progn
      (setq verts nil)
      (foreach p ed
        (if (= (car p) 10)
          (setq verts (cons (list (float (cadr p)) (float (caddr p)))
                            verts))))
      (reverse verts))
    nil))

(defun mark:fill-pt-in (x y verts / inside i j n xi yi xj yj)
  (setq inside nil
        i 0
        n (length verts))
  (while (< i n)
    (setq j  (rem (1+ i) n)
          xi (car (nth i verts))
          yi (cadr (nth i verts))
          xj (car (nth j verts))
          yj (cadr (nth j verts)))
    (if (and (/= (> yi y) (> yj y))
             (< x (+ xi (* (/ (- xj xi) (- yj yi)) (- y yi)))))
      (setq inside (not inside)))
    (setq i (1+ i)))
  inside)

(defun mark:fill-poly-area (verts / a i n x0 y0 x1 y1)
  (setq a 0.0
        i 0
        n (length verts))
  (while (< i n)
    (setq x0 (car (nth i verts))
          y0 (cadr (nth i verts))
          x1 (car (nth (rem (1+ i) n) verts))
          y1 (cadr (nth (rem (1+ i) n) verts)))
    (setq a (+ a (- (* x0 y1) (* x1 y0))))
    (setq i (1+ i)))
  (abs (* a 0.5)))

;;; ---- РЕЖИМ 1: полилинии -----------------------------------------------


;;; ---- Режимы 5/6: универсальная геометрия ----------------------------
(defun mark:fill-all-ss (/ filter ss)
  (setq filter (list (cons 0 "INSERT,LINE,LWPOLYLINE,POLYLINE,MLINE,ARC")))
  (setq ss (vl-catch-all-apply 'ssget (list filter)))
  (if (or (vl-catch-all-error-p ss) (null ss)) nil ss))

;; Дубликат вставки в одной точке — один каркас (копии блоков).
(defun mark:fill-block-seen? (seen nm e / p hit q)
  (setq p (cdr (assoc 10 (entget e)))
        hit nil)
  (foreach q seen
    (if (and (= (strcase nm) (strcase (car q)))
             p (<= (distance (list (car p) (cadr p) 0.0)
                            (list (car (cdr q)) (cadr (cdr q)) 0.0))
                    *mark:fill-tol*))
      (setq hit t)))
  hit)

(defun mark:fill-lwpoly-closed? (e / ed)
  ;; Замкнутая полилиния — контур ячейки (семантика 7-Полилиния).
  (setq ed (if e (entget e) nil))
  (and ed
       (= "LWPOLYLINE" (cdr (assoc 0 ed)))
       (cdr (assoc 70 ed))
       (= 1 (logand 1 (cdr (assoc 70 ed))))))

(defun mark:fill-mode-all-grid (cells pts / ss i e ed typ nm r segs blocks
                                       seen n-open axes vs hs items boxes
                                       polys n-poly n-polyo n-line
                                       bb msegs rblocks n-hidden
                                       vs2 out glaz)
  (mark:out "5-Сетка-все-типы: каркас — блоки (динамика) и мультилинии; отрезки, дуги, полилинии — не каркас (Ред. 48.6).")
  (mark:out "Выберите объекты каркаса рамкой. Enter — отмена.")
  (setq *mark:fill-geo* nil
        *mark:cell-hs*  nil)
  (setq ss (mark:fill-all-ss))
  (if (null ss)
    (list cells pts 'cancel)
    (progn
      (mark:fill-type-stat ss)
      ;; Разбор выбора: блоки -> каркас-вставки, остальное -> отрезки.
      ;; Проёмы (окно/дверь) — не каркас: линии рамы заходят в световой
      ;; проём и занижают ячейку (Ред. 47.6). Скрытые слои — тоже не
      ;; каркас (Ред. 48.0): выключенный или замороженный слой пропускаем.
      (setq segs    nil
            blocks  nil
            seen    nil
            polys   nil
            msegs   nil
            rblocks nil
            glaz    nil
            n-poly  0
            n-polyo 0
            n-line  0
            n-hidden 0
            n-open  0
            i       (sslength ss))
      (while (> i 0)
        (setq i   (1- i)
              e   (ssname ss i)
              ed  (entget e)
              typ (if ed (cdr (assoc 0 ed)) nil))
        (cond
          ((or (null ed) (not (mark:ent-visible? e)))
           (setq n-hidden (1+ n-hidden)))
          ((= typ "INSERT")
           (setq nm (mark:fill-eff-name e))
           (cond
             ;; Ред. 48.3: «Атрибуты витража» — префикс для марок (TEST 02/03)
             ((mark:blk-match? e *mark:block-glazing*)
              (setq glaz (cons e glaz)))
             ((mark:fill-skip-block? nm) nil)
             ((mark:fill-opening-block? nm)
              (setq n-open (1+ n-open)))
             ((mark:fill-block-seen? seen nm e) nil)
             (t
              (setq blocks (cons (list 0.0 e) blocks)
                    seen   (cons (cons nm (cdr (assoc 10 ed))) seen))
              ;; блоки-ригели: длины для шкалы «в свету» (зеркало MarkZV)
              (if (and (mark:strp nm)
                       (wcmatch (strcase nm) "*РИГЕЛЬ*"))
                (setq rblocks (cons e rblocks))))))
          ;; Ред. 48.4: замкнутая полилиния — не каркас и не ячейка.
          ;; Сетка — по динамике и мультилиниям (Ред. 48.6).
          ;; контур полилинии — только режимы точки и 7-Полилиния.
          ((and (= typ "LWPOLYLINE") (mark:fill-lwpoly-closed? e))
           (setq n-poly (1+ n-poly)))
          ;; Ред. 48.6: отрезки и дуги — не каркас (решение заказчика
          ;; 2026-09-29): в режимах сетки каркас — только мультилинии
          ;; и динамика; полуширина отрезка была допущением 25 мм.
          ((member typ '("LINE" "ARC"))
           (setq n-line (1+ n-line)))
          ((= typ "MLINE")
           (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
           (if (and r (not (vl-catch-all-error-p r)))
             (progn
               (setq segs (append r segs))
               ;; шкала: только MLINE (зеркало MarkZV, *mk:seg-types*)
               (setq msegs (append r msegs)))))
          ;; Ред. 48.6: незамкнутая полилиния и старый POLYLINE — не
          ;; каркас режима сетки; контур — режимы точки (2/6) и 7.
          ((member typ '("LWPOLYLINE" "POLYLINE"))
           (setq n-polyo (1+ n-polyo)))))
      (if (> n-hidden 0)
        (mark:out
          (strcat "[INFO] Пропущено на скрытых слоях: " (itoa n-hidden)
                  " — не каркас.")))
      (if (> n-open 0)
        (mark:out
          (strcat "[INFO] Блоков проёма в выборе пропущено: " (itoa n-open)
                  " — окно/дверь не каркас.")))
      ;; Ред. 48.3: «Атрибуты витража» из выбора каркаса — префикс марок
      (if glaz
        (progn
          (setq *mark:glazings* (reverse glaz))
          (mark:out
            (strcat "[INFO] \"Атрибуты витража\": " (itoa (length glaz))
                    " — префикс для марок."))))
      (mark:out
        (strcat "[INFO] Каркас: блоков " (itoa (length blocks))
                ", отрезков " (itoa (length segs))))
      (setq boxes nil
            vs    nil
            hs    nil)
      (cond
        ;; Только линии — дословно конвейер 1-Сетка-мультилинии:
        ;; оси, Т-объединение, проверка замкнутости grid-valid.
        ((null blocks)
         (if segs
           (progn
             (mark:out
               "[INFO] Линейный каркас — расчёт как в 1-Сетка-мультилинии.")
             (setq r (mark:fill-segs->cells segs))
             (mark:out
               (strcat "[INFO] Сетка: осей X " (itoa (cadr r))
                       " Y " (itoa (caddr r))
                       "  ячеек " (itoa (length (car r)))))
             (if (null (car r))
               (mark:out "[INFO] Мало осей — увеличьте *mark:fill-tol*."))
             ;; Ред. 48.31 (TZ п.40): оси из тех же отрезков — секции
             ;; и Т-членения линейного каркаса считаются единым
             ;; механизмом sec-columns / sec-geo-one, как в смешанном
             ;; пути. Без импостов все вертикали — границы, секция
             ;; равна своей ширине; вставки не меняются (тот же
             ;; конвейер ячеек mark:fill-segs->cells).
             (setq boxes (car r)
                   items (mark:fill-seg-items segs)
                   vs    (car items)
                   hs    (cadr items))
             (mark:out
               (strcat "[INFO] Осей: вертикальных " (itoa (length vs))
                       ", горизонтальных " (itoa (length hs)) ".")))))
        ;; Блочный и смешанный каркас: единый расчёт осей — ячейка может
        ;; быть собрана из блоков и линий (Ред. 47.7); оси нужны и для
        ;; секций/ярусов (Ред. 48.0).
        (t
         (if segs
           (mark:out "[INFO] Смешанный каркас: блоки и линии, единый расчёт осей.")
           (mark:out "[INFO] Блочный каркас — расчёт как в 3-Сетка-динамика."))
         (setq axes  (mark:fill-owned-axes blocks)
               vs    (car axes)
               hs    (cadr axes)
               items (mark:fill-seg-items segs)
               vs    (append vs (car items))
               hs    (append hs (cadr items)))
         (mark:out
           (strcat "[INFO] Осей: вертикальных " (itoa (length vs))
                   ", горизонтальных " (itoa (length hs)) "."))
         (setq boxes (mark:fill-items->cells vs hs))))
      ;; Секции/ярусы (Ред. 48.0): гео ячейки = (ширина секции,
      ;; высота яруса). Границы секции — соседние неимпостные колонки,
      ;; ярус — горизонтали, охватывающие секцию. Линейный каркас
      ;; (Т-объединение) — собственная секция, как раньше.
      (setq vs2 (if (and vs hs) (mark:sec-columns vs hs) nil))
      (setq out nil)
      (foreach bb boxes
        (setq out (cons (append bb (mark:sec-geo-one bb vs2 hs)) out)))
      (setq boxes (reverse out))
      ;; Ред. 48.8 (З-1): высоты всех ячеек каркаса (включая проёмные) —
      ;; источник ярусной шкалы: буквы не зависят от наличия заполнений.
      (setq *mark:cell-hs* nil)
      (foreach bb boxes
        (if (numberp (nth 5 bb))
          (setq *mark:cell-hs* (cons (nth 5 bb) *mark:cell-hs*))))
      ;; Ред. 48.4: замкнутые полилинии ячейками не стали — только лог.
      (if (> n-poly 0)
        (mark:out
          (strcat "[INFO] Замкнутых полилиний пропущено: " (itoa n-poly)
                  " — не каркас и не ячейки.")))
      ;; Ред. 48.6: пропущенные отрезки/дуги и полилинии — в лог.
      (if (> n-polyo 0)
        (mark:out
          (strcat "[INFO] Незамкнутых полилиний пропущено: " (itoa n-polyo)
                  " — не каркас режима сетки (контур — режимы 2/6/7).")))
      (if (> n-line 0)
        (mark:out
          (strcat "[INFO] Отрезков и дуг пропущено: " (itoa n-line)
                  " — не каркас (только мультилинии и динамика).")))
      ;; Шкала Блок 1: длины ригелей в свету — зеркало MarkZV Ред. 3.7.
      (setq *mark:scale1* (mark:sec-scale1 msegs rblocks))
      (if *mark:sec-diag* (mark:sec-diag vs hs vs2))
      ;; Фильтр проёмов: зона сетки, лог, пропуск ячеек с окнами/дверями.
      (foreach bb (mark:fill-filter-opening-cells boxes)
        (setq r (mark:fill-add cells pts bb)
              cells (car r)
              pts   (cadr r)))
      (list cells pts))))


(defun mark:fill-all-near-ss (pt win / p0 p1 filter ss ins out i)
  (setq p0 (list (- (car pt) win) (- (cadr pt) win))
        p1 (list (+ (car pt) win) (+ (cadr pt) win))
        filter (list (cons 0 "INSERT,LINE,LWPOLYLINE,POLYLINE,MLINE,ARC"))
        ss (vl-catch-all-apply 'ssget (list "C" p0 p1 filter))
        out (if (and ss (not (vl-catch-all-error-p ss))) ss nil)
        ins (mark:fill-ss-inserts pt win))
  (if ins
    (progn
      (if (null out) (setq out (ssadd)))
      (setq i (sslength ins))
      (while (> i 0)
        (setq i (1- i))
        (if (null (ssmemb (ssname ins i) out))
          (ssadd (ssname ins i) out)))))
  out)

(defun mark:fill-mode-all-point (cells pts / pt ptw win ss i e ed typ nm r segs
                                          best best-area a verts bb)
  (setq pt (getpoint "\n6-Точка-все-типы: укажите точку внутри ячейки <Enter>: "))
  (if (null pt)
    (list cells pts 'cancel)
    (progn
      (setq ptw (mark:fill-pt-wcs pt)
            win (if (and (numberp *mark:fill-window*)
                         (> *mark:fill-window* 0.0))
                  *mark:fill-window* 5000.0)
            ss  (mark:fill-all-near-ss ptw win))
      (if (null ss)
        (progn
          (setq win (* win 4.0)
                ss (mark:fill-all-near-ss ptw win)))
        nil)
      (if (null ss)
        (list cells pts)
        (progn
          ;; Все типы — в один список отрезков: блоки дают текущую
          ;; видимость (мультилиния в блоке — с полушириной), линии и
          ;; полилинии — отрезки (у MLINE — полуширина). Смешанная
          ;; ячейка «блок + линия» собирается из общего списка (Ред. 47.7).
          (setq segs nil
                best nil
                best-area nil
                i    (sslength ss))
          (while (> i 0)
            (setq i   (1- i)
                  e   (ssname ss i)
                  ed  (entget e)
                  typ (if ed (cdr (assoc 0 ed)) nil))
            (cond
              ((= typ "INSERT")
               (setq nm (mark:fill-eff-name e))
               (if (and (not (mark:fill-skip-block? nm))
                        (not (mark:fill-opening-block? nm)))
                 (progn
                   (setq r (mark:fill-segs-of-ins e))
                   (if r (setq segs (append r segs))))))
              ;; Замкнутая полилиния: контур ячейки. Если точка внутри —
              ;; это она (как в 2/7); рёбра идут в отрезки с hw 0.
              ((and (= typ "LWPOLYLINE") (mark:fill-lwpoly-closed? e))
               (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
               (if (and r (not (vl-catch-all-error-p r)))
                 (setq segs (append (mark:fill-lines-hw0 r) segs)))
               (setq verts (mark:fill-poly-verts e))
               (if (and verts (mark:fill-pt-in (car pt) (cadr pt) verts))
                 (progn
                   (setq a (mark:fill-poly-area verts))
                   (if (or (null best-area) (< a best-area))
                     (setq best-area a
                           best e)))))
              ((member typ '("LINE" "ARC" "MLINE"))
               (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
               (if (and r (not (vl-catch-all-error-p r)))
                 (setq segs (append r segs))))
              ;; Незамкнутая полилиния и старый POLYLINE: hw 0, без отступа.
              ((member typ '("LWPOLYLINE" "POLYLINE"))
               (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
               (if (and r (not (vl-catch-all-error-p r)))
                 (setq segs (append (mark:fill-lines-hw0 r) segs))))))
          (mark:out
            (strcat "[INFO] Отрезков каркаса рядом с точкой: "
                    (itoa (length segs))))
          (cond
            ;; 1) Точка в замкнутой полилинии — контур ячейки без отступа
            ;;    (как 2-Точка-мультилинии и 7-Полилиния).
            (best
             (setq bb (mark:cell-bb best))
             (if bb
               (progn
                 (mark:out
                   "[INFO] Точка в замкнутой полилинии — контур без отступа.")
                 (mark:fill-add cells pts bb))
               (progn
                 (mark:out "[WARN] Габарит ячейки не взят.")
                 (list cells pts))))
            (t
             ;; 2) Проверенная Точка-динамика: ячейка между блоками.
             (setq r (mark:fill-dyn-cell pt ptw cells pts))
             ;; 3) Общий список отрезков: лучи, затем сетка и минимальная
             ;;    ячейка под точкой (как в Точка-мультилинии).
             (if (or (null r) (null (car r)))
               (if (>= (length segs) 4)
                 (setq r (mark:fill-try-segs segs pt cells pts))))
             (if r r (list cells pts)))))))))

(defun mark:fill-mode-poly (cells pts / sel e bb r)
  (mark:out "Тыкайте в границы ячеек (замкнутые полилинии и т.п.).")
  (mark:out "Enter — конец выбора.")
  (while
    (progn
      (setq sel
        (vl-catch-all-apply 'entsel
          (list "\nГраница ячейки <Enter — конец>: ")))
      (and (not (vl-catch-all-error-p sel)) sel))
    (setq e  (car sel)
          bb (mark:cell-bb e))
    (if (null bb)
      (mark:out "[WARN] Не удалось взять габарит — пропуск.")
      (progn
        (setq r   (mark:fill-add cells pts bb)
              cells (car r)
              pts   (cadr r)))))
  (list cells pts))

(defun mark:fill-mode-poly-one (cells pts / sel e bb r)
  ;; Один клик = одна полилиния = сразу одна вставка (через points-loop).
  (setq sel (vl-catch-all-apply 'entsel
              (list "\nГраница ячейки (полилиния) <Enter — конец>: ")))
  (if (or (vl-catch-all-error-p sel) (null sel))
    (progn
      (mark:out "[INFO] Конец режима «7-Полилиния».")
      (list nil nil 'cancel))
    (progn
      (setq e  (car sel)
            bb (mark:cell-bb e))
      (if (null bb)
        (progn
          (mark:out "[WARN] Не удалось взять габарит — пропуск.")
          (list cells pts))
        (progn
          (setq r     (mark:fill-add cells pts bb)
                cells (car r)
                pts   (cadr r))
          (list cells pts))))))

;;; ---- РЕЖИМ 2: точка внутри --------------------------------------------

(defun mark:fill-ss-near (pt win / ss filter)
  ;; 1) окно + фильтр типов
  (setq filter (list (cons 0 "LINE,LWPOLYLINE,POLYLINE,MLINE,ARC")))
  (setq ss
    (vl-catch-all-apply 'ssget
      (list "C"
            (list (- (car pt) win) (- (cadr pt) win))
            (list (+ (car pt) win) (+ (cadr pt) win))
            filter)))
  (if (or (vl-catch-all-error-p ss) (null ss))
    (progn
      ;; 2) окно без фильтра
      (setq ss
        (vl-catch-all-apply 'ssget
          (list "C"
                (list (- (car pt) win) (- (cadr pt) win))
                (list (+ (car pt) win) (+ (cadr pt) win)))))
      (if (or (vl-catch-all-error-p ss) (null ss))
        (progn
          (mark:out "[INFO] Окно пустое — берём все линии чертежа (X)…")
          ;; 3) все нужные типы в чертеже (как штриховка ищет границы)
          (setq ss
            (vl-catch-all-apply 'ssget (list "X" filter)))
          (if (or (vl-catch-all-error-p ss) (null ss))
            nil
            ss))
        ss))
    ss))

(defun mark:fill-segs-near (ss pt win / i e r g typ mid d dx dy lim
                               msegs osegs mnear onear
                               mcnt ocnt base near)
  ;; MLINE-предпочтение: если есть MLINE-сегменты — сетка только по ним
  (setq msegs nil
        osegs nil
        mnear nil
        onear nil
        i    (if ss (sslength ss) 0)
        lim  (* 1.4143 (float win)))
  (repeat i
    (setq i  (1- i)
          e  (ssname ss i)
          r  (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
    (if (and (not (vl-catch-all-error-p r)) r)
      (progn
        (setq typ (cdr (assoc 0 (entget e))))
        (foreach g r
          (setq mid (list (/ (+ (nth 0 g) (nth 2 g)) 2.0)
                          (/ (+ (nth 1 g) (nth 3 g)) 2.0))
                dx  (- (car mid) (car pt))
                dy  (- (cadr mid) (cadr pt))
                d   (sqrt (+ (* dx dx) (* dy dy))))
          (if (and typ (= typ "MLINE"))
            (progn
              (setq msegs (cons g msegs))
              (if (<= d lim)
                (setq mnear (cons g mnear))))
            (progn
              (setq osegs (cons g osegs))
              (if (<= d lim)
                (setq onear (cons g onear)))))))))
  (setq msegs (reverse msegs)
        osegs (reverse osegs)
        mnear (reverse mnear)
        onear (reverse onear)
        mcnt  (length msegs)
        ocnt  (length osegs))
  (if (> mcnt 0)
    (progn
      (setq base msegs
            near mnear))
    (progn
      (setq base (append msegs osegs)
            near (append mnear onear))))
  (mark:out
    (strcat "[INFO] Отрезков MLINE: " (itoa mcnt)
            "  прочих: " (itoa ocnt)
            "  близко: " (itoa (length near))))
  (if (and near (>= (length near) 4))
    near
    base))

(defun mark:fill-cell-by-rays (segs pt / px py left right bottom top
                                     x0 y0 x1 y1 sx sy is-h is-v hw)
  (setq px (float (car pt))
        py (float (cadr pt))
        left   nil
        right  nil
        bottom nil
        top    nil)
  (foreach seg segs
    (setq x0 (min (float (nth 0 seg)) (float (nth 2 seg)))
          x1 (max (float (nth 0 seg)) (float (nth 2 seg)))
          y0 (min (float (nth 1 seg)) (float (nth 3 seg)))
          y1 (max (float (nth 1 seg)) (float (nth 3 seg)))
          hw (if (and (cddddr seg) (numberp (nth 4 seg)))
               (float (nth 4 seg))
               25.0))
    (setq is-h (<= (- y1 y0) 10.0)
          is-v (<= (- x1 x0) 10.0))
    ;; Горизонтальный отрезок над или под точкой, перекрывающий X точки
    (if (and is-h
             (<= (- x0 20.0) px)
             (>= (+ x1 20.0) px))
      (progn
        (setq sy (/ (+ y0 y1) 2.0))
        (if (<= sy py)
          ;; Нижняя мультилиния -> верхняя полка внутреннего контура: sy + hw
          (if (or (null bottom) (> (+ sy hw) bottom))
            (setq bottom (+ sy hw)))
          ;; Верхняя мультилиния -> нижняя полка внутреннего контура: sy - hw
          (if (or (null top) (< (- sy hw) top))
            (setq top (- sy hw))))))
    ;; Вертикальный отрезок слева или справа от точки, перекрывающий Y точки
    (if (and is-v
             (<= (- y0 20.0) py)
             (>= (+ y1 20.0) py))
      (progn
        (setq sx (/ (+ x0 x1) 2.0))
        (if (<= sx px)
          ;; Левая мультилиния -> правая полка внутреннего контура: sx + hw
          (if (or (null left) (> (+ sx hw) left))
            (setq left (+ sx hw)))
          ;; Правая мультилиния -> левая полка внутреннего контура: sx - hw
          (if (or (null right) (< (- sx hw) right))
            (setq right (- sx hw)))))))
  (if (and left right bottom top
           (> (- right left) 50.0)
           (> (- top bottom) 50.0))
    ;; Координаты не округляем: база вставки — точный левый нижний угол.
    (list left bottom right top)
    nil))


;;; ---- каркас из блока под точкой (динамический блок тоже) ------------
;; ssget не видит линии внутри INSERT. Сетка по выбранным MLINE/LINE не трогается.
;; Матрица (a b c d e f): x' = a*x + b*y + c, y' = d*x + e*y + f.

(defun mark:fill-mat-mul (p c / pa pb pc pd pe pf ca cb cc cd ce cf)
  (setq pa (nth 0 p) pb (nth 1 p) pc (nth 2 p)
        pd (nth 3 p) pe (nth 4 p) pf (nth 5 p)
        ca (nth 0 c) cb (nth 1 c) cc (nth 2 c)
        cd (nth 3 c) ce (nth 4 c) cf (nth 5 c))
  (list (+ (* pa ca) (* pb cd))
        (+ (* pa cb) (* pb ce))
        (+ (* pa cc) (* pb cf) pc)
        (+ (* pd ca) (* pe cd))
        (+ (* pd cb) (* pe ce))
        (+ (* pd cf) (* pe cf) pf)))

(defun mark:fill-mat-of (ed / p sx sy rot cs sn)
  (setq p   (cdr (assoc 10 ed))
        sx  (cdr (assoc 41 ed))
        sy  (cdr (assoc 42 ed))
        rot (cdr (assoc 50 ed)))
  (if (or (null sx) (not (numberp sx))) (setq sx 1.0))
  (if (or (null sy) (not (numberp sy))) (setq sy 1.0))
  (if (or (null rot) (not (numberp rot))) (setq rot 0.0))
  (if (null p) (setq p (list 0.0 0.0 0.0)))
  (setq cs (cos rot)
        sn (sin rot))
  (list (* sx cs) (* -1.0 sy sn) (float (car p))
        (* sx sn) (* sy cs)       (float (cadr p))))

(defun mark:fill-mat-pt (m pt / x y)
  (setq x (float (car pt))
        y (float (cadr pt)))
  (list (+ (* (nth 0 m) x) (* (nth 1 m) y) (nth 2 m))
        (+ (* (nth 3 m) x) (* (nth 4 m) y) (nth 5 m))))

(defun mark:fill-mat-scale (m / sx sy)
  (setq sx (sqrt (+ (* (nth 0 m) (nth 0 m)) (* (nth 3 m) (nth 3 m))))
        sy (sqrt (+ (* (nth 1 m) (nth 1 m)) (* (nth 4 m) (nth 4 m)))))
  (max sx sy 0.001))

(defun mark:fill-segs-xform (segs m / out s p0 p1 sc hw)
  (setq out nil
        sc  (mark:fill-mat-scale m))
  (foreach s segs
    (setq p0 (mark:fill-mat-pt m (list (nth 0 s) (nth 1 s)))
          p1 (mark:fill-mat-pt m (list (nth 2 s) (nth 3 s)))
          hw (if (and (> (length s) 4) (numberp (nth 4 s)))
               (* (float (nth 4 s)) sc)
               nil))
    (setq out
      (cons (if hw
              (list (car p0) (cadr p0) (car p1) (cadr p1) hw)
              (list (car p0) (cadr p0) (car p1) (cadr p1)))
            out)))
  (reverse out))

(defun mark:fill-arc-segs (ed / cen rad a0 a1 n i ang verts)
  (setq cen (cdr (assoc 10 ed))
        rad (cdr (assoc 40 ed))
        a0  (cdr (assoc 50 ed))
        a1  (cdr (assoc 51 ed)))
  (if (and cen rad a0 a1)
    (progn
      (if (< a1 a0)
        (setq a1 (+ a1 (* 2.0 pi))))
      (setq n 8
            i 0
            verts nil)
      (while (<= i n)
        (setq ang (+ a0 (* (/ (- a1 a0) (float n)) i))
              verts (cons (list (+ (float (car cen)) (* (float rad) (cos ang)))
                                (+ (float (cadr cen)) (* (float rad) (sin ang))))
                          verts)
              i (1+ i)))
      (mark:fill-verts-to-segs (reverse verts) nil))
    nil))

(defun mark:fill-poly-local (e / ed p rad verts closed)
  (setq ed (entget e)
        verts nil
        closed nil
        p (entnext e))
  (if (and ed (cdr (assoc 70 ed)) (= 1 (logand 1 (cdr (assoc 70 ed)))))
    (setq closed t))
  (while (and p (setq rad (entget p)) (/= "SEQEND" (cdr (assoc 0 rad))))
    (if (and (= "VERTEX" (cdr (assoc 0 rad))) (assoc 10 rad))
      (setq verts (cons (list (float (car (cdr (assoc 10 rad))))
                              (float (cadr (cdr (assoc 10 rad)))))
                        verts)))
    (setq p (entnext p)))
  (setq verts (reverse verts))
  (if (>= (length verts) 2)
    (mark:fill-verts-to-segs verts closed)
    nil))

;; Отрезки в системе блока. LINE/ARC читаем сами: extract-segs для LINE
;; берёт cadr после cdr и для сетки чертежа его не меняем.
(defun mark:fill-local-segs (e typ ed / p q)
  (cond
    ((= typ "LINE")
     (setq p (cdr (assoc 10 ed))
           q (cdr (assoc 11 ed)))
     (if (and p q)
       (list (list (float (car p)) (float (cadr p))
                   (float (car q)) (float (cadr q))))
       nil))
    ((= typ "ARC")
     (mark:fill-arc-segs ed))
    ((= typ "POLYLINE")
     (mark:fill-poly-local e))
    (t
     (mark:fill-extract-segs e))))

(defun mark:fill-skip-block? (nm)
  (and (mark:strp nm)
       (or (mark:name= nm *mark:block-fill*)
           (mark:name= nm *mark:block-glazing*)
           (mark:name= nm *mark:ar-block*))))

(defun mark:fill-eff-name (e / obj nm)
  (setq obj (mark:vla e)
        nm  (if obj (mark:ax-get obj "EffectiveName") nil))
  (if (mark:strp nm)
    nm
    (cdr (assoc 2 (entget e)))))

(defun mark:fill-def-segs (bname mat depth / rec e ed typ segs sub nm m2)
  (if (or (not (mark:strp bname)) (= bname "") (>= depth 6))
    nil
    (progn
      (setq rec (tblsearch "BLOCK" bname)
            e   (if rec (cdr (assoc -2 rec)) nil)
            segs nil)
      (while (and e (setq ed (entget e)) (/= "ENDBLK" (cdr (assoc 0 ed))))
        (setq typ (cdr (assoc 0 ed)))
        (cond
          ((= typ "INSERT")
           (setq nm  (cdr (assoc 2 ed))
                 m2  (mark:fill-mat-mul mat (mark:fill-mat-of ed))
                 sub (if (mark:fill-skip-block? nm)
                       nil
                       (mark:fill-def-segs nm m2 (1+ depth)))
                 segs (append sub segs)))
          ((member typ '("LINE" "ARC" "LWPOLYLINE" "POLYLINE" "MLINE"))
           (setq sub (mark:fill-local-segs e typ ed)
                 segs (append (mark:fill-segs-xform sub mat) segs))))
        (setq e (entnext e)))
      segs)))

(defun mark:fill-mat-inv (m / a b c d e f det)
  (setq a (nth 0 m) b (nth 1 m) c (nth 2 m)
        d (nth 3 m) e (nth 4 m) f (nth 5 m)
        det (- (* a e) (* b d)))
  (if (< (abs det) 1.0e-9)
    nil
    (list (/ e det) (/ (- b) det) (/ (- (* b f) (* e c)) det)
          (/ (- d) det) (/ a det) (/ (- (* d c) (* a f)) det))))

(defun mark:fill-pt-wcs (pt / p)
  (setq p (vl-catch-all-apply 'trans (list pt 1 0)))
  (if (or (vl-catch-all-error-p p) (null p))
    pt
    p))

(defun mark:fill-def-bb (bname / segs s x0 y0 x1 y1)
  (setq segs (mark:fill-def-segs bname (list 1.0 0.0 0.0 0.0 1.0 0.0) 0))
  (if (null segs)
    nil
    (progn
      (setq x0 (nth 0 (car segs))
            y0 (nth 1 (car segs))
            x1 x0
            y1 y0)
      (foreach s segs
        (setq x0 (min x0 (nth 0 s) (nth 2 s))
              y0 (min y0 (nth 1 s) (nth 3 s))
              x1 (max x1 (nth 0 s) (nth 2 s))
              y1 (max y1 (nth 1 s) (nth 3 s))))
      (list x0 y0 x1 y1))))

(defun mark:fill-def-bb-cached (bname / pair bb)
  (setq pair (assoc bname *mark:dyn-bb*))
  (if pair
    (cdr pair)
    (progn
      (setq bb (mark:fill-def-bb bname))
      (setq *mark:dyn-bb* (cons (cons bname bb) *mark:dyn-bb*))
      bb)))

(defun mark:fill-pt-in-ins (pt e / ed mat inv bb lp nm)
  (setq ed (entget e)
        nm (if ed (cdr (assoc 2 ed)) nil)
        mat (if ed (mark:fill-mat-of ed) nil)
        inv (if mat (mark:fill-mat-inv mat) nil)
        bb (if (mark:strp nm) (mark:fill-def-bb-cached nm) nil))
  (if (or (null inv) (null bb))
    nil
    (progn
      (setq lp (mark:fill-mat-pt inv pt))
      (and (>= (car lp) (- (nth 0 bb) 1.0))
           (<= (car lp) (+ (nth 2 bb) 1.0))
           (>= (cadr lp) (- (nth 1 bb) 1.0))
           (<= (cadr lp) (+ (nth 3 bb) 1.0))))))

(defun mark:fill-hits-best (hits / best)
  (setq best nil)
  (foreach h hits
    (if (or (null best) (< (car h) (car best)))
      (setq best h)))
  best)

(defun mark:fill-hits-drop (hits h / out dropped)
  (setq out nil
        dropped nil)
  (foreach x hits
    (if (and (null dropped)
             (eq (cadr x) (cadr h)))
      (setq dropped t)
      (setq out (cons x out))))
  (reverse out))

(defun mark:fill-hits-sort (hits / best out)
  (setq out nil)
  (while hits
    (setq best (mark:fill-hits-best hits)
          out  (cons best out)
          hits (mark:fill-hits-drop hits best)))
  (reverse out))

(defun mark:fill-hits-take (hits n / out)
  (setq out nil)
  (while (and hits (> n 0))
    (setq out (cons (car hits) out)
          hits (cdr hits)
          n (1- n)))
  (reverse out))

(defun mark:fill-hit-area (pt e / bb x y a)
  (setq x (float (car pt))
        y (float (cadr pt))
        bb (mark:cell-bb e)
        a nil)
  (if (and bb
           (>= x (nth 0 bb)) (<= x (nth 2 bb))
           (>= y (nth 1 bb)) (<= y (nth 3 bb))
           (> (- (nth 2 bb) (nth 0 bb)) 1.0)
           (> (- (nth 3 bb) (nth 1 bb)) 1.0))
    (setq a (* (- (nth 2 bb) (nth 0 bb))
               (- (nth 3 bb) (nth 1 bb)))))
  (if (and (null a) (mark:fill-pt-in-ins pt e))
    (progn
      (setq bb (mark:fill-def-bb-cached (cdr (assoc 2 (entget e)))))
      (if bb
        (setq a (max 1.0 (* (- (nth 2 bb) (nth 0 bb))
                            (- (nth 3 bb) (nth 1 bb))))))))
  a)

(defun mark:fill-ss-inserts (pt win / ss)
  (setq ss (vl-catch-all-apply 'ssget
             (list "C"
                   (list (- (float (car pt)) win) (- (float (cadr pt)) win))
                   (list (+ (float (car pt)) win) (+ (float (cadr pt)) win))
                   (list (cons 0 "INSERT")))))
  (if (or (vl-catch-all-error-p ss) (null ss))
    nil
    ss))

(defun mark:fill-ins-dist (pt e / ed p dx dy)
  (setq ed (entget e)
        p  (if ed (cdr (assoc 10 ed)) nil))
  (if (or (null p) (null (car p)) (null (cadr p)))
    1.0e99
    (progn
      (setq dx (- (float (car p)) (float (car pt)))
            dy (- (float (cadr p)) (float (cadr pt))))
      (sqrt (+ (* dx dx) (* dy dy))))))


(defun mark:fill-bb-dist (pt e / bb x y dx dy)
  (setq bb (mark:cell-bb e)
        x  (float (car pt))
        y  (float (cadr pt)))
  (if (and bb
           (> (abs (- (nth 2 bb) (nth 0 bb))) 1.0)
           (> (abs (- (nth 3 bb) (nth 1 bb))) 1.0))
    (progn
      (setq dx (max 0.0 (- (nth 0 bb) x) (- x (nth 2 bb)))
            dy (max 0.0 (- (nth 1 bb) y) (- y (nth 3 bb))))
      (sqrt (+ (* dx dx) (* dy dy))))
    (mark:fill-ins-dist pt e)))

(defun mark:fill-hit-report (hits / acc pair nm out first piece)
  (setq acc nil)
  (foreach hit hits
    (setq nm (mark:fill-eff-name (cadr hit))
          pair (assoc nm acc))
    (if pair
      (setq acc (subst (cons nm (1+ (cdr pair))) pair acc))
      (setq acc (cons (cons nm 1) acc))))
  (setq out nil
        first t)
  (foreach pair acc
    (setq piece (strcat (if (car pair) (car pair) "?") "=" (itoa (cdr pair))))
    (if first
      (setq first nil
            out piece)
      (setq out (strcat out " " piece))))
  (mark:out
    (strcat "[INFO] Блоков каркаса: " (itoa (length hits))
            (if out (strcat " (" out ")") ""))))

(defun mark:fill-near-inserts (pt win / ss i e nm d hits)
  (mark:out
    (strcat "[INFO] Ищу вставки в пределах " (rtos win 2 0) " от точки…"))
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list (cons 0 "INSERT"))))
        hits nil)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (if (and (not (mark:fill-skip-block? nm))
                 (not (mark:fill-opening-block? nm)))
          (progn
            (setq d (mark:fill-bb-dist pt e))
            (if (<= d win)
              (setq hits (cons (list d e) hits))))))))
  (mark:fill-hits-take (mark:fill-hits-sort hits) 40))

(defun mark:fill-set-near (hits pt win / best d)
  (setq best nil)
  (foreach hit hits
    (setq d (mark:fill-bb-dist pt (cadr hit)))
    (if (or (null best) (< d best))
      (setq best d)))
  (and best (<= best win)))

(defun mark:fill-hits-segs (hits / segs part)
  (setq segs nil
        *mark:dyn-quiet* t)
  (foreach hit hits
    (setq part (mark:fill-segs-of-ins (cadr hit)))
    (if part
      (setq segs (append part segs))))
  (setq *mark:dyn-quiet* nil)
  segs)

(defun mark:fill-pick-frame (/ ss i e nm hits n-open)
  (mark:out "[INFO] Выберите стойки и ригели рамкой. Enter — пропуск.")
  (setq ss (vl-catch-all-apply 'ssget
             (list (list (cons 0 "INSERT")))))
  (setq hits nil
        n-open 0)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (cond
          ((mark:fill-skip-block? nm) nil)
          ;; Окно/дверь — заполнение, не каркас: линии рамы заходят
          ;; в световой проём (нахлёст) и занижают ячейку. Каркас —
          ;; только стойки, ригели и направляющие.
          ((mark:fill-opening-block? nm)
           (setq n-open (1+ n-open)))
          (t (setq hits (cons (list 0.0 e) hits)))))))
  (if (> n-open 0)
    (mark:out
      (strcat "[INFO] Блоков проёма в выборе пропущено: " (itoa n-open)
              " — окно/дверь не каркас.")))
  (if hits
    (mark:fill-hit-report hits))
  hits)

(defun mark:fill-insert-hits (ptu ptw / ss hits win)
  ;; Пустая ячейка не лежит внутри габарита стойки. Берём все вставки рядом.
  (setq win (if (and (numberp *mark:fill-window*) (> *mark:fill-window* 0.0))
              *mark:fill-window*
              5000.0)
        ss  (mark:fill-ss-inserts ptu win)
        hits nil)
  (if ss
    (setq hits (mark:fill-hits-take
                 (mark:fill-hits-sort (mark:fill-ss-hits ss ptw))
                 40)))
  (if (null hits)
    (setq hits (mark:fill-near-inserts ptw win)))
  (if (null hits)
    (setq hits (mark:fill-near-inserts ptw (* win 4.0))))
  (mark:fill-hit-report hits)
  hits)

(defun mark:fill-ss-hits (ss pt / i e nm hits)
  (setq hits nil
        i (if ss (sslength ss) 0))
  (repeat i
    (setq i (1- i)
          e (ssname ss i)
          nm (mark:fill-eff-name e))
    (if (and (not (mark:fill-skip-block? nm))
             (not (mark:fill-opening-block? nm)))
      (setq hits (cons (list (mark:fill-bb-dist pt e) e) hits))))
  hits)

(defun mark:fill-inserts-at (pt / hits)
  (setq hits (mark:fill-insert-hits pt pt))
  (if hits (cadr (car hits)) nil))

(defun mark:fill-block-segs-at (pt / e ed nm mat segs)
  (setq e (mark:fill-inserts-at pt))
  (if (null e)
    nil
    (progn
      (setq ed   (entget e)
            nm   (cdr (assoc 2 ed))
            mat  (mark:fill-mat-of ed)
            segs (mark:fill-def-segs nm mat 0))
      (mark:out
        (strcat "[INFO] Блок под точкой: «" (mark:fill-eff-name e)
                "», отрезков " (itoa (length segs))))
      segs)))

(defun mark:fill-seg-hw (s)
  (if (and (> (length s) 4) (numberp (nth 4 s)))
    (float (nth 4 s))
    25.0))

(defun mark:fill-box-closed (segs box / x0 y0 x1 y1 tol s xa ya xb yb hw sx sy
                                  okL okR okB okT)
  ;; Четыре стороны должны доходить до углов. Иначе это не ячейка.
  (setq x0 (nth 0 box)
        y0 (nth 1 box)
        x1 (nth 2 box)
        y1 (nth 3 box)
        tol *mark:fill-tol*
        okL nil
        okR nil
        okB nil
        okT nil)
  (foreach s segs
    (setq xa (min (float (nth 0 s)) (float (nth 2 s)))
          ya (min (float (nth 1 s)) (float (nth 3 s)))
          xb (max (float (nth 0 s)) (float (nth 2 s)))
          yb (max (float (nth 1 s)) (float (nth 3 s)))
          hw (mark:fill-seg-hw s)
          sx (/ (+ (float (nth 0 s)) (float (nth 2 s))) 2.0)
          sy (/ (+ (float (nth 1 s)) (float (nth 3 s))) 2.0))
    (if (and (<= (- xb xa) 10.0)
             (<= ya (+ y0 tol))
             (>= yb (- y1 tol)))
      (progn
        (if (<= (abs (- (+ sx hw) x0)) tol) (setq okL t))
        (if (<= (abs (- (- sx hw) x1)) tol) (setq okR t))))
    (if (and (<= (- yb ya) 10.0)
             (<= xa (+ x0 tol))
             (>= xb (- x1 tol)))
      (progn
        (if (<= (abs (- (+ sy hw) y0)) tol) (setq okB t))
        (if (<= (abs (- (- sy hw) y1)) tol) (setq okT t)))))
  (and okL okR okB okT))

(defun mark:fill-try-segs (segs pt cells pts / bb r)
  (setq bb (mark:fill-cell-by-rays segs pt))
  (cond
    ((and bb (mark:fill-box-closed segs bb))
     (mark:out "[INFO] Внутренний контур ячейки найден лучами.")
     (mark:fill-add cells pts bb))
    (bb
     (mark:out "[INFO] Лучи нашли незамкнутый контур — заполнение не ставлю.")
     nil)
    (t
     (setq r  (mark:fill-segs->cells segs)
           bb (mark:fill-smallest-cell (car r) pt))
     (mark:out
       (strcat "[INFO] Сетка блока: осей X " (itoa (cadr r))
               " Y " (itoa (caddr r))
               "  ячеек " (itoa (length (car r)))))
     (cond
       ((and bb (mark:fill-box-closed segs bb))
        (mark:fill-add cells pts bb))
       (bb
        (mark:out "[INFO] Ячейка сетки не замкнута — пропуск.")
        nil)
       (t nil)))))

(defun mark:fill-mode-point (cells pts / pt ss i e verts bb
                                 best best-area a r segs win)
  (setq pt (getpoint "\nТочка-мультилинии внутри ячейки <Enter — конец>: "))
  (if (null pt)
    (progn
      (mark:out "[INFO] Конец режима «Точка-мультилинии».")
      (list nil nil 'cancel))
    (progn
      (setq ss (vl-catch-all-apply 'ssget
                 (list "X" (list (cons 0 "LWPOLYLINE")))))
      (setq best nil
            best-area nil)
      (if (and ss (not (vl-catch-all-error-p ss)))
        (progn
          (setq i (sslength ss))
          (repeat i
            (setq i    (1- i)
                  e    (ssname ss i)
                  verts (mark:fill-poly-verts e))
            (if (and verts
                     (mark:fill-pt-in (car pt) (cadr pt) verts))
              (progn
                (setq a (mark:fill-poly-area verts))
                (if (or (null best-area) (< a best-area))
                  (progn
                    (setq best-area a
                          best e))))))))
      (if best
        (progn
          (setq bb (mark:cell-bb best))
          (if bb
            (progn
              (setq r    (mark:fill-add cells pts bb)
                    cells (car r)
                    pts   (cadr r))
              (list cells pts))
            (progn
              (mark:out "[WARN] Габарит ячейки не взят.")
              (list cells pts))))
        (progn
          (mark:out "[INFO] Замкнутой полилинии нет — ищем по сетке линий…")
          (setq win (if (and (numberp *mark:fill-window*)
                             (> *mark:fill-window* 0.0))
                      *mark:fill-window*
                      5000.0))
          (setq ss (mark:fill-ss-near pt win))
          (if (null ss)
            (progn
              (mark:out "[INFO] Линий не найдено (окно и X-выбор пусты).")
              (list cells pts))
            (progn
              (mark:fill-type-stat ss)
              (setq segs (mark:fill-segs-near ss pt win))
              (if (null segs)
                (progn
                  (mark:out "[WARN] Отрезки не извлечены — см. типы выше.")
                  (list cells pts))
                (progn
                  ;; Сначала ищем непосредственный локальный внутренний контур лучами вокруг точки
                  (setq bb (mark:fill-cell-by-rays segs pt))
                  (if bb
                    (progn
                      (mark:out "[INFO] Внутренний контур ячейки найден лучами.")
                      (setq r    (mark:fill-add cells pts bb)
                            cells (car r)
                            pts   (cadr r))
                      (list cells pts))
                    (progn
                      ;; Иначе расчет по общей сетке
                      (setq r  (mark:fill-segs->cells segs)
                            bb (mark:fill-smallest-cell (car r) pt))
                      (mark:out
                        (strcat "[INFO] Сетка: осей X " (itoa (cadr r))
                                " Y " (itoa (caddr r))
                                "  ячеек " (itoa (length (car r)))))
                      (if (null bb)
                        (progn
                          (mark:out "[INFO] Ячейка под точкой не найдена.")
                          (if (< (cadr r) 2)
                            (mark:out
                              "[INFO] Мало осей — увеличьте *mark:fill-window* или *mark:fill-tol*."))
                          (list cells pts))
                        (progn
                          (setq r    (mark:fill-add cells pts bb)
                                cells (car r)
                                pts   (cadr r))
                          (list cells pts))))))))))))))

;;; ---- замкнутость ячеек сетки мультилиний ----------------------------
;; Ячейка нужна только в замкнутом контуре: стойки на обеих осях
;; перекрывают её по высоте целиком, ригели сверху и снизу — по ширине
;; (стыки ригелей в допуск схлопываются). Иначе заполнение прилипает к
;; разорванному/незамкнутому участку между мультилиниями: например, к
;; линии парапета, до которой стойки не доходят, — и после объединения
;; Т-стыков получается одна огромная ячейка на весь разрыв.

;; Каркас для проверок: стойки (ось y0 y1), ригели (ось x0 x1).
(defun mark:fill-frame-vh (segs / v h s a b c d)
  (setq v nil
        h nil)
  (foreach s segs
    (setq a (min (float (nth 0 s)) (float (nth 2 s)))
          b (max (float (nth 0 s)) (float (nth 2 s)))
          c (min (float (nth 1 s)) (float (nth 3 s)))
          d (max (float (nth 1 s)) (float (nth 3 s))))
    (cond
      ((eq (mark:fill-axis s) 'v)
       (setq v (cons (list (/ (+ a b) 2.0) c d) v)))
      ((eq (mark:fill-axis s) 'h)
       (setq h (cons (list (/ (+ c d) 2.0) a b) h)))))
  (list v h))

;; Есть ли на оси ax стойка, перекрывающая высоту [lo..hi] целиком.
(defun mark:fill-post-covers (posts ax lo hi / it hit)
  (setq hit nil)
  (foreach it posts
    (if (and (null hit)
             (<= (abs (- (nth 0 it) ax)) (+ (* 2.0 *mark:fill-tol*) 10.0))
             (<= (nth 1 it) (+ lo *mark:fill-tol*))
             (>= (nth 2 it) (- hi *mark:fill-tol*)))
      (setq hit t)))
  hit)

;; Закрывают ли ригели оси ay просвет [lo..hi] по ширине (стыки в допуск).
(defun mark:fill-rail-covers (rails ay lo hi / spans it)
  (setq spans nil)
  (foreach it rails
    (if (<= (abs (- (nth 0 it) ay)) (+ *mark:fill-tol* 25.0))
      (setq spans (cons (list (nth 1 it) (nth 2 it)) spans))))
  (mark:fill-covers spans lo hi *mark:fill-tol*))

;; Прогон ячеек сетки: шире/выше предела — разрыв между участками;
;; без полного перекрытия сторон — незамкнутый контур, заполнение не ставим.
(defun mark:fill-grid-valid (cells segs / vh posts rails out cell
                                    ax0 ay0 ax1 ay1 cw ch n-gap n-open)
  (setq vh     (mark:fill-frame-vh segs)
        posts  (car vh)
        rails  (cadr vh)
        out    nil
        n-gap  0
        n-open 0)
  (foreach cell cells
    ;; ячейка хранит внутренний контур: ось профиля = грань +- 25 мм
    (setq ax0 (- (nth 0 cell) 25.0)
          ay0 (- (nth 1 cell) 25.0)
          ax1 (+ (nth 2 cell) 25.0)
          ay1 (+ (nth 3 cell) 25.0)
          cw  (- (nth 2 cell) (nth 0 cell))
          ch  (- (nth 3 cell) (nth 1 cell)))
    (if (or (and (numberp *mark:fill-max-w*) (> cw *mark:fill-max-w*))
            (and (numberp *mark:fill-max-h*) (> ch *mark:fill-max-h*)))
      (setq n-gap (1+ n-gap))
      (if (not (and (mark:fill-post-covers posts ax0 ay0 ay1)
                    (mark:fill-post-covers posts ax1 ay0 ay1)
                    (mark:fill-rail-covers rails ay0 ax0 ax1)
                    (mark:fill-rail-covers rails ay1 ax0 ax1)))
        (setq n-open (1+ n-open))
        (setq out (cons cell out)))))
  (if (> (+ n-gap n-open) 0)
    (mark:out
      (strcat "[INFO] Пропущено ячеек: разрыв между участками " (itoa n-gap)
              ", незамкнутый контур " (itoa n-open) ".")))
  (reverse out))

(defun mark:fill-segs->cells (segs / xs ys sx sy x0 x1 y0 y1
                                  ix iy nxs nys out ax tmp v
                                  mx col-ys col-sy seg-x0 seg-x1 seg-y0
                                  has-v0 has-v1 seg-y1)
  (setq xs nil
        ys nil)
  (foreach seg segs
    (setq ax (mark:fill-axis seg))
    (cond
      ((eq ax 'v)
       (setq xs (cons (/ (+ (nth 0 seg) (nth 2 seg)) 2.0) xs)))
      ((eq ax 'h)
       (setq ys (cons (/ (+ (nth 1 seg) (nth 3 seg)) 2.0) ys)))))
  (setq sx (mark:fill-axes-avg xs)
        sy (mark:fill-axes-avg ys))
  ;; Фильтр ложных нулей
  (if (and sy (> (length sy) 1) (< (car sy) 50.0) (> (cadr sy) 1000.0))
    (setq sy (cdr sy)))
  (if (and sx (> (length sx) 1) (< (car sx) 50.0) (> (cadr sx) 1000.0))
    (setq sx (cdr sx)))
  ;; Оси не округляем: угол вставки сетки — точная координата.
  (setq nxs (length sx)
        nys (length sy)
        out nil)
  ;; Проходим по каждому вертикальному пролету [x0 .. x1]
  (if (and (>= nxs 2) (>= nys 2))
    (progn
      (setq ix 0)
      (while (< ix (1- nxs))
        (setq x0 (nth ix sx)
              x1 (nth (1+ ix) sx)
              mx (/ (+ x0 x1) 2.0)
              col-ys nil)
        ;; Отбираем только те горизонтальные ригели, которые физически перекрывают центр пролета mx
        (foreach seg segs
          (if (eq (mark:fill-axis seg) 'h)
            (progn
              (setq seg-x0 (min (float (nth 0 seg)) (float (nth 2 seg)))
                    seg-x1 (max (float (nth 0 seg)) (float (nth 2 seg)))
                    seg-y0 (/ (+ (nth 1 seg) (nth 3 seg)) 2.0))
              (if (and (<= (- seg-x0 *mark:fill-tol*) mx)
                       (>= (+ seg-x1 *mark:fill-tol*) mx))
                (setq col-ys (cons seg-y0 col-ys))))))
        (setq col-sy (mark:fill-axes-avg col-ys))
        (if (and col-sy (> (length col-sy) 1) (< (car col-sy) 50.0) (> (cadr col-sy) 1000.0))
          (setq col-sy (cdr col-sy)))
        ;; ВАЖНО: если в этом пролете нет минимум 2 собственных ригелей — это пустой межвитражный разрыв!
        ;; Никакого фолбэка на чужие глобальные ригели!
        (if (and col-sy (>= (length col-sy) 2))
          (progn
            (setq iy 0)
            (while (< iy (1- (length col-sy)))
              (setq y0 (nth iy col-sy)
                    y1 (nth (1+ iy) col-sy))
              ;; Проверяем, что стойки x0 и x1 физически соприкасаются с ячейкой [y0 .. y1]
              (setq has-v0 nil
                    has-v1 nil)
              (foreach seg segs
                (if (eq (mark:fill-axis seg) 'v)
                  (progn
                    (setq seg-y0 (min (float (nth 1 seg)) (float (nth 3 seg)))
                          seg-y1 (max (float (nth 1 seg)) (float (nth 3 seg)))
                          seg-x0 (/ (+ (nth 0 seg) (nth 2 seg)) 2.0))
                    (if (and (<= (- seg-y0 *mark:fill-tol*) y1)
                             (>= (+ seg-y1 *mark:fill-tol*) y0))
                      (progn
                        (if (<= (abs (- seg-x0 x0)) *mark:fill-tol*) (setq has-v0 t))
                        (if (<= (abs (- seg-x0 x1)) *mark:fill-tol*) (setq has-v1 t)))))))
              ;; Создаем ячейку с отступом полуширины профиля 25 мм
              (if (and has-v0 has-v1
                       (> (- x1 x0) 70.0)
                       (> (- y1 y0) 70.0))
                (setq out (cons (list (+ x0 25.0)
                                      (+ y0 25.0)
                                      (- x1 25.0)
                                      (- y1 25.0))
                                out)))
              (setq iy (1+ iy)))))
        (setq ix (1+ ix)))))
  (list (mark:fill-grid-valid (mark:fill-merge-t-cells out segs) segs)
        nxs nys (length segs)))
(defun mark:fill-merge-t-cells (cells segs / changed out a b x0a y0a x1a y1a
                                           x0b y0b x1b y1b gap mid-x my has-col rest cand merged)
  (setq out cells
        changed t)
  (while changed
    (setq changed nil
          cand    out
          out     nil)
    (while cand
      (setq a    (car cand)
            cand (cdr cand)
            merged nil)
      (setq x0a (nth 0 a)
            y0a (nth 1 a)
            x1a (nth 2 a)
            y1a (nth 3 a))
      (setq rest nil)
      (foreach b cand
        ;; Проверяем, лежат ли ячейки на одном высотном уровне
        (if (and (null merged)
                 (<= (abs (- y0a (nth 1 b))) 5.0)
                 (<= (abs (- y1a (nth 3 b))) 5.0))
          (progn
            (setq x0b (nth 0 b)
                  x1b (nth 2 b))
            ;; Зазор между внутренними контурами ячеек (ширина профиля стойки ~50 мм)
            (setq gap (min (abs (- x0b x1a)) (abs (- x0a x1b))))
            (if (<= gap (+ 50.0 (* 2.0 *mark:fill-tol*)))
              (progn
                (setq mid-x (if (<= x1a x0b)
                              (/ (+ x1a x0b) 2.0)
                              (/ (+ x1b x0a) 2.0))
                      my    (/ (+ y0a y1a) 2.0)
                      has-col nil)
                ;; Проверяем, есть ли реальная стойка между этими двумя ячейками на высоте my
                (foreach seg segs
                  (if (eq (mark:fill-axis seg) 'v)
                    (progn
                      (if (and (<= (min (float (nth 1 seg)) (float (nth 3 seg))) (- my 15.0))
                               (>= (max (float (nth 1 seg)) (float (nth 3 seg))) (+ my 15.0))
                               (<= (abs (- (/ (+ (nth 0 seg) (nth 2 seg)) 2.0) mid-x)) (+ *mark:fill-tol* 10.0)))
                        (setq has-col t)))))
                (if (not has-col)
                  (progn
                    ;; Стойки между ячейками нет (Т-стык снизу!) — объединяем в одно широкое заполнение
                    (setq a (list (min x0a x0b) y0a (max x1a x1b) y1a)
                          merged  t
                          changed t))
                  (setq rest (cons b rest))))
              (setq rest (cons b rest))))
          (setq rest (cons b rest))))
      (setq cand rest
            out  (cons a out))))
  (reverse out))

(defun mark:fill-cell-has (bb pt)
  (and pt
       (>= (float (car pt)) (nth 0 bb))
       (<= (float (car pt)) (nth 2 bb))
       (>= (float (cadr pt)) (nth 1 bb))
       (<= (float (cadr pt)) (nth 3 bb))))

(defun mark:fill-smallest-cell (cells pt / best best-area a)
  (setq best nil
        best-area nil)
  (foreach bb cells
    (if (mark:fill-cell-has bb pt)
      (progn
        (setq a (* (- (nth 2 bb) (nth 0 bb))
                   (- (nth 3 bb) (nth 1 bb))))
        (if (or (null best-area) (< a best-area))
          (progn
            (setq best-area a
                  best bb))))))
  best)

;;; ---- РЕЖИМ 3: массив мультилиний > сетка ------------------------------

;; отрезки из LINE / LWPOLYLINE > ((x0 y0 x1 y1) ...)
(defun mark:fill-e-bb (e / obj r a b x0 y0 x1 y1)
  ;; AABB через GetBoundingBox с отсечением аномалий (0,0)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (vl-catch-all-error-p obj) (null obj))
    nil
    (progn
      (setq a nil
            b nil)
      ;; vla-форма: vlax-invoke-method не гарантирует возврат по ссылке
      (setq r (vl-catch-all-apply 'vla-GetBoundingBox
                 (list obj 'a 'b)))
      (if (or (vl-catch-all-error-p r) (null a) (null b))
        nil
        (progn
          (setq a (vl-catch-all-apply 'vlax-safearray->list (list a))
                b (vl-catch-all-apply 'vlax-safearray->list (list b)))
          (if (or (vl-catch-all-error-p a) (vl-catch-all-error-p b)
                  (null a) (null b))
            nil
            (progn
              (setq x0 (float (car a))
                    y0 (float (cadr a))
                    x1 (float (car b))
                    y1 (float (cadr b)))
              ;; Отсекаем артефакты: если y0 или x0 схлопнулись в 0.0 при координатах > 1000
              (if (and (< y0 10.0) (> y1 1000.0))
                (setq y0 y1))
              (if (and (< x0 10.0) (> x1 1000.0))
                (setq x0 x1))
              (list x0 y0 x1 y1))))))))

;;; ---- разбор вершин > отрезки -----------------------------------------

(defun mark:fill-verts-to-segs (verts closed / segs n i p0 p1)
  (setq segs nil
        n    (length verts)
        i    0)
  (while (< i n)
    (setq p0 (nth i verts)
          p1 (nth (rem (1+ i) n) verts))
    (if (or (< i (1- n)) closed)
      (setq segs (cons (list (float (car p0)) (float (cadr p0))
                             (float (car p1)) (float (cadr p1)))
                       segs)))
    (setq i (1+ i)))
  (reverse segs))

(defun mark:round1 (v)
  (atof (rtos (float v) 2 0)))

(defun mark:mline-warn (msg)
  (if (and (numberp *mark:mline-warns*) (< *mark:mline-warns* 3))
    (progn
      (mark:out (strcat "[WARN] MLINE: " msg))
      (setq *mark:mline-warns* (1+ *mark:mline-warns*))
      (if (= *mark:mline-warns* 3)
        (mark:out
          "[WARN] MLINE: далее предупреждения о вершинах не выводятся."))))
  nil)

;; Фактическая полуширина мультилинии — из её габарита. Профили бывают не
;; только 50 мм: жёсткие 25 мм по умолчанию занижают ячейку (стойка 40 мм
;; даёт минус 5 мм с каждой стороны). Габарит берём в системе самого блока,
;; матрица вставки затем масштабирует полуширину (mark:fill-segs-xform).
(defun mark:fill-mline-hw (e segs / bb ax w h)
  (setq bb (mark:fill-e-bb e)
        ax (if segs (mark:fill-axis (car segs)) nil))
  (if (and bb ax)
    (progn
      (setq w (- (nth 2 bb) (nth 0 bb))
            h (- (nth 3 bb) (nth 1 bb)))
      (cond
        ;; прямая горизонтальная: толщина = высота габарита
        ((and (eq ax 'h) (<= h (+ (* 3.0 *mark:fill-tol*) 10.0))) (/ h 2.0))
        ;; прямая вертикальная: толщина = ширина габарита
        ((and (eq ax 'v) (<= w (+ (* 3.0 *mark:fill-tol*) 10.0))) (/ w 2.0))
        ;; изогнутая или крупная — как раньше, без полуширины
        (t nil)))
    nil))

;; Отрезки мультилинии: ось из вершин, полуширина — пятым элементом.
(defun mark:fill-mline-segs-hw (e segs / hw out s)
  (setq hw  (mark:fill-mline-hw e segs)
        out nil)
  (if (null hw)
    segs
    (progn
      (foreach s segs
        (setq out
          (cons (list (nth 0 s) (nth 1 s) (nth 2 s) (nth 3 s) hw) out)))
      (reverse out))))

(defun mark:fill-extract-segs (e / ed typ verts segs i n ang p rad
                               bb x0 y0 x1 y1 w h yc xc tol closed)
  (setq ed   (entget e)
        segs nil)
  (if (null ed)
    nil
    (progn
      (setq typ (cdr (assoc 0 ed)))
      (cond
        ((= typ "LINE")
         (setq p   (cdr (assoc 10 ed))
               rad (cdr (assoc 11 ed)))
         (if (and p rad)
           (setq segs
             (list (list (float (cadr p))   (float (caddr p))
                         (float (cadr rad)) (float (caddr rad)))))))

        ((= typ "ARC")
         (setq p   (cdr (assoc 10 ed))
               rad (cdr (assoc 40 ed)))
         (if (and p rad (cdr (assoc 50 ed)) (cdr (assoc 51 ed)))
           (progn
             (setq n     16
                   i     0
                   verts nil)
             (while (<= i n)
               (setq ang   (+ (cdr (assoc 50 ed))
                              (* (/ (- (cdr (assoc 51 ed))
                                       (cdr (assoc 50 ed)))
                                    (float n))
                                 i))
                     ang   (* ang (/ pi 180.0))
                     verts (cons (list
                                   (+ (float (cadr p))
                                      (* (float rad) (cos ang)))
                                   (+ (float (caddr p))
                                      (* (float rad) (sin ang))))
                                 verts)
                     i     (1+ i)))
             (setq segs (mark:fill-verts-to-segs (reverse verts) nil)))))

        ((or (= typ "LWPOLYLINE") (= typ "POLYLINE"))
         (setq verts nil
               closed nil)
         (if (= typ "LWPOLYLINE")
           (progn
             (if (and (cdr (assoc 70 ed))
                      (= 1 (logand 1 (cdr (assoc 70 ed)))))
               (setq closed t))
             (foreach p ed
               (if (= (car p) 10)
                 (setq verts (cons (list (float (cadr p))
                                         (float (caddr p)))
                                   verts))))
             (setq verts (reverse verts)))
           (progn
             (if (and (cdr (assoc 70 ed))
                      (= 1 (logand 1 (cdr (assoc 70 ed)))))
               (setq closed t))
             (setq p (entnext e))
             (while p
               (setq rad (entget p))
               (if (or (null rad) (= "SEQEND" (cdr (assoc 0 rad))))
                 (setq p nil)
                 (progn
                   (if (= "VERTEX" (cdr (assoc 0 rad)))
                     (setq verts (cons (list (float (cadr (assoc 10 rad)))
                                             (float (cadr (assoc 11 rad))))
                                       verts)))
                   (setq p (entnext p)))))
             (setq verts (reverse verts))))
         (if (>= (length verts) 2)
           (setq segs (mark:fill-verts-to-segs verts closed))))

        ((= typ "MLINE")
         (setq verts nil
               tol   *mark:fill-tol*)
         (foreach p ed
           (if (= (car p) 10)
             (setq verts (cons (list (float (cadr p))
                                     (float (caddr p)))
                               verts))))
         (setq verts (reverse verts))
         (if (< (length verts) 2)
           (progn
             (setq verts nil)
             (foreach p ed
               (if (= (car p) 11)
                 (setq verts (cons (list (float (cadr p))
                                         (float (caddr p)))
                                   verts))))
             (setq verts (reverse verts))))
         (if (>= (length verts) 2)
           (setq segs (mark:fill-mline-segs-hw
                         e (mark:fill-verts-to-segs verts nil)))
           (progn
             (setq bb (mark:fill-e-bb e))
             (if (null bb)
               (mark:mline-warn "нет вершин и нет габарита — пропуск.")
               (progn
                 (setq x0 (nth 0 bb)
                       y0 (nth 1 bb)
                       x1 (nth 2 bb)
                       y1 (nth 3 bb)
                       w  (- x1 x0)
                       h  (- y1 y0))
                 (cond
                   ;; Горизонтальная мультилиния (осевая по yc, полуширина h/2)
                   ((and (>= w 10.0) (<= h (+ (* 3.0 tol) 10.0)))
                    (setq yc (/ (+ y0 y1) 2.0))
                    (setq segs (list (list x0 yc x1 yc (/ h 2.0)))))
                   ;; Вертикальная мультилиния (осевая по xc, полуширина w/2)
                   ((and (>= h 10.0) (<= w (+ (* 3.0 tol) 10.0)))
                    (setq xc (/ (+ x0 x1) 2.0))
                    (setq segs (list (list xc y0 xc y1 (/ w 2.0)))))
                   ;; Крупный габарит
                   ((and (> w 10.0) (> h 10.0))
                    (setq yc (/ (+ y0 y1) 2.0))
                    (setq segs (list (list x0 yc x1 yc 25.0))))
                   (t
                    (mark:mline-warn
                      (strcat "габарит слишком мал ("
                              (rtos w 2 1) "x" (rtos h 2 1) ").")))))))))
        (t nil))
      segs)))

;; Русские имена типов для строки "[INFO] Типы:" (Ред. 48.12);
;; незнакомый тип выводится как есть, латиницей.
(defun mark:type-ru (typ)
  (cond
    ((member typ '("LWPOLYLINE" "POLYLINE")) "полилинии")
    ((= typ "INSERT") "блоки")
    ((= typ "MLINE") "мультилинии")
    ((= typ "LINE") "отрезки")
    ((= typ "ARC") "дуги")
    ((= typ "CIRCLE") "окружности")
    (t typ)))

(defun mark:fill-type-stat (ss / i e ed typ acc pair out first piece)
  (setq acc nil
        i   (if ss (sslength ss) 0))
  (repeat i
    (setq i   (1- i)
          e   (ssname ss i)
          ed  (entget e)
          typ (if (and ed (mark:strp (cdr (assoc 0 ed)))) (cdr (assoc 0 ed)) "?")
          pair (assoc typ acc))
    (if pair
      (setq acc (subst (cons typ (1+ (cdr pair))) pair acc))
      (setq acc (cons (cons typ 1) acc))))
  (setq out   nil
        first t)
  (foreach pair (reverse acc)
    (setq piece (strcat (mark:type-ru (car pair)) "=" (itoa (cdr pair))))
    (if first
      (progn
        (setq first nil
              out   piece))
      (setq out (strcat out " " piece))))
  (mark:out (strcat "[INFO] Типы: " (if out out "(пусто)")))
  nil)

(defun mark:fill-axis (seg / dx dy adx ady ln)
  (setq dx  (- (nth 2 seg) (nth 0 seg))
        dy  (- (nth 3 seg) (nth 1 seg))
        adx (abs dx)
        ady (abs dy)
        ln  (sqrt (+ (* dx dx) (* dy dy))))
  (if (< ln 1e-6)
    nil
    (cond
      ((<= ady (* *mark:fill-slope* ln)) 'h)
      ((<= adx (* *mark:fill-slope* ln)) 'v)
      ((>= adx ady) 'h)
      (t 'v))))

(defun mark:fill-axes-avg (vals / sorted out cluster sum n v)
  (if (null vals)
    nil
    (progn
      (setq sorted  (vl-sort vals '<)
            out     nil
            cluster nil
            sum     0.0
            n       0)
      (foreach v sorted
        (if (and cluster (> (- v (car cluster)) *mark:fill-tol*))
          (progn
            (setq out     (cons (/ sum (float n)) out)
                  cluster (list v)
                  sum     v
                  n       1))
          (progn
            (if (null cluster)
              (setq cluster (list v))
              (setq cluster (cons v cluster)))
            (setq sum (+ sum v)
                  n   (1+ n)))))
      (if (and cluster (> n 0))
        (setq out (cons (/ sum (float n)) out)))
      (reverse out))))



(defun mark:fill-cluster-items (items / sorted out cluster v)
  (if (null items)
    nil
    (progn
      (setq sorted (vl-sort (append items nil)
                     '(lambda (a b) (< (car a) (car b))))
            out nil
            cluster nil)
      (foreach v sorted
        (if (and cluster (> (- (car v) (car (car cluster))) *mark:fill-tol*))
          (setq out (cons cluster out)
                cluster (list v))
          (setq cluster (cons v cluster))))
      (if cluster (setq out (cons cluster out)))
      (reverse out))))

(defun mark:fill-inner-edge (items sign / it e best)
  (setq best nil)
  (foreach it items
    (setq e (+ (nth 0 it) (* sign (nth 3 it))))
    (if (or (null best)
            (and (> sign 0.0) (> e best))
            (and (< sign 0.0) (< e best)))
      (setq best e)))
  best)

(defun mark:fill-item-spans (items / it out)
  (setq out nil)
  (foreach it items
    (setq out (cons (list (nth 1 it) (nth 2 it)) out)))
  out)

(defun mark:fill-covers (spans lo hi gap / s merged last a b p0 p1 hit)
  (setq merged nil
        hit nil)
  (if spans
    (foreach s (vl-sort (append spans nil)
                 '(lambda (a b) (< (car a) (car b))))
      (setq p0 (car s)
            p1 (cadr s))
      (if (null merged)
        (setq merged (list (list p0 p1)))
        (progn
          (setq last (car merged)
                a (car last)
                b (cadr last))
          (if (<= p0 (+ b gap))
            (setq merged (cons (list a (if (> p1 b) p1 b)) (cdr merged)))
            (setq merged (cons (list p0 p1) merged)))))))
  (foreach s merged
    (if (and (<= (car s) (+ lo gap))
             (>= (cadr s) (- hi gap)))
      (setq hit t)))
  hit)

(defun mark:fill-owners (items / it o out)
  (setq out nil)
  (foreach it items
    (setq o (nth 4 it))
    (if (not (member o out))
      (setq out (cons o out))))
  out)

(defun mark:fill-owners-differ (a b)
  (and a b (not (equal a b))))

(defun mark:fill-overlap-owners (items lo hi gap / it o out)
  (setq out nil)
  (foreach it items
    (if (and (<= (nth 1 it) (+ hi gap))
             (>= (nth 2 it) (- lo gap)))
      (progn
        (setq o (nth 4 it))
        (if (not (member o out))
          (setq out (cons o out))))))
  out)

(defun mark:fill-v-split (cols iL iR lo hi gap / i hit)
  (setq i (1+ iL)
        hit nil)
  (while (and (< i iR) (null hit))
    (if (mark:fill-covers (mark:fill-item-spans (nth i cols)) lo hi gap)
      (setq hit t))
    (setq i (1+ i)))
  hit)

(defun mark:fill-span-items (items lo hi gap / it out)
  (setq out nil)
  (foreach it items
    (if (mark:fill-covers (list (list (nth 1 it) (nth 2 it))) lo hi gap)
      (setq out (cons it out))))
  out)

;; Ред. 48.39: у блока стойки/ригеля поперечные отрезки — это торцы
;; профиля, а не самостоятельные оси каркаса. Раньше они попадали в vs/hs,
;; а fill-inner-edge брал экстремум «ось ± полуширина» по всему кластеру.
;; В результате торец ригеля мог сместить грань ячейки внутрь неё.
;; Оставляем у именованного блока каркаса только продольные отрезки. Для вертикального
;; ригеля (импоста) главная ось определяется по самому длинному отрезку,
;; поэтому его вертикаль сохраняется, а горизонтальные торцы отбрасываются.
(defun mark:fill-frame-role? (e / nm)
  (setq nm (mark:fill-eff-name e))
  (if (mark:strp nm)
    (progn
      (setq nm (strcase nm))
      (or (wcmatch nm "*СТОЙКА*")
          (wcmatch nm "*РИГЕЛЬ*")
          (wcmatch nm "*ИМПОСТ*")))
    nil))

(defun mark:fill-main-axis (segs / s ax dx dy len best best-len)
  (setq best nil
        best-len 0.0)
  (foreach s segs
    (setq dx  (- (float (nth 2 s)) (float (nth 0 s)))
          dy  (- (float (nth 3 s)) (float (nth 1 s)))
          ax  (mark:fill-axis s)
          len (sqrt (+ (* dx dx) (* dy dy))))
    (if (and ax (> len best-len))
      (setq best ax
            best-len len)))
  best)

(defun mark:fill-frame-segs (e segs / main s out)
  (if (not (mark:fill-frame-role? e))
    segs
    (progn
      (setq main (mark:fill-main-axis segs)
            out nil)
      (if (null main)
        segs
        (progn
          (foreach s segs
            (if (eq (mark:fill-axis s) main)
              (setq out (cons s out))))
          (reverse out))))))

;; Ред. 48.41: в определении профиля несколько MLINE могут задавать
;; параллельные линии одного и того же элемента, а не независимые оси сетки.
;; Фактический протокол: 124 стойки дают 1364 = 124 x 11 вертикалей,
;; 439 ригелей — 4829 = 439 x 11 горизонталей. Экстремум «ось +/- полуширина»
;; по такому набору сдвигает грань на смещение внутренней линии (10–25 мм).
;; Для именованного MLINE-блока оставляем одну рабочую ось в точке вставки —
;; тем же базисе, который MarkV хранит как INS_PT стойки/ригеля. Пролёт —
;; объединение пролётов продольных отрезков; полуширина — у ближайшего к оси
;; вставки отрезка. Если точка вставки дальше допуска от всех отрезков, исходный
;; набор сохраняем: это уже другая схема блока, её нельзя сдвигать без данных.
(defun mark:fill-frame-axis-segs (e segs / ed nm main p ax s pos a b d
                                  best-d best-hw lo hi out)
  (setq ed (if e (entget e) nil)
        nm (if ed (cdr (assoc 2 ed)) nil))
  (if (or (null segs)
          (not (mark:fill-frame-role? e))
          (not (mark:fill-mline-p nm)))
    segs
    (progn
      (setq main   (mark:fill-main-axis segs)
            p      (if ed (cdr (assoc 10 ed)) nil)
            ax     (if (and main p)
                     (if (eq main 'v) (float (car p)) (float (cadr p)))
                     nil)
            best-d nil
            best-hw nil
            lo     nil
            hi     nil)
      (if (null ax)
        segs
        (progn
          (foreach s segs
            (if (eq (mark:fill-axis s) main)
              (progn
                (if (eq main 'v)
                  (setq pos (/ (+ (float (nth 0 s)) (float (nth 2 s))) 2.0)
                        a   (min (float (nth 1 s)) (float (nth 3 s)))
                        b   (max (float (nth 1 s)) (float (nth 3 s))))
                  (setq pos (/ (+ (float (nth 1 s)) (float (nth 3 s))) 2.0)
                        a   (min (float (nth 0 s)) (float (nth 2 s)))
                        b   (max (float (nth 0 s)) (float (nth 2 s)))))
                (setq d (abs (- pos ax)))
                (if (or (null best-d) (< d best-d))
                  (setq best-d  d
                        best-hw (if (and (> (length s) 4)
                                         (numberp (nth 4 s)))
                                  (float (nth 4 s))
                                  nil)))
                (setq lo (if (null lo) a (min lo a))
                      hi (if (null hi) b (max hi b))))))
          (if (and (numberp best-d) (<= best-d *mark:fill-tol*))
            (progn
              (setq out (if (eq main 'v)
                          (list ax lo ax hi)
                          (list lo ax hi ax)))
              (if (numberp best-hw)
                (setq out (append out (list best-hw))))
              (list out))
            segs))))))

(defun mark:fill-owned-axes (hits / i n vs hs part s x0 y0 x1 y1 dx dy ln hw nm ed
                                 hwmin hwmax)
  (setq i 0
        n (length hits)
        vs nil
        hs nil
        hwmin nil
        hwmax nil
        *mark:line-vis* 0
        *mark:expl-n* 0
        *mark:def-n* 0
        *mark:vis-n* 0
        *mark:hid-n* 0
        *mark:vis-open* 0
        *mark:copy-left* 0
        *mark:dyn-quiet* t
        *mark:no-expl-undo* t)
  (if (> n 0)
    (mark:out (strcat "[INFO] Текущая видимость, блоков " (itoa n) "…")))
  (foreach hit hits
    (setq i (1+ i)
          ed (entget (cadr hit))
          nm (if ed (cdr (assoc 2 ed)) nil)
          part (mark:fill-frame-segs
                 (cadr hit) (mark:fill-segs-of-ins (cadr hit)))
          part (mark:fill-frame-axis-segs (cadr hit) part))
    (if (and part (not (mark:fill-mline-p nm)))
      (setq *mark:line-vis* (1+ *mark:line-vis*)))
    (if (and (> n 100) (= (rem i 250) 0))
      (mark:out
        (strcat "[INFO] Читаю блоки: " (itoa i) " из " (itoa n))))
    (foreach s part
      (setq x0 (min (float (nth 0 s)) (float (nth 2 s)))
            y0 (min (float (nth 1 s)) (float (nth 3 s)))
            x1 (max (float (nth 0 s)) (float (nth 2 s)))
            y1 (max (float (nth 1 s)) (float (nth 3 s)))
            dx (- x1 x0)
            dy (- y1 y0)
            ln (sqrt (+ (* dx dx) (* dy dy))))
      (if (>= ln 30.0)
        (progn
          (setq hw (if (and (> (length s) 4) (numberp (nth 4 s)))
                     (float (nth 4 s))
                     25.0))
          (if (and (> (length s) 4) (numberp (nth 4 s)) (> (float (nth 4 s)) 0.0))
            (progn
              (if (or (null hwmin) (< hw hwmin)) (setq hwmin hw))
              (if (or (null hwmax) (> hw hwmax)) (setq hwmax hw))))
          (cond
            ((<= dy (* *mark:fill-slope* ln))
             (setq hs (cons (list (/ (+ (float (nth 1 s)) (float (nth 3 s))) 2.0)
                                  x0 x1 hw i)
                            hs)))
            ((<= dx (* *mark:fill-slope* ln))
             (setq vs (cons (list (/ (+ (float (nth 0 s)) (float (nth 2 s))) 2.0)
                                  y0 y1 hw i)
                            vs))))))))
  (setq *mark:dyn-quiet* nil
        *mark:no-expl-undo* nil)
  (if (or (and (numberp *mark:expl-n*) (> *mark:expl-n* 0))
          (and (numberp *mark:def-n*) (> *mark:def-n* 0))
          (and (numberp *mark:vis-n*) (> *mark:vis-n* 0)))
    (mark:out
      (strcat "[INFO] Геометрия: текущая видимость "
              (itoa (if (numberp *mark:vis-n*) *mark:vis-n* 0))
              ", разборка "
              (itoa (if (numberp *mark:expl-n*) *mark:expl-n* 0))
              (if (and (numberp *mark:def-n*) (> *mark:def-n* 0))
                (strcat ", мультилиния " (itoa *mark:def-n*))
                "")
              (if (and (numberp *mark:hid-n*) (> *mark:hid-n* 0))
                (strcat ", скрыто " (itoa *mark:hid-n*))
                "")
              (if (and (numberp *mark:vis-open*) (> *mark:vis-open* 0))
                ", фильтр пуст"
                ""))))
  (if (and (numberp *mark:copy-left*) (> *mark:copy-left* 0))
    (mark:out
      (strcat "[WARN] Не удалены копии блоков: " (itoa *mark:copy-left*))))
  (mark:out
    (strcat "[INFO] Отрезков каркаса: " (itoa (+ (length vs) (length hs)))
            "  вертикальных " (itoa (length vs))
            "  горизонтальных " (itoa (length hs))
            (if (and (numberp *mark:line-vis*) (> *mark:line-vis* 0))
              (strcat ". Видимых блоков из линий: " (itoa *mark:line-vis*)
                      ", отступ 0")
              "")
            (if (and hwmin hwmax (numberp *mark:def-n*) (> *mark:def-n* 0))
              (strcat "; полуширина мультилиний " (rtos hwmin 2 1)
                      (if (< hwmin hwmax)
                        (strcat ".." (rtos hwmax 2 1))
                        ""))
              "")))
  (list vs hs))

(defun mark:fill-seg-items (segs / vs hs s x0 y0 x1 y1 dx dy ln hw k)
  ;; Отрезки линий -> элементы осей (ось y0 y1 hw владелец), тот же формат,
  ;; что дают блоки в mark:fill-owned-axes. Владелец — уникальный
  ;; отрицательный номер на отрезок: каркас из линий равноправен с
  ;; блоками. Полуширина — из отрезка (MLINE), иначе 25 мм, как в 1/2.
  (setq vs nil
        hs nil
        k   0)
  (foreach s segs
    (setq x0 (min (float (nth 0 s)) (float (nth 2 s)))
          y0 (min (float (nth 1 s)) (float (nth 3 s)))
          x1 (max (float (nth 0 s)) (float (nth 2 s)))
          y1 (max (float (nth 1 s)) (float (nth 3 s)))
          dx  (- x1 x0)
          dy  (- y1 y0)
          ln  (sqrt (+ (* dx dx) (* dy dy)))
          k   (1- k)
          hw  (if (and (> (length s) 4) (numberp (nth 4 s)))
                (float (nth 4 s))
                25.0))
    (if (>= ln 30.0)
      (cond
        ((<= dy (* *mark:fill-slope* ln))
         (setq hs (cons (list (/ (+ (float (nth 1 s)) (float (nth 3 s))) 2.0)
                              x0 x1 hw k)
                        hs)))
        ((<= dx (* *mark:fill-slope* ln))
         (setq vs (cons (list (/ (+ (float (nth 0 s)) (float (nth 2 s))) 2.0)
                              y0 y1 hw k)
                        vs))))))
  (list vs hs))

(defun mark:fill-items->cells (vs hs / cols rows iL iR nL nR
                                Lcol Rcol xL xR w spanning iB nB
                                bot top yB yT h gap boxes
                                oL oR n-own n-open)
  ;; Ячейка только если четыре стороны доходят до углов и
  ;; противоположные стороны принадлежат разным владельцам.
  (setq *mark:h-bins* (mark:fill-index-hs hs)
        cols (mark:fill-cluster-items vs)
        gap (+ *mark:fill-tol* 25.0)
        boxes nil
        n-own 0
        n-open 0
        iL 0
        nL (length cols))
  (while (< iL nL)
    (setq iR (1+ iL))
    (while (< iR nL)
      (setq Lcol (nth iL cols)
            Rcol (nth iR cols)
            oL (mark:fill-owners Lcol)
            oR (mark:fill-owners Rcol)
            xL (mark:fill-inner-edge Lcol 1.0)
            xR (mark:fill-inner-edge Rcol -1.0)
            w (if (and xL xR) (- xR xL) 0.0))
      (cond
        ((not (mark:fill-owners-differ oL oR))
         (setq n-own (1+ n-own)))
        ((or (<= w 50.0)
             (and (numberp *mark:fill-max-w*) (> w *mark:fill-max-w*)))
         (if (and (numberp *mark:fill-max-w*) (> w *mark:fill-max-w*))
           (setq iR nL))
         nil)
        (t
         (setq spanning (mark:fill-span-items
                          (mark:fill-hs-near *mark:h-bins* xL xR gap)
                          xL xR gap)
               rows (mark:fill-cluster-items spanning)
               iB 0
               nB (length rows))
         (while (< iB (1- nB))
           (setq bot (nth iB rows)
                 top (nth (1+ iB) rows)
                 yB (mark:fill-inner-edge bot 1.0)
                 yT (mark:fill-inner-edge top -1.0)
                 h (if (and yB yT) (- yT yB) 0.0))
           (cond
             ((not (mark:fill-owners-differ
                     (mark:fill-owners bot)
                     (mark:fill-owners top)))
              (setq n-own (1+ n-own)))
             ((or (<= h 50.0)
                  (and (numberp *mark:fill-max-h*) (> h *mark:fill-max-h*)))
              nil)
             ((not (and (mark:fill-covers (mark:fill-item-spans Lcol) yB yT gap)
                        (mark:fill-covers (mark:fill-item-spans Rcol) yB yT gap)))
              (setq n-open (1+ n-open))
              (if (<= n-open 8)
                (mark:out
                  (strcat "  неполный контур: "
                          (mark:rjust (rtos xL 2 1) 12)
                          "," (mark:rjust (rtos yB 2 1) 12)
                          "  W=" (mark:rjust (rtos w 2 0) 5)
                          "  H=" (mark:rjust (rtos h 2 0) 5)))))
             ((mark:fill-v-split cols iL iR yB yT gap)
              (setq n-open (1+ n-open))
              (if (<= n-open 8)
                (mark:out
                  (strcat "  неполный контур: "
                          (mark:rjust (rtos xL 2 1) 12)
                          "," (mark:rjust (rtos yB 2 1) 12)
                          "  W=" (mark:rjust (rtos w 2 0) 5)
                          "  H=" (mark:rjust (rtos h 2 0) 5)))))
             ((not (and (mark:fill-owners-differ
                          (mark:fill-overlap-owners Lcol yB yT gap)
                          (mark:fill-overlap-owners Rcol yB yT gap))
                        (mark:fill-owners-differ
                          (mark:fill-overlap-owners bot xL xR gap)
                          (mark:fill-overlap-owners top xL xR gap))))
              (setq n-own (1+ n-own)))
             (t
              (setq boxes (cons (list xL yB xR yT) boxes))))
           (setq iB (1+ iB)))))
      (setq iR (1+ iR)))
    (setq iL (1+ iL)))
  (mark:out
    (strcat "[INFO] Замкнутых ячеек: " (itoa (length boxes))
            ". Отклонено кандидатов (не ячейки): один блок " (itoa n-own)
            ", неполный контур " (itoa n-open) "."))
  (if (null boxes)
    (mark:out "[INFO] Незамкнутый контур и камеры профиля не заполняю."))
  (reverse boxes))

(defun mark:fill-closed-cells (hits / axes vs hs)
  ;; Блоки каркаса -> замкнутые ячейки (как в 3-Сетка-динамика).
  ;; Тело расчёта — mark:fill-items->cells, общее с 5-Сетка-все-типы.
  (setq axes (mark:fill-owned-axes hits)
        vs (car axes)
        hs (cadr axes))
  (mark:fill-items->cells vs hs))

(defun mark:fill-grid-zone (bbs / x0 y0 x1 y1)
  ;; Общая зона сетки по всем ячейкам (xmin ymin xmax ymax).
  (if (null bbs)
    nil
    (progn
      (setq x0 (nth 0 (car bbs)) y0 (nth 1 (car bbs))
            x1 (nth 2 (car bbs)) y1 (nth 3 (car bbs)))
      (foreach bb bbs
        (if (< (nth 0 bb) x0) (setq x0 (nth 0 bb)))
        (if (< (nth 1 bb) y0) (setq y0 (nth 1 bb)))
        (if (> (nth 2 bb) x1) (setq x1 (nth 2 bb)))
        (if (> (nth 3 bb) y1) (setq y1 (nth 3 bb))))
      (list x0 y0 x1 y1))))

(defun mark:fill-filter-opening-cells (bbs / out skipped bb)
  ;; Ограничиваем загрузку проёмов зоной сетки — быстрее и без лишних видов.
  (setq *mark:grid-zone* (mark:fill-grid-zone bbs))
  (setq out nil skipped 0)
  (foreach bb bbs
    (if (mark:fill-cell-opening? bb)
      (setq skipped (1+ skipped))
      (setq out (cons bb out))))
  (if (> skipped 0)
    (mark:out (strcat "[INFO] Ячеек с окнами/дверями пропущено: "
                      (itoa skipped))))
  (reverse out))

(defun mark:fill-mode-grid-dyn (cells pts / hits boxes bb r)
  (mark:out "Сетка-динамика: выберите стойки и ригели. Только замкнутый контур.")
  (setq hits (mark:fill-pick-frame))
  (if (null hits)
    (progn
      (mark:out "[INFO] Выбор отменён.")
      (list cells pts))
    (progn
      (setq boxes (mark:fill-filter-opening-cells
                     (mark:fill-closed-cells hits)))
      (foreach bb boxes
        (setq r (mark:fill-add cells pts bb)
              cells (car r)
              pts (cadr r)))
      (list cells pts))))

(defun mark:fill-mode-grid (cells pts / ss i e r segs total bb typ ed)
  (mark:out "Сетка-мультилинии: выберите мультилинии. Блоки стоек не нужны.")
  (setq ss (vl-catch-all-apply 'ssget (list (list (cons 0 "MLINE,LINE,ARC")))))
  (if (or (vl-catch-all-error-p ss) (null ss))
    (progn
      (mark:out "[INFO] Выбор отменён.")
      (list cells pts))
    (progn
      (setq i     (sslength ss)
            total i
            segs  nil)
      (mark:fill-type-stat ss)
      (repeat i
        (setq i   (1- i)
              e   (ssname ss i)
              ed  (entget e)
              typ (if ed (cdr (assoc 0 ed)) nil))
        ;; Игнорируем полилинии в режиме сетки
        (if (not (or (= typ "LWPOLYLINE") (= typ "POLYLINE")))
          (progn
            (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
            (if (and (not (vl-catch-all-error-p r)) r)
              (setq segs (append segs r))))))
      (mark:out
        (strcat "[INFO] Извлечено отрезков: " (itoa (length segs))
                " из " (itoa total)))
      (if (null segs)
        (progn
          (mark:out "[WARN] Отрезки не извлечены — см. типы выше.")
          (list cells pts))
        (progn
          (setq r (mark:fill-segs->cells segs))
          (mark:out
            (strcat "[INFO] Сетка: осей X " (itoa (cadr r))
                    " Y " (itoa (caddr r))
                    "  ячеек " (itoa (length (car r)))))
          (if (null (car r))
            (progn
              (mark:out
                "[INFO] Мало осей — увеличьте *mark:fill-tol*.")
              (list cells pts))
            (progn
              (foreach bb (mark:fill-filter-opening-cells (car r))
                (setq r    (mark:fill-add cells pts bb)
                      cells (car r)
                      pts   (cadr r)))
              (list cells pts))))))))

(defun mark:fill-apply (cells pts / doc space n ins x0 y0 x1 y1
                        w h obj lst cell t0 t1 t_ins)
  (if (null cells)
    (progn
      (mark:out "[INFO] Нет ячеек — вставок нет.")
      nil)
    (progn
      (setq doc   (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
            space (if doc (mark:ax-get doc "ModelSpace") nil)
            n     0
            lst   nil)
      (if (null space)
        (progn
          (mark:out "[ERROR] ModelSpace недоступен.")
          (mark:note-error)
          nil)
        (progn
          (setq t0 (getvar "MILLISECS"))
          (setq *mark:fill-undo-off* nil
                *mark:fill-miss* nil
                *mark:fill-geo* nil)
          (if (and doc (not *mark:batch-undo*))
            (mark:ax-invoke-ok doc "StartUndoMark" nil))
          (foreach cell cells
            ;; Угол точный. Размер блока — кратно миллиметру.
            (setq x0 (float (nth 0 cell))
                  y0 (float (nth 1 cell))
                  x1 (float (nth 2 cell))
                  y1 (float (nth 3 cell))
                  w  (mark:round1 (- x1 x0))
                  h  (mark:round1 (- y1 y0)))
            (setq obj (mark:fill-insert space x0 y0))
            (if obj
              (progn
                (mark:fill-set-dims obj w h)
                (mark:fill-clear-attrs obj)
                (setq n   (1+ n)
                      ins (vl-catch-all-apply
                            'vlax-vla-object->ename (list obj)))
                (if (and (not (vl-catch-all-error-p ins)) ins)
                  (progn
                    (setq lst (cons ins lst))
                    ;; Ред. 48.0: гео ячейки для маркировки
                    (if (and (nth 4 cell) (nth 5 cell))
                      (setq *mark:fill-geo*
                        (cons (cons ins (list (nth 4 cell) (nth 5 cell)))
                              *mark:fill-geo*)))))
                (mark:out
                  (strcat "  ячейка " (mark:rjust (itoa n) 3)
                          ": " (mark:rjust (rtos x0 2 4) 12) "," (mark:rjust (rtos y0 2 4) 12)
                          "  W=" (mark:rjust (rtos w 2 0) 5)
                          "  H=" (mark:rjust (rtos h 2 0) 5)
                          " ||| вставка " (mark:rjust (itoa n) 3)
                          ": W=" (mark:rjust (rtos w 2 0) 5)
                          "  H=" (mark:rjust (rtos h 2 0) 5))))))
          (setq t1 (getvar "MILLISECS")
                t_ins (/ (- t1 t0) 1000.0))
          (if (and doc (not *mark:batch-undo*) (not *mark:fill-undo-off*))
            (mark:ax-invoke-ok doc "EndUndoMark" nil))
          (setq *mark:fill-undo-off* nil)
          (mark:out
            (strcat "[INFO] Вставлено заполнений: " (itoa n)
                    " (время вставки: " (rtos t_ins 2 2) " с, "
                    "всего: " (rtos (/ (- (getvar "MILLISECS") t0) 1000.0) 2 2) " с)"))
          (reverse lst))))))

(defun mark:fill-as-insert (e / ed)
  (setq ed (if e (entget e) nil))
  (if (and ed (= "INSERT" (cdr (assoc 0 ed))))
    e
    nil))

(defun mark:fill-pick-insert (/ sel e ed nest parents p typ)
  (mark:out "[INFO] Кликните линию каркаса блока, не пустую ячейку. Enter — пропуск.")
  (setq sel (vl-catch-all-apply 'entsel
              (list "\nЛиния блока <Enter — пропуск>: ")))
  (if (or (vl-catch-all-error-p sel) (null sel) (null (car sel)))
    nil
    (progn
      (setq e (mark:fill-as-insert (car sel)))
      (if (null e)
        (progn
          (setq nest (vl-catch-all-apply 'nentselp (list (cadr sel))))
          (if (and (not (vl-catch-all-error-p nest)) nest (listp nest) (cadddr nest))
            (progn
              (setq parents (cadddr nest)
                    p nil)
              (foreach x parents
                (setq p x))
              (setq e (mark:fill-as-insert p))))))
      (if e
        e
        (progn
          (setq ed (entget (car sel))
                typ (if ed (cdr (assoc 0 ed)) "?"))
          (mark:out (strcat "[WARN] Это не блок (" typ "). Нужна линия каркаса."))
          nil)))))

(defun mark:fill-shown? (ent / obj en ed v)
  ;; DXF 60 и Visible. Чужое состояние видимости в разборке невидимо.
  (setq obj (if (= (type ent) 'VLA-OBJECT) ent (mark:ax-catch-vla ent))
        en  (cond
              ((= (type ent) 'ENAME) ent)
              (obj (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
              (t nil))
        ed  (if (and en (not (vl-catch-all-error-p en))) (entget en) nil))
  (if (and ed (= 1 (cdr (assoc 60 ed))))
    nil
    (progn
      (setq v (if obj (vl-catch-all-apply 'vla-get-Visible (list obj)) nil))
      (not (or (eq v :vlax-false) (eq v 0))))))

(defun mark:fill-as-list (x / r)
  (cond
    ((listp x) x)
    ((= (type x) 'VARIANT)
     (setq r (vl-catch-all-apply 'vlax-safearray->list
               (list (vlax-variant-value x))))
     (if (or (vl-catch-all-error-p r) (not (listp r))) nil r))
    (t nil)))

(defun mark:fill-shift-segs (segs dx dy / s hw out)
  (setq out nil)
  (foreach s segs
    (setq hw (if (and (> (length s) 4) (numberp (nth 4 s))) (nth 4 s) 0.0)
          out (cons (list (- (float (nth 0 s)) dx)
                          (- (float (nth 1 s)) dy)
                          (- (float (nth 2 s)) dx)
                          (- (float (nth 3 s)) dy)
                          hw)
                    out)))
  (reverse out))

(defun mark:fill-ins-pt (obj / p)
  (setq p (vl-catch-all-apply 'vlax-get (list obj 'InsertionPoint)))
  (cond
    ((or (null p) (vl-catch-all-error-p p)) nil)
    ((= (type p) 'VARIANT)
     (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value p))))
    ((listp p) p)
    (t nil)))

(defun mark:fill-sweep-far (x y / ss i e echo n)
  ;; Окно далеко от каркаса. Оригиналы сюда не попадают.
  (setq echo (getvar "CMDECHO")
        n 0)
  (setvar "CMDECHO" 0)
  (setq ss (vl-catch-all-apply 'ssget
             (list "_C"
                   (list (- x 200000.0) (- y 200000.0) 0.0)
                   (list (+ x 200000.0) (+ y 200000.0) 0.0))))
  (setvar "CMDECHO" echo)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i))
        (if (and e (entget e))
          (progn
            (mark:fill-erase (mark:ax-catch-vla e))
            (if (not (entget e))
              (setq n (1+ n))))))))
  n)

(defun mark:fill-exploded-segs (lst depth force-hid / segs hid ent en ed typ sub
                                     piece shown pair)
  ;; (видимые . скрытые). Скрытые — другие состояния видимости.
  (setq segs nil
        hid nil)
  (if (and lst (listp lst) (< depth 3))
    (foreach ent lst
      (if ent
        (progn
          (setq en (vl-catch-all-apply 'vlax-vla-object->ename (list ent))
                ed (if (and en (not (vl-catch-all-error-p en))) (entget en) nil)
                typ (if ed (cdr (assoc 0 ed)) nil)
                shown (and (not force-hid) (mark:fill-shown? ent))
                piece nil)
          (cond
            ((member typ '("LINE" "ARC" "LWPOLYLINE" "POLYLINE" "MLINE"))
             (setq piece (mark:fill-local-segs en typ ed))
             (if shown
               (setq segs (append segs piece))
               (progn
                 (if piece
                   (setq *mark:hid-n*
                     (1+ (if (numberp *mark:hid-n*) *mark:hid-n* 0))))
                 (setq hid (append hid piece)))))
            ((and (= typ "INSERT") (< depth 2))
             (setq sub (mark:fill-as-list
                         (vl-catch-all-apply 'vlax-invoke (list ent "Explode"))))
             (if (listp sub)
               (progn
                 (setq pair (mark:fill-exploded-segs sub (1+ depth) (not shown)))
                 (if shown
                   (setq segs (append segs (car pair))
                         hid (append hid (cdr pair)))
                   (setq hid (append hid (car pair) (cdr pair))))))))
          (mark:fill-erase ent)))))
  (cons segs hid))


(defun mark:fill-erase (obj / en lay doc layers lk)
  (if obj
    (progn
      (setq en (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
      (vl-catch-all-apply 'vla-Delete (list obj))
      (if (and en (not (vl-catch-all-error-p en)) (entget en))
        (vl-catch-all-apply 'entdel (list en)))
      ;; Закрытый слой не отдаёт entdel. Открыть, стереть, закрыть обратно.
      (if (and en (not (vl-catch-all-error-p en)) (entget en))
        (progn
          (setq lay (cdr (assoc 8 (entget en)))
                doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
                layers (if doc (mark:ax-get doc "Layers") nil)
                lk (if (and layers lay)
                     (vl-catch-all-apply 'vla-Item (list layers lay))
                     nil))
          (if (and lk (not (vl-catch-all-error-p lk)))
            (progn
              (vl-catch-all-apply 'vla-put-Lock (list lk :vlax-false))
              (vl-catch-all-apply 'entdel (list en))
              (vl-catch-all-apply 'vla-put-Lock (list lk :vlax-true))))))
      (if (and en (not (vl-catch-all-error-p en)) (entget en))
        (setq *mark:copy-left*
          (1+ (if (numberp *mark:copy-left*) *mark:copy-left* 0)))))))


(defun mark:fill-explode-segs (e / doc obj copy lst segs p0 dx dy moved fx fy pair)
  ;; Копия в стороне и невидима: на каркасе её нет, даже если стирание сорвётся.
  ;; Разборка даёт текущую видимость, не габарит всех состояний.
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        obj (mark:ax-catch-vla e)
        segs nil
        dx 10000000.0
        dy 10000000.0
        moved nil)
  (if (or (null doc) (null obj))
    nil
    (progn
      (if (and (null *mark:pt-undo*) (null *mark:no-expl-undo*))
        (mark:ax-invoke-ok doc "StartUndoMark" nil))
      (setq copy (vl-catch-all-apply 'vla-Copy (list obj)))
      (if (or (vl-catch-all-error-p copy) (null copy))
        (setq segs nil)
        (progn
          (setq p0 (mark:fill-ins-pt copy))
          (if (and p0 (listp p0))
            (progn
              (setq fx (+ (float (car p0)) dx)
                    fy (+ (float (cadr p0)) dy)
                    moved (not (vl-catch-all-error-p
                                 (vl-catch-all-apply 'vla-Move
                                   (list copy
                                         (vlax-3d-point (float (car p0))
                                                        (float (cadr p0))
                                                        0.0)
                                         (vlax-3d-point fx fy 0.0))))))))
          (setq lst (mark:fill-as-list
                      (vl-catch-all-apply 'vlax-invoke (list copy "Explode"))))
          (if (listp lst)
            (progn
              (setq pair (mark:fill-exploded-segs lst 0 nil)
                    segs (if (car pair)
                           (car pair)
                           (progn
                             (if (cdr pair)
                               (setq *mark:vis-open*
                                 (1+ (if (numberp *mark:vis-open*) *mark:vis-open* 0))))
                             (cdr pair)))))
            (setq segs nil))
          (if moved
            (setq segs (mark:fill-shift-segs segs dx dy)))
          (vl-catch-all-apply 'vla-put-Visible (list copy :vlax-false))
          (mark:fill-erase copy)
          (if (and moved fx fy)
            (mark:fill-sweep-far fx fy))))
      (if (and (null *mark:pt-undo*) (null *mark:no-expl-undo*))
        (mark:ax-invoke-ok doc "EndUndoMark" nil))
      segs)))


(defun mark:fill-def-has-mline (bname depth / rec e ed typ nm hit)
  (if (or (not (mark:strp bname)) (= bname "") (>= depth 4))
    nil
    (progn
      (setq rec (tblsearch "BLOCK" bname)
            e   (if rec (cdr (assoc -2 rec)) nil)
            hit nil)
      (while (and e (null hit)
                  (setq ed (entget e))
                  (/= "ENDBLK" (cdr (assoc 0 ed))))
        (setq typ (cdr (assoc 0 ed)))
        (cond
          ((= typ "MLINE") (setq hit t))
          ((= typ "INSERT")
           (setq nm (cdr (assoc 2 ed))
                 hit (if nm (mark:fill-def-has-mline nm (1+ depth)) nil))))
        (setq e (entnext e)))
      hit)))

(defun mark:fill-lines-hw0 (segs / s out)
  (setq out nil)
  (foreach s segs
    (setq out
      (cons (if (and (> (length s) 4) (numberp (nth 4 s)))
              s
              (list (nth 0 s) (nth 1 s) (nth 2 s) (nth 3 s) 0.0))
            out)))
  (reverse out))

(defun mark:fill-segs-of-ins (e / pair ed nm mat segs)
  ;; Мультилиния — из определения, с полушириной.
  ;; Линии динблока — только текущая видимость. Полное определение
  ;; содержит все состояния: это максимальный габарит, его не берём.
  (setq pair (assoc e *mark:seg-cache*))
  (if pair
    (cdr pair)
    (progn
      (setq ed  (entget e)
            nm  (if ed (cdr (assoc 2 ed)) nil)
            mat (if ed (mark:fill-mat-of ed) nil))
      (if (mark:fill-mline-p nm)
        (setq segs (if mat (mark:fill-def-segs nm mat 0) nil)
              *mark:def-n* (1+ (if (numberp *mark:def-n*) *mark:def-n* 0)))
        (progn
          (setq segs (mark:fill-lines-hw0 (mark:fill-explode-segs e)))
          (if (>= (length segs) 2)
            (setq *mark:expl-n* (1+ (if (numberp *mark:expl-n*) *mark:expl-n* 0))
                  *mark:vis-n* (1+ (if (numberp *mark:vis-n*) *mark:vis-n* 0)))
            (setq segs nil))
          (if (and (null *mark:dyn-quiet*) (null segs))
            (mark:out "[WARN] Текущая видимость не прочитана. Габарит всех состояний не беру."))))
      (if (null *mark:dyn-quiet*)
        (mark:out
          (strcat "[INFO] Блок «" (mark:fill-eff-name e)
                  "»: отрезков " (itoa (length segs))
                  (if (and nm (mark:fill-def-has-mline nm 0))
                    ""
                    ", текущая видимость, отступ 0"))))
      (setq *mark:seg-cache* (cons (cons e segs) *mark:seg-cache*))
      segs)))


(defun mark:fill-try-rays (segs pt cells pts / bb)
  (setq bb (mark:fill-cell-by-rays segs pt))
  (cond
    ((and bb (mark:fill-box-closed segs bb))
     (mark:out "[INFO] Внутренний контур ячейки найден лучами.")
     (mark:fill-add cells pts bb))
    (bb
     (mark:out "[INFO] Лучи нашли незамкнутый контур — заполнение не ставлю.")
     nil)
    (t nil)))

(defun mark:fill-hit-cell (hits pt cells pts / boxes bb)
  ;; Та же ячейка, что в Сетка-динамика: стороны могут быть из нескольких линий.
  (setq boxes (mark:fill-closed-cells hits)
        bb (if boxes (mark:fill-smallest-cell boxes pt) nil))
  (cond
    (bb
     (mark:out "[INFO] Ячейка из замкнутого контура блоков.")
     (mark:fill-add cells pts bb))
    (boxes
     (mark:out "[INFO] Замкнутые ячейки есть, но точка вне их.")
     nil)
    (t nil)))

(defun mark:fill-dyn-from (hits pt cells pts / r segs)
  (setq r nil)
  (if hits
    (setq r (mark:fill-hit-cell hits pt cells pts)))
  (if (null r)
    (progn
      (setq segs (mark:fill-hits-segs hits))
      (mark:out
        (strcat "[INFO] Отрезков каркаса: " (itoa (length segs))))
      (if (>= (length segs) 4)
        (setq r (mark:fill-try-rays segs pt cells pts)))))
  r)

(defun mark:fill-mline-p (bname / pair hit)
  (if (or (null bname) (= bname ""))
    nil
    (progn
      (setq pair (assoc bname *mark:mline-cache*))
      (if pair
        (eq (cdr pair) 'yes)
        (progn
          (setq hit (mark:fill-def-has-mline bname 0))
          (setq *mark:mline-cache*
            (cons (cons bname (if hit 'yes 'no)) *mark:mline-cache*))
          hit)))))

(defun mark:fill-bname-local (bname / pair segs)
  (setq pair (assoc bname *mark:udef-cache*))
  (if pair
    (cdr pair)
    (progn
      (setq segs (mark:fill-lines-hw0
                   (mark:fill-def-segs bname (list 1.0 0.0 0.0 0.0 1.0 0.0) 0)))
      (setq *mark:udef-cache* (cons (cons bname segs) *mark:udef-cache*))
      segs)))

(defun mark:fill-bin (x)
  (fix (/ x 2000.0)))

(defun mark:fill-index-hs (hs / acc it k0 k1 k bins item pk p)
  (setq acc nil)
  (foreach it hs
    (setq k0 (mark:fill-bin (nth 1 it))
          k1 (mark:fill-bin (nth 2 it))
          k k0)
    (if (> k k1)
      (setq pk k k k1 k1 pk))
    (while (<= k k1)
      (setq acc (cons (cons k it) acc)
            k (1+ k))))
  (if (null acc)
    nil
    (progn
      (setq acc (vl-sort acc '(lambda (a b) (< (car a) (car b))))
            bins nil
            item nil
            k nil)
      (foreach p acc
        (if (and k (= (car p) k))
          (setq item (cons (cdr p) item))
          (progn
            (if k (setq bins (cons (cons k item) bins)))
            (setq k (car p)
                  item (list (cdr p))))))
      (if k (setq bins (cons (cons k item) bins)))
      bins)))

(defun mark:fill-hs-near (bins xL xR gap / k0 k1 k b out)
  (if (null bins)
    nil
    (progn
      (setq k0 (mark:fill-bin (- xL gap))
            k1 (mark:fill-bin (+ xR gap))
            k k0
            out nil)
      (while (<= k k1)
        (setq b (assoc k bins))
        (if b (setq out (append (cdr b) out)))
        (setq k (1+ k)))
      out)))

(defun mark:fill-hits-merge (a b / out e seen)
  (setq out nil
        seen nil)
  (foreach h (append a b)
    (setq e (cadr h))
    (if (not (member e seen))
      (setq seen (cons e seen)
            out (cons h out))))
  (reverse out))

(defun mark:fill-hit-rank (pt e / bb)
  ;; Огромный габарит динблока не должен вытеснять соседние стойки.
  (setq bb (mark:cell-bb e))
  (if (and bb
           (or (> (abs (- (nth 2 bb) (nth 0 bb))) 8000.0)
               (> (abs (- (nth 3 bb) (nth 1 bb))) 8000.0)))
    (mark:fill-ins-dist pt e)
    (mark:fill-bb-dist pt e)))

(defun mark:fill-dyn-candidates (pt win / ss i e nm d hits n-open)
  (mark:out
    (strcat "[INFO] Ищу вставки в пределах " (rtos win 2 0) " мм…"))
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list (cons 0 "INSERT"))))
        hits nil
        n-open 0)
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              nm (mark:fill-eff-name e))
        (cond
          ((mark:fill-skip-block? nm) nil)
          ;; Окно/дверь — заполнение, не каркас (как в Сетке-динамика).
          ((mark:fill-opening-block? nm)
           (setq n-open (1+ n-open)))
          (t
           (setq d (mark:fill-hit-rank pt e))
           (if (<= d win)
             (setq hits (cons (list d e) hits))))))))
  (setq *mark:dyn-open-skip* n-open)
  (vl-sort hits '(lambda (a b) (< (car a) (car b)))))

(defun mark:fill-bb-area (e / bb)
  (setq bb (mark:cell-bb e))
  (if bb
    (* (abs (- (nth 2 bb) (nth 0 bb)))
       (abs (- (nth 3 bb) (nth 1 bb))))
    1.0e12))

(defun mark:fill-dyn-pick (hits)
  ;; Габарит динблока больше ячейки. По площади не отсекать — иначе беру 0.
  (mark:fill-hits-take hits 80))

(defun mark:fill-dyn-cell (ptu ptw cells pts / all hits r)
  ;; Ячейка — пустота между вставками, не одна стойка.
  (setq r nil)
  (if *mark:dyn-set*
    (progn
      (mark:out "[INFO] Пробую прежний каркас.")
      (setq r (mark:fill-dyn-from *mark:dyn-set* ptw cells pts)
            hits *mark:dyn-set*)))
  (if (null r)
    (progn
      (setq all (mark:fill-dyn-candidates ptw 6000.0)
            hits (mark:fill-dyn-pick all))
      (mark:out
        (strcat "[INFO] В пределах 6000 мм: " (itoa (length all))
                ", беру " (itoa (length hits))
                (if (and (numberp *mark:dyn-open-skip*)
                         (> *mark:dyn-open-skip* 0))
                  (strcat "; проёмов пропущено: "
                          (itoa *mark:dyn-open-skip*))
                  "")))
      (mark:fill-hit-report hits)
      (setq r (mark:fill-dyn-from hits ptw cells pts))))
  (if r
    (progn
      (setq *mark:dyn-set* hits)
      r)
    (progn
      (mark:out "[INFO] Ячейка между блоками каркаса не собрана.")
      (list cells pts))))

(defun mark:fill-mode-dyn (cells pts / pt)
  (setq pt (getpoint "\nТочка-динамика внутри ячейки <Enter — конец>: "))
  (if (null pt)
    (progn
      (mark:out "[INFO] Конец режима «Точка-динамика».")
      (list nil nil 'cancel))
    (mark:fill-dyn-cell pt (mark:fill-pt-wcs pt) cells pts)))

(defun mark:fill-pt-undo-end (/ doc)
  (if *mark:pt-undo*
    (progn
      (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))))
  (setq *mark:pt-undo* nil
        *mark:batch-undo* nil))

(defun mark:fill-pt-err (msg)
  (mark:fill-pt-undo-end)
  (setq *error* *mark:pt-olderr*)
  (if (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort"))
    (mark:emit (strcat "\n" msg)))
  (princ))

(defun mark:fill-points-loop (modefn start done / going r n ins one acc-cells acc-pts doc)
  ;; Один UNDO на весь заход: все вставки режима «точка».
  (setq *mark:pt-olderr* *error*
        *error* mark:fill-pt-err
        doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        *mark:batch-undo* t
        *mark:pt-undo* nil)
  (if doc
    (progn
      (mark:ax-invoke-ok doc "StartUndoMark" nil)
      (setq *mark:pt-undo* t)))
  (setq going     t
        n         0
        ins       nil
        acc-cells nil
        acc-pts   nil)
  (mark:out start)
  (mark:out "[INFO] UNDO снимет все вставки этого захода разом.")
  (while going
    (setq r (apply modefn (list acc-cells acc-pts)))
    (cond
      ((or (null r) (eq (caddr r) 'cancel))
       (setq going nil))
      ((> (length (car r)) (length acc-cells))
       (setq one (mark:fill-apply (list (car (car r))) (cadr r))
             acc-cells (car r)
             acc-pts   (cadr r))
       (if one
         (setq ins (append ins one)
               n   (length ins))))
      (t nil)))
  (mark:fill-pt-undo-end)
  (setq *error* *mark:pt-olderr*)
  (mark:out (strcat done (itoa n)))
  ins)

(defun mark:fill-ask-mode (/ kw mode)
  ;; Два ключа на «Точка» или «Сетка» нельзя: AutoCAD берёт первый.
  ;; Поэтому динамика в списке с цифрой. Команда Точка-динамика — отдельно.
  (mark:out "Точка-динамика: команда, либо 4, либо Д. Буква Т — Точка-мультилинии.")
  (initget (strcat "1-Сетка-мультилинии 2-Точка-мультилинии "
                   "3-Сетка-динамика 4-Точка-динамика 5-Сетка-все-типы 6-Точка-все-типы 7-Полилиния "
                   "Динамика-точка Мультилинии-точка 1 2 3 4 5 6 7 P"))
  (setq kw (getkword
    "\nРежим [1-Сетка-мультилинии/2-Точка-мультилинии/3-Сетка-динамика/4-Точка-динамика/5-Сетка-все-типы/6-Точка-все-типы/7-Полилиния] <1-Сетка-мультилинии>: "))
  (cond
    ((or (null kw) (= kw "1-Сетка-мультилинии") (= kw "1"))
     (setq mode "1"))
    ((or (= kw "2-Точка-мультилинии") (= kw "Мультилинии-точка") (= kw "2"))
     (setq mode "2"))
    ((or (= kw "3-Сетка-динамика") (= kw "3"))
     (setq mode "3"))
    ((or (= kw "4-Точка-динамика") (= kw "4")
         (= kw "Динамика-точка") (= kw "Д"))
     (setq mode "4"))
    ((or (= kw "5-Сетка-все-типы") (= kw "5"))
     (setq mode "5"))
    ((or (= kw "6-Точка-все-типы") (= kw "6"))
     (setq mode "6"))
    ((or (= kw "7-Полилиния") (= kw "7") (= kw "P"))
     (setq mode "P"))
    (t (setq mode "1")))
  (mark:out
    (strcat "[INFO] Ключ: " (if kw kw "<Enter>")
            " = "
            (cond ((= mode "2") "Точка-мультилинии")
                  ((= mode "3") "Сетка-динамика")
                  ((= mode "5") "Сетка-все-типы")
                  ((= mode "6") "Точка-все-типы")
                  ((= mode "4") "Точка-динамика")
                  ((= mode "1") "Сетка-мультилинии")
                  ((= mode "P") "7-Полилиния")
                  (t "Сетка-мультилинии"))))
  mode)

(defun mark:fill-highlight-partial (/ ss e n)
  ;; Подсветить блоки окон/дверей, не полностью заполняющие свои ячейки.
  (setq n (length *mark:opening-partial*))
  (if (> n 0)
    (progn
      (setq ss (ssadd))
      (foreach e *mark:opening-partial*
        (if (and e (not (vl-catch-all-error-p
                          (vl-catch-all-apply 'entget (list e)))))
          (ssadd e ss)))
      (sssetfirst nil ss)
      (mark:out
        (strcat "[INFO] Подсвечено проёмов с неполным заполнением: "
                (itoa n) ". Проверьте выделенные окна/двери."))))
  (princ))

(defun mark:fill-main (/ kw mode cells pts r t_geom_start t_geom ins-list forced)
  (setq forced *mark:fill-force*
        *mark:fill-force* nil)
  (mark:cmd-line
    "МАРКАЗАПБЛОК — вставка «Заполнение в витраж». Список: 1-Сетка-мультилинии, 2-Точка-мультилинии, 3-Сетка-динамика, 4-Точка-динамика, 5-Сетка-все-типы, 6-Точка-все-типы, 7-Полилиния. Марки не пишет.")
  (mark:reset-state)
  (setq *mark:opening-bbs*     nil
        *mark:opening-loaded*  nil
        *mark:opening-partial* nil
        *mark:grid-zone*       nil)
  (mark:banner)
  (mark:out "МАРКАЗАПБЛОК — вставка «Заполнение в витраж» по ячейкам")
  (setq mode (if forced forced (mark:fill-ask-mode)))
  (if forced
    (mark:out
      (strcat "[INFO] Команда = "
              (cond ((= mode "2") "Точка-мультилинии")
                    ((= mode "3") "Сетка-динамика")
                  ((= mode "5") "Сетка-все-типы")
                  ((= mode "6") "Точка-все-типы")
                    ((= mode "4") "Точка-динамика")
                    ((= mode "1") "Полилинии")
                    (t "Сетка-мультилинии")))))
  (setq cells nil
        pts   nil
        ins-list nil
        t_geom_start (getvar "MILLISECS"))
  (cond
    ((= mode "2")
     (setq ins-list
       (mark:fill-points-loop
         'mark:fill-mode-point
         "[INFO] Точка-мультилинии: ячейка за ячейкой. Enter — конец."
         "[INFO] Режим «Точка-мультилинии» завершён. Вставлено: ")))
    ((= mode "4")
     (setq *mark:dyn-ins* nil
           *mark:dyn-set* nil
           *mark:dyn-bb*  nil
           ins-list
       (mark:fill-points-loop
         'mark:fill-mode-dyn
         "[INFO] Точка-динамика: ячейка из блоков каркаса. Enter — конец."
         "[INFO] Режим «Точка-динамика» завершён. Вставлено: "))
     (setq *mark:dyn-ins* nil
           *mark:dyn-set* nil
           *mark:dyn-bb*  nil))
    ((= mode "3")
     (setq r        (mark:fill-mode-grid-dyn cells pts)
           cells    (car r)
           pts      (cadr r)
           t_geom   (/ (- (getvar "MILLISECS") t_geom_start) 1000.0)
           ins-list (mark:fill-apply cells pts))
     (if cells
       (mark:out (strcat "[ТАЙМИНГ] Расчет геометрии сетки: " (rtos t_geom 2 2) " с"))))
    ((= mode "1")
     (setq r        (mark:fill-mode-grid cells pts)
           cells    (car r)
           pts      (cadr r)
           t_geom   (/ (- (getvar "MILLISECS") t_geom_start) 1000.0)
           ins-list (mark:fill-apply cells pts))
     (if cells
       (mark:out (strcat "[ТАЙМИНГ] Расчет геометрии сетки: " (rtos t_geom 2 2) " с"))))
    ((= mode "5")
     (setq r (mark:fill-mode-all-grid cells pts)
           cells (car r) pts (cadr r)
           t_geom (/ (- (getvar "MILLISECS") t_geom_start) 1000.0)
           ins-list (mark:fill-apply cells pts))
     (if cells
       (mark:out (strcat "[ТАЙМИНГ] Расчет геометрии сетки: " (rtos t_geom 2 2) " с"))))
    ((= mode "6")
     (setq ins-list
       (mark:fill-points-loop
         'mark:fill-mode-all-point
         "[INFO] 6-Точка-все-типы: точки внутри ячеек. Enter — завершение."
         "[INFO] Точка-все-типы завершена. Вставлено: ")))
    (t
     (setq ins-list
       (mark:fill-points-loop
         'mark:fill-mode-poly-one
         "[INFO] 7-Полилиния: тыкайте в полилинии-границы ячеек. Каждая — сразу вставка. Enter — конец."
         "[INFO] Режим «7-Полилиния» завершён. Вставлено: "))))
  (mark:out (strcat "Ошибок: " (itoa *mark:errors*)))
  (mark:out (strcat "Предупреждений: " (itoa *mark:warnings*)))
  ;; После заполнения — подсветить проёмы, не полностью занявшие ячейки.
  (mark:fill-highlight-partial)
  (setq *mark:fills* ins-list)
  (princ))
;;;=====================================================================
;;;  Ред. 48.29: НЕСКОЛЬКО ВИТРАЖЕЙ В ОДНОМ ПРОГОНЕ (МАРКАЗАП)
;;;  Кластеризация заполнений по связности: зазор между прямоугольниками
;;;  меньше *mark:vz-gap* — один витраж (проёмные ячейки не разрывают).
;;;  Блоки «Атрибуты витража» привязываются к ближайшему кластеру; при
;;;  двух и более в кластере — WARN, берётся ближайший к центру.
;;;  Витраж без блока — марки без префикса (TEST 02 NOT FOUND).
;;;  Один кластер — поведение и лог побайтово как раньше (48.28).
;;;=====================================================================

;; Связаны ли прямоугольники (зазор меньше порога по ОБЕИМ осям)
(defun mark:vz-near? (a b / ax0 ay0 ax1 ay1 bx0 by0 bx1 by1 dx dy)
  (setq ax0 (nth 0 a) ay0 (nth 1 a) ax1 (nth 2 a) ay1 (nth 3 a)
        bx0 (nth 0 b) by0 (nth 1 b) bx1 (nth 2 b) by1 (nth 3 b)
        dx  (max 0.0 (- (max ax0 bx0) (min ax1 bx1)))
        dy  (max 0.0 (- (max ay0 by0) (min ay1 by1))))
  (and (< dx *mark:vz-gap*) (< dy *mark:vz-gap*)))

(defun mark:vz-remove-list (lst del / out)
  (setq out nil)
  (foreach x lst
    (if (not (member x del))
      (setq out (cons x out))))
  (reverse out))

;; Минимальный X кластера — порядок витражей «слева направо»
(defun mark:vz-minx (cluster / m bb x)
  (setq m nil)
  (foreach e cluster
    (setq bb (mark:fill-e-bb e))
    (if bb
      (progn
        (setq x (float (nth 0 bb)))
        (if (or (null m) (< x m))
          (setq m x)))))
  (if m m 1.0e12))

;; Кластеры заполнений: связные компоненты bbox, слева направо
(defun mark:vz-clusters (fills / bbs nobb rest cur queue e0 bb0 hit out e bb r)
  (setq bbs  nil
        nobb nil)
  (foreach e fills
    (setq bb (mark:fill-e-bb e))
    (if bb
      (setq bbs (cons (cons e bb) bbs))
      (setq nobb (cons e nobb))))
  (setq out  nil
        rest bbs)
  (while rest
    (setq cur   (list (car rest))
          rest  (cdr rest)
          queue cur)
    (while queue
      ;; e0 — запись (ename x0 y0 x1 y1) из очереди: bbox уже при ней
      (setq e0    (car queue)
            queue (cdr queue)
            bb0   (cdr e0)
            hit   nil)
      (foreach r rest
        (if (mark:vz-near? bb0 (cdr r))
          (setq hit (cons r hit))))
      (foreach h hit
        (setq cur (cons h cur)))
      (setq rest  (mark:vz-remove-list rest hit)
            queue (append queue hit)))
    (setq out (cons (mapcar 'car cur) out)))
  ;; без bbox — отдельные кластеры в конец
  (foreach e nobb
    (setq out (cons (list e) out)))
  (vl-sort out
    '(lambda (a b) (< (mark:vz-minx a) (mark:vz-minx b)))))

;; Расстояние точки до прямоугольника (0 — точка внутри)
(defun mark:vz-dist (pt bb / dx dy)
  (setq dx (max 0.0 (- (nth 0 bb) (car pt)) (- (car pt) (nth 2 bb)))
        dy (max 0.0 (- (nth 1 bb) (cadr pt)) (- (cadr pt) (nth 3 bb))))
  (sqrt (+ (* dx dx) (* dy dy))))

;; Объединённый bbox кластера
(defun mark:vz-cbb (cluster / x0 y0 x1 y1 bb e f)
  (setq x0 nil y0 nil x1 nil y1 nil f nil)
  (foreach e cluster
    (setq bb (mark:fill-e-bb e))
    (if bb
      (progn
        (if (or (null x0) (< (nth 0 bb) x0)) (setq x0 (nth 0 bb)))
        (if (or (null y0) (< (nth 1 bb) y0)) (setq y0 (nth 1 bb)))
        (if (or (null x1) (> (nth 2 bb) x1)) (setq x1 (nth 2 bb)))
        (if (or (null y1) (> (nth 3 bb) y1)) (setq y1 (nth 3 bb)))
        (setq f t))))
  (if f (list x0 y0 x1 y1) (list 0.0 0.0 0.0 0.0)))

;; Ближайший к центру прямоугольника блок
(defun mark:vz-nearest (blks cbb / cx cy best bd d pt)
  (setq cx (/ (+ (nth 0 cbb) (nth 2 cbb)) 2.0)
        cy (/ (+ (nth 1 cbb) (nth 3 cbb)) 2.0)
        best nil
        bd   nil)
  (foreach b blks
    (setq pt (cdr (assoc 10 (entget b))))
    (if pt
      (progn
        (setq d (distance (list cx cy 0.0)
                          (list (float (car pt)) (float (cadr pt)) 0.0)))
        (if (or (null bd) (< d bd))
          (setq bd d best b)))))
  (if best best (car blks)))

(defun mark:vz-replace (lst old new / out)
  (setq out nil)
  (foreach x lst
    (setq out (cons (if (equal x old) new x) out)))
  (reverse out))

(defun mark:vz-join (lst sep / out)
  (setq out nil)
  (foreach x lst
    (setq out (if out (strcat out sep x) x)))
  out)

;; Имя витража для шапки блока лога
(defun mark:vz-title (blocks / g res)
  (cond
    ((null blocks) "без блока \"Атрибуты витража\"")
    (t
     (progn
       (setq g   (car blocks)
             res (mark:find-attr g *mark:attr-vitrage*))
       (if (and (car res) (mark:strp (cadr res)) (/= (mark:trim (cadr res)) ""))
         (mark:trim (cadr res))
         "\"Атрибуты витража\" без значения")))))

;; Подмена состояния под кластер: fills, glazings, префикс, fill-geo
(defun mark:vz-set (c ageo / out)
  (setq *mark:fills*        (car c)
        *mark:glazings*     (cadr c)
        *mark:prefix*       ""
        *mark:prefix-found* nil
        out                 nil)
  (foreach rec ageo
    (if (member (car rec) (car c))
      (setq out (cons rec out))))
  (setq *mark:fill-geo* (reverse out)))

;; Префикс кластера: значение «Витраж» его блока ("" без блока)
(defun mark:vz-pfx-of (blocks / g res)
  (if blocks
    (progn
      (setq g   (car blocks)
            res (mark:find-attr g *mark:attr-vitrage*))
      (if (and (car res) (mark:strp (cadr res)) (/= (mark:trim (cadr res)) ""))
        (mark:trim (cadr res))
        ""))
    ""))

;; Имена витражей по порядку кластеров (для шапки лога)
(defun mark:vz-names (cls / out p)
  (setq out nil)
  (foreach c cls
    (setq p (mark:vz-pfx-of (cadr c)))
    (setq out (cons (if (= p "") "без блока" p) out)))
  (reverse out))

;; Карта ename -> префикс витража (совместная маркировка)
(defun mark:vz-map-build (cls / map)
  (setq map nil)
  (foreach c cls
    (foreach e (car c)
      (setq map (cons (cons e (mark:vz-pfx-of (cadr c))) map))))
  (reverse map))

;; Префикс записи: из карты, иначе общий *mark:prefix*
(defun mark:vz-rec-pfx (r / e p)
  (setq e (mark:rec-get r 'ename)
        p (if *mark:vz-map* (cdr (assoc e *mark:vz-map*)) nil))
  (if p p *mark:prefix*))

;; Уникальные префиксы карты (для ИТОГ и TEST 03)
(defun mark:vz-map-pfxs (/ out)
  (setq out nil)
  (foreach p *mark:vz-map*
    (if (not (member (cdr p) out))
      (setq out (cons (cdr p) out))))
  (reverse out))

;; Кластеры + привязка блоков «Атрибуты витража».
;; Возвращает ((fills blocks) ...), слева направо.
(defun mark:vz-split (fills glazings / cls res c g pt d best bd r blks out)
  (setq cls (mark:vz-clusters fills)
        res nil)
  (foreach c cls
    (setq res (cons (list c nil) res)))
  (setq res (reverse res))
  (foreach g glazings
    (setq pt (cdr (assoc 10 (entget g))))
    (if pt
      (progn
        (setq best nil
              bd   nil)
        (foreach r res
          (setq d (mark:vz-dist pt (mark:vz-cbb (car r))))
          (if (or (null bd) (< d bd))
            (setq bd d best r)))
        (if best
          (setq res (mark:vz-replace res best
                      (list (car best) (append (cadr best) (list g)))))))))
  (setq out nil)
  (foreach r res
    (setq blks (cadr r))
    (cond
      ((null blks)
       (setq out (cons (list (car r) nil) out)))
      ((= (length blks) 1)
       (setq out (cons (list (car r) blks) out)))
      (t
       (progn
         (mark:out
           (strcat "[WARN] Витраж: " (itoa (length blks))
                   " блока \"Атрибуты витража\" — взят ближайший к центру."))
         (mark:note-warning)
         (setq out
           (cons (list (car r)
                       (list (mark:vz-nearest blks (mark:vz-cbb (car r)))))
                 out))))))
  (reverse out))

;; Этап МАРКАРОВКАЗАП по витражам: один кластер — как раньше
;; (лог прежний); несколько — полный блок диагностики per витраж
(defun mark:vz-marks (/ af ag ageo cls n i c was-reuse ans)
  (if (and (not *mark:reuse-sel*) (null *mark:fills*))
    (mark:select))
  (if *mark:fills*
    (progn
      (setq af   *mark:fills*
            ag   *mark:glazings*
            ageo *mark:fill-geo*
            cls  (mark:vz-split af ag)
            n    (length cls)
            i    0
            *mark:vz-map*  nil
            *mark:vz-mode* nil)
      (setq was-reuse       *mark:reuse-sel*
            *mark:reuse-sel* t)
      ;; Ред. 48.30: несколько витражей — спрашиваем режим маркировки
      (if (> n 1)
        (progn
          (mark:out "")
          (mark:out
            (strcat "[INFO] Витражей в выборе: " (itoa n)
                    " (" (mark:vz-join (mark:vz-names cls) ", ") ")"))
          (initget "Совместная Раздельная")
          (setq ans (getkword
                      "\nМаркировка нескольких витражей [Совместная/Раздельная] <Совместная>: "))
          (setq *mark:vz-mode*
                (if (member ans '("Раздельная")) "separate" "joint"))
          (mark:out "")
          (mark:out
            (if (= *mark:vz-mode* "joint")
              "[INFO] Совместная: шкалы секций и строк общие на все витражи, марки с префиксами витражей."
              "[INFO] Раздельная: каждому витражу свои шкалы секций и строк."))))
      (cond
        ;; совместная: один расчёт на всё выделение, per-витражный префикс
        ((= *mark:vz-mode* "joint")
         (progn
           (setq *mark:vz-map* (mark:vz-map-build cls))
           (mark:main)))
        ;; раздельная: полный блок диагностики per витраж
        ((= *mark:vz-mode* "separate")
         (progn
           (mark:out "")
           (mark:out
             (strcat "[INFO] Витражей в выборе: " (itoa n)))
           (foreach c cls
             (setq i (1+ i))
             (mark:out "")
             (mark:out
               (strcat "--- Витраж " (itoa i) " из " (itoa n) ": "
                       (mark:vz-title (cadr c))
                       " — заполнений " (itoa (length (car c))) " ---"))
             (mark:vz-set c ageo)
             (mark:main))))
        ;; один витраж — прежний путь (лог побайтово)
        (t (mark:main)))
      (setq *mark:reuse-sel* was-reuse
            *mark:fills*     af
            *mark:glazings*  ag
            *mark:fill-geo*  ageo))))

;; Этап МАРКАЗАПРЯД по витражам: каждому — свои базы рядовки
;; (Y снизу и X справа своего габарита) и своя группа
(defun mark:vz-rows (/ af ag ageo cls n i c)
  (if *mark:fills*
    (progn
      (setq af   *mark:fills*
            ag   *mark:glazings*
            ageo *mark:fill-geo*
            cls  (mark:vz-split af ag)
            n    (length cls)
            i    0)
      (if (= n 1)
        (mark:ar-main)
        (progn
          (mark:out "")
          (mark:out
            (strcat "[INFO] Витражей в выборе: " (itoa n)))
          (foreach c cls
            (setq i (1+ i))
            (mark:out "")
            (mark:out
              (strcat "--- Витраж " (itoa i) " из " (itoa n) ": "
                      (mark:vz-title (cadr c)) " ---"))
            (mark:vz-set c ageo)
            (mark:ar-main))))
      (setq *mark:fills*    af
            *mark:glazings* ag
            *mark:fill-geo* ageo))))

;; Ред. 48.26: этап ведомости в МАРКАЗАП — устойчив к Esc.
;; Группировка рядовок выполняется раньше — на этапе МАРКАЗАПРЯД
;; (mark:ar-make-group в конце mark:ar-main), до ведомости. Но Esc
;; на вопросах «Создать таблицу AutoCAD?» / «Экспорт в XLS?»
;; прерывал весь пакет a-all: EndUndoMark не закрывался, флаги
;; *mark:reuse-sel* / *mark:batch-undo* оставались взведёнными, и
;; один U откатывал разом вставки, марки, рядовку и ГРУППЫ.
;; Теперь отмена ведомости — штатный выход: всё созданное
;; сохраняется, маркер закрывается, флаги сбрасываются.
(defun mark:a-table-stage (/ r)
  (setq r (vl-catch-all-apply 'mtab:main nil))
  (if (vl-catch-all-error-p r)
    (mark:out
      "[INFO] Ведомость отменена (Esc) — вставки, марки, рядовка и группы сохранены.")
    r))

(defun mark:a-all (/ kw mode doc r cells pts new-fills)
  (mark:log-start)
  (mark:cmd-line
    "МАРКАЗАП — полный цикл. Сетка: вставка, марки, рядовка, ведомость. Заполнения: без вставки.")
  (mark:reset-state)
  (mark:out "========================================")
  (mark:out (strcat " МАРКАЗАП — универсальный пакет  " *mark:rev*))
  (mark:out "========================================")
  (mark:out (strcat "[LOG] Протокол: " (mark:log-path)))
  (if *mark:log-failed*
    (mark:out "[WARN] Файл протокола недоступен; сообщения только в командной строке."))
  (initget "Сетка Заполнения С З S Z 1 2")
  (setq kw (getkword "\nИсточник [Сетка/Заполнения] <Сетка>: "))
  (cond
    ((or (null kw) (= kw "Сетка") (= kw "С") (= kw "S") (= kw "1"))
     (setq mode "grid"))
    (t
     (setq mode "blocks")))
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
  (if doc
    (mark:ax-invoke-ok doc "StartUndoMark" nil))
  (setq *mark:batch-undo* t)
  (if (eq mode "grid")
    (progn
      ;; ВАРИАНТ 1: ПО СЕТКЕ (полный цикл: МАРКАЗАПБЛОК -> МАРКАРОВКАЗАП -> МАРКАЗАПРЯД -> МАРКАЗАПТАБЛ)
      (mark:out "[ЭТАП 1/4] МАРКАЗАПБЛОК — раскладка по сетке, режим 5-Сетка-все-типы...")
      (setq r (mark:fill-mode-all-grid nil nil)
            cells (car r)
            pts   (cadr r))
      (if (null cells)
        (mark:out "[INFO] Сетка не выбрана или ячеек нет — пакет прерван.")
        (progn
          (setq new-fills (mark:fill-apply cells pts))
          (if new-fills
            (progn
              (setq *mark:fills* new-fills
                    *mark:reuse-sel* t
                    *mark:sel-total* (length new-fills))
              ;; МАРКАРОВКАЗАП
              (mark:out "[ЭТАП 2/4] МАРКАРОВКАЗАП — маркировка заполнений...")
              (mark:vz-marks)
              ;; МАРКАЗАПРЯД
              (mark:out "[ЭТАП 3/4] МАРКАЗАПРЯД — расстановка рядовки...")
              (mark:vz-rows)
              ;; МАРКАЗАПТАБЛ
              (mark:out "[ЭТАП 4/4] МАРКАЗАПТАБЛ — ведомость заполнения...")
              (mark:a-table-stage))))))
    (progn
      ;; ВАРИАНТ 2: ИЗ ГОТОВЫХ БЛОКОВ (МАРКАРОВКАЗАП -> МАРКАЗАПРЯД -> МАРКАЗАПТАБЛ)
      (mark:out "[ЭТАП 1/3] МАРКАРОВКАЗАП — маркировка готовых блоков...")
      (setq *mark:reuse-sel* nil)
      (mark:vz-marks)
      (if *mark:fills*
        (progn
          (setq *mark:reuse-sel* t)
          (mark:out "[ЭТАП 2/3] МАРКАЗАПРЯД — расстановка рядовки...")
          (mark:vz-rows)
          (mark:out "[ЭТАП 3/3] МАРКАЗАПТАБЛ — ведомость заполнения...")
          (mark:a-table-stage))
        (mark:out "[INFO] Нет заполнений для обработки — пакет прерван."))))
  (if doc
    (mark:ax-invoke-ok doc "EndUndoMark" nil))
  (setq *mark:reuse-sel*  nil
        *mark:batch-undo* nil)
  (mark:out (strcat "[LOG] Протокол: " (mark:log-path)))
  (if *mark:log-failed*
    (mark:out "[WARN] Файл протокола недоступен; сообщения только в командной строке."))
  (princ))
;;;=====================================================================
;;;  РЕД. 48.0: СЕКЦИИ, ЯРУСЫ, ПОДЫНДЕКСЫ, ШКАЛА «В СВЕТУ»
;;;  Синхронизация с MarkZV (SYNC_MARKZV.md): цифра марки = цифра
;;;  ригеля той же длины в свету; подындекс ширины .1/.2/.3;
;;;  подындекс высоты — точка + строчная литера (.а/.б); стм/снд.
;;;=====================================================================

;; Слой выключен или заморожен — объект скрыт, не каркас
(defun mark:ent-hidden? (e / ed nm d c f)
  (setq ed (if e (entget e) nil)
        nm (if ed (cdr (assoc 8 ed)) nil))
  (cond
    ((and nm (setq d (tblsearch "LAYER" nm)))
     (setq c (cdr (assoc 62 d))
           f (cdr (assoc 70 d)))
     (or (and (numberp c) (minusp c))
         (and (numberp f) (= 1 (logand 1 f)))))
    (t nil)))

;; Видим ли объект: DXF 60, VLA Visible и слой
(defun mark:ent-visible? (e)
  (and (mark:fill-shown? e)
       (not (mark:ent-hidden? e))))

;; Конец вертикального элемента попадает в горизонтальный: допуск по
;; оси 55 = полуширина 25 + допуск 30 (шов «по грани» даёт 25..50);
;; без wide — строго внутри пролёта, wide — и у торца
;; (зеркало mk:beam-hit; Ред. 48.11, 48.13)
(defun mark:sec-end-hit (x y hs wide / hit h e1 e2)
  ;; wide: допуск и к торцу горизонтали (Ред. 48.11, Т-членения).
  ;; y-допуск 55, не 30: ствол «по грани» даёт 25..50 от оси
  ;; горизонтали — тридцати не хватало (Ред. 48.13, колонка 1700).
  (setq hit nil)
  (foreach h hs
    (if (null hit)
      (progn
        (if wide
          (setq e1 (- (nth 1 h) 55.0)
                e2 (+ (nth 2 h) 55.0))
          (setq e1 (+ (nth 1 h) 30.0)
                e2 (- (nth 2 h) 30.0)))
        (if (and (<= (abs (- y (nth 0 h))) 55.0)
                 (> x e1)
                 (< x e2))
          (setq hit t)))))
  hit)

;; Импост («т») — вертикаль, не являющаяся опорой (SYNC §3): ригель
;; проходит над/под ней не прерываясь — конец вертикали строго в
;; середине пролёта ригеля (sec-end-hit, wide=nil). Хотя бы один конец;
;; опора-стойка — вертикаль, к которой ригели примыкают только
;; торцами. Правило 48.11 требовало пару «строгий + торцевой» —
;; полновысотный импост (низ вообще без горизонталей) оставался
;; границей: столбец 1700 в В-7. Ред. 48.17 по данным МАРКАЗАПГЕО
;; 48.16: на В-7 и ТБ-1 меняется ровно одна вертикаль (ось 68520.3,
;; граница становится импостом).
(defun mark:sec-impost? (v hs)
  (or (mark:sec-end-hit (nth 0 v) (nth 1 v) hs nil)
      (mark:sec-end-hit (nth 0 v) (nth 2 v) hs nil)))

;; Стойки-границы секций: неимпостные вертикальные элементы
(defun mark:sec-columns (vs hs / out v)
  (setq out nil)
  (foreach v vs
    (if (not (mark:sec-impost? v hs))
      (setq out (cons v out))))
  (reverse out))

;; [DIAG] Компактный разбор секционной геометрии: одна строка на
;; горизонталь и на вертикаль — статус (стойка-граница/импост),
;; вердикты sec-end-hit для низа и верха (строго/торец) и прогноз
;; правила-кандидата «импост = хотя бы один строгий конец» (Ред. 48.16;
;; в 48.14–48.15 был детальный построчный разбор концов — блок на
;; 89 вертикалей не копировался из консоли целиком). Включается
;; командой МАРКАЗАПГЕО, печатается один раз и сбрасывает флаг сама.
;; Только печать — расчёты не меняет.
(defun mark:sec-diag (vs hs vs2 / sv sh v h n1 n2 s1 s2 w1 w2 kp ks)
  (setq *mark:sec-diag* nil)
  (mark:out "[DIAG] === Геометрия секций ===")
  (mark:out
    (strcat "[DIAG] Вертикалей " (itoa (length vs))
            ", стоек-границ " (itoa (length vs2))
            ", импостов " (itoa (- (length vs) (length vs2)))
            ", горизонталей " (itoa (length hs)) "."))
  (setq sh nil)
  (foreach h hs
    (setq sh (mark:sec-ins-ax sh h)))
  (foreach h sh
    (mark:out
      (strcat "[DIAG] гор y=" (rtos (nth 0 h) 2 1)
              " x=" (rtos (nth 1 h) 2 1) ".." (rtos (nth 2 h) 2 1)
              " hw=" (rtos (nth 3 h) 2 0))))
  (setq sv nil)
  (foreach v vs
    (setq sv (mark:sec-ins-ax sv v)))
  (setq kp 0)
  (foreach v sv
    (setq s1 (mark:sec-end-hit (nth 0 v) (nth 1 v) sh nil)
          w1 (mark:sec-end-hit (nth 0 v) (nth 1 v) sh t)
          s2 (mark:sec-end-hit (nth 0 v) (nth 2 v) sh nil)
          w2 (mark:sec-end-hit (nth 0 v) (nth 2 v) sh t)
          n1 0
          n2 0)
    (foreach h sh
      (if (<= (abs (- (nth 1 v) (nth 0 h))) 200.0)
        (setq n1 (1+ n1)))
      (if (<= (abs (- (nth 2 v) (nth 0 h))) 200.0)
        (setq n2 (1+ n2))))
    (if (or s1 s2)
      (setq kp (1+ kp)))
    (mark:out
      (strcat "[DIAG] верт x=" (rtos (nth 0 v) 2 1)
              " y=" (rtos (nth 1 v) 2 1) ".." (rtos (nth 2 v) 2 1)
              " hw=" (rtos (nth 3 v) 2 0)
              (if (member v vs2) " граница" " импост")
              " | низ[" (itoa n1) "] С=" (if s1 "ДА" "НЕТ")
              " Т=" (if w1 "ДА" "НЕТ")
              " | верх[" (itoa n2) "] С=" (if s2 "ДА" "НЕТ")
              " Т=" (if w2 "ДА" "НЕТ")
              " | прогноз: " (if (or s1 s2) "импост" "граница"))))
  (mark:out
    (strcat "[DIAG] Прогноз правила «импост = хотя бы один строгий конец»: "
            "импостов " (itoa kp)
            ", границ " (itoa (- (length vs) kp)) "."))
  (setq ks "")
  (foreach h *mark:scale1*
    (setq ks (strcat ks " " (itoa h))))
  (mark:out (strcat "[DIAG] Шкала Р (длины ригелей, мм):" ks))
  (mark:out "[DIAG] === Конец геометрии секций ==="))

;; Колонка той же секции за краем-импостом: пересекает ярус ячейки
;; и выходит за него по высоте — внутренние оси-обрезки колонки
;; не считаются (Ред. 48.13: правая колонка для ячейки 1000 x 3010
;; в разделённой колонке 1700 — она ниже импоста на 320 мм).
(defun mark:sec-tier-col? (items y0 y1 / sp a b hit)
  (setq hit nil)
  (foreach sp (mark:fill-item-spans items)
    (setq a (nth 0 sp)
          b (nth 1 sp))
    (if (and (<= a y1)
             (>= b y0)
             (or (<= a (- y0 30.0))
                 (>= b (+ y1 30.0))))
      (setq hit t))))

;; Гео ячейки: (ширина секции высота яруса). Границы секции —
;; соседние колонки-границы; ярус — горизонтали, охватывающие секцию.
;; Без осей (полилинии, линейный каркас с Т-объединением) — свой размер.
(defun mark:sec-geo-one (bb vs2 hs / x0 y0 x1 y1 cols c ccols face xl xr
                             cov yb yt h f)
  (setq x0 (nth 0 bb)
        y0 (nth 1 bb)
        x1 (nth 2 bb)
        y1 (nth 3 bb))
  (if (or (null vs2) (null hs))
    (list (mark:round1 (- x1 x0)) (mark:round1 (- y1 y0)))
    (progn
      ;; Ред. 48.11: граница секции — колонка, доходящая до яруса ячейки
      ;; (проверка та же, что L/R в fill-items->cells); короткие торцевые
      ;; оси ширину подъячеек не срезают.
      (setq cols (mark:fill-cluster-items vs2)
            ccols nil
            xl    nil
            xr    nil)
      (foreach c cols
        (if (mark:fill-covers (mark:fill-item-spans c)
                              y0 y1 (+ *mark:fill-tol* 25.0))
          (setq ccols (cons c ccols))))
      (setq ccols (reverse ccols))
      (foreach c ccols
        (setq face (mark:fill-inner-edge c 1.0))
        (if (and face (<= face (+ x0 1.0))
                 (or (null xl) (> face xl)))
          (setq xl face)))
      (foreach c ccols
        (setq face (mark:fill-inner-edge c -1.0))
        (if (and face (>= face (- x1 1.0))
                 (or (null xr) (< face xr)))
          (setq xr face)))
      ;; Ред. 48.13: край ячейки — импост (его нет в vs2), а колонки,
      ;; охватывающей ячейку, дальше нет — секция продолжается за
      ;; импост: граница — колонка, заходящая на ярус ячейки и
      ;; выходящая за него по высоте (обрезки внутри колонки мимо).
      (if (null xl)
        (foreach c cols
          (if (and (mark:sec-tier-col? c y0 y1)
                   (setq face (mark:fill-inner-edge c 1.0))
                   face
                   (<= face (+ x0 1.0))
                   (or (null xl) (> face xl)))
            (setq xl face))))
      (if (null xr)
        (foreach c cols
          (if (and (mark:sec-tier-col? c y0 y1)
                   (setq face (mark:fill-inner-edge c -1.0))
                   face
                   (>= face (- x1 1.0))
                   (or (null xr) (< face xr)))
            (setq xr face))))
      (if (null xl) (setq xl x0))
      (if (null xr) (setq xr x1))
      (setq cov nil)
      (foreach h hs
        (if (and (<= (nth 1 h) (+ xl 30.0))
                 (>= (nth 2 h) (- xr 30.0)))
          (setq cov (cons h cov))))
      (setq yb nil
            yt nil)
      (foreach h cov
        (setq f (+ (nth 0 h) (nth 3 h)))
        (if (and (<= f (+ y0 30.0))
                 (or (null yb) (> f yb)))
          (setq yb f)))
      (foreach h cov
        (setq f (- (nth 0 h) (nth 3 h)))
        (if (and (>= f (- y1 30.0))
                 (or (null yt) (< f yt)))
          (setq yt f)))
      (if (null yb) (setq yb y0))
      (if (null yt) (setq yt y1))
      (list (mark:round1 (- xr xl)) (mark:round1 (- yt yb))))))

;; Значение динамического свойства по маске имени
(defun mark:dyn-by-mask (e mask / pairs p v out)
  (setq pairs (mark:dyn-pairs e)
        out   nil)
  (foreach p pairs
    (if (and (null out)
             (mark:strp (car p))
             (wcmatch (strcase (car p)) (strcase mask)))
      (progn
        (setq v (mark:unwrap (cdr p)))
        (if (numberp v)
          (setq out v)))))
  out)

;; Длина блока-ригеля: свойство «*лин*», иначе габарит (зеркало mk:dim-src)
(defun mark:sec-block-len (e / v bb)
  (setq v (mark:dyn-by-mask e "*лин*"))
  (if (numberp v)
    v
    (progn
      (setq bb (mark:fill-e-bb e))
      (if bb
        (- (nth 2 bb) (nth 0 bb))
        nil))))

;; Пролёт блока-ригеля: (y x0 x1) | nil
(defun mark:sec-block-span (e / L p)
  (setq L (mark:sec-block-len e)
        p (cdr (assoc 10 (entget e))))
  (if (and (numberp L) (> L 0.0) p)
    (list (float (cadr p)) (float (car p)) (+ (float (car p)) L))
    nil))

;; Вставка записи (ось нач кон) по возрастанию оси, без lambda
(defun mark:sec-ins-ax (sorted r / out done q)
  (setq out  nil
        done nil)
  (foreach q sorted
    (if (and (null done) (>= (nth 0 q) (nth 0 r)))
      (setq out (cons r out)
            done t))
    (setq out (cons q out)))
  (if (null done)
    (setq out (cons r out)))
  (reverse out))

;; Вставка числа по возрастанию, без lambda
(defun mark:sec-ins-num (sorted k / out done q)
  (setq out  nil
        done nil)
  (foreach q sorted
    (if (and (null done) (>= q k))
      (setq out (cons k out)
            done t))
    (setq out (cons q out)))
  (if (null done)
    (setq out (cons k out)))
  (reverse out))

;; Ключ шкалы: целые мм (зеркало mk:size-key)
(defun mark:sec-key (v)
  (if (numberp v)
    (fix (+ v 0.5))
    nil))

;; Сортировка + дедупликация целых ключей
(defun mark:sec-keys-sort (keys / sorted out q)
  (setq sorted nil)
  (foreach q keys
    (if (numberp q)
      (setq sorted (mark:sec-ins-num sorted q))))
  (setq out nil)
  (foreach q sorted
    (if (or (null out) (/= (car out) q))
      (setq out (cons q out))))
  (reverse out))

;; Блок 1 шкалы: длины горизонтальных элементов в свету.
;; Зеркало MarkZV Ред. 3.7: геометрия MLINE (порог 50 мм, склейка
;; соосных 200 мм при перекрытии), блоки «*ригель*»; блоки в приоритете.
(defun mark:sec-scale1 (msegs rblocks / hrecs s x0 x1 ax len sorted out cur
                                 cax0 cmin cmax g hit sp bspans keys)
  (setq hrecs nil)
  (foreach s msegs
    (setq x0  (min (float (nth 0 s)) (float (nth 2 s)))
          x1  (max (float (nth 0 s)) (float (nth 2 s)))
          ax  (/ (+ (float (nth 1 s)) (float (nth 3 s))) 2.0)
          len (- x1 x0))
    (if (> len 50.0)
      (setq hrecs (cons (list ax x0 x1) hrecs))))
  (setq sorted nil)
  (foreach g hrecs
    (setq sorted (mark:sec-ins-ax sorted g)))
  (setq out  nil
        cur  nil
        cax0 nil
        cmin nil
        cmax nil)
  (foreach g sorted
    (if (and cur
             (<= (- (nth 0 g) cax0) 200.0)
             (> (min (nth 2 g) cmax) (max (nth 1 g) cmin)))
      (setq cmin (min cmin (nth 1 g))
            cmax (max cmax (nth 2 g)))
      (progn
        (if cur
          (setq out (cons (list cax0 cmin cmax) out)))
        (setq cur  t
              cax0 (nth 0 g)
              cmin (nth 1 g)
              cmax (nth 2 g)))))
  (if cur
    (setq out (cons (list cax0 cmin cmax) out)))
  (setq out (reverse out))
  ;; пролёты блоков-ригелей
  (setq bspans nil)
  (foreach sp rblocks
    (setq sp (mark:sec-block-span sp))
    (if sp
      (setq bspans (cons sp bspans))))
  (setq keys nil)
  (foreach sp bspans
    (setq keys (cons (mark:sec-key (- (nth 2 sp) (nth 1 sp))) keys)))
  ;; треки, не покрытые блоками
  (foreach g out
    (setq hit nil)
    (foreach sp bspans
      (if (and (null hit)
               (<= (abs (- (nth 0 g) (nth 0 sp))) 200.0)
               (> (min (nth 2 g) (nth 2 sp))
                  (max (nth 1 g) (nth 1 sp))))
        (setq hit t)))
    (if (null hit)
      (setq keys (cons (mark:sec-key (- (nth 2 g) (nth 1 g))) keys))))
  (mark:sec-keys-sort keys))

;; Строка шкалы по ключу
(defun mark:sec-row-of (rows k / hit row)
  (setq hit nil)
  (foreach row rows
    (if (and (null hit) (= (nth 0 row) k))
      (setq hit row)))
  hit)

;; Вставка строки шкалы по возрастанию размера
(defun mark:sec-ins-row (sorted r / out done q)
  (setq out  nil
        done nil)
  (foreach q sorted
    (if (and (null done) (>= (nth 0 q) (nth 0 r)))
      (setq out (cons r out)
            done t))
    (setq out (cons q out)))
  (if (null done)
    (setq out (cons r out)))
  (reverse out))

;; Итоговая шкала: Блок 1 (ригели) + Блок 2 (секции без ригеля).
;; Возвращает ((размер_мм источник) ...), источник Р/З/РЗ.
(defun mark:sec-scale-full (/ secw out k row sorted)
  (setq secw nil)
  (foreach row *mark:fill-geo*
    (if (and (cdr row) (numberp (cadr row)))
      (setq secw (cons (mark:sec-key (cadr row)) secw))))
  (setq out nil)
  (foreach k *mark:scale1*
    (if (null (mark:sec-row-of out k))
      (setq out (cons (list k (if (member k secw) "РЗ" "Р")) out))))
  (foreach k secw
    (if (and (null (mark:sec-row-of out k))
             (null (member k *mark:scale1*)))
      (setq out (cons (list k "З") out))))
  (setq sorted nil)
  (foreach row out
    (setq sorted (mark:sec-ins-row sorted row)))
  sorted)

;; Источник секции: РЗ/З/Р. Ред. 48.20: поиск по РАЗМЕРУ столбца —
;; позиции scale-final больше не совпадают с номерами (номера
;; сплошные по фактическим столбцам, Р-длины в нумерации не участвуют).
(defun mark:sec-src-of (w / row sc)
  (setq row nil)
  (if (and (numberp w) *mark:scale-final*)
    (foreach sc *mark:scale-final*
      (if (and (null row)
               (mark:same-num? w (nth 0 sc)))
        (setq row sc))))
  (if row
    (nth 1 row)
    "?"))

;; Карта подындексов: ((ключ . (размеры)) ...)
(defun mark:map-add (map key val / hit)
  (setq hit (assoc key map))
  (if hit
    (subst (cons key (cons val (cdr hit))) hit map)
    (cons (cons key (list val)) map)))

(defun mark:map-sort (map / out e)
  (setq out nil)
  (foreach e map
    (setq out (cons (cons (car e)
                          (mark:sort-unique (mark:positives (cdr e))))
                    out)))
  out)

;; Списки ширин/высот записей (запасная шкала W/H)
(defun mark:recs-widths (/ out r w)
  (setq out nil)
  (foreach r *mark:records*
    (setq w (mark:rec-get r 'width))
    (if (numberp w)
      (setq out (cons w out))))
  (reverse out))

(defun mark:recs-heights (/ out r h)
  (setq out nil)
  (foreach r *mark:records*
    (setq h (mark:rec-get r 'height))
    (if (numberp h)
      (setq out (cons h out))))
  (reverse out))

;; Готовые заполнения без раскладки: попросить каркас и рассчитать
;; секции/ярусы на месте (решение заказчика 2026-09-29).
;; Ред. 48.32: кэш выбора каркаса — ОДИН выбор на все проходы витражей
;; раздельной маркировки (сбрасывается при новом выборе заполнений).
;; Ред. 48.35: setq вместо defvar — в AutoLISP НЕТ defvar (форма из
;; других диалектов Lisp); в 48.32-48.34 LOAD падал на ней:
;; «no function definition: DEFVAR», файл не загружался целиком.
;; Глобальные инициализируются setq на верхнем уровне (как *mark:rev*).
(setq *mark:carc-cache* nil)

;; Ред. 48.32: список enames набора выбора (в порядке выбора)
(defun mark:ss-list (ss / i out)
  (setq i   (sslength ss)
        out nil)
  (repeat i
    (setq i   (1- i)
          out (cons (ssname ss i) out)))
  out)

;; Ред. 48.32: набор из кэша каркаса (удалённые объекты пропускаем)
(defun mark:carc-ss (/ s e)
  (setq s nil)
  (foreach e *mark:carc-cache*
    (if (entget e)
      (if s
        (ssadd e s)
        (setq s (ssadd e)))))
  (if (and s (> (sslength s) 0))
    s
    nil))

(defun mark:sec-standalone (/ ss i e ed typ nm r segs blocks msegs rblocks
                               seen polys axes vs hs items boxes bb n-poly
                               n-polyo n-line vs2 out ins hit cnt glaz cs)
  ;; Ред. 48.32: сначала кэш предыдущего выбора каркаса — следующие
  ;; проходы витражей не спрашивают рамку заново
  (setq ss nil)
  (if *mark:carc-cache*
    (progn
      (setq cs (mark:carc-ss))
      (if cs
        (progn
          (mark:out
            (strcat "[INFO] Каркас: использован предыдущий выбор ("
                    (itoa (sslength cs)) " объект(ов))."))
          (setq ss cs)))))
  (if (null ss)
    (progn
      (mark:out "Для расчёта секций выберите каркас рамкой. Enter — отмена.")
      (setq ss (vl-catch-all-apply 'ssget nil))
      (if (and ss (not (vl-catch-all-error-p ss)))
        (setq *mark:carc-cache* (mark:ss-list ss)))))
  (if (or (vl-catch-all-error-p ss) (null ss))
    (progn
      (mark:out "[INFO] Каркас не выбран.")
      nil)
    (progn
      (setq segs    nil
            blocks  nil
            msegs   nil
            rblocks nil
            seen    nil
            polys   nil
            glaz    nil
            n-poly  0
            n-polyo 0
            n-line  0
            i       (sslength ss))
      (while (> i 0)
        (setq i   (1- i)
              e   (ssname ss i)
              ed  (entget e)
              typ (if ed (cdr (assoc 0 ed)) nil))
        (cond
          ((or (null ed) (not (mark:ent-visible? e))) nil)
          ((= typ "INSERT")
           (setq nm (mark:fill-eff-name e))
           (cond
             ;; Ред. 48.3: «Атрибуты витража» — префикс для марок
             ((mark:blk-match? e *mark:block-glazing*)
              (setq glaz (cons e glaz)))
             ((mark:fill-skip-block? nm) nil)
             ((mark:fill-opening-block? nm) nil)
             ((mark:fill-block-seen? seen nm e) nil)
             (t
              (setq blocks (cons (list 0.0 e) blocks)
                    seen   (cons (cons nm (cdr (assoc 10 ed))) seen))
              (if (and (mark:strp nm)
                       (wcmatch (strcase nm) "*РИГЕЛЬ*"))
                (setq rblocks (cons e rblocks))))))
          ;; Ред. 48.4: замкнутая полилиния — не каркас и не ячейка
          ;; (контур полилинии — только режимы точки и 7-Полилиния).
          ((and (= typ "LWPOLYLINE") (mark:fill-lwpoly-closed? e))
           (setq n-poly (1+ n-poly)))
          ;; Ред. 48.6: отрезки и дуги — не каркас; каркас — только
          ;; мультилинии и динамика (как в режиме 5 МАРКАЗАП).
          ((member typ '("LINE" "ARC"))
           (setq n-line (1+ n-line)))
          ((= typ "MLINE")
           (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
           (if (and r (not (vl-catch-all-error-p r)))
             (progn
               (setq segs (append r segs))
               (setq msegs (append r msegs)))))
          ;; Ред. 48.6: незамкнутая полилиния и POLYLINE — не каркас.
          ((member typ '("LWPOLYLINE" "POLYLINE"))
           (setq n-polyo (1+ n-polyo)))))
      ;; Ред. 48.3: «Атрибуты витража» из выбора каркаса — префикс марок.
      ;; Ред. 48.32: только если префикс ещё не определён — раньше
      ;; перезапись затирала блок текущего витража (mark:vz-set), и в
      ;; раздельной маркировке все витражи получали префикс первого
      ;; по порядку выбора блока (В-7 маркировался как ТБ-1).
      (if (and glaz (null *mark:glazings*))
        (setq *mark:glazings* (reverse glaz)))
      (setq boxes nil
            vs    nil
            hs    nil)
      (cond
        ;; Ред. 48.31 (TZ п.40): оси из отрезков — секции и
        ;; Т-членения линейного каркаса считаются как в смешанном.
        ((null blocks)
         (if segs
           (setq boxes (car (mark:fill-segs->cells segs))
                 items (mark:fill-seg-items segs)
                 vs    (car items)
                 hs    (cadr items))))
        (t
         (setq axes  (mark:fill-owned-axes blocks)
               vs    (car axes)
               hs    (cadr axes)
               items (mark:fill-seg-items segs)
               vs    (append vs (car items))
               hs    (append hs (cadr items))
               boxes (mark:fill-items->cells vs hs))))
      ;; Ред. 48.4: замкнутые полилинии ячейками не стали — только лог.
      (if (> n-poly 0)
        (mark:out
          (strcat "[INFO] Замкнутых полилиний пропущено: " (itoa n-poly)
                  " — не каркас и не ячейки.")))
      ;; Ред. 48.6: пропущенные отрезки/дуги и полилинии — в лог.
      (if (> n-line 0)
        (mark:out
          (strcat "[INFO] Отрезков и дуг пропущено: " (itoa n-line)
                  " — не каркас (только мультилинии и динамика).")))
      (if (> n-polyo 0)
        (mark:out
          (strcat "[INFO] Незамкнутых полилиний пропущено: " (itoa n-polyo)
                  " — не каркас (контур — режимы 2/6/7).")))
      (setq vs2 (if (and vs hs) (mark:sec-columns vs hs) nil))
      (setq *mark:scale1* (mark:sec-scale1 msegs rblocks))
      (if *mark:sec-diag* (mark:sec-diag vs hs vs2))
      ;; Ред. 48.8 (З-1): высоты всех ячеек каркаса (включая проёмные
      ;; и глухие) — источник ярусной шкалы, как в режиме 5 МАРКАЗАП.
      ;; Та же функция, что для заполнений (sec-geo-one, целые мм).
      (setq *mark:cell-hs* nil)
      (foreach bb boxes
        (setq hit (mark:sec-geo-one bb vs2 hs))
        (if (and hit (numberp (nth 1 hit)))
          (setq *mark:cell-hs* (cons (nth 1 hit) *mark:cell-hs*))))
      ;; сопоставление: точка вставки блока = угол ячейки
      (setq out nil
            cnt 0)
      (foreach e *mark:fills*
        (setq ins (cdr (assoc 10 (entget e)))
              hit nil)
        (if ins
          (progn
            (foreach bb boxes
              (if (and (null hit)
                       (<= (abs (- (float (nth 0 bb)) (float (car ins)))) 2.0)
                       (<= (abs (- (float (nth 1 bb)) (float (cadr ins)))) 2.0))
                (setq hit (mark:sec-geo-one bb vs2 hs))))))
        (if hit
          (setq out (cons (cons e hit) out))
          (setq cnt (1+ cnt))))
      (if (> cnt 0)
        (progn
          (mark:out
            (strcat "[WARN] Вне ячеек каркаса: " (itoa cnt)
                    " заполнени(й) — марка не формируется (причина в TEST 13)."))
          (mark:note-warning)))
      (setq *mark:fill-geo* (reverse out))
      (mark:out
        (strcat "[INFO] Каркас: ячеек " (itoa (length boxes))
                ", заполнений с гео: " (itoa (length out)) "."))
      *mark:fill-geo*)))

;; Сортировка строк-групп по номеру
(defun mark:sec-sort-rows (rows / sorted out r q done)
  (setq sorted nil)
  (foreach r rows
    (setq out  nil
          done nil)
    (foreach q sorted
      (if (and (null done) (>= (nth 0 q) (nth 0 r)))
        (setq out (cons r out)
              done t))
      (setq out (cons q out)))
    (if (null done)
      (setq out (cons r out)))
    (setq sorted (reverse out)))
  sorted)

(defun mark:sec-join (lst / out s)
  (setq out "")
  (foreach s lst
    (if (= out "")
      (setq out s)
      (setq out (strcat out ", " s))))
  out)

;; TEST 16: таблицы секций и ярусов (ТЗ 3.0, §7.1)
(defun mark:test-16 (/ rows lines r n row q size src subs cnt div k sw-cnt
                        lrows llines letter lsize lsubs lhidx sh-cnt ln
                        wkeys wk sw hkeys hk sl beam-rows bad was nbeam)
  (mark:out "")
  (mark:out "[TEST 16] Секции и строки")
  (setq rows nil)
  (foreach r *mark:records*
    (setq n (mark:rec-get r 'hnum))
    (if (numberp n)
      (progn
        (setq row nil)
        (foreach q rows
          (if (and (null row) (= (nth 0 q) n))
            (setq row q)))
        (if row
          (setq rows (subst (list n (cons r (nth 1 row))) row rows))
          (setq rows (cons (list n (list r)) rows))))))
  (setq rows (mark:sec-sort-rows rows))
  (setq div    0
        sw-cnt 0
        bad    0
        nbeam  0
        lines  nil)
  (foreach row rows
    (setq n    (nth 0 row)
          r    (car (nth 1 row))
          size (mark:rec-get r 'secw)
          src  (mark:sec-src-of size)
          subs nil
          wkeys nil
          cnt  0)
    (foreach q (nth 1 row)
      (setq cnt (1+ cnt))
      (if (numberp (mark:rec-get q 'subw))
        (progn
          (setq sw-cnt (1+ sw-cnt))
          ;; Ред. 48.7: ключ — округлённая ширина; подъячейки печатаются
          ;; по возрастанию, как и индексы .1/.2 (было — в порядке записей).
          (setq k (mark:mm-key (mark:rec-get q 'width)))
          (if (and k (not (member k wkeys)))
            (setq wkeys (cons k wkeys))))))
    (setq wkeys (mark:sort-unique wkeys)
          subs  nil)
    (foreach wk wkeys
      (setq sw nil)
      (foreach q (nth 1 row)
        (if (and (null sw)
                 (numberp (mark:rec-get q 'subw))
                 (= wk (mark:mm-key (mark:rec-get q 'width))))
          (setq sw (mark:rec-get q 'subw))))
      (if sw
        (setq subs (cons (strcat (rtos wk 2 0)
                                 " (." (itoa sw) ")") subs))))
    (setq subs (reverse subs))
    (if subs
      (setq div (1+ div)))
    ;; Ред. 48.37: номер секции должен совпадать с номером ригеля (Рг N).
    ;; Совпало — «=»; у столбца без ригеля (З, Блок 2) сверки нет.
    (setq was (mark:rjust (itoa n) 3)
          was (if (and (> n *mark:scale-shift*) (= src "З"))
                (strcat was "  ")
                (progn
                  (setq nbeam (1+ nbeam))
                  (if (mark:num-match? n size)
                    (strcat was "= ")
                    (progn (setq bad (1+ bad)) (strcat was "! "))))))
    (setq lines
      (cons (strcat "  " was
                    "  " (mark:rjust (mark:fmt-raw size) 6)
                    "  " src
                    "  "
                    (if subs
                      (mark:sec-join subs)
                      "—")
                    "  заполнений: " (itoa cnt))
            lines)))
  (mark:out
    (strcat "  Секций: " (itoa (length rows))
            ", из них разделённых: " (itoa div)))
  (mark:out
    (strcat "  Сверка номеров с ригелями MarkZV (Рг): "
            (if (= bad 0) "совпало" (strcat "расхождений " (itoa bad)))
            ", ригелей в шкале: " (itoa *mark:scale-shift*)
            ", секций с ригелем " (itoa nbeam)
            ", без ригеля (З) " (itoa (- (length rows) nbeam))))
  (if rows
    (progn
      (mark:out "  №       W      источник  подъячейки")
      (mark:out "  (= — номер совпал с ригелем, ! — не совпал)")
      (foreach ln (reverse lines)
        (mark:out ln))))
  ;; Ред. 48.37: нумерация — по ригелям, поэтому номера без столбца
  ;; (ригель нарисован, заполнения той ширины нет) возможны; они не
  ;; дефект: номер закреплён за размером и не «едет» при правке фрамуги.
  ;; ярусы: группировка по букве
  (setq lrows nil)
  (foreach r *mark:records*
    (setq lhidx (mark:rec-get r 'hidx))
    (if (numberp lhidx)
      (progn
        (setq row nil)
        (foreach q lrows
          (if (and (null row) (= (nth 0 q) lhidx))
            (setq row q)))
        (if row
          (setq lrows (subst (list lhidx (cons r (nth 1 row))) row lrows))
          (setq lrows (cons (list lhidx (list r)) lrows))))))
  (setq lrows  (mark:sec-sort-rows lrows)
        llines nil
        sh-cnt 0)
  (foreach row lrows
    (setq r      (car (nth 1 row))
          letter (if (mark:rec-get r 'letter) (mark:rec-get r 'letter) "?")
          lsize  (mark:rec-get r 'tierh)
          lsubs  nil
          hkeys  nil
          cnt    0)
    (foreach q (nth 1 row)
      (setq cnt (1+ cnt))
      (if (mark:rec-get q 'subh)
        (progn
          (setq sh-cnt (1+ sh-cnt))
          ;; Ред. 48.38: показываем и геометрическую подъячейку без
          ;; литеры — если она единственная по высоте в этом ярусе.
          ;; Ред. 48.7: ключ — округлённая высота; по возрастанию,
          ;; как и литеры .а/.б (было — в порядке записей).
          (setq k (mark:mm-key (mark:rec-get q 'height)))
          (if (and k (not (member k hkeys)))
            (setq hkeys (cons k hkeys))))))
    (setq hkeys (mark:sort-unique hkeys)
          lsubs nil)
    (foreach hk hkeys
      (setq sl nil)
      (foreach q (nth 1 row)
        (if (and (null sl)
                 (mark:rec-get q 'subh)
                 (= hk (mark:mm-key (mark:rec-get q 'height))))
          (setq sl (if (mark:rec-get q 'subl)
                     (strcat " (" (mark:rec-get q 'subl) ")")
                     ""))))
      (if sl
        (setq lsubs (cons (strcat (rtos hk 2 0) sl) lsubs))))
    (setq lsubs (reverse lsubs))
    (setq llines
      (cons (strcat "  " (mark:rjust letter 4)
                    "  " (mark:rjust (mark:fmt-raw lsize) 6)
                    "  "
                    (if lsubs
                      (mark:sec-join lsubs)
                      "—")
                    "  заполнений: " (itoa cnt))
            llines)))
  (if lrows
    (progn
      (mark:out
        (strcat "  Строк: " (itoa (length lrows))))
      (foreach ln (reverse llines)
        (mark:out ln))))
  ;; Ред. 48.20: INFO-строка о буквах без заполнений (48.19) снята —
  ;; шкала строк строится по фактическим строкам, пропусков букв нет.
  (mark:out
    (strcat "  [INFO] Подъячеек по ширине: " (itoa sw-cnt)
            ", по высоте: " (itoa sh-cnt))))

;; Разбор марки с подындексами: "Б.а12.1" > ("Б.а" "12.1")
(defun mark:ar-parse2 (m / tail i n letter num)
  ;; Ред. 48.3: префикс «Витраж» отрезаем (ar-tail, как в ar-parse);
  ;; точку подындекса номера сохраняем: «Т-1 С10.1» -> («С» «10.1»)
  (setq tail (mark:ar-tail (mark:ar-strip-suffix (mark:trim m))))
  (if (or (null tail) (= tail ""))
    nil
    (progn
      (setq i     1
            n     (strlen tail)
            letter ""
            num    "")
      (while (and (<= i n)
                  (mark:ar-letter? (substr tail i 1))
                  (not (member (strcase (substr tail i 1)) '("Й" "З"))))
        (setq letter (strcat letter (strcase (substr tail i 1)))
              i     (1+ i)))
      (if (and (<= (+ i 1) n)
               (= (substr tail i 1) ".")
               (mark:ar-letter? (substr tail (+ i 1) 1)))
        (progn
          (setq letter (strcat letter "."
                               (strcase (substr tail (+ i 1) 1) t))
                i     (+ i 2))))
      (while (and (<= i n) (mark:ar-digit? (substr tail i 1)))
        (setq num (strcat num (substr tail i 1))
              i   (1+ i)))
      (if (and (<= (+ i 1) n)
               (= (substr tail i 1) ".")
               (mark:ar-digit? (substr tail (+ i 1) 1)))
        (progn
          (setq num (strcat num ".")
                i   (+ i 1))
          (while (and (<= i n) (mark:ar-digit? (substr tail i 1)))
            (setq num (strcat num (substr tail i 1))
                  i   (1+ i)))))
      (if (and (/= letter "") (/= num ""))
        (list letter num)
        nil))))

;; Натуральный ключ обозначения: цифровые группы как числа
(defun mark:nat-key (s / out i n c dig)
  (setq out nil
        i   1
        n   (strlen s))
  (while (<= i n)
    (setq c (substr s i 1))
    (if (mark:ar-digit? c)
      (progn
        (setq dig "")
        (while (and (<= i n) (mark:ar-digit? (substr s i 1)))
          (setq dig (strcat dig (substr s i 1))
                i   (1+ i)))
        (setq out (cons (cons 1 (atof dig)) out)))
      (progn
        (setq out (cons (cons 0 c) out)
              i   (1+ i)))))
  (reverse out))

;; Сравнение натуральных ключей: 9 < 10 < 12.1 < 13
(defun mark:nat-key< (a b / i av bv res)
  ;; Ред. 48.30 (фикс): инкремент i и эквивалентность равных ключей —
  ;; раньше при равных первых токенах цикл не продвигался (вечное
  ;; зависание; в старой сортировке ветка была фактически мёртвой)
  (setq i   0
        res nil)
  (while (and (null res)
              (or (< i (length a)) (< i (length b))))
    (cond
      ((and (>= i (length a)) (>= i (length b)))
       (setq res 'eq))
      ((>= i (length a)) (setq res 'lt))
      ((>= i (length b)) (setq res 'gt))
      (t
        (setq av (nth i a)
              bv (nth i b))
        (cond
          ((and (= (car av) 1) (= (car bv) 1))
           (cond
             ((< (cdr av) (cdr bv)) (setq res 'lt))
             ((> (cdr av) (cdr bv)) (setq res 'gt))))
          ((and (= (car av) 0) (= (car bv) 0))
           (cond
             ((< (ascii (cdr av)) (ascii (cdr bv))) (setq res 'lt))
             ((> (ascii (cdr av)) (ascii (cdr bv))) (setq res 'gt))))
          (t
            ;; разные типы: символ «.» (46) перед цифровой группой
            (if (= (car av) 0)
              (setq res 'lt)
              (setq res 'gt))))
        (setq i (1+ i)))))
  (eq res 'lt))

(defun mark:nat-less (a b)
  (mark:nat-key< (mark:nat-key a) (mark:nat-key b)))

;; Слой «Размеры» для таблицы ведомости (создаётся при отсутствии)
(defun mark:tbl-layer (doc tbl / layers)
  (if (null (tblsearch "LAYER" "Размеры"))
    (progn
      (setq layers (mark:ax-get doc "Layers"))
      (if layers
        (mark:ax-invoke layers "Add" "Размеры"))))
  (mark:ax-put tbl "Layer" "Размеры"))

;; Файл сверки «<имя чертежа> Шкала.txt»: индекс;размер_мм;источник
;; Ред. 48.20: индекс = номер фактического столбца (сплошная
;; шкала номеров). Р-длины шкалы осей без столбца — в конце
;; файла с индексом «-» и источником «Р».
;; Ред. 48.21: шкала столбцов собирается ЛОКАЛЬНО из fill-geo —
;; *mark:widths* к этому моменту перезаписан mark:ar-reindex
;; строками обозначений («1» … «9.1»), читать его как числа
;; нельзя (поломка 48.20: numberp "9.1").
(defun mark:scale-write-file (/ path f row i w src rest ws n)
  (if *mark:scale-final*
    (progn
      (setq path (strcat (getvar "dwgprefix")
                         (vl-filename-base (getvar "dwgname"))
                         " Шкала.txt"))
      (setq f (open path "w"))
      (if f
        (progn
          (write-line "индекс;размер_мм;источник" f)
          ;; Ред. 48.37: индекс строки — номер марки (по ригелям, затем
          ;; Блок 2). Шкала считается заново (mark:num-keys):
          ;; *mark:widths* к этому моменту уже перезаписан рядовкой.
          (setq ws (mark:num-keys))
          (setq i 0)
          (foreach w ws
            (setq i   (1+ i)
                  src (mark:sec-src-of w))
            (write-line
              (strcat (itoa i) ";"
                      (rtos w 2 0) ";"
                      src)
              f))
          (setq rest nil)
          (foreach row *mark:scale-final*
            (if (null (mark:index-of ws (nth 0 row)))
              (setq rest (cons row rest))))
          (foreach row (reverse rest)
            (write-line
              (strcat "-;"
                      (rtos (float (nth 0 row)) 2 0) ";"
                      (nth 1 row))
              f))
          ;; Ред. 48.37: итог по шкале номеров (строки «-» — ригель
          ;; без столбца, номер за размером закреплён)
          (if *mark:beam-scale*
            (progn
              (setq n (length *mark:beam-scale*))
              (write-line
                (strcat "; ригелей MarkZV: " (itoa n)
                        ", номеров всего: " (itoa (length ws))
                        ", без столбца: " (itoa (length rest)))
                f)))
          (close f)
          (mark:out (strcat "[OK] Шкала: " path)))
        (mark:out "[WARN] Шкала не записана: файл недоступен.")))))

(defun c:МАРКАРОВКАЗАП () (mark:main))
(defun c:MARKZ () (mark:main))
(defun c:МАРКАРОВКАЗАП () (mark:main))
(defun c:MARKAR () (mark:ar-main))
(defun c:МАРКАЗАПРЯД () (mark:ar-main))
(defun c:MARKA () (mark:a-all))
(defun c:МАРКАЗАП () (mark:a-all))
(defun c:МАРКАЗАПБЛОК () (mark:fill-main))
(defun c:MARKFILL () (mark:fill-main))

(defun mark:test-line (x0 y0 x1 y1)
  (entmake (list '(0 . "LINE")
                 (list 10 x0 y0 0.0)
                 (list 11 x1 y1 0.0))))

(defun mark:test-purge (nm / ss i e doc blocks blk)
  (setq ss (vl-catch-all-apply 'ssget
             (list "X" (list '(0 . "INSERT") (cons 2 nm)))))
  (if (and ss (not (vl-catch-all-error-p ss)))
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i))
        (entdel (ssname ss i)))))
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
  (if doc
    (progn
      (setq blocks (mark:ax-get doc "Blocks")
            blk (if blocks
                  (vl-catch-all-apply 'vla-item (list blocks nm))
                  nil))
      (if (and blk (not (vl-catch-all-error-p blk)))
        (vl-catch-all-apply 'vla-delete (list blk)))))
  t)

(defun mark:test-make-block (/ r)
  (setq r (entmake (list '(0 . "BLOCK")
                         '(2 . "MARKZ_TEST_CELL")
                         '(70 . 0)
                         (list 10 0.0 0.0 0.0))))
  (if (null r)
    nil
    (progn
      (mark:test-line 0.0 0.0 2000.0 0.0)
      (mark:test-line 0.0 1000.0 2000.0 1000.0)
      (mark:test-line 0.0 0.0 0.0 1000.0)
      (mark:test-line 2000.0 0.0 2000.0 1000.0)
      (mark:test-line 800.0 0.0 800.0 1000.0)
      (entmake '((0 . "ENDBLK")))
      t)))

(defun mark:test-insert (nm x y / r)
  (setq r (entmake (list '(0 . "INSERT")
                         (cons 2 nm)
                         (list 10 x y 0.0)
                         '(41 . 1.0)
                         '(42 . 1.0)
                         '(50 . 0.0))))
  (if (and r (listp r) (assoc -1 r))
    (cdr (assoc -1 r))
    (if r (entlast) nil)))

(defun c:МАРКАЗАПТЕСТ (/ doc e segs bb w h ok x y hits found)
  (mark:out "========================================")
  (mark:out (strcat " ТЕСТ ЯЧЕЙКИ БЛОКА  " *mark:rev*))
  (mark:out "Прямоугольник 2000x1000, стойка на 800. Ожидание: W=800 H=1000")
  (mark:out "========================================")
  (setq doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        ok nil)
  (if doc (mark:ax-invoke-ok doc "StartUndoMark" nil))
  (mark:test-purge "MARKZ_TEST_CELL")
  (if (not (mark:test-make-block))
    (mark:out "[TEST] Блок не создан — FAIL")
    (progn
      (setq e (mark:test-insert "MARKZ_TEST_CELL" 100000.0 100000.0))
      (if (null e)
        (mark:out "[TEST] Вставка не создана — FAIL")
        (progn
          (setq x 100400.0
                y 100500.0
                segs (mark:fill-segs-of-ins e)
                bb (if segs (mark:fill-cell-by-rays segs (list x y)) nil))
          (mark:out (strcat "[TEST] Отрезков из блока: " (itoa (length segs))))
          (if (null bb)
            (mark:out "[TEST] Ячейка по отрезкам блока — FAIL")
            (progn
              (setq w (- (nth 2 bb) (nth 0 bb))
                    h (- (nth 3 bb) (nth 1 bb))
                    ok (and (> w 700.0) (< w 900.0)
                            (> h 900.0) (< h 1100.0)))
              (mark:out
                (strcat "[TEST] Ячейка W=" (rtos w 2 1)
                        " H=" (rtos h 2 1)
                        (if ok " — OK" " — FAIL")))))
          (setq hits (mark:fill-insert-hits (list x y) (list x y))
                found nil)
          (foreach hit hits
            (if (eq (cadr hit) e) (setq found t)))
          (mark:out
            (strcat "[TEST] Окно нашло тестовый блок: "
                    (if found "OK" "FAIL")))
          (if (null found) (setq ok nil))
          (entdel e)))))
  (mark:test-purge "MARKZ_TEST_CELL")
  (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))
  (mark:out (if ok "[TEST] Итог — OK" "[TEST] Итог — FAIL"))
  (princ))


(defun mark:hex-val (s / i n c)
  (setq i 1
        n 0)
  (while (and s (<= i (strlen s)))
    (setq c (ascii (substr s i 1))
          n (+ (* n 16)
               (cond
                 ((and (>= c 48) (<= c 57)) (- c 48))
                 ((and (>= c 65) (<= c 70)) (- c 55))
                 ((and (>= c 97) (<= c 102)) (- c 87))
                 (t 0)))
          i (1+ i)))
  n)

(defun mark:copy-key-generic (e / ed out code val)
  ;; Полный ключ для любого примитива: все DXF-коды, кроме служебных
  ;; (дескриптор, владелец, реакторы). Копией считается только полностью
  ;; идентичный объект (тип, слой, цвет, геометрия и т.д.).
  (setq ed (entget e) out "")
  (foreach pair ed
    (setq code (car pair) val (cdr pair))
    (if (not (member code '(-1 -2 5 330 331 350 360 361 102 100 92 310)))
      (setq out
        (strcat out "|" (itoa code) ":"
          (cond ((mark:strp val) val)
                ((numberp val) (rtos (float val) 2 4))
                ((listp val)
                 (apply 'strcat
                   (mapcar '(lambda (x)
                      (if (numberp x) (strcat "," (rtos (float x) 2 4))
                                      (strcat "," (vl-princ-to-string x))))
                           val)))
                (t (vl-princ-to-string val)))))))
  out)

(defun mark:copy-dyn-key (e / obj pairs parts pair nm val)
  ;; Строка из всех динамических свойств (включая Видимость), чтобы
  ;; копией считался только полностью идентичный блок.
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (vl-catch-all-error-p obj) (null obj))
    ""
    (progn
      (setq pairs (mark:load-dyn-pairs obj)
            parts nil)
      (foreach pair pairs
        (if (mark:strp (car pair))
          (progn
            (setq nm  (strcase (car pair))
                  val (mark:unwrap (cdr pair))
                  parts (cons (strcat nm "="
                                (cond ((numberp val) (rtos (float val) 2 4))
                                      ((mark:strp val) val)
                                      (t (vl-princ-to-string val))))
                              parts))))
        )
      ;; Сортировка, чтобы порядок свойств не влиял на ключ.
      (setq parts (vl-sort parts '(lambda (a b) (< a b))))
      (apply 'strcat (mapcar '(lambda (x) (strcat "|" x)) parts)))))

(defun mark:copy-key (e / ed typ p nm rot sx sy)
  ;; Для вставок блоков — имя + точка + поворот + масштаб + вся динамика
  ;; (включая Видимость). Для остальных примитивов — полный DXF-ключ.
  (setq ed  (entget e)
        typ (if ed (cdr (assoc 0 ed)) nil))
  (cond
    ((null ed) nil)
    ((= typ "INSERT")
     (setq p  (cdr (assoc 10 ed))
           nm (mark:fill-eff-name e))
     (if (or (null p) (null (car p)) (mark:fill-skip-block? nm))
       nil
       (progn
         (setq rot (cdr (assoc 50 ed))
               sx  (cdr (assoc 41 ed))
               sy  (cdr (assoc 42 ed)))
         (strcat "INSERT|" (if nm nm "?")
                 "|" (rtos (float (car p)) 2 2)
                 "|" (rtos (float (cadr p)) 2 2)
                 "|" (rtos (if (numberp rot) rot 0.0) 2 4)
                 "|" (rtos (if (numberp sx) sx 1.0) 2 4)
                 "|" (rtos (if (numberp sy) sy 1.0) 2 4)
                 ;; полное совпадение динамики, включая Видимость
                 (mark:copy-dyn-key e)))))
    (t
     ;; Любой другой примитив: полный ключ по DXF-кодам.
     (strcat typ (mark:copy-key-generic e)))))

(defun mark:copies-group-name ()
  ;; Уникальное имя, чтобы не конфликтовать с существующей группой.
  (strcat "МАРКА_КОПИИ_" (menucmd "M=$(edtime,$(getvar,date),HHMMSS)")))

(defun mark:copies-make-group (enames / doc groups grp name vobjs arr e vo)
  ;; Собирает копии в именованную группу AutoCAD и возвращает её объект.
  (setq doc    (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
        groups (if doc (mark:ax-get doc "Groups") nil))
  (if (null groups)
    nil
    (progn
      (setq name (mark:copies-group-name)
            grp  (vl-catch-all-apply 'vla-Add (list groups name)))
      (if (vl-catch-all-error-p grp)
        nil
        (progn
          (setq vobjs nil)
          (foreach e enames
            (setq vo (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
            (if (not (vl-catch-all-error-p vo))
              (setq vobjs (cons vo vobjs))))
          (if vobjs
            (progn
              (setq arr (vlax-make-safearray
                          vlax-vbObject
                          (cons 0 (1- (length vobjs)))))
              (vlax-safearray-fill arr vobjs)
              (vl-catch-all-apply 'vla-AppendItems (list grp arr))
              (list name grp))
            nil))))))

;; Ред. 48.14: включить [DIAG]-печать геометрии секций на следующий
;; МАРКАЗАП (Источник Сетка или Заполнения). Расчёты не меняет.
(defun c:МАРКАЗАПГЕО ()
  (setq *mark:sec-diag* t)
  (mark:out "МАРКАЗАПГЕО — печать геометрии секций включена.")
  (mark:out
    "Запустите МАРКАЗАП: после строк про оси будет разбор вертикалей и горизонталей. Флаг сбросится сам после первой печати.")
  (princ))

(defun c:МАРКАЗАПКОПИИ (/ ss i e key acc groups g keep n extras ss2 grp skipped-spds)
  ;; Ручной поиск по команде пользователя. Ищет полностью идентичные дубли
  ;; ЛЮБЫХ примитивов (тип, слой, цвет, геометрия, для блоков — ещё и вся
  ;; динамика, включая Видимость). Команда собирает лишние копии в группу и
  ;; подсвечивает их, НО ничего не удаляет — решение принимает пользователь.
  ;; Внутренние процедуры, которым нужно создавать/удалять такие элементы,
  ;; работают отдельно и этой командой не затрагиваются.
  (mark:out "МАРКАЗАПКОПИИ — найти полностью идентичные дубли объектов (все примитивы).")
  (setq ss (vl-catch-all-apply 'ssget (list "X"))
        acc nil
        extras nil
        n 0)
  (if (or (vl-catch-all-error-p ss) (null ss))
    (mark:out "[INFO] Объектов нет.")
    (progn
      (setq i (sslength ss))
      (mark:out (strcat "[INFO] Объектов в чертеже: " (itoa i)))
      (setq skipped-spds 0)
      (repeat i
        (setq i (1- i)
              e (ssname ss i))
        (if (mark:spds? e)
          ;; Объекты SPDS/прокси пропускаем: AutoCAD не отдаёт их координаты
          ;; корректно, дубли по ним считать нельзя.
          (setq skipped-spds (1+ skipped-spds))
          (progn
            (setq key (mark:copy-key e))
            (if key
              (setq acc (cons (cons key e) acc))))))
      (if (> skipped-spds 0)
        (mark:out (strcat "[INFO] Пропущено объектов SPDS/прокси: "
                          (itoa skipped-spds))))
      (setq acc (vl-sort acc '(lambda (a b) (< (car a) (car b))))
            groups nil
            g nil
            key nil)
      (foreach pair acc
        (if (and key (= (car pair) key))
          (setq g (cons (cdr pair) g))
          (progn
            (if (and key (> (length g) 1))
              (setq groups (cons g groups)))
            (setq key (car pair)
                  g (list (cdr pair))))))
      (if (and key (> (length g) 1))
        (setq groups (cons g groups)))
      ;; В каждой группе оставляем «оригинал» (наименьший handle),
      ;; остальные считаем лишними копиями.
      (foreach g groups
        (setq keep nil)
        (foreach e g
          (if (or (null keep)
                  (< (mark:hex-val (cdr (assoc 5 (entget e))))
                     (mark:hex-val (cdr (assoc 5 (entget keep))))))
            (setq keep e)))
        (foreach e g
          (if (not (eq e keep))
            (setq extras (cons e extras)))))
      (if (null extras)
        (mark:out "[INFO] Лишних копий не найдено.")
        (progn
          (setq n (length extras))
          ;; Соберём копии в pickset и подсветим их (grip-выделение).
          (setq ss2 (ssadd))
          (foreach e extras (ssadd e ss2))
          (setq grp (mark:copies-make-group extras))
          (if grp
            (mark:out
              (strcat "[INFO] Копии собраны в группу: " (car grp)))
            (mark:out "[WARN] Не удалось создать группу; только подсветка."))
          ;; Ручной поиск: только собираем в группу и подсвечиваем.
          ;; Удаление — на усмотрение пользователя (Delete/Стереть вручную).
          (sssetfirst nil ss2)
          (mark:out
            (strcat "[INFO] Найдено лишних копий: " (itoa n)
                    ". Собраны в группу и подсвечены."))
          (mark:out
            "[INFO] Копии не удалялись. При необходимости удалите их вручную.")))))
  (princ))

(defun c:МАРКАЗАПБЛОК () (mark:fill-main))
(defun c:MARKFILL () (mark:fill-main))
(defun c:Сетка-мультилинии () (setq *mark:fill-force* "1") (c:МАРКАЗАПБЛОК))
(defun c:Точка-мультилинии () (setq *mark:fill-force* "2") (c:МАРКАЗАПБЛОК))
(defun c:Сетка-динамика () (setq *mark:fill-force* "3") (c:МАРКАЗАПБЛОК))
(defun c:Точка-динамика () (setq *mark:fill-force* "4") (c:МАРКАЗАПБЛОК))
(defun c:Полилинии () (setq *mark:fill-force* "P") (c:МАРКАЗАПБЛОК))

;; Снять старые имена из памяти AutoCAD при повторной загрузке
(setq c:МАРКИРОВКА nil)
(setq c:МАРКАЗ nil)
(setq c:МАРКАР nil)
(setq c:МАРКАЗАЛ nil)
(setq c:МАРКАЗАЛА nil)

(defun mark:snapshot-path (/ src dir ch)
  (setq src (mark:find-source)
        dir (if src (vl-filename-directory src) nil))
  (if (or (null dir) (= dir ""))
    (setq dir (getvar "DWGPREFIX")))
  (if (and (mark:strp dir) (/= dir ""))
    (progn
      (setq ch (substr dir (strlen dir) 1))
      (if (and (/= ch "/") (/= ch "\\"))
        (setq dir (strcat dir "/"))))
    (setq dir ""))
  (strcat dir "MARKZ_SNAPSHOT.txt"))

(defun mark:ax-objname (e / obj r)
  ;; Класс объекта (ObjectName). Для SPDS/прокси это НЕ AcDbBlockReference.
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (vl-catch-all-error-p obj) (null obj))
    "?"
    (progn
      (setq r (vl-catch-all-apply 'vlax-get-property (list obj "ObjectName")))
      (if (or (vl-catch-all-error-p r) (not (mark:strp r))) "?" r))))

(defun mark:spds? (e / on typ)
  ;; Признак объекта SPDS/иной внешней программы или прокси.
  (setq on  (strcase (mark:ax-objname e))
        typ (strcase (cond ((cdr (assoc 0 (entget e)))) (t ""))))
  (or (wcmatch on "*SPDS*")
      (wcmatch on "*СПДС*")
      (wcmatch on "*PROXY*")
      (wcmatch on "*ZOMBIE*")
      (wcmatch typ "*PROXY*")
      (wcmatch typ "*SPDS*")))

(defun mark:diag-name (e / obj nm raw)
  (setq raw (cdr (assoc 2 (entget e)))
        obj (vl-catch-all-apply 'vlax-ename->vla-object (list e))
        nm  (if (and obj (not (vl-catch-all-error-p obj)))
              (mark:ax-get obj "EffectiveName")
              nil))
  (list (if (mark:strp nm) nm "<нет EffectiveName>")
        (if (mark:strp raw) raw "?")))

(defun c:МАРКАДИАГ (/ ss i e ed typ on names nm raw match eb cnt)
  ;; Диагностика: ВЫДЕЛИТЕ рамкой окно/дверь — покажет для каждого объекта
  ;; тип, класс (ObjectName), EffectiveName, имя блока, габариты и признак SPDS.
  (mark:out "МАРКАДИАГ — выделите рамкой окно/дверь (можно один объект).")
  (setq ss (vl-catch-all-apply 'ssget nil))
  (if (or (vl-catch-all-error-p ss) (null ss))
    (mark:out "[INFO] Ничего не выбрано.")
    (progn
      (setq i   (sslength ss)
            cnt 0)
      (mark:out (strcat "[INFO] Выбрано объектов: " (itoa i)))
      (repeat i
        (setq i   (1- i)
              e   (ssname ss i)
              ed  (entget e)
              typ (cdr (assoc 0 ed)))
        (if (< cnt 15)
          (progn
            (setq cnt   (1+ cnt)
                  on    (mark:ax-objname e)
                  names (mark:diag-name e)
                  nm    (car names)
                  raw   (cadr names)
                  match (mark:fill-opening-block? nm)
                  eb    (mark:fill-vla-bb e))
            (mark:out
              (strcat "  #" (itoa cnt)
                      "  DXF-тип=" (if (mark:strp typ) typ "?")
                      "  Класс=" on
                      (if (mark:spds? e) "  [SPDS/прокси]" "")))
            (mark:out
              (strcat "      EffName='" nm "'"
                      "  Block='" (if (mark:strp raw) raw "?") "'"
                      "  проём=" (if match "ДА" "нет")))
            (if eb
              (mark:out
                (strcat "      BB X " (rtos (nth 0 eb) 2 1)
                        ".." (rtos (nth 2 eb) 2 1)
                        "  Y " (rtos (nth 1 eb) 2 1)
                        ".." (rtos (nth 3 eb) 2 1)))
              (mark:out "      BB: нет (VLA не дал габарит)")))))))
  (princ))

(defun c:SNAPSHOT (/ ss i e bb lst fn f ed typ p0 p1 rad w h)
  (mark:emit "\nВыберите элементы для снятия геометрии (SNAPSHOT): ")
  (setq ss (ssget))
  (if (null ss)
    (mark:emit "\n[INFO] Ничего не выбрано.")
    (progn
      (setq i (sslength ss)
            lst nil)
      (repeat i
        (setq i (1- i)
              e (ssname ss i)
              ed (entget e)
              typ (cdr (assoc 0 ed))
              bb (mark:fill-e-bb e))
        (setq lst (cons (list typ
                              (if bb (mapcar 'mark:round1 bb) nil))
                        lst)))
      (setq fn (mark:snapshot-path)
            f  (open fn "w"))
      (if f
        (progn
          (write-line (strcat "; SNAPSHOT: " (itoa (length lst)) " элементов") f)
          (foreach item lst
            (write-line (vl-prin1-to-string item) f))
          (close f)
          (mark:emit (strcat "\n[OK] Снапшот геометрии сохранен в " fn "\n")))
        (mark:emit (strcat "\n[ERROR] Не удалось открыть файл " fn "\n")))))
  (princ))

;;; ---- Сообщение загрузки и краткая справка ---------------------------

(defun mark:repeat (ch n / out)
  (setq out "")
  (repeat n (setq out (strcat out ch)))
  out)
(defun mark:ljust (s w / d)
  ;; Return exactly w characters; do not append an extra space.
  (setq s (if (mark:strp s) s "") d (- w (strlen s)))
  (if (> d 0) (strcat s (mark:repeat " " d)) s))
(defun mark:help-row (cmd desc)
  (mark:emit (strcat "\n  | " (mark:ljust cmd 25) "| " (mark:ljust desc 34) " |")))
(defun mark:help-border ()
  (mark:emit (strcat "\n  +" (mark:repeat "-" 26) "+" (mark:repeat "-" 36) "+")))
(defun mark:help-header ()
  (mark:emit (strcat "\n  | " (mark:ljust "КОМАНДА" 25) "| " (mark:ljust "ЧТО ДЕЛАЕТ" 34) " |")))
(mark:emit (strcat "\n[MARKZ] " (if (mark:strp *mark:rev*) *mark:rev* "Ред. неизвестна")
  " / " (if (mark:strp *mark:build*) *mark:build* "Ред. неизвестна") " загружена."))
(mark:help-border)
(mark:help-header)
(mark:help-border)
(mark:help-row "МАРКАЗ (MARKZ)" "Маркировка заполнений")
(mark:help-row "МАРКАР (MARKAR)" "Рядовка заполнений")
(mark:help-row "МАРКАТАБЛ (MARKTABLE)" "Ведомость: таблица + XLS/CSV")
(mark:help-row "МАРКА (MARKA)" "Полный пакет обработки")
(mark:help-row "МАРКАЗАЛ (MARKFILL)" "Раскладка заполнений по ячейкам")
(mark:help-border)
(princ)
