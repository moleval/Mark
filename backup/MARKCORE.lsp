;;;=====================================================================
;;;  MARKCORE.lsp  —  ЕДИНОЕ ЯДРО MarkZ / MarkV
;;;  Ред. ядра: 1.0 от 2026-11-06
;;;
;;;  ЗАЧЕМ. MarkZ (заполнения) и MarkV (каркас) работают с одними и теми же
;;;  примитивами: строки, числа, ActiveX, имена блоков, журнал, атрибуты,
;;;  динамические свойства, строки шкалы. До этой редакции каждый модуль держал
;;;  собственную копию, и копии расходились. Теперь реализация одна, а в
;;;  модулях остались тонкие обёртки со своими именами (mark:*, mk:*), поэтому
;;;  вызовы внутри модулей не изменились.
;;;
;;;  ПРАВИЛА ЯДРА:
;;;   1. Только префикс mc:. Ядро не знает о mark:/mk:, о командах AutoCAD и
;;;      о глобальных переменных модулей. Все данные приходят аргументами.
;;;   2. Ядро само ничего не печатает: princ и запись журнала делает модуль.
;;;   3. Побочные эффекты только у явно названных функций:
;;;      mc:log-append, mc:log-touch, mc:scale-write, mc:attr-set.
;;;   4. Порядок defun внутри ядра не важен; повторная загрузка безопасна.
;;;
;;;  ЗАГРУЗКА. APPLOAD или (load "MARKCORE.lsp"). Порядок не важен: каждый
;;;  модуль находит ядро сам и загружает его, если функций mc:* ещё нет.
;;;=====================================================================

(setq *mc:core-rev* "1.0")

;; Флаг готовности: модули проверяют именно его — проверка существования
;; функций в этой сборке AutoCAD недоступна. Сбрасывается в nil при старте
;; загрузки, чтобы после
;; неудачной перезагрузки не остался устаревший t, и выставляется в t
;; последней строкой файла — после всех defun.
(setq *mc:core-ready* nil)

;; COM нужен всем функциям ActiveX; грузим сами, чтобы ядро было самодостаточным.
(vl-load-com)

;;;--------------------- 1. Строки и числа -----------------------------

;; Переносимая проверка строки (не зависит от `stringp` из Visual LISP).
(defun mc:strp (x)
  (and x (eq (type x) 'STR)))

;; Обрезка краевых пробелов; не-строка возвращается как есть.
(defun mc:trim (s)
  (if (mc:strp s)
    (vl-string-trim " \t\r\n" s)
    s))

;; Сравнение имён без регистра и краевых пробелов.
(defun mc:name= (s1 s2)
  (and (mc:strp s1)
       (mc:strp s2)
       (= (strcase (mc:trim s1))
          (strcase (mc:trim s2)))))

;; VARIANT -> значение; всё остальное возвращается как есть.
(defun mc:unwrap (v / w)
  (setq w (vl-catch-all-apply 'vlax-variant-value (list v)))
  (if (vl-catch-all-error-p w)
    v
    w))

;; Число из числа, строки или VARIANT; nil — не число.
(defun mc:numval (v / u)
  (setq u (mc:unwrap v))
  (cond
    ((numberp u) (float u))
    ((numberp v) (float v))
    ((and (mc:strp u) (distof u 2)) (distof u 2))
    ((and (mc:strp v) (distof v 2)) (distof v 2))
    (t nil)))

;; Округление до целых мм (ключ сортировки и печать размеров).
(defun mc:round1 (v)
  (atof (rtos (float v) 2 0)))

(defun mc:fmt-raw (v / u)
  (setq u (mc:unwrap v))
  (if (or (numberp u) (mc:strp u))
    (setq v u))
  (cond
    ((null v) "NIL")
    ((numberp v)
     (if (= (float v) (fix (float v)))
       (itoa (fix (float v)))
       (rtos (float v) 2 8)))
    ((mc:strp v) (strcat "\"" v "\""))
    (t (vl-princ-to-string (mc:unwrap v)))))

;; Печать размеров в целых мм — без хвостов вида 409.99999847.
(defun mc:fmt-mm (v / u)
  (setq u (mc:unwrap v))
  (if (numberp u)
    (rtos (mc:round1 u) 2 0)
    (mc:fmt-raw v)))

(defun mc:mm-key (v / u)
  (setq u (mc:unwrap v))
  (if (numberp u)
    (mc:round1 u)
    nil))

;; 0.5 мм — шум float/округления, а не другой типоразмер.
(defun mc:same-num? (a b)
  (and (numberp a) (numberp b) (equal a b 0.5)))

;; Целочисленный ключ размера с шагом 0.5 мм.
(defun mc:dim-key (x)
  (if (and (numberp x) (> x 0.0))
    (fix (+ (* x 2.0) 0.5))
    nil))

(defun mc:positives (lst / out)
  (setq out nil)
  (foreach n lst
    (if (and (numberp n) (> n 0.0))
      (setq out (cons (float n) out))))
  (reverse out))

(defun mc:insert-sorted (lst n / res done hit)
  (setq res nil
        done nil
        hit   nil)
  (foreach x lst
    (if (mc:same-num? n x)
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

(defun mc:sort-unique (nums / res n hit x)
  (setq res nil)
  (foreach n nums
    (setq hit nil)
    (foreach x res
      (if (and (null hit) (mc:same-num? x n))
        (setq hit t)))
    (if (not hit)
      (setq res (mc:insert-sorted res n))))
  res)

;; Индекс значения в списке с допуском 0.5 мм (0-based); nil — не найдено.
(defun mc:index-of (lst v / i found)
  (setq i     0
        found nil)
  (foreach x lst
    (if (and (null found) (mc:same-num? x v))
      (setq found i))
    (setq i (1+ i)))
  found)

;; Подпись «label  value» с выравниванием по колонке.
(defun mc:pad-line (label value)
  (strcat " "
          label
          (substr "                                  " 1
                  (max 1 (- 36 (strlen label))))
          value))

;; Выравнивание вправо до ширины w.
(defun mc:rjust (s w / d)
  (setq d (- w (strlen s)))
  (if (> d 0)
    (strcat (substr "                            " 1 d) s)
    s))

;;;--------------------- 2. Записи (alist) -----------------------------

(defun mc:rec-get (rec key)
  (cdr (assoc key rec)))

;; Замена поля записи, а не добавление второй пары с тем же ключом:
;; assoc всегда возвращает первую, поэтому append молча терял значение.
(defun mc:rec-put (rec key val / old)
  (setq old (assoc key rec))
  (if old
    (subst (cons key val) old rec)
    (append rec (list (cons key val)))))

;;;--------------------- 3. ActiveX ------------------------------------

(defun mc:e->vla (e)
  (vlax-ename->vla-object e))

;; Безопасное чтение свойства: nil при любой ошибке COM.
(defun mc:ax-get (obj prop / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (vl-catch-all-error-p r)
    nil
    r))

;; Безопасная запись свойства: t — записано, nil — ошибка.
(defun mc:ax-put (obj prop val / r)
  (setq r (vl-catch-all-apply 'vlax-put-property (list obj prop val)))
  (if (vl-catch-all-error-p r)
    nil
    t))

;; Вызов метода; arg = nil — метод без аргументов.
(defun mc:ax-invoke (obj method arg / r)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (if arg
                                (list obj method arg)
                                (list obj method))))
  (if (vl-catch-all-error-p r)
    nil
    r))

;; T если метод вызван без ошибки (void-методы возвращают nil — это OK).
(defun mc:ax-invoke-ok (obj method arg / r)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (if arg
                                (list obj method arg)
                                (list obj method))))
  (not (vl-catch-all-error-p r)))

;;;--------------------- 4. Имена блоков -------------------------------

;; Все известные имена вставки: DXF-2, Name, EffectiveName (в этом порядке).
(defun mc:block-names (e / obj names nm)
  (setq names nil
        nm    (cdr (assoc 2 (entget e))))
  (if (mc:strp nm)
    (setq names (cons nm names)))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (and (not (vl-catch-all-error-p obj)) obj)
    (progn
      (setq nm (mc:ax-get obj "Name"))
      (if (and (mc:strp nm) (not (member nm names)))
        (setq names (cons nm names)))
      (setq nm (mc:ax-get obj "EffectiveName"))
      (if (and (mc:strp nm) (not (member nm names)))
        (setq names (cons nm names)))))
  (reverse names))

;; Точное имя ИЛИ маска wcmatch (* ?), без регистра.
(defun mc:name-match? (actual target / a t*)
  (and (mc:strp actual)
       (mc:strp target)
       (progn
         (setq a   (strcase (mc:trim actual))
               t*  (strcase (mc:trim target)))
         (or (= a t*)
             (wcmatch a t*)))))

;; Вставка подходит под имя/маску. Только INSERT: у MLINE в DXF-2 лежит имя
;; стиля мультилинии, и без этой проверки линии принимались за блоки.
(defun mc:blk-match? (e target / names nm ok ed)
  (setq ok nil
        ed (entget e)
        names nil)
  (if (and ed (= (cdr (assoc 0 ed)) "INSERT"))
    (setq names (mc:block-names e)))
  (foreach nm names
    (if (and (null ok) (mc:name-match? nm target))
      (setq ok t)))
  ok)

;;;--------------------- 5. Журнал -------------------------------------

;; Каталог текущего чертежа с завершающим разделителем. "" — чертёж
;; не сохранён: файл журнала создаётся в текущем каталоге AutoCAD.
(defun mc:log-dir (/ dir ch)
  (setq dir (vl-catch-all-apply 'getvar (list "DWGPREFIX")))
  (if (or (vl-catch-all-error-p dir)
          (not (mc:strp dir)))
    (setq dir ""))
  (if (> (strlen dir) 0)
    (progn
      (setq ch (substr dir (strlen dir) 1))
      (if (and (/= ch "/") (/= ch "\\"))
        (setq dir (strcat dir "/")))))
  dir)

;; Полный путь журнала: <каталог DWG>/<basename>.
(defun mc:log-path (basename)
  (strcat (mc:log-dir) basename))

;; Дописать строку в журнал.
;; t — записано (или писать нечего); nil — ошибка ввода-вывода.
;; Файл открывается на каждую строку: при Esc/сбое уже напечатанное
;; не остаётся в незакрытом буфере.
(defun mc:log-append (path msg / f r text)
  (cond
    ((not (and (mc:strp path) (> (strlen path) 0) (mc:strp msg)))
     t)
    (t
     (setq f (vl-catch-all-apply 'open (list path "a")))
     (if (or (null f) (vl-catch-all-error-p f))
       nil
       (progn
         ;; Если сообщение уже заканчивается \n, не дублируем пустую строку.
         (setq text msg)
         (if (and (> (strlen text) 0)
                  (= (substr text (strlen text) 1) "\n"))
           (setq text (substr text 1 (1- (strlen text)))))
         (setq r (vl-catch-all-apply 'write-line (list text f)))
         (vl-catch-all-apply 'close (list f))
         (not (vl-catch-all-error-p r)))))))

;; Проверка, что журнал можно открыть на дозапись (файл создаётся).
(defun mc:log-touch (path / f)
  (setq f (vl-catch-all-apply 'open (list path "a")))
  (if (or (null f) (vl-catch-all-error-p f))
    nil
    (progn
      (vl-catch-all-apply 'close (list f))
      t)))

;;;--------------------- 6. Атрибуты -----------------------------------

;; INSERT с постоянными атрибутами (DXF 66 = 1)?
(defun mc:attr-insert? (ename / ed)
  (setq ed (if ename (entget ename) nil))
  (and ed
       (= (cdr (assoc 0 ed)) "INSERT")
       (equal (cdr (assoc 66 ed)) 1)))

;; Цепочка ATTRIB до SEQEND (без проверки 66: политику задаёт вызывающий).
(defun mc:attr-chain (ename / sub data out)
  (setq sub   (if ename (entnext ename) nil)
        out   nil)
  (while sub
    (setq data (entget sub))
    (cond
      ((null data)
       (setq sub nil))
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (setq out (cons sub out))
       (setq sub (entnext sub)))
      (t
       ;; SEQEND или чужой объект — цепочка закончилась
       (setq sub nil))))
  (reverse out))

;; Значение атрибута строго у INSERT с 66=1; краевые пробелы срезаются.
(defun mc:attr-dxf (ename tag / val)
  (if (mc:attr-insert? ename)
    (foreach a (mc:attr-chain ename)
      ;; как и раньше: при повторном теге побеждает последний
      (if (mc:name= (cdr (assoc 2 (entget a))) tag)
        (setq val (cdr (assoc 1 (entget a)))))))
  (if (mc:strp val)
    (mc:trim val)
    nil))

;; Все атрибуты вставки: alist (тег . значение) в порядке вставки.
(defun mc:attr-list (ename / sub data attrs)
  (setq sub   (if ename (entnext ename) nil)
        attrs nil)
  (while sub
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (setq attrs (cons (cons (cdr (assoc 2 data))
                               (cdr (assoc 1 data)))
                         attrs)))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if sub (setq sub (entnext sub))))
  (reverse attrs))

;; Есть ли атрибут с таким тегом (строго у INSERT с 66=1).
(defun mc:attr-has? (ename tag / found)
  (setq found nil)
  (if (mc:attr-insert? ename)
    (foreach a (mc:attr-chain ename)
      (if (mc:name= (cdr (assoc 2 (entget a))) tag)
        (setq found t))))
  found)

;; Запись значения атрибута: entmod + entupd вставки. t — записано.
(defun mc:attr-set (ename tag newval / done data)
  (setq done nil)
  (if (mc:attr-insert? ename)
    (foreach a (mc:attr-chain ename)
      (setq data (entget a))
      (if (and (null done)
               (mc:name= (cdr (assoc 2 data)) tag))
        (progn
          (entmod (subst (cons 1 newval) (assoc 1 data) data))
          (entupd ename)
          (setq done t)))))
  done)

;; ename атрибута: 1) цепочка INSERT(-2) > ATTRIB(-2), 2) entnext до SEQEND.
;; Работает и там, где DXF-2 у вставки недоступен.
(defun mc:attr-rec (e tag / head ent data result)
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
                (mc:name= (cdr (assoc 2 data)) tag))
           (setq result ent))
          (t
           (setq ent (cdr (assoc -2 data))))))))
  (if (null result)
    (progn
      (setq ent (entnext e))
      (while (and ent (null result))
        (setq data (entget ent))
        (cond
          ((null data)
           (setq ent nil))
          ((mc:name= (cdr (assoc 0 data)) "ATTRIB")
           (if (mc:name= (cdr (assoc 2 data)) tag)
             (setq result ent)
             (setq ent (entnext ent))))
          ((mc:name= (cdr (assoc 0 data)) "SEQEND")
           (setq ent nil))
          (t
           (setq ent nil))))))
  result)

;; Объект атрибута через ActiveX-коллекцию Attributes.
(defun mc:attr-obj (obj tag / result attrs i cnt a tagv)
  (setq result nil)
  (if obj
    (progn
      (setq attrs (mc:ax-get obj "Attributes"))
      (if attrs
        (progn
          (setq cnt (mc:ax-get attrs "Count"))
          (if (and (numberp cnt) (> cnt 0))
            (progn
              (setq i 0)
              (while (and (< i cnt) (null result))
                (setq a    (mc:ax-invoke attrs "Item" i)
                      tagv (if a (mc:ax-get a "TagString") nil))
                (if (and tagv (mc:name= tagv tag))
                  (setq result a))
                (setq i (1+ i)))))))))
  result)

;; Полный поиск значения (MarkZ): DXF-цепочка, entnext, затем ActiveX.
;; Возврат (T значение) | (NIL NIL); пустой атрибут даёт "".
(defun mc:attr-find (e tag / a data val obj)
  (setq a (mc:attr-rec e tag))
  (cond
    (a
     (progn
       (setq data (entget a)
             val  (cdr (assoc 1 data)))
       (list t (if (mc:strp val) val ""))))
    (t
     (progn
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
       (if (or (vl-catch-all-error-p obj) (null obj))
         (list nil nil)
         (progn
           (setq a (mc:attr-obj obj tag))
           (if a
             (progn
               (setq val (mc:ax-get a "TextString"))
               (list t (if (mc:strp val) val "")))
             (list nil nil))))))))

;;;--------------------- 7. Динамические свойства ----------------------

;; Имя свойства dynamic block: PropertyName | Name | Description.
(defun mc:dyn-item-name (item / nm)
  (setq nm (mc:ax-get item "PropertyName"))
  (if (not (mc:strp nm))
    (setq nm (mc:ax-get item "Name")))
  (if (not (mc:strp nm))
    (setq nm (mc:ax-get item "Description")))
  nm)

(defun mc:dyn-item-value (item / val)
  (setq val (mc:ax-get item "Value"))
  (if (null val)
    (setq val (mc:ax-get item "PropertyValue")))
  (mc:unwrap val))

;; Один элемент коллекции свойств -> (имя . значение) или nil.
(defun mc:dyn-item-pair (item / nm val)
  (setq nm  (mc:dyn-item-name item)
        val (mc:dyn-item-value item))
  (if (mc:strp nm)
    (cons nm val)
    nil))

;; Блок — динамический?
(defun mc:dynamic? (obj / v)
  (setq v (mc:ax-get obj "IsDynamicBlock"))
  (and v (= v :vlax-true)))

;; Пары (имя . значение) из GetDynamicBlockProperties. Кэш — в модуле:
;; ядро не держит глобального состояния.
(defun mc:dyn-pairs (obj / r v lst item pair out)
  (setq out nil)
  (setq r (vl-catch-all-apply 'vlax-invoke-method
                              (list obj "GetDynamicBlockProperties")))
  (if (or (vl-catch-all-error-p r) (null r))
    nil
    (progn
      (setq v (mc:unwrap r))
      ;; safearray > list (без get-ubound)
      (setq lst (vl-catch-all-apply 'vlax-safearray->list (list v)))
      (cond
        ((and (not (vl-catch-all-error-p lst)) (listp lst))
         (progn
           (foreach item lst
             (setq item (mc:unwrap item)
                   pair (mc:dyn-item-pair item))
             (if pair
               (setq out (cons pair out))))
           (reverse out)))
        (t
         (progn
           ;; не список — пробуем как одиночный объект
           (setq pair (mc:dyn-item-pair v))
           (if pair
             (list pair)
             (progn
               ;; последняя попытка: сам r как коллекция Item(0)
               (setq out  nil
                     item (mc:ax-invoke r "Item" 0))
               (if item
                 (progn
                   (setq item (mc:unwrap item)
                         pair (mc:dyn-item-pair item))
                   (if pair (setq out (list pair)))))
               out))))))))

;;;--------------------- 8. Шкала --------------------------------------

;; Номер значения в шкале с допуском: 1..N; 0 — нет совпадения.
;; Так нумеруются длины ригелей и ширины «в свету» в обоих модулях.
(defun mc:scale-index (v scale tol / i idx)
  (setq i   1
        idx 0)
  (foreach s scale
    (if (and (= idx 0) (equal s v tol))
      (setq idx i))
    (setq i (1+ i)))
  idx)

;; Строка файла шкалы: «индекс;размер_мм;источник».
(defun mc:scale-row (idx size src)
  (strcat idx ";" (rtos (float size) 2 0) ";" src))

;; Ригель без столбца: «-;размер_мм;источник».
(defun mc:scale-row-reserve (size src)
  (strcat "-;" (rtos (float size) 2 0) ";" src))

;; Итог шкалы: сколько ригелей, номеров и ригелей без столбца.
(defun mc:scale-trailer (beams total reserved)
  (strcat "; ригелей MarkZV: " (itoa beams)
          ", номеров всего: " (itoa total)
          ", без столбца: " (itoa reserved)))

;;;--------------------- 9. Служебное --------------------------------

;; Версия ядра строкой — печатается модулями в баннере диагностики.
(defun mc:core-version ()
  (if (mc:strp *mc:core-rev*) *mc:core-rev* "?"))

;; Все определения выше выполнены — ядро готово к работе.
(setq *mc:core-ready* t)

(princ)
