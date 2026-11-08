;;;=====================================================================
;;;  MARK.lsp - ЕДИНАЯ СБОРКА MarkZ + MarkV
;;;  Состав: MARKCORE.lsp (ядро), MARKZ.lsp (заполнения), MarkV.lsp (каркас)
;;;  Собрано tools/build_single.py; править исходники, а не этот файл.
;;;=====================================================================

;;;=====================================================================
;;;  РЕЕСТР ЗАМЕЧАНИЙ (MarkZ + MarkV). Только mc:*.
;;;=====================================================================
(setq *mc:remarks* nil)
(setq *mc:remark-hold* nil)

(defun mc:remark (code sev text ename x y)
  ;; Read-only probe MarkZ не должен добавлять замечания в отчёт.
  (if (not *mark:probe-quiet*)
    (setq *mc:remarks*
          (cons (list code sev text ename x y) *mc:remarks*)))
  nil)

(defun mc:remark-reset ()
  (setq *mc:remarks* nil)
  nil)

(defun mc:remark-drop (code ename / out r)
  (setq out nil)
  (foreach r *mc:remarks*
    (if (not (and (= (car r) code) (eq (nth 3 r) ename)))
      (setq out (cons r out))))
  (setq *mc:remarks* (reverse out))
  nil)

(defun mc:remark-count (code / n r)
  (setq n 0)
  (foreach r *mc:remarks*
    (if (= (car r) code) (setq n (1+ n))))
  n)

(defun mc:remark-count-sev (sev / n r)
  (setq n 0)
  (foreach r *mc:remarks*
    (if (= (cadr r) sev) (setq n (1+ n))))
  n)

(defun mc:remark-code (text / s)
  (setq s (strcase text))
  (cond
    ((vl-string-search "ВНЕ ЯЧЕЕК" s) "G01S")
    ((vl-string-search "ВНЕ СЕТКИ" s) "G02")
    ((vl-string-search "ДУБЛИКАТ" s) "DUP")
    ((vl-string-search "ПЕРЕКРЫТИЕ" s) "OVL")
    ((vl-string-search "2 БЛОКА" s) "V01")
    ((vl-string-search "НЕ РАЗОБРАНА" s) "M01")
    ((vl-string-search "МАРКА\" ПУСТ" s) "M02")
    ((vl-string-search "XLS" s) "X01")
    ((vl-string-search "SCALE-PROBE" s) "SCL")
    ((vl-string-search "НЕ ПОЛНОСТЬЮ ЗАПОЛНЯЕТ" s) "OPN")
    (t "GEN")))

(defun mc:remark-code-name (code)
  (cond
    ((= code "G01")  "вне ячеек каркаса")
    ((= code "G01S") "вне ячеек без зоны проёма")
    ((= code "G02")  "вне сетки MarkV")
    ((= code "DUP")  "дубликаты")
    ((= code "OVL")  "перекрытия")
    ((= code "V01")  "два блока «Атрибуты витража»")
    ((= code "M01")  "марка не разобрана")
    ((= code "M02")  "атрибут «Марка» пуст")
    ((= code "X01")  "XLS/отчёт не записаны")
    ((= code "SCL")  "шкала MarkV без подтверждения MarkZ")
    ((= code "OPN")  "проём не полностью покрывает ячейку")
    ((= code "GEN")  "прочее замечание прогона")
    ((= code "D01")  "спецмарка Дв")
    ((= code "O01")  "спецмарка Ок")
    ((= code "R01")  "спецмарка Вр")
    (t nil)))

(defun mc:remark-capture (msg / s)
  (if (and (not *mc:remark-hold*) (mc:strp msg))
    (progn
      (setq s (mc:trim msg))
      (cond
        ((wcmatch s "`[ERROR]*") (mc:remark (mc:remark-code s) "ERROR" s nil nil nil))
        ((wcmatch s "`[WARN]*")  (mc:remark (mc:remark-code s) "WARN"  s nil nil nil)))))
  msg)
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
;;;=====================================================================
;;;  MARKZ.lsp  —  ЗАПОЛНЕНИЕ АТРИБУТОВ МАРКИ ЗАПОЛНЕНИЙ
;;;  Файл: D:\MARKZ.lsp
;;;  Команды: МАРКАЗАП, МАРКАРОВКАЗАП, МАРКАЗАПРЯД, МАРКАЗАПТАБЛ, МАРКАЗАПБЛОК
;;;
;;;  Режим самодиагностики TEST 01 ... TEST 16.
;;;  Проверка скобок: внешний AutoExtraction (CHKALL / CHKFILE).
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
(setq *mark:fill-geo*    nil)     ;; (ename . (ширина маркировочной секции высота яруса))
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
;; Ред. 48.46: рамки и состояние независимых шкал по витражам.
(setq *mark:vz-frames*        nil)
(setq *mark:vz-by-frames*     nil)
(setq *mark:vz-current-frame* nil)
(setq *mark:vz-carc-map*      nil)
(setq *mark:vz-state-list*    nil)
;; t только во время read-only probe для MarkV: не писать промежуточный лог.
(setq *mark:probe-quiet*      nil)
(setq *mark:probe-readonly*    nil)
(setq *mark:probe-unsupported* nil)
;; Ред. 48.44: краткоживущий кэш AABB на время mark:vz-split.
;; Один GetBoundingBox на заполнение; ActiveX не вызывается из comparator
;; vl-sort и не повторяется для каждого блока «Атрибуты витража».
(setq *mark:vz-bb-cache* nil)

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
(setq *mark:rev*    "Ред. 48.49")
(setq *mark:build*  "Ред. 36.8")
(setq *mark:file-rev* "Ред. 1.3")
(setq *mark:file-note* "короткий баннер, логика без изменений")

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
        *mark:vz-bb-cache*  nil
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
  (if (not *mark:reuse-sel*)
    (setq *mark:opening-bbs*     nil
          *mark:opening-loaded*  nil
          *mark:opening-partial* nil
          *mark:grid-zone*       nil)))

(mark:reset-state)

;;;--------------------- Сообщения ------------------------------------

;; Ред. 36.5: сообщения дублируются в файл. Раньше mark:out был только
;; prompt, поэтому при закрытой/обрезанной консоли протокол терялся.
;; nil — автоматический путь: <папка текущего DWG>/MARKZ_LOG.txt.
(setq *mark:log-file*   nil)
(setq *mark:log-failed* nil)

;; Ред. 49 (ядро): путь журнала строит mc:log-path.
;; *mark:log-file* остаётся ручным переопределением для пакетных прогонов.
(defun mark:log-path ()
  (if (and (mark:strp *mark:log-file*)
           (> (strlen *mark:log-file*) 0))
    *mark:log-file*
    (mc:log-path "MARKZ_LOG.txt")))

;; Файл открывается на каждую строку: даже при Esc/сбое уже напечатанное
;; не остаётся в незакрытом буфере. Ошибка записи не должна ронять маркировку.
;; Ред. 49 (ядро): дозапись строки — mc:log-append. Флаг ошибки остаётся
;; в модуле: после первой неудачи журнал больше не трогаем.
(defun mark:log-write (msg)
  (if (not *mark:log-failed*)
    (if (not (mc:log-append (mark:log-path) msg))
      (setq *mark:log-failed* t)))
  nil)

;; Каждый пакет добавляет свой блок; одиночные команды дописывают в него.
;; Ред. 49 (ядро): проверка доступности файла — mc:log-touch.
(defun mark:log-start (/ path)
  (setq *mark:log-failed* nil
        path               (mark:log-path))
  (if (not (mc:log-touch path))
    (setq *mark:log-failed* t))
  (if (not *mark:log-failed*)
    (mark:log-write
      (strcat "=== MARKZ " *mark:rev* " / " *mark:build* " ===")))
  path)

;; Единственная точка вывода: princ сохраняет сообщение в командной строке,
;; а log-write оставляет его в файле. Возврат nil — как у prompt.
(defun mark:emit (msg)
  (if (not (mark:strp msg))
    (setq msg (vl-princ-to-string msg)))
  ;; Probe MarkV вызывает геометрию MarkZ до основного прогона. Его
  ;; промежуточные строки не должны попадать ни в консоль, ни в MARKZ_LOG.
  (if (not *mark:probe-quiet*)
    (progn
      (mc:remark-capture msg)
      (mark:log-write msg)
      (princ msg)))
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
  (mc:strp x))

(defun mark:trim (s)
  (mc:trim s))

(defun mark:name= (s1 s2)
  (mc:name= s1 s2))

(defun mark:fmt-raw (v)
  (mc:fmt-raw v))

(defun mark:mm-key (v)
  (mc:mm-key v))

(defun mark:fmt-mm (v)
  (mc:fmt-mm v))

(defun mark:numval (v)
  (mc:numval v))

(defun mark:pad-line (label value)
  (mc:pad-line label value))

;; Выравнивание вправо: колонки лога «ячейка/вставка» стоят ровно.
(defun mark:rjust (s w)
  (mc:rjust s w))

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


(defun mark:same-num? (a b)
  (mc:same-num? a b))

;; округление размера для ключа индекса (0.5 мм)
(defun mark:dim-key (x)
  (mc:dim-key x))

(defun mark:sort-unique (nums)
  (mc:sort-unique nums))

(defun mark:positives (lst)
  (mc:positives lst))

(defun mark:index-of (lst v)
  (mc:index-of lst v))

(defun mark:rec-get (r key)
  (mc:rec-get r key))

(defun mark:rec-put (r key val)
  (mc:rec-put r key val))

(defun mark:vla (e)
  (mc:e->vla e))

;; Имя блока: DXF-код 2 + имя через ActiveX (для динамических блоков)
;; Безопасное чтение ActiveX-свойства через vlax-get-property
(defun mark:ax-get (obj prop)
  (mc:ax-get obj prop))

;; Безопасная запись ActiveX-свойства
(defun mark:ax-put (obj prop val)
  (mc:ax-put obj prop val))

;; Вызов ActiveX-метода (аргументы после method)
(defun mark:ax-invoke (obj method arg)
  (mc:ax-invoke obj method arg))

;; T если метод вызван без ошибки (void-методы возвращают nil — это OK)
(defun mark:ax-invoke-ok (obj method arg)
  (mc:ax-invoke-ok obj method arg))

;; Все известные имена вставки: DXF-2, Name, EffectiveName
(defun mark:block-names (e)
  (mc:block-names e))

;; Точное имя ИЛИ маска wcmatch в target (* ?)

;; Ред. 49 (ядро): имена и сравнение — mc:block-names / mc:name-match?.
(defun mark:blk-match? (e target / names nm ok)
  (setq ok    nil
        names (mc:block-names e))
  (foreach nm names
    (if (and (null ok) (mc:name-match? nm target))
      (setq ok t)))
  ok)

;;;--------------------- Атрибуты -------------------------------------

;; Поиск ATTRIB по DXF-цепочке INSERT(-2) > ATTRIB(-2) > ...
;; Возвращает ename атрибута или nil.
;; Принимает ename вставки.
;; Универсальный поиск: tag передаём явно
(defun mark:find-attr-in-ename (e tag)
  (mc:attr-rec e tag))

;; Через ActiveX-коллекцию Attributes (без vla-get-*)
(defun mark:find-attr-obj (obj tag)
  (mc:attr-obj obj tag))

;; Чтение значения атрибута по ename вставки: (T значение) | (NIL NIL)
(defun mark:find-attr (e tag)
  (mc:attr-find e tag))

;;;--------------------- Dynamic Properties ---------------------------

;; (T значение) | (NIL NIL), если свойство недоступно
;; Развернуть VARIANT > значение
(defun mark:unwrap (v)
  (mc:unwrap v))

;; Имя свойства dynamic block: PropertyName | Name | Description
(defun mark:dyn-item-name (item)
  (mc:dyn-item-name item))


;; Разбор одного элемента коллекции свойств > cons или nil
(defun mark:dyn-item-pair (item)
  (mc:dyn-item-pair item))

;; Пары (имя . значение) из GetDynamicBlockProperties
(defun mark:load-dyn-pairs (obj)
  (mc:dyn-pairs obj))

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
           *mark:carc-cache* nil
           *mark:vz-carc-map* nil
           *mark:vz-state-list* nil
           *mark:vz-current-frame* nil)
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
                            subw subl subh mapW mapH fullH fullW lst pos v)
  ;; Ред. 48.0: марка от секции и яруса. Подындексы — глобально по
  ;; номеру секции (.1/.2/.3 по возрастанию ширины) и букве яруса
  ;; (.а/.б/.в по возрастанию высоты). Без гео — марка не формируется.
  ;; Ред. 48.38: литера по высоте нужна только для различения нескольких
  ;; фактических высот внутри одного геометрического яруса. fullH —
  ;; ярусы, где есть заполнение полной высоты tierh.
  ;; Ред. 48.43: тот же принцип для ширины. Одинаковые Т-половины не
  ;; получают ".1" только из-за отличия от ширины секции. Подындекс нужен
  ;; при полной ширине + подъячейке или при двух и более разных подъячейках.
  (setq mapW  nil
        mapH  nil
        fullH nil
        fullW nil
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
        (if (and iw (mark:same-num? w (nth 0 geo)))
          (setq fullW (cons iw fullW)))
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
            ;; Одна фактическая ширина у всех подъячеек секции ничего не
            ;; различает: без полноразмерного заполнения подындекс не нужен.
            (if (or (member iw fullW) (> (length lst) 1))
              (foreach v lst
                (setq pos (1+ pos))
                (if (and (null subw) (mark:same-num? v w))
                  (setq subw pos))))))
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
            (progn
              ;; Вне ячеек: Дв/Ок/Вр по зонам проёмов.
              ;; В цикле внеячеечных заполнений нет — там не сканируем.
              (if (not *mark:cycle-ss*) (mark:special-outside))
              (mark:report t))))
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
           ((and (null pa) (member e *mark:special-enames*)) nil)
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
        (cond
          ((null ed) (setq e nil))
          ((= "ATTRIB" (cdr (assoc 0 ed)))
           (progn
             (if (assoc 1 ed)
               (entmod (subst (cons 1 "") (assoc 1 ed) ed))
               (entmod (append ed (list (cons 1 "")))))
             (setq cnt (1+ cnt))
             (setq e (entnext e))))
          (t (setq e nil))))))
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
  (if *mark:cycle-ss*
    *mark:cycle-ss*
    (progn
      (setq filter (list (cons 0 "INSERT,LINE,LWPOLYLINE,POLYLINE,MLINE,ARC")))
      (setq ss (vl-catch-all-apply 'ssget (list filter)))
      (if (or (vl-catch-all-error-p ss) (null ss)) nil ss))))

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
      (setq boxes (mark:fill-equal-t-cells boxes hs))
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
      ;; Одиночная ячейка у стойки: 195 -> 200 по ригелю (после Т-половин).
      (setq boxes (mark:fill-single-rail-cells boxes hs))
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


(defun mark:fill-pt-wcs (pt / p)
  (setq p (vl-catch-all-apply 'trans (list pt 1 0)))
  (if (or (vl-catch-all-error-p p) (null p))
    pt
    p))







(defun mark:fill-hits-take (hits n / out)
  (setq out nil)
  (while (and hits (> n 0))
    (setq out (cons (car hits) out)
          hits (cdr hits)
          n (1- n)))
  (reverse out))


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
  (mc:round1 v))

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

;; Ред. 48.43: вертикальная грань не должна заходить внутрь пролёта
;; верхнего/нижнего ригеля. Если конец ригеля лежит снаружи вычисленной
;; грани более чем на 5 мм (5.5 — защита от погрешности координат) и не
;; далее *mark:fill-tol*, привязываем грань к ближайшему такому концу.
;; Номинальные 5 мм здесь не меняются: разрешённый случай равных Т-половин
;; отдельно проверяется mark:fill-equal-t-cells по соседней ячейке и ригелю.
;; Дальние концы не трогаем; сужение по внутренним концам не выполняется.
(defun mark:fill-rail-cell-x (bot top xL xR / it a b d left right)
  (setq left  nil
        right nil)
  (foreach it (append bot top)
    (setq a (float (nth 1 it))
          b (float (nth 2 it)))
    (if (< a xL)
      (progn
        (setq d (- xL a))
        (if (and (> d 5.5)
                 (<= d *mark:fill-tol*)
                 (or (null left) (> a left)))
          (setq left a))))
    (if (> b xR)
      (progn
        (setq d (- b xR))
        (if (and (> d 5.5)
                 (<= d *mark:fill-tol*)
                 (or (null right) (< b right)))
          (setq right b)))))
  (list (if left left xL)
        (if right right xR)))

(defun mark:fill-items->cells (vs hs / cols rows iL iR nL nR
                                Lcol Rcol xL xR w spanning iB nB
                                bot top yB yT h gap boxes
                                oL oR n-own n-open xedges rail-hit n-rail cL cR)
  ;; Ячейка только если четыре стороны доходят до углов и
  ;; противоположные стороны принадлежат разным владельцам.
  (setq *mark:h-bins* (mark:fill-index-hs hs)
        cols (mark:fill-cluster-items vs)
        gap (+ *mark:fill-tol* 25.0)
        boxes nil
        n-own 0
        n-open 0
        n-rail 0
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
            ;; Пролёты bot/top уже проверены fill-span-items. Копии cL/cR
            ;; уточняем только наружу в диапазоне (5; *mark:fill-tol*] мм.
            ;; Исходные xL/xR не меняем: они нужны всем рядам этой пары
            ;; колонок и всем проверкам замкнутости/владельцев без побочных
            ;; эффектов от предыдущей ячейки.
            (setq xedges (mark:fill-rail-cell-x bot top xL xR)
                  rail-hit (or (> (abs (- (car xedges) xL)) 1e-6)
                               (> (abs (- (cadr xedges) xR)) 1e-6))
                  cL (car xedges)
                  cR (cadr xedges))
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
              (if rail-hit (setq n-rail (1+ n-rail)))
              (setq boxes (cons (list cL yB cR yT) boxes))))
           (setq iB (1+ iB)))))
      (setq iR (1+ iR)))
    (setq iL (1+ iL)))
  (mark:out
    (strcat "[INFO] Замкнутых ячеек: " (itoa (length boxes))
            ". Отклонено кандидатов (не ячейки): один блок " (itoa n-own)
            ", неполный контур " (itoa n-open) "."))
  (if (null boxes)
    (mark:out "[INFO] Незамкнутый контур и камеры профиля не заполняю."))
  (mark:out (strcat "[INFO] Ячеек с привязкой границ к ригелям: "
                    (itoa n-rail) "."))
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
                        w h obj lst cell t0 t1 t_ins geo-widths)
  (if (null cells)
    (progn
      (mark:out "[INFO] Нет ячеек — вставок нет.")
      nil)
    (progn
      (setq doc   (mark:ax-get (vlax-get-acad-object) "ActiveDocument")
            space (if doc (mark:ax-get doc "ModelSpace") nil)
            n     0
            lst   nil
            geo-widths nil)
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
                      (progn
                        (setq *mark:fill-geo*
                          (cons (cons ins (list (nth 4 cell) (nth 5 cell)))
                                *mark:fill-geo*))
                        (setq geo-widths (cons (cons ins w) geo-widths))))))
                (mark:out
                  (strcat "  ячейка " (mark:rjust (itoa n) 3)
                          ": " (mark:rjust (rtos x0 2 4) 12) "," (mark:rjust (rtos y0 2 4) 12)
                          "  W=" (mark:rjust (rtos w 2 0) 5)
                          "  H=" (mark:rjust (rtos h 2 0) 5)
                          " ||| вставка " (mark:rjust (itoa n) 3)
                          ": W=" (mark:rjust (rtos w 2 0) 5)
                          "  H=" (mark:rjust (rtos h 2 0) 5))))))
          ;; Ред. 48.48: после геометрического расчёта одиночная
          ;; подгруппа с фактической длиной ригеля получает его номер.
          (setq *mark:fill-geo*
                (mark:fill-geo-rigel-widths *mark:fill-geo* geo-widths))
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

(defun mark:fill-explode-segs (e)
  ;; Probe не имеет права создавать/перемещать/удалять даже временные копии.
  ;; Если для чтения нужен Explode, результат считается неподтверждённым;
  ;; вызывающий MarkV должен показать fallback, а не получить ложную шкалу.
  (if *mark:probe-readonly*
    (progn
      (setq *mark:probe-unsupported* t)
      nil)
    (mark:fill-explode-segs-impl e)))

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


(defun mark:fill-explode-segs-impl (e / doc obj copy lst segs p0 dx dy moved fx fy pair)
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

;; AABB из краткоживущего кэша группировки; вне её — прежний прямой расчёт.
(defun mark:vz-bb-of (e / hit bb)
  (setq hit (assoc e *mark:vz-bb-cache*))
  (if hit
    (cdr hit)
    (progn
      (setq bb (mark:fill-e-bb e)
            *mark:vz-bb-cache* (cons (cons e bb) *mark:vz-bb-cache*))
      bb)))
(defun mark:vz-minx (cluster / m bb x)
  (setq m nil)
  (foreach e cluster
    (setq bb (mark:vz-bb-of e))
    (if bb
      (progn
        (setq x (float (nth 0 bb)))
        (if (or (null m) (< x m))
          (setq m x)))))
  (if m m 1.0e12))

;; Кластеры заполнений: связные компоненты bbox, слева направо
(defun mark:vz-clusters (fills / bbs nobb rest cur queue e0 bb0 hit out e bb r)
  (setq bbs  nil
        nobb nil
        *mark:vz-bb-cache* nil)
  (foreach e fills
    (setq bb (mark:fill-e-bb e))
    ;; Сохраняем и nil: это тоже результат, повторный ActiveX-запрос не нужен.
    (setq *mark:vz-bb-cache*
          (cons (cons e bb) *mark:vz-bb-cache*))
    (if bb
      (setq bbs (cons (cons e bb) bbs))
      (setq nobb (cons e nobb))))
  ;; Контрольная граница для LOG-10: обрыв до этой строки означает первый
  ;; обход GetBoundingBox, после неё — собственно связность/сортировку.
  (mark:out (strcat "[INFO] Габариты для группировки: "
                    (itoa (length bbs))
                    "; без габарита: " (itoa (length nobb)) "."))
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
    (setq bb (mark:vz-bb-of e))
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
;; Ключ состояния — ename рамки; для старого geometric fallback — префикс.
(defun mark:vz-state-key (c / g)
  (setq g (if (cadr c) (car (cadr c)) nil))
  (if g g (mark:vz-pfx-of (cadr c))))

;; В состояние входят именно данные, от которых зависят номера и рядовка.
;; Состояние хранится как (prefix . (('*mark:...* . value) ...)).
(defun mark:vz-state-save (c / key state)
  (setq key (mark:vz-state-key c)
        state
        (cons (mark:vz-pfx-of *mark:glazings*)
          (list
            (cons '*mark:fills*        *mark:fills*)
            (cons '*mark:glazings*     *mark:glazings*)
            (cons '*mark:fill-geo*     *mark:fill-geo*)
            (cons '*mark:scale1*       *mark:scale1*)
            (cons '*mark:scale-final*  *mark:scale-final*)
            (cons '*mark:beam-scale*   *mark:beam-scale*)
            (cons '*mark:scale-shift*  *mark:scale-shift*)
            (cons '*mark:num-was*      *mark:num-was*)
            (cons '*mark:widths*       *mark:widths*)
            (cons '*mark:heights*      *mark:heights*)
            (cons '*mark:cell-hs*      *mark:cell-hs*))))
  (setq *mark:vz-state-list*
        (cons (cons key state) *mark:vz-state-list*))
  state)

(defun mark:vz-state-apply (state / p)
  (foreach p (cdr state)
    (set (car p) (cdr p)))
  state)

(defun mark:vz-state-restore (c / hit)
  (setq hit (assoc (mark:vz-state-key c) *mark:vz-state-list*))
  (if hit (mark:vz-state-apply (cdr hit)))
  hit)

;; В раздельном режиме каркас тоже принадлежит текущей рамке. Карта строится
;; один раз на общий выбор, поэтому девять проходов не повторяют AABB-запросы.
(defun mark:vz-carcass-map-build (ss / i e ed typ bb fr best besta ar)
  (setq i (sslength ss))
  (while (> i 0)
    (setq i (1- i)
          e (ssname ss i)
          ed (entget e)
          typ (if ed (cdr (assoc 0 ed)) nil))
    ;; Для прочих DXF-типов ActiveX не нужен: sec-standalone их не использует
    ;; как каркас (в частности, не трогаем SPDSPOLYMORPHMARK).
    (if (member typ '("INSERT" "MLINE"))
      (progn
        (setq bb (mark:fill-e-bb e)
              best nil
              besta 0.0)
        (foreach fr *mark:vz-frames*
          (setq ar (mark:vz-overlap-area bb (cadr fr)))
          (if (> ar besta) (setq besta ar best fr)))
        (setq *mark:vz-carc-map*
              (cons (cons e (if (and best (> besta 0.0)) (car best) nil))
                    *mark:vz-carc-map*)))))
  (mark:out (strcat "[INFO] Раздельный каркас: привязка к рамкам, объектов "
                    (itoa (length *mark:vz-carc-map*)) "."))
  *mark:vz-carc-map*)

(defun mark:vz-carcass-filter (ss / pair out n)
  (cond
    ((or (null ss)
         (not (= *mark:vz-mode* "separate"))
         (not *mark:vz-by-frames*)
         (null *mark:vz-current-frame*))
     ss)
    (t
     (if (null *mark:vz-carc-map*)
       (mark:vz-carcass-map-build ss))
     (setq out nil n 0)
     (foreach pair *mark:vz-carc-map*
       (if (equal (cdr pair) *mark:vz-current-frame*)
         (progn
           (setq out (if out (ssadd (car pair) out) (ssadd (car pair)))
                 n (1+ n)))))
     (mark:out (strcat "[INFO] Каркас витража "
                       (mark:vz-pfx-of (list *mark:vz-current-frame*))
                       ": " (itoa n) " объект(ов)."))
     out)))

(defun mark:vz-set (c ageo / out)
  (setq *mark:fills*        (car c)
        *mark:glazings*     (cadr c)
        *mark:vz-current-frame*
          (if (and *mark:vz-by-frames* (cadr c)) (car (cadr c)) nil)
        *mark:prefix*
          (if (and *mark:vz-by-frames* (cadr c))
            (mark:vz-pfx-of (list (car (cadr c))))
            "")
        *mark:prefix-found* nil
        out                 nil)
  (setq *mark:prefix-found*
        (if (and (mark:strp *mark:prefix*)
                 (/= *mark:prefix* "")) t nil))
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
(defun mark:vz-frame-point (p dx dy ang / c z)
  (setq c (cos ang) z (sin ang))
  (list (+ (car p) (- (* dx c) (* dy z)))
        (+ (cadr p) (+ (* dx z) (* dy c)))))

;; CaptureBBox блока «Атрибуты витража»: то же правило, что mk:frame-capture-bb
;; в MarkV. Если динамические свойства недоступны, используется обычный AABB.
(defun mark:vz-frame-bb (e / d p o w h vf hf sx sy ang pts xlo xhi ylo yhi)
  (setq d (entget e)
        p (cdr (assoc 10 d))
        o (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
  (if (or (null p) (null o) (vl-catch-all-error-p o))
    nil
    (progn
      (setq w  (mark:dyn-by-mask e "*Ширина рамки*")
            h  (mark:dyn-by-mask e "*Высота рамки*")
            vf (mark:dyn-by-mask e "*Вертикаль*")
            hf (mark:dyn-by-mask e "*Горизонталь*")
            sx (if (cdr (assoc 41 d)) (cdr (assoc 41 d)) 1.0)
            sy (if (cdr (assoc 42 d)) (cdr (assoc 42 d)) sx)
            ang (if (cdr (assoc 50 d)) (cdr (assoc 50 d)) 0.0))
      (if (and (numberp w) (numberp h) (> w 0.0) (> h 0.0))
        (progn
          (setq pts
            (list
              (mark:vz-frame-point p 0.0 0.0 ang)
              (mark:vz-frame-point p (if (= vf 1) (- (* w sx)) (* w sx)) 0.0 ang)
              (mark:vz-frame-point p (if (= vf 1) (- (* w sx)) (* w sx))
                                  (if (= hf 1) (* h sy) (- (* h sy))) ang)
              (mark:vz-frame-point p 0.0 (if (= hf 1) (* h sy) (- (* h sy))) ang)))
          (setq xlo (apply 'min (mapcar 'car pts))
                xhi (apply 'max (mapcar 'car pts))
                ylo (apply 'min (mapcar 'cadr pts))
                yhi (apply 'max (mapcar 'cadr pts)))
          (list xlo ylo xhi yhi))
        (mark:fill-e-bb e)))))

(defun mark:vz-overlap-area (a b / x1 x2 y1 y2)
  (if (and a b)
    (progn
      (setq x1 (max (nth 0 a) (nth 0 b))
            x2 (min (nth 2 a) (nth 2 b))
            y1 (max (nth 1 a) (nth 1 b))
            y2 (min (nth 3 a) (nth 3 b)))
      (if (and (> x2 x1) (> y2 y1)) (* (- x2 x1) (- y2 y1)) 0.0))
    0.0))

;; ((ename (x0 y0 x1 y1) prefix) ...), сортировка не вызывает ActiveX.
(defun mark:vz-frame-list (glazings / out g bb)
  (setq out nil)
  (foreach g glazings
    (setq bb (mark:vz-frame-bb g))
    (if bb
      (setq out (cons (list g bb (mark:vz-pfx-of (list g))) out))))
  (vl-sort (reverse out)
    '(lambda (a b)
       (if (= (nth 0 (cadr a)) (nth 0 (cadr b)))
         (< (nth 1 (cadr a)) (nth 1 (cadr b)))
         (< (nth 0 (cadr a)) (nth 0 (cadr b)))))))

;; Назначение по максимальной площади пересечения, а не по близости
;; геометрических кластеров. Возвращает ((fills (glazing)) ...) либо nil,
;; если хотя бы одно заполнение осталось вне рамок (тогда нужен старый fallback).
(defun mark:vz-frame-split (fills glazings / frames groups e bb fr best besta ar
                                  hit old new extra out g)
  (setq frames (mark:vz-frame-list glazings)
        *mark:vz-frames* frames
        groups nil
        extra nil)
  (foreach fr frames (setq groups (cons (list (car fr) nil) groups)))
  (setq groups (reverse groups))
  (foreach e fills
    (setq bb (mark:vz-bb-of e)
          best nil
          besta 0.0)
    (foreach fr frames
      (setq ar (mark:vz-overlap-area bb (cadr fr)))
      (if (> ar besta) (setq besta ar best fr)))
    (if (and best (> besta 0.0))
      (progn
        (setq old (assoc (car best) groups)
              new (list (car best) (cons e (cadr old)))
              groups (mark:vz-replace groups old new)))
      (setq extra (cons e extra))))
  (if (or (null frames) extra)
    nil
    (progn
      (setq out nil)
      (foreach g groups
        (if (cadr g)
          (setq out (cons (list (reverse (cadr g)) (list (car g))) out))))
      (setq out (reverse out))
      (if out
        (progn
          (mark:out (strcat "[INFO] Группировка по рамкам: витражей "
                            (itoa (length out)) ", заполнений "
                            (itoa (length fills)) ", вне рамок 0."))
          out)
        nil))))

;; Публичная группировка: сначала рамки, затем прежняя геометрическая связность.
(defun mark:vz-split (fills glazings / framed out)
  (setq *mark:vz-by-frames* nil
        *mark:vz-frames* nil
        *mark:vz-bb-cache* nil)
  (setq framed (mark:vz-frame-split fills glazings))
  (if framed
    (progn
      (setq *mark:vz-by-frames* t
            *mark:vz-bb-cache* nil)
      framed)
    (progn
      (setq *mark:vz-frames* nil
            out (mark:vz-split-geometric fills glazings))
      out)))

(defun mark:vz-split-geometric (fills glazings / cls res c g pt d best bd r blks out)
  (setq *mark:vz-by-frames* nil
        *mark:vz-frames* nil)
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
  (setq out (reverse out)
        *mark:vz-bb-cache* nil)
  out)

;; Этап МАРКАРОВКАЗАП по витражам: один кластер — как раньше
;; (лог прежний); несколько — полный блок диагностики per витраж
(defun mark:vz-split-for-mode (fills glazings)
  (if (= *mark:vz-mode* "separate")
    (mark:vz-split fills glazings)
    ;; LOG-9: совместная и одиночная геометрия не меняются.
    (mark:vz-split-geometric fills glazings)))

(defun mark:vz-marks (/ af ag ageo geo-out cls n i c was-reuse ans)
  (if (and (not *mark:reuse-sel*) (null *mark:fills*))
    (mark:select))
  (if *mark:fills*
    (progn
      (setq af   *mark:fills*
            ag   *mark:glazings*
            ageo *mark:fill-geo*
            geo-out nil
            cls  (mark:vz-split-frame-first af ag)
            n    (length cls)
            i    0
            *mark:vz-map*  nil
            *mark:vz-by-frames* nil
            *mark:vz-mode* nil
            *mark:vz-state-list* nil
            *mark:vz-carc-map* nil
            *mark:vz-current-frame* nil)
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
      (if (= *mark:vz-mode* "separate")
        (progn
          (setq cls (mark:vz-split af ag)
                n (length cls))
          (mark:out
            (strcat "[INFO] Раздельная привязка по рамкам: витражей "
                    (itoa n) " (" (mark:vz-join (mark:vz-names cls) ", ") ")."))))
      ;; Готовые заполнения: общая геометрия считается один раз, до деления
      ;; на витражи. Иначе локальный фильтр каркаса теряет общие стойки.
      (if (null ageo)
        (setq ageo (vl-catch-all-apply 'mark:sec-standalone nil)))
      (cond
        ;; совместная: один расчёт на всё выделение, per-витражный префикс
        ((= *mark:vz-mode* "joint")
         (progn
           (setq *mark:vz-map* (mark:vz-map-build-by-frames af ag))
           (if (null *mark:vz-map*)
             (setq *mark:vz-map* (mark:vz-map-build cls)))
           (mark:main)
           (setq *mark:written-total* *mark:written*)
           (setq *mark:stem-total* *mark:cnt-stem*
                 *mark:sand-total* *mark:cnt-sand*)
           (setq geo-out *mark:fill-geo*)))
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
             (mark:sec-local-scale)
             (mark:main)
             (setq *mark:written-total*
                   (if (numberp *mark:written-total*)
                     (+ *mark:written-total* *mark:written*)
                     *mark:written*))
             (setq *mark:stem-total*
                   (if (numberp *mark:stem-total*)
                     (+ *mark:stem-total* *mark:cnt-stem*)
                     *mark:cnt-stem*)
                   *mark:sand-total*
                   (if (numberp *mark:sand-total*)
                     (+ *mark:sand-total* *mark:cnt-sand*)
                     *mark:cnt-sand*))
             (mark:vz-state-save c)
             ;; Ред. 48.45: ветка готовых блоков входит с ageo=nil.
             ;; Сохраняем геометрию всех проходов для Шкала.txt/ведомости.
             (setq geo-out (append geo-out *mark:fill-geo*)))))
        ;; один витраж — прежний путь (лог побайтово)
        (t
         (mark:main)
         (setq *mark:written-total* *mark:written*)
         (setq *mark:stem-total* *mark:cnt-stem*
               *mark:sand-total* *mark:cnt-sand*)
         (setq geo-out *mark:fill-geo*)))
      (setq *mark:reuse-sel* was-reuse
            *mark:fills*     af
            *mark:glazings*  ag
            ;; Прежнюю геометрию сетки сохраняем как раньше. Если её не
            ;; было (источник «Заполнения»), нельзя возвращать nil после
            ;; успешного standalone-расчёта: mark:num-keys терял секции.
            *mark:fill-geo*  (if ageo ageo geo-out)))))

;; Этап МАРКАЗАПРЯД по витражам: каждому — свои базы рядовки
;; (Y снизу и X справа своего габарита) и своя группа
(defun mark:vz-rows (/ af ag ageo cls n i c)
  (if *mark:fills*
    (progn
      (setq af   *mark:fills*
            ag   *mark:glazings*
            ageo *mark:fill-geo*
            cls  (mark:vz-split-for-mode af ag)
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
            (if (= *mark:vz-mode* "separate")
              (if (mark:vz-state-restore c)
                (mark:ar-main)
                (progn
                  (mark:out (strcat "[ERROR] Нет сохранённой шкалы витража "
                                    (mark:vz-title (cadr c))
                                    "; рядовка пропущена, чтобы не наследовать чужую шкалу."))
                  (mark:note-error)))
              (mark:ar-main)))))
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

(defun mark:a-all (preset-kw / kw mode doc r cells pts new-fills)
  (mark:log-start)
  ;; После отдельного МАРКАВ оставляем его замечания для общего отчёта.
  (if (and (not *mark:cycle-ss*) (null *mk:last-groups*))
    (mc:remark-reset))
  (mark:cmd-line
    "МАРКАЗАП — полный цикл. Сетка: вставка, марки, рядовка, ведомость. Заполнения: без вставки.")
  (mark:reset-state)
  (setq *mark:run-written* nil *mark:run-stem* nil *mark:run-sand* nil
        *mark:written-total* 0 *mark:stem-total* 0 *mark:sand-total* 0
        *mark:special-dv* 0 *mark:special-ok* 0
        *mark:special-vr* 0 *mark:special-left* 0
        *mark:special-enames* nil
        *mark:zone-list* nil)
  (mark:out "========================================")
  (mark:out (strcat " МАРКАЗАП — универсальный пакет  " *mark:rev*))
  (mark:out "========================================")
  (mark:out (strcat "[LOG] Протокол: " (mark:log-path)))
  (if *mark:log-failed*
    (mark:out "[WARN] Файл протокола недоступен; сообщения только в командной строке."))
  (if (mark:strp preset-kw)
    (setq kw preset-kw)
    (progn
      (initget "Сетка Заполнения С З S Z 1 2")
      (setq kw (getkword "\nИсточник [Сетка/Заполнения] <Сетка>: "))))
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
      (mark:save-run-counters)
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
      (mark:save-run-counters)
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
  ;; Одиночный пакет (не МАРКАВСЕ): отчёт и сводка.
  (if (not *mark:cycle-ss*)
    (progn
      (mark:report-write)
      (mark:remarks-summary)))
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
;; границей: столбец 1700 в В-7. Ред. 48.17 по данным диагностики секций
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
;; флагом *mark:sec-diag*, печатается один раз и сбрасывает флаг сама.
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
(defun mark:sec-geo-one (bb vs2 hs / x0 y0 x1 y1 cols c ccols it face xl xr
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
      ;; Ред. 48.47: один X-кластер может содержать оси разных,
      ;; разнесённых по Y витражей. Сначала брать экстремум всего
      ;; кластера нельзя: после привязки грани к ригелю такой экстремум
      ;; может не совпасть с фактическим краем ячейки, и колонка будет
      ;; ошибочно отброшена. Проверяем грань каждой оси отдельно;
      ;; сам кластер и построенные ячейки не меняются.
      (foreach c ccols
        (foreach it c
          (setq face (+ (nth 0 it) (nth 3 it)))
          (if (and face (<= face (+ x0 1.0))
                   (or (null xl) (> face xl)))
            (setq xl face))))
      (foreach c ccols
        (foreach it c
          (setq face (- (nth 0 it) (nth 3 it)))
          (if (and face (>= face (- x1 1.0))
                   (or (null xr) (< face xr)))
            (setq xr face))))
      ;; Ред. 48.13: край ячейки — импост (его нет в vs2), а колонки,
      ;; охватывающей ячейку, дальше нет — секция продолжается за
      ;; импост: граница — колонка, заходящая на ярус ячейки и
      ;; выходящая за него по высоте (обрезки внутри колонки мимо).
      (if (null xl)
        (foreach c cols
          (if (mark:sec-tier-col? c y0 y1)
            (foreach it c
              (setq face (+ (nth 0 it) (nth 3 it)))
              (if (and face
                       (<= face (+ x0 1.0))
                       (or (null xl) (> face xl)))
                (setq xl face))))))
      (if (null xr)
        (foreach c cols
          (if (mark:sec-tier-col? c y0 y1)
            (foreach it c
              (setq face (- (nth 0 it) (nth 3 it)))
              (if (and face
                       (>= face (- x1 1.0))
                       (or (null xr) (< face xr)))
                (setq xr face))))))
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


;; Ред. 48.43: вспомогательные функции равных Т-половин.
(defun mark:fill-nth-set (lst idx val / out i x)
  (setq out nil
        i   0)
  (foreach x lst
    (setq out (cons (if (= i idx) val x) out)
          i   (1+ i)))
  (reverse out))

(defun mark:fill-t-rail-end? (hs bb target side / xL xR yB yT h hit gap)
  ;; target — наружный конец, на который предлагается расширить меньшую
  ;; Т-половину. Разрешаем только фактический конец её bot/top.
  (setq xL   (nth 0 bb)
        yB   (nth 1 bb)
        xR   (nth 2 bb)
        yT   (nth 3 bb)
        gap  (+ *mark:fill-tol* 25.0)
        hit  nil)
  (foreach h hs
    (if (and (or (<= (abs (- (+ (nth 0 h) (nth 3 h)) yB)) 1.0)
                 (<= (abs (- (- (nth 0 h) (nth 3 h)) yT)) 1.0))
             (<= (nth 1 h) (+ xL gap))
             (>= (nth 2 h) (- xR gap))
             (or (and (> side 0)
                      (<= (abs (- (nth 1 h) target)) 1.0))
                 (and (< side 0)
                      (<= (abs (- (nth 2 h) target)) 1.0))))
      (setq hit t)))
  hit)

(defun mark:fill-equal-t-cells (boxes hs / i j n a b lb rb li ri lw rw dw
                                  gap uni secw tier target newsec oldsec
                                  newbox changed)
  ;; Две соседние ячейки одной ярусной строки, которые вместе покрывают
  ;; всю секцию, — равные Т-половины. Если их ширины различаются не более
  ;; чем на 5 мм (0.5 — допуск представления), меньшую расширяем только
  ;; наружу до ширины большей и только при подтверждённом конце ригеля.
  ;; Обе получают новую общую ширину секции. Старая ширина в шкалу
  ;; не попадает: шкала содержит только фактические размеры.
  (setq i       0
        n       (length boxes)
        changed 0)
  (while (< i (1- n))
    (setq j (1+ i))
    (while (< j n)
      (setq a (nth i boxes)
            b (nth j boxes))
      (if (and (>= (length a) 6) (>= (length b) 6)
               (numberp (nth 4 a)) (numberp (nth 4 b))
               (numberp (nth 5 a)) (numberp (nth 5 b))
               (<= (abs (- (nth 1 a) (nth 1 b))) 0.1)
               (<= (abs (- (nth 3 a) (nth 3 b))) 0.1)
               (<= (abs (- (nth 4 a) (nth 4 b))) 0.1)
               (<= (abs (- (nth 5 a) (nth 5 b))) 0.1))
        (progn
          (if (<= (nth 0 a) (nth 0 b))
            (setq lb a rb b li i ri j)
            (setq lb b rb a li j ri i))
          (setq lw    (- (nth 2 lb) (nth 0 lb))
                rw    (- (nth 2 rb) (nth 0 rb))
                dw    (abs (- lw rw))
                gap   (- (nth 0 rb) (nth 2 lb))
                uni   (- (nth 2 rb) (nth 0 lb))
                secw  (nth 4 lb)
                tier  (nth 5 lb))
          (if (and (> dw 0.1) (<= dw 5.5)
                   (> gap 1.0) (<= gap (+ *mark:fill-tol* 25.0))
                   (<= (abs (- secw uni)) 1.0))
            (progn
              (if (< lw rw)
                (setq target (- (nth 2 lb) rw))
                (setq target (+ (nth 0 rb) lw)))
              (if (mark:fill-t-rail-end?
                    hs (if (< lw rw) lb rb) target
                    (if (< lw rw) 1.0 -1.0))
                (progn
                  (setq newsec (if (< lw rw)
                                 (- (nth 2 rb) target)
                                 (- target (nth 0 lb)))
                        oldsec secw)
                  (if (< lw rw)
                    (progn
                      (setq newbox (list target (nth 1 lb) (nth 2 lb) (nth 3 lb)
                                         newsec tier)
                            boxes  (mark:fill-nth-set boxes li newbox)
                            newbox (list (nth 0 rb) (nth 1 rb) (nth 2 rb) (nth 3 rb)
                                         newsec tier)
                            boxes  (mark:fill-nth-set boxes ri newbox)))
                    (progn
                      (setq newbox (list (nth 0 lb) (nth 1 lb) (nth 2 lb) (nth 3 lb)
                                         newsec tier)
                            boxes  (mark:fill-nth-set boxes li newbox)
                            newbox (list (nth 0 rb) (nth 1 rb) target (nth 3 rb)
                                         newsec tier)
                            boxes  (mark:fill-nth-set boxes ri newbox))))
                  (setq changed (1+ changed))
                  (if *mark:sec-diag*
                    (mark:out
                      (strcat "[INFO] Равные Т-половины: секция "
                              (rtos oldsec 2 0) " -> " (rtos newsec 2 0)
                              ", ширина " (rtos (if (< lw rw) rw lw) 2 0) ".")))))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (mark:out
    (strcat "[INFO] Выровнено пар равных Т-половин: "
            (itoa changed) "."))
  boxes)

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

;;;--- Маркировочная ширина по фактическому ригелю (Ред. 48.48) ---
;;; Ред. 48.48: номер одиночной подгруппы — по фактическому ригелю.
;; Геометрия Т-секции уже рассчитана и здесь не меняется. Если внутри
;; родительской секции нет полноразмерного заполнения и все её заполнения
;; имеют одну фактическую ширину, совпадающую с длиной ригеля, эта ширина
;; становится маркировочной секцией. Например: W=1100 при secW=2250 -> Рг17.
;; Полная ширина + подъячейка и несколько разных подширин не переназначаются:
;; для них сохраняется родительский номер и правило .1/.2.
;; widths = ((ename . фактическая_W) ...), полученная из координат ячеек.
(defun mark:fill-geo-rigel-widths (geo widths / sub full bad out rec e secw key
                                   w hit lst direct changed logged)
  (setq sub nil
        full nil
        bad nil
        out nil
        changed 0
        logged nil)
  (foreach rec geo
    (setq e    (car rec)
          secw (nth 0 (cdr rec))
          w    (cdr (assoc e widths)))
    (if (numberp secw)
      (setq key (mark:sec-key secw))
      (setq key nil))
    (if (and key (numberp w) (> w 0.0))
      (if (mark:same-num? w secw)
        (setq full (cons key full))
        (setq sub (mark:map-add sub key w)))
      (if key
        (setq bad (cons key bad)))))
  (setq sub (mark:map-sort sub))
  (foreach rec geo
    (setq secw (nth 0 (cdr rec))
          key  (if (numberp secw) (mark:sec-key secw) nil)
          hit  (if key (assoc key sub) nil)
          lst  (if hit (cdr hit) nil)
          direct nil)
    ;; Неизвестная ширина хотя бы одного заполнения делает группу
    ;; непригодной для переназначения; это не повод менять её марку.
    (if (and key hit
             (not (member key full))
             (not (member key bad))
             (= (length lst) 1)
             (mark:index-of *mark:scale1* (car lst)))
      (setq direct (car lst)))
    (if direct
      (progn
        (setq changed (1+ changed))
        (if (not (member key logged))
          (progn
            (setq logged (cons key logged))
            (mark:out
              (strcat "[INFO] Маркировочная ширина по фактическому ригелю: "
                      (rtos (float secw) 2 0) " -> "
                      (rtos (float direct) 2 0) "."))))))
    (setq out
      (cons (cons (car rec)
                  (cons (if direct direct (nth 0 (cdr rec)))
                        (cdr (cdr rec))))
            out)))
  (if (> changed 0)
    (mark:out
      (strcat "[INFO] Заполнений с номером фактического ригеля: "
              (itoa changed) ".")))
  (reverse out))

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
                               n-polyo n-line vs2 out ins hit hitw cnt glaz cs
                               geo-widths)
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
  (if (and ss (not (vl-catch-all-error-p ss)))
    (setq ss (mark:vz-carcass-filter ss)))
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
      (setq *mark:cell-hs* nil
            out nil)
      (foreach bb boxes
        (setq out (cons (append bb (mark:sec-geo-one bb vs2 hs)) out)))
      (setq boxes (mark:fill-single-rail-cells (mark:fill-equal-t-cells (reverse out) hs) hs)
            out   nil)
      (foreach bb boxes
        (if (numberp (nth 5 bb))
          (setq *mark:cell-hs* (cons (nth 5 bb) *mark:cell-hs*))))
      ;; сопоставление: точка вставки блока = угол ячейки; геометрия
      ;; секции уже уточнена общим правилом равных Т-половин.
      (setq cnt 0
            geo-widths nil)
      (foreach e *mark:fills*
        (setq ins (cdr (assoc 10 (entget e)))
              hit nil)
        (if ins
          (progn
            (setq hitw nil)
            (foreach bb boxes
              (if (and (null hit)
                       (<= (abs (- (float (nth 0 bb)) (float (car ins)))) 2.0)
                       (<= (abs (- (float (nth 1 bb)) (float (cadr ins)))) 2.0))
                (setq hit  (list (nth 4 bb) (nth 5 bb))
                      hitw (mark:round1 (- (nth 2 bb) (nth 0 bb))))))))
        (if hit
          (progn
            (setq out (cons (cons e hit) out)
                  geo-widths (cons (cons e hitw) geo-widths)))
          (progn
            (mc:remark "G01" "WARN" "вне ячеек каркаса" e
                       (if ins (car ins) nil)
                       (if ins (cadr ins) nil))
            (setq cnt (1+ cnt)))))
      (if (> cnt 0)
        (mark:out
          (strcat "[INFO] Вне ячеек каркаса: " (itoa cnt)
                  " заполнени(й) — проверка зон проёмов (Дв/Ок/Вр).")))
      (setq *mark:fill-geo*
            (mark:fill-geo-rigel-widths (reverse out) geo-widths))
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
(defun mark:scale-write-file-one (/ path f row i w src rest ws n)
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
              (mc:scale-row (itoa i) w src)
              f))
          (setq rest nil)
          (foreach row *mark:scale-final*
            (if (null (mark:index-of ws (nth 0 row)))
              (setq rest (cons row rest))))
          (foreach row (reverse rest)
            (write-line
              (mc:scale-row-reserve (nth 0 row) (nth 1 row))
              f))
          ;; Ред. 48.37: итог по шкале номеров (строки «-» — ригель
          ;; без столбца, номер за размером закреплён)
          (if *mark:beam-scale*
            (progn
              (setq n (length *mark:beam-scale*))
              (write-line
                (mc:scale-trailer n (length ws) (length rest))
                f)))
          (close f)
          (mark:out (strcat "[OK] Шкала: " path)))
        (mark:out "[WARN] Шкала не записана: файл недоступен.")))))

;; Раздельная шкала: каждый витраж имеет собственные номера. Не смешиваем
;; их в один список и не берём состояние последнего прохода.
(defun mark:scale-write-file-separate (/ path f state-entry state row ws i w src rest n
                                        af ag ageo)
  (setq af *mark:fills* ag *mark:glazings* ageo *mark:fill-geo*
        path (strcat (getvar "dwgprefix")
                     (vl-filename-base (getvar "dwgname")) " Шкала.txt")
        f (open path "w"))
  (if f
    (progn
      (write-line "витраж;индекс;размер_мм;источник" f)
      (foreach state-entry (reverse *mark:vz-state-list*)
        (setq state (cdr state-entry))
        (mark:vz-state-apply state)
        (if *mark:scale-final*
          (progn
            (setq ws (mark:num-keys) i 0)
            (foreach w ws
              (setq i (1+ i) src (mark:sec-src-of w))
              (write-line
                (strcat (car state) ";" (mc:scale-row (itoa i) w src)) f))
            (setq rest nil)
            (foreach row *mark:scale-final*
              (if (null (mark:index-of ws (nth 0 row)))
                (setq rest (cons row rest))))
            (foreach row (reverse rest)
              (write-line
                (strcat (car state) ";-;" (rtos (float (nth 0 row)) 2 0) ";" (nth 1 row)) f))
            (if *mark:beam-scale*
              (progn
                (setq n (length *mark:beam-scale*))
                (write-line
                  (strcat "; " (car state) ": ригелей MarkZV " (itoa n)
                          ", номеров всего: " (itoa (length ws))
                          ", без столбца: " (itoa (length rest))) f))))))
      (close f)
      (setq *mark:fills* af *mark:glazings* ag *mark:fill-geo* ageo)
      (mark:out (strcat "[OK] Раздельная шкала: " path)))
    (progn
      (setq *mark:fills* af *mark:glazings* ag *mark:fill-geo* ageo)
      (mark:out "[WARN] Шкала не записана: файл недоступен."))))

(defun mark:scale-write-file ()
  (if (and (= *mark:vz-mode* "separate") *mark:vz-state-list*)
    (mark:scale-write-file-separate)
    (mark:scale-write-file-one)))

;;;---------------------------------------------------------------------
;;; Read-only источник общей/локальной шкалы для MarkV (Ред. 48.46).
;;; MarkV вызывает probe до маркировки и ведомости. Используются те же ячейки,
;;; длины ригелей и ширины ячеек, что в обычном МАРКАЗАП; атрибуты,
;;; вставки, рядовка и геометрия чертежа не изменяются.
;;;---------------------------------------------------------------------
(defun mark:probe-save (symbols / out s v)
  (setq out nil)
  (foreach s symbols
    (setq v (vl-catch-all-apply 'eval (list s)))
    (if (vl-catch-all-error-p v) (setq v nil))
    (setq out (cons (cons s v) out)))
  (reverse out))

(defun mark:probe-restore (saved / p)
  (foreach p saved (set (car p) (cdr p)))
  nil)

(defun mark:scale-probe-run (enames / fills glazings e)
  (if (null enames)
    nil
    (progn
      (setq fills nil glazings nil)
      (foreach e enames
        (cond
          ((mark:blk-match? e *mark:block-fill*)
           (setq fills (cons e fills)))
          ((mark:blk-match? e *mark:block-glazing*)
           (setq glazings (cons e glazings)))))
      (setq *mark:fills* (reverse fills)
            *mark:glazings* (reverse glazings)
            *mark:sel-total* (length enames)
            *mark:found-names* nil
            *mark:others* 0
            *mark:prefix* ""
            *mark:prefix-found* nil
            *mark:carc-cache* enames
            *mark:reuse-sel* t
            *mark:vz-mode* "joint"
            *mark:vz-by-frames* nil
            *mark:vz-frames* nil
            *mark:vz-current-frame* nil
            *mark:vz-carc-map* nil
            *mark:vz-state-list* nil
            *mark:vz-map* nil
            *mark:fill-geo* nil
            *mark:scale1* nil
            *mark:scale-final* nil
            *mark:beam-scale* nil
            *mark:scale-shift* 0
            *mark:num-was* nil
            *mark:widths* nil
            *mark:heights* nil
            *mark:cell-hs* nil
            *mark:sec-diag* nil
            *mark:dyn-cache* nil
            *mark:seg-cache* nil
            *mark:vz-bb-cache* nil
            *mark:opening-bbs* nil
            *mark:opening-loaded* nil
            *mark:opening-partial* nil
            *mark:grid-zone* nil
            *mark:records* nil
            *mark:errors* 0
            *mark:warnings* 0
            *mark:cnt-stem* 0
            *mark:cnt-sand* 0
            *mark:written* 0
            *mark:precheck-ok* nil
            *mark:apply-failed* 0
            *mark:probe-quiet* t
            *mark:probe-readonly* t
            *mark:probe-unsupported* nil)
      (mark:sec-standalone)
      (if (or *mark:probe-unsupported*
              (not (or *mark:fill-geo* *mark:scale1*)))
        nil
        (mark:num-keys)))))

(defun mark:scale-probe (enames / saved r)
  ;; Сохраняем даже счётчики и кэши: следующий настоящий МАРКАЗАП должен
  ;; начаться с состояния, которое было до вызова MarkV.
  (setq saved
    (mark:probe-save
      '(*mark:fills* *mark:glazings* *mark:sel-total* *mark:found-names*
        *mark:others* *mark:prefix* *mark:prefix-found* *mark:carc-cache*
        *mark:reuse-sel* *mark:vz-mode* *mark:vz-by-frames* *mark:vz-frames*
        *mark:vz-current-frame* *mark:vz-carc-map* *mark:vz-state-list*
        *mark:vz-map* *mark:fill-geo* *mark:scale1* *mark:scale-final*
        *mark:beam-scale* *mark:scale-shift* *mark:num-was* *mark:widths*
        *mark:heights* *mark:cell-hs* *mark:sec-diag*
        *mark:dyn-cache* *mark:seg-cache* *mark:vz-bb-cache*
        *mark:opening-bbs* *mark:opening-loaded* *mark:opening-partial*
        *mark:grid-zone* *mark:records* *mark:errors* *mark:warnings*
        *mark:cnt-stem* *mark:cnt-sand* *mark:written* *mark:precheck-ok*
        *mark:apply-failed* *mark:probe-quiet* *mark:probe-readonly*
        *mark:probe-unsupported* *mark:h-bins* *mark:line-vis* *mark:expl-n*
        *mark:def-n* *mark:vis-n* *mark:hid-n* *mark:vis-open*
        *mark:copy-left* *mark:dyn-quiet* *mark:no-expl-undo* *mark:pt-undo*)))
  (setq r (vl-catch-all-apply 'mark:scale-probe-run (list enames)))
  (mark:probe-restore saved)
  (if (vl-catch-all-error-p r) nil r))

(defun c:МАРКАРОВКАЗАП () (mark:main))
(defun c:MARKZ () (mark:main))
(defun c:MARKAR () (mark:ar-main))
(defun c:МАРКАЗАПРЯД () (mark:ar-main))
(defun c:MARKA () (mark:a-all nil))
(defun c:МАРКАЗАП () (mark:a-all nil))
(defun c:МАРКАЗАПБЛОК () (mark:fill-main))
(defun c:MARKFILL () (mark:fill-main))








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



;; Ред. 48.14: включить [DIAG]-печать геометрии секций на следующий
;; МАРКАЗАП (Источник Сетка или Заполнения). Расчёты не меняет.



;; Снять старые имена из памяти AutoCAD при повторной загрузке
(setq c:МАРКИРОВКА nil)
(setq c:МАРКАЗ nil)
(setq c:МАРКАР nil)
(setq c:МАРКАЗАЛ nil)
(setq c:МАРКАЗАЛА nil)







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
;;;=====================================================================
;;; MarkV.lsp  (ранее MarkZV.lsp)
;;; Модуль сбора данных, построения 2D модели витража
;;; и автозаполнения атрибутов стоек и ригелей.
;;;
;;; Марки СТОЕК и ригелей ставятся по витражам: каждая рамка «Атрибуты
;;; витража» обрабатывается отдельно — свой префикс, свои шкалы номеров,
;;; свои группы на чертеже; ведомость одна и общая.
;;;=====================================================================



;;;=====================================================================

;;;=====================================================================
;; Версия — как у заказчика: редакция и билд отдельными записями «Ред. N.N».
;; В логах печатается «Ред. 7.0 / Ред. 33.19» — две редакции, без слова build.

(setq *mk:ver*            "Ред. 7.1")
(setq *mk:build*          "Ред. 33.20")

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
(setq *mk:log-file*       nil)   ; nil = <папка текущего DWG>/MARKV_LOG.txt
(setq *mk:log-failed*     nil)
(setq *mk:cached-data*    nil)
(setq *mk:last-ss*        nil)   ; выборка последней МАРКАВГЕОМЕТРИЯ
(setq *mk:dyn-cache*      nil)   ; кэш динамических свойств: ename -> alist
(setq *mk:diag-said*      nil)   ; уже напечатанные диагностики прогона
(setq *mk:said-collect*   nil)   ; строка «Все-типы: отрезков …» — один раз за прогон

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
(setq *mk:quiet-edges*           t)     ; t — не предупреждать о выходе панели за границы
;;                                      (панель при этом не двигается: выход за
;;                                       границы — справочная информация, а не
;;                                       дефект; nil — печатать предупреждения)
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
;; Ред. 33.19: область нумерации MarkV/MarkZ.
(setq *mk:mark-mode*           nil)   ; "joint" / "separate"
(setq *mk:joint-scale*         nil)   ; общая шкала, полученная probe MarkZ
(setq *mk:joint-scale-source*  nil)
(setq *mk:vitrage-scales*      nil)   ; (tag scale source), только separate
(setq *mk:current-scale-source* nil)
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
;;;   Базы можно зарегистрировать из Lisp: (mk:register-base "ИМЯ" '(...)).
;;;=====================================================================
(setq *mk:size-bases* nil)          ; ((имя . таблица) ...)

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
(defun mk:strp (x)
  (mc:strp x))

;; Ред. 33.18: весь prompt-вывод идёт через mk:emit и дублируется в файл.
;; Это не дамп MARKZV_MODEL.txt, а полный протокол команд и ошибок.
;; Ред. 33.20 (ядро): путь журнала строит mc:log-path.
(defun mk:log-path ()
  (if (and (mc:strp *mk:log-file*) (> (strlen *mk:log-file*) 0))
    *mk:log-file*
    (mc:log-path "MARKV_LOG.txt")))

;; Ред. 33.20 (ядро): дозапись строки — mc:log-append.
(defun mk:log-write (msg)
  (if (not *mk:log-failed*)
    (if (not (mc:log-append (mk:log-path) msg))
      (setq *mk:log-failed* t)))
  nil)

;; Ред. 33.20 (ядро): проверка доступности файла — mc:log-touch.
(defun mk:log-start (/ path)
  (setq *mk:log-failed* nil
        path             (mk:log-path))
  (if (not (mc:log-touch path))
    (setq *mk:log-failed* t))
  (if (not *mk:log-failed*)
    (mk:log-write
      (strcat "=== MarkV " *mk:ver* " / " *mk:build* " ===")))
  path)

;; Возвращаем nil, как prompt; princ не добавляет лишних кавычек/строк.
(defun mk:emit (msg)
  (if (not (mk:strp msg))
    (setq msg (vl-princ-to-string msg)))
  (mc:remark-capture msg)
  (mk:log-write msg)
  (princ msg)
  nil)

;; Обновление поля записи: замена через subst, а не append.
;; (append …) добавлял бы ВТОРУЮ пару с тем же ключом, а assoc
;; всегда возвращает первую — значение молча не менялось.
(defun mk:rec-put (rec key val)
  (mc:rec-put rec key val))

(defun mk:rec-get (rec key)
  (mc:rec-get rec key))

(defun mk:numval (x)
  (mc:numval x))

(defun mk:ax-get (obj prop)
  (mc:ax-get obj prop))

;; Симметричная запись свойства — через vlax-put-property, как и чтение
(defun mk:ax-put (obj prop val)
  (mc:ax-put obj prop val))

(defun mk:trim (s)
  (mc:trim s))

(defun mk:name= (s1 s2)
  (mc:name= s1 s2))

;; Ред. 33.20 (ядро): только INSERT; имена — mc:block-names, сравнение —
;; mc:name-match?. У MLINE в DXF-2 лежит имя стиля мультилинии.
(defun mk:blk-match? (ename target)
  (mc:blk-match? ename target))

;;;=====================================================================
;;; 3. ИЗВЛЕЧЕНИЕ АТРИБУТОВ
;;;=====================================================================
;; У обычных объектов (MLINE, LINE, LWPOLYLINE…) entnext возвращает СЛЕДУЮЩИЙ
;; объект чертежа, а не подобъект. Без этой проверки обход уходил в соседние
;; блоки и читал/писал ЧУЖИЕ атрибуты «Марка».
(defun mk:has-attrs-insert? (ename)
  (mc:attr-insert? ename))

(defun mk:get-attr (ename tag)
  (mc:attr-dxf ename tag))

(defun mk:get-all-attrs (ename)
  (mc:attr-list ename))

(defun mk:has-attr? (ename tag)
  (mc:attr-has? ename tag))

;;;=====================================================================
;;; 4. ДИНАМИЧЕСКИЕ СВОЙСТВА И ГАБАРИТЫ
;;;=====================================================================

;; Все пары (имя . значение) динамического блока; VARIANT развёрнут
;; Ред. 33.20 (ядро): пары свойств — mc:dyn-pairs; имена обрезаются здесь,
;; потому что MarkV сравнивает их с маской без краевых пробелов.
(defun mk:dyn-pairs-raw (obj / out)
  (setq out nil)
  (if (mc:dynamic? obj)
    (foreach pr (mc:dyn-pairs obj)
      (setq out (cons (cons (mc:trim (car pr)) (cdr pr)) out))))
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
     (mk:emit (strcat "\n  [WARN] В выборке несколько блоков «" *mk:block-vitrage* "». Взят ближайший."))
     (setq e (mk:nearest-insert lst pt)))
    (t
     (setq *mk:vitrage-src* "не выбран" e nil)
     (mk:emit (strcat "\n  [WARN] Блок «" *mk:block-vitrage* "» не выбран в текущей области."))))
  (mk:vitrage-rec e))

(defun mk:read-vitrage-block ()
  (if (null *mk:vitrage-cache*)
    (setq *mk:vitrage-cache* (list (mk:read-vitrage-block-scan))))
  (car *mk:vitrage-cache*))


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
        (cons 'LENGTH (- s1 s0)) (cons 'SIZE_SRC "GEOM") (cons 'GEOM t)
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
        (cons 'LENGTH (- s1 s0)) (cons 'SIZE_SRC "GEOM") (cons 'GEOM t)
        (cons 'CROSS cross) (cons 'ARTICLE (mk:ent-article (nth 0 tr)))
        (cons 'VISIBILITY nil)
        (cons 'TOP_ELEM nil) (cons 'BOT_ELEM nil) (cons 'SUFFIX nil)
      ) beams))))
  ;; Этапы 1 и 3 собирают одни и те же витражи, поэтому строка про отрезки
  ;; печаталась 18 раз подряд с одинаковыми числами. Она полезна только
  ;; тогда, когда в выборке есть геометрия: при «отрезков 0» она ничего не
  ;; сообщает, а «из блоков N/M» дублирует строку витража.
  (if (and (or (> (length segs) 0) (> (length posts) 0) (> (length beams) 0))
           (null *mk:said-collect*))
    (progn
      (setq *mk:said-collect* t)
      (mk:emit (strcat "\n  Все-типы: отрезков " (itoa (length segs))
                      ", из геометрии стоек " (itoa (length posts))
                      ", ригелей " (itoa (length beams))
                      "; из блоков " (itoa (length bposts))
                      "/" (itoa (length bbeams))
                      " (дальше по витражам не печатается)"))))
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
    (mk:emit (strcat "\n  [WARN] " label ": отброшено без габаритов — " (itoa bad))))
  (reverse out))

(defun mk:sanitize-frame (els label / out bad)
  (setq out nil bad 0)
  (foreach el els
    (if (mk:valid-frame? el)
      (setq out (cons el out))
      (setq bad (1+ bad))))
  (if (> bad 0)
    (mk:emit (strcat "\n  [WARN] " label ": отброшено без размеров — " (itoa bad))))
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
               (mk:emit (strcat "\n  [WARN] Панель скорректирована по X: " (rtos new-x 2 1)))))
      (if (< ysc 0.0)
        (progn (setq new-y (+ new-y h))
               (mk:emit (strcat "\n  [WARN] Панель скорректирована по Y: " (rtos new-y 2 1)))))
      ;; Панель у самого края витража — НЕ зеркалим и НЕ сдвигаем.
      ;; Раньше здесь вычиталась ширина: панель, стоящая вплотную к правой
      ;; границе, уезжала на свой размер влево — отсюда были и «панель за
      ;; правой границей», и двойные перекрытия ЗАПОЛНЕНИЕ + ДВЕРЬ.
      ;; Границы витража — справочные: INS_PT панели правится только по
      ;; отражению блока (XSCALE/YSCALE < 0), этого достаточно.
      ;; Предупреждаем, но НЕ двигаем панель: сдвиг «на ширину» был ошибкой.
      (if (and (> (+ new-x w) (+ max-x *mk:tol-skew*))
               (null *mk:quiet-edges*))
        (mk:emit (strcat "\n  [WARN] Панель за правой границей витража: "
                        (rtos (+ new-x w) 2 1) " > " (rtos max-x 2 1)
                        " — положение не меняется")))
      (if (and (> (+ new-y h) (+ max-y *mk:tol-skew*))
               (null *mk:quiet-edges*))
        (mk:emit (strcat "\n  [WARN] Панель за верхней границей витража: "
                        (rtos (+ new-y h) 2 1) " > " (rtos max-y 2 1)
                        " — положение не меняется")))
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
              (mk:emit (strcat "\n  [ERROR] Дубликат: " (mk:safe-str (cdr (assoc 'TYPE el1)))
                              " в точке " (rtos (car pt1) 2 1) "," (rtos (cadr pt1) 2 1)))
              (setq count (1+ count))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (if (> count 0)
    (mk:emit (strcat "\n  [ИТОГО] Дубликатов: " (itoa count))))
  count)

;; Пара «заполнение + окно/дверь» одного проёма — не ошибка: это один
;; световой проём, на который поставлены два блока (заполнение и дверь/окно).

;; Пара «заполнение + проём» (окно/дверь) — это один и тот же световой проём:
;; проём рисуется поверх заполнений и обычно занимает несколько ячеек, поэтому
;; сравнивать габариты бессмысленно. Такая пара перекрытием не считается.
(defun mk:opening-pair? (p1 p2 / t1 t2)
  (setq t1 (cdr (assoc 'TYPE p1)) t2 (cdr (assoc 'TYPE p2)))
  (or (and (= t1 "ЗАПОЛНЕНИЕ") (member t2 '("ОКНО" "ДВЕРЬ")))
      (and (= t2 "ЗАПОЛНЕНИЕ") (member t1 '("ОКНО" "ДВЕРЬ")))))


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
          (if (and (not (mk:opening-pair? p1 p2))
                   (< x1 (+ x2 w2)) (> (+ x1 w1) x2)
                   (< y1 (+ y2 h2)) (> (+ y1 h1) y2))
            (progn
              (mk:emit (strcat "\n  [ERROR] Перекрытие: " (mk:safe-str (cdr (assoc 'TYPE p1)))
                              " и " (mk:safe-str (cdr (assoc 'TYPE p2)))
                              " в области " (mk:safe-num x1) "," (mk:safe-num y1)))
              (setq count (1+ count))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (if (> count 0)
    (mk:emit (strcat "\n  [ИТОГО] Перекрытий: " (itoa count))))
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
            (mk:emit (strcat "\n  [WARN] Вне сетки: " (mk:safe-str (cdr (assoc 'TYPE el)))
                            " в точке " (rtos x 2 1) "," (rtos y 2 1)))
            (setq count (1+ count)))))))
  (if (> count 0)
    (mk:emit (strcat "\n  [ИТОГО] Вне сетки: " (itoa count))))
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
            (mk:emit (strcat "\n  [INFO] Выступающая стойка: " (rtos x 2 1) "," (rtos y 2 1)))
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
  ;; mk:emk нельзя: он складывает объекты в *mk:drawn* (это только графика
  ;; модели). Слой создаётся напрямую entmake — и, если слой уже есть,
  ;; ничего не делаем, чтобы не перебить цвета настройки заказчика.
  ;; Ред. 33.17: пары DXF — ТОЛЬКО точечные: (70 . 0), а не (70 0).
  ;; Запись без точки даёт «неверная DXF-группа: (70 0)» и валит этап 1
  ;; (слой не создаётся, а ошибка всплывает позже, на entmake объектов).
  (if (null (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   '(70 . 0)
                   '(290 . 1)
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
        (mk:emit "\n  [WARN] Коллекция Groups недоступна — группа не создана.")
        (progn
          (setq old (vl-catch-all-apply 'vlax-invoke-method
                      (list groups "Item" name)))
          (if (and old (not (vl-catch-all-error-p old)))
            (vl-catch-all-apply 'vlax-invoke-method (list old "Delete")))
          (setq grp (vl-catch-all-apply 'vlax-invoke-method
                      (list groups "Add" name)))
          (if (or (vl-catch-all-error-p grp) (null grp))
            (mk:emit (strcat "\n  [WARN] Группа \"" name "\" не создана."))
            (progn
              (setq n   (length enames)
                    arr (vlax-make-safearray vlax-vbObject (cons 0 (1- n)))
                    i   0)
              (foreach e enames
                (vlax-safearray-put-element arr i (vlax-ename->vla-object e))
                (setq i (1+ i)))
              (vl-catch-all-apply 'vlax-invoke-method
                (list grp "AppendItems" arr))
              (mk:emit (strcat "\n[OK] Группа \"" name "\": "
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
  (mk:emit (strcat "\n[OK] 2D модель отрисована на слое: " layer))
  (mk:make-group *mk:group-model* (reverse *mk:drawn*)))

;;;=====================================================================
;;; 16. ТЕСТОВАЯ ВИЗУАЛИЗАЦИЯ
;;;=====================================================================





;;;=====================================================================
;;; 17. ВЫВОД ДАННЫХ В ФАЙЛ
;;;=====================================================================

;;;=====================================================================
;;; 18. АВТОЗАПОЛНЕНИЕ АТРИБУТОВ
;;;=====================================================================
(defun mk:set-attr (ename tag newval)
  (mc:attr-set ename tag newval))

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

;; Диагностика прогона — один раз: этапы 1 и 3 обходят одни и те же витражи,
;; и одинаковые строки («Шкала размеров в свету: 7», «Артикулов ригелей: 2»)
;; печатались дважды на витраж. Ключ — сам текст: разные значения видны,
;; повторы молчат.
(defun mk:say-once (msg)
  (if (not (member msg *mk:diag-said*))
    (progn (setq *mk:diag-said* (cons msg *mk:diag-said*))
           (mk:emit msg))))

(defun mk:diag-reset ()
  (setq *mk:diag-said* nil *mk:said-collect* nil))

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
;; Общая шкала для совместного режима. Точный список включает не только
;; длины ригелей, но и ширины ячеек MarkZ; поэтому объединять только
;; LEN_IDX всех групп недостаточно. MarkZ уже загружен в штатном рабочем
;; комплекте и возвращает тот же список, который попадёт в Шкала.txt.
(defun mk:joint-fallback-scale (/ all beams b v out g saved-prefix)
  (setq all nil
        saved-prefix *mk:last-prefix*)
  (foreach g *mk:last-groups*
    (setq beams (cdr (assoc 'BEAMS (mk:group-data g))))
    (foreach b beams
      (setq v (mk:rec-get b 'LENGTH))
      (if (and (numberp v) (null (mk:rec-get b 'VERT)))
        (setq all (cons (atof (mk:size-key v)) all)))))
  (setq out nil)
  (foreach v (vl-sort all '<)
    (if (not (member v out)) (setq out (cons v out))))
  (setq *mk:last-prefix* saved-prefix)
  (reverse out))

;; Общая шкала joint = объединение локальных шкал MarkZ. mark:num-keys —
;; чистое объединение «ригели + ширины ячеек», поэтому союз по
;; витражам даёт ту же шкалу, что один общий проход по всей выборке.
(defun mk:scale-merge (a b / out v hit x)
  (setq out a)
  (foreach v b
    (setq hit nil)
    (foreach x out
      (if (<= (abs (- x v)) *mk:tol-size*) (setq hit t)))
    (if (null hit) (setq out (cons v out))))
  (vl-sort out '<))

(defun mk:joint-probe-union (/ out ok grp r)
  (setq out nil ok t)
  (foreach grp *mk:last-groups*
    (setq r (vl-catch-all-apply 'mark:scale-probe (list (nth 2 grp))))
    (if (and (not (vl-catch-all-error-p r)) r)
      (setq out (mk:scale-merge out r))
      (setq ok nil)))
  (if ok out nil))

(defun mk:joint-length-scale (/ r)
  (setq *mk:current-scale-source* "fallback")
  (if (null *mk:joint-scale*)
    (progn
      ;; Сначала общий проход — как в принятой редакции: он видит всю
      ;; геометрию и потому даёт правильную шкалу. Если общий проход
      ;; упёрся в динамический блок (read-only Explode запрещён),
      ;; берём союз локальных probe по витражам.
      (setq r (vl-catch-all-apply 'mark:scale-probe (list *mk:last-scope*)))
      (if (or (null r) (vl-catch-all-error-p r))
        (setq r (mk:joint-probe-union)))
      (if (and (not (vl-catch-all-error-p r)) r)
        (progn
          (setq *mk:joint-scale* r
                *mk:joint-scale-source* "MarkZ"
                *mk:current-scale-source* "MarkZ")
          (mk:emit (strcat "\n  [INFO] Общая шкала MarkZ: "
                           (itoa (length r)) " номеров.")))
        (progn
          (setq *mk:joint-scale* (mk:joint-fallback-scale)
                *mk:joint-scale-source* "fallback")
          (mk:emit
            "\n  [WARN] MarkZ scale-probe недоступен или dynamic block требует Explode: использована только общая шкала ригелей; синхронизация не подтверждена.")))))
  *mk:joint-scale*)



;; Раздельный режим: probe вызывается на enames одной рамки. Это даёт
;; независимые полные шкалы MarkZ (ригели + ячейки), а не
;; девять копий общей шкалы и не прежнее несовпадение «только ригели vs ячейки».
(defun mk:vitrage-length-scale (fallback / tag grp hit r result source)
  (setq tag *mk:last-prefix*
        grp (if tag (assoc tag *mk:last-groups*) nil)
        hit (if tag (assoc tag *mk:vitrage-scales*) nil))
  (if hit
    (progn
      (setq *mk:current-scale-source* (nth 2 hit))
      (nth 1 hit))
    (progn
      (setq r (if grp
                (vl-catch-all-apply 'mark:scale-probe (list (nth 2 grp)))
                nil))
      (if (and (not (vl-catch-all-error-p r)) r)
        (setq result r source "MarkZ")
        (setq result fallback source "fallback"))
      (setq *mk:vitrage-scales*
            (cons (list tag result source) *mk:vitrage-scales*)
            *mk:current-scale-source* source)
      (if (= source "MarkZ")
        (mk:say-once
          (strcat "\n  [INFO] " (if tag tag "<витраж>")
                  ": локальная шкала MarkZ, " (itoa (length result)) " номеров."))
        (mk:say-once
          (strcat "\n  [WARN] " (if tag tag "<витраж>")
                  ": MarkZ scale-probe недоступен или dynamic block требует Explode; оставлена локальная шкала только по ригелям, синхронизация не подтверждена.")))
      result)))

(defun mk:length-scale (beams panels / bvals pvals out extra v)
  (setq *mk:current-scale-source* "local")
  ;; Сначала собираем прежнюю шкалу ригелей как fallback и для одиночного
  ;; витража. В мультивитражном joint/separate она заменяется read-only
  ;; расчётом MarkZ: общим для joint или scoped по текущей рамке для separate.
  ;; Поэтому Блок 2 (З) не теряется и не расходится между модулями.
  (setq bvals nil)
  (foreach b beams
    (setq v (mk:rec-get b 'LENGTH))
    (if (and (numberp v) (null (mk:rec-get b 'VERT)))
      (setq bvals (cons (atof (mk:size-key v)) bvals))))
  (setq out nil)
  (foreach v (vl-sort bvals '<)
    (if (not (member v out)) (setq out (cons v out))))
  (setq out (reverse out))
  (cond
    ((and (= *mk:mark-mode* "joint")
          (> (length *mk:last-groups*) 1))
     (setq out (mk:joint-length-scale)))
    ((and (= *mk:mark-mode* "separate")
          (> (length *mk:last-groups*) 1))
     (setq out (mk:vitrage-length-scale out))))
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
  (mk:say-once (strcat "\n  [INFO] Шкала размеров в свету: " (itoa (length out))
                       (cond
                         ((= *mk:mark-mode* "joint") " (совместная MarkZ): ")
                         ((= *mk:current-scale-source* "MarkZ") " (витраж MarkZ): ")
                         (t " (по ригелям): "))
                       (mk:join (mapcar '(lambda (v) (rtos v 2 0)) out) " ")))
  ;; Не дефект: в MarkZ заполнение шириной, которой нет среди длин ригелей,
  ;; получает подындекс .1/.2 у Т-узла. Печатается один раз за прогон и с
  ;; самой шкалой — по ней сразу видно, что 895 (например) в шкалу 195/395/…
  ;; не входит и подындекс ожидаем.
  (if extra
    (mk:say-once
      (strcat "\n  [INFO] " (if *mk:last-prefix* *mk:last-prefix* "")
              ": ширин заполнений вне шкалы ригелей "
              (itoa (length extra))
              " — в MarkZ это подъячейки Т-узла (подындекс .1/.2): "
              (mk:join (mapcar '(lambda (v) (rtos v 2 0)) extra) " "))))
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

;; Ред. 33.20 (ядро): номер в шкале — mc:scale-index с допуском *mk:tol-size*.
(defun mk:scale-index (v scale)
  (mc:scale-index v scale *mk:tol-size*))

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
(defun mk:size-mark-table (elements / pairs sorted out labels)
  (setq pairs (mk:article-cross-pairs elements) out nil)
  (if (> (length pairs) 1)
    (progn
      (setq sorted (vl-sort pairs '(lambda (a b) (< (cdr a) (cdr b)))))
      (if (equal (cdr (car sorted)) (cdr (last sorted)) *mk:tol-size*)
        (mk:emit (strcat "\n  [WARN] Габариты сечений артикулов ригелей одинаковы"
                        " — маркеры размера не присвоены. Задайте *mk:article-size*."))
        (progn
          (setq labels (mk:size-labels (length sorted)))
          (foreach pr sorted
            (if (> (strlen (car labels)) 0)
              (setq out (cons (cons (car pr) (car labels)) out)))
            (setq labels (cdr labels)))
          nil))))
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
         (mk:emit (strcat "\n  [WARN] Текстовый стиль «" *mk:label-style*
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

;; Запись марки. Атрибут «Марка» есть — пишем в атрибут; иначе подпись
;; выноской. Мультилинии и геометрические элементы («Все-типы», «Мультилинии»)
;; атрибута не имеют в принципе, поэтому получают подпись ВСЕГДА — даже если
;; у соседнего блока атрибут есть.

(defun mk:write-marks (elements mark-str / count skip-count lab-count e geom)
  (setq count 0 skip-count 0 lab-count 0)
  (foreach el elements
    (setq e    (mk:rec-get el 'ENAME)
          geom (or (mk:rec-get el 'GEOM)
                   (and e (= (cdr (assoc 0 (entget e))) "MLINE"))))
    (if (and e (null geom) (mk:has-attr? e *mk:attr-mark*))
      (progn
        (mk:set-attr e *mk:attr-mark* mark-str)
        (setq count (1+ count)))
      (if (mk:label-mark el mark-str)
        (setq lab-count (1+ lab-count))
        (setq skip-count (1+ skip-count)))))
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
    (mk:emit (strcat "\n  [INFO] Симметричных Т-узлов (оба ригеля «"
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
      (mk:emit (strcat "\n  Ригелей с нераспознанным артикулом: " (itoa n)))
      (initget "КП50 КП50К Ввод")
      (setq ans (getkword "\n  Припуск для них [КП50/КП50К/Ввод] <КП50>: "))
      (cond
        ((= ans "КП50К") (setq *mk:allowance-unknown* *mk:allowance-kp50k*))
        ((= ans "Ввод")
         (initget 4)
         (setq v (getreal "\n  Припуск на элемент, мм <0>: "))
         (setq *mk:allowance-unknown* (if (numberp v) v 0.0)))
        (t (setq *mk:allowance-unknown* 0.0)))
      (mk:emit (strcat "\n  Принято: +" (rtos *mk:allowance-unknown* 2 1)
                      " мм к " (itoa n) " ригел(ю/ям) без артикула."))))
  (if (numberp *mk:allowance-unknown*) *mk:allowance-unknown* 0.0))

;; Вертикальный ригель («т») считается как КП50 — без припуска
(defun mk:beam-cut-len-el (el / len)
  (setq len (mk:rec-get el 'LENGTH))
  (cond
    ((not (numberp len)) 0.0)
    ((mk:rec-get el 'VERT) (float len))
    (t (+ (float len) (mk:beam-allowance-for (mk:rec-get el 'ARTICLE))))))

(defun mk:tab-mp (len cnt / l)
  ;; В ведомости длина печатается целыми мм — от неё же считаем м.п.,
  ;; иначе 1455.0 и 1454.9999 дают в соседних строках 1.46 и 1.45.
  (setq l (fix (+ (float len) 0.5)))
  (/ (* (float l) cnt) 1000.0))

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
    (mk:emit "\n  [WARN] Объединение ячеек не выполнено."))
  res)

(defun mk:tab-set (tbl r c val)
  (vl-catch-all-apply 'vlax-invoke-method (list tbl "SetText" r c val)))

(defun mk:tab-align (tbl r c a)
  (vl-catch-all-apply 'vlax-invoke-method (list tbl "SetCellAlignment" r c a)))

;;;--- Быстрые таблицы (FAST_TABLES) ----------------------------------
;;  Метод из AutoExtraction (common/table-utils.lsp, docs/fast-tables.md):
;;  объект AcDbTable пересобирается целиком при КАЖДОМ изменении, поэтому
;;  заполнение «ячейка за ячейкой» на 160 строк = тысячи полных пересборок
;;  (таблица появлялась через минуты). Правильный порядок:
;;    1) одна вставка сразу конечного размера — vla-AddTable;
;;    2) сразу подавить пересборку — vla-put-RegenerateTableSuppressed T;
;;    3) заполнить всё (ширины, тексты, выравнивания, объединения) —
;;       под подавлением каждый вызов дешёвый;
;;    4) снять подавление (:vlax-false) — единственная полная пересборка.
;;       vla-Update после этого НЕ вызывать — это была бы вторая пересборка.
;;  Длинная ведомость дополнительно режется на таблицы по
;;  *mk:tab-ideal-rows* строк: несколько небольших таблиц пересобираются
;;  заметно быстрее одной очень длинной, а читать их удобнее.

(setq *mk:tab-ideal-rows* 50)   ; целевое число строк данных на таблицу
(setq *mk:tab-row-h* 8.0)       ; высота строки данных, мм
(setq *mk:tab-gap* 40.0)        ; зазор между таблицами по вертикали, мм

(defun mk:tab-fast-begin (tbl)
  (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list tbl :vlax-true))
  tbl)

(defun mk:tab-fast-end (tbl)
  (vl-catch-all-apply 'vla-put-RegenerateTableSuppressed (list tbl :vlax-false))
  tbl)

;; Точка следующей таблицы: вниз по Y от предыдущей (столбиком)
(defun mk:tab-next-point (pt h gap)
  (list (car pt) (- (cadr pt) (+ h gap)) 0.0))

;; Раскладка позиций по кускам: кусок закрывается, когда набрано не меньше
;; per строк И начинается новый раздел (витраж) — раздел никогда не
;; разрывается между таблицами, иначе подытог остался бы не на месте.
(defun mk:tab-chunks (data per / out cur n prev rec)
  (setq out nil cur nil n 0 prev nil)
  (foreach rec data
    (if (and cur (>= n per) (not (and prev (= (nth 1 rec) prev))))
      (progn (setq out (cons (reverse cur) out) cur nil n 0)))
    (setq cur  (cons rec cur)
          n    (1+ n)
          prev (nth 1 rec)))
  (if cur (setq out (cons (reverse cur) out)))
  (reverse out))

;; Число разделов (витражей) подряд внутри куска
(defun mk:tab-nsect (chunk / n cur)
  (setq n 0 cur nil)
  (foreach rec chunk
    (if (not (and cur (= (nth 1 rec) cur)))
      (progn (setq n (1+ n)) (setq cur (nth 1 rec)))))
  n)

;; Вся ведомость в чертёж: одна или несколько таблиц столбиком.
;; pt — точка вставки первой таблицы (nil допустим только вне пакета).
;; Возвращает (TBL PT) последней таблицы или nil.
;; Суммарная ширина таблицы ведомости (мм) — по фактическим ширинам колонок
(setq *mk:tab-col-w* '(14.0 42.0 42.0 32.0 32.0 32.0))

(defun mk:tab-width (/ s)
  (setq s 0.0)
  (foreach w *mk:tab-col-w* (setq s (+ s w)))
  s)

;; Убрать таблицы прошлого прогона (слой ведомости). Возвращает число
;; удалённых; 0 — значит, слой пуст или это первый прогон.
(defun mk:clear-old-tables (/ ss i e n)
  (setq n 0)
  (if (and (not *mk:cycle-tables-cleared*)
           *mk:reuse-table-layer* (tblsearch "LAYER" *mk:layer-table*))
    (progn
      (setq ss (ssget "_X" (list (cons 8 *mk:layer-table*) (cons 0 "ACAD_TABLE"))))
      (if ss
        (repeat (setq i (sslength ss))
          (setq e (ssname ss (setq i (1- i))))
          (if (not (vl-catch-all-error-p (vl-catch-all-apply 'entdel (list e))))
            (setq n (1+ n)))))))
  n)

;; Точка справа от ведомости. pt — левый НИЖНИЙ угол (так его понимает
;; vla-AddTable), поэтому по Y сдвига нет: таблица закладных встаёт на одной
;; линии низа с ведомостью, а не на её верх.
(defun mk:tab-right-point (pt gap)
  (list (+ (car pt) (mk:tab-width) gap) (cadr pt) 0.0))

(defun mk:tab-draw-all (data pt / chunks total ci chunk
                                     tbl last-tbl last-pt
                                     nRows row rec cur cnt mp n-sec n-row
                                     cnt-all mp-all grp-cnt grp-mp
                                     doc space old-echo t0 ms h)
  (if (null data)
    (progn (mk:emit "\n  [INFO] Нет данных для таблицы.") nil)
    (progn
      (if (and (null pt) (not *mk:batch*))
        (setq pt (getpoint "\nУкажите точку вставки таблицы: ")))
      (if (null pt)
        (progn (mk:emit "\n  [INFO] Таблица пропущена (нет точки вставки).") nil)
        (progn
          (mk:ensure-layer *mk:layer-table* 7)
          (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object))
                space    (vla-get-ModelSpace doc)
                old-echo (getvar "CMDECHO")
                chunks   (mk:tab-chunks data *mk:tab-ideal-rows*)
                total    (length chunks)
                ci       0
                last-tbl nil
                last-pt  pt
                t0       (getvar "MILLISECS")
                n-row    0
                cnt-all  0
                mp-all   0.0)
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (foreach rec data
            (setq cnt-all (+ cnt-all (nth 5 rec))
                  mp-all  (+ mp-all (mk:tab-mp (nth 4 rec) (nth 5 rec)))))
          (foreach chunk chunks
            (setq ci    (1+ ci)
                  n-sec (mk:tab-nsect chunk)
                  ;; 2 строки шапки + строки данных + подытог каждого раздела
                  ;; (+ строка «Итого» в последней таблице)
                  ;; формула точная: последняя записанная строка = nRows-1
                  nRows (+ 2 (length chunk) n-sec
                           (if (= ci total) 1 0)))
            (setq tbl (vl-catch-all-apply 'vla-AddTable
                        (list space (vlax-3d-point pt) nRows 6 10.0 30.0)))
            (if (vl-catch-all-error-p tbl)
              (progn
                (mk:emit (strcat "\n  [ERROR] AddTable (таблица " (itoa ci)
                                " из " (itoa total) "): "
                                (vl-catch-all-error-message tbl)))
                (setq tbl nil))
              (progn
                (mk:tab-fast-begin tbl)
                (mk:ax-put tbl "Layer" *mk:layer-table*)
                (vl-catch-all-apply 'vla-SetColumnWidth
                  (list tbl 0 (nth 0 *mk:tab-col-w*)))
                (vl-catch-all-apply 'vla-SetColumnWidth
                  (list tbl 1 (nth 1 *mk:tab-col-w*)))
                (vl-catch-all-apply 'vla-SetColumnWidth
                  (list tbl 2 (nth 2 *mk:tab-col-w*)))
                (vl-catch-all-apply 'vla-SetColumnWidth
                  (list tbl 3 (nth 3 *mk:tab-col-w*)))
                (vl-catch-all-apply 'vla-SetColumnWidth
                  (list tbl 4 (nth 4 *mk:tab-col-w*)))
                (vl-catch-all-apply 'vla-SetColumnWidth
                  (list tbl 5 (nth 5 *mk:tab-col-w*)))
                (mk:tab-set tbl 0 0
                  (if (> total 1)
                    (strcat *mk:tab-title* "  (" (itoa ci) " из "
                            (itoa total) ")")
                    *mk:tab-title*))
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 10.0))
                (setq row 0)
                (foreach hdr '("№" "Артикул" "Марка" "Длина, мм"
                               "Кол-во, шт." "Всего, м.п.")
                  (setq row (1+ row))
                  (mk:tab-set tbl 1 row hdr)
                  (mk:tab-align tbl 1 row 5))
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl 1 10.0))
                (setq row 2 cur nil grp-cnt 0 grp-mp 0.0)
                (foreach rec chunk
                  (if (and cur (/= (nth 1 rec) cur))
                    (progn
                      ;; подытог по разделу
                      (mk:tab-set tbl row 0 "")
                      (mk:tab-set tbl row 1 (strcat "   {\\L" cur "}"))
                      (mk:tab-merge tbl row)
                      (mk:tab-align tbl row 1 4)
                      (mk:tab-set tbl row 4 (itoa grp-cnt))
                      (mk:tab-set tbl row 5 (mk:tab-num grp-mp))
                      (mk:tab-align tbl row 4 5)
                      (mk:tab-align tbl row 5 5)
                      (vl-catch-all-apply 'vla-SetRowHeight
                        (list tbl row *mk:tab-row-h*))
                      (setq row (1+ row) grp-cnt 0 grp-mp 0.0)))
                  (setq cur     (nth 1 rec)
                        n-row   (1+ n-row)
                        cnt     (nth 5 rec)
                        mp      (mk:tab-mp (nth 4 rec) cnt)
                        grp-cnt (+ grp-cnt cnt)
                        grp-mp  (+ grp-mp mp))
                  (mk:tab-set tbl row 0 (itoa n-row))
                  (mk:tab-set tbl row 1 (nth 2 rec))
                  (mk:tab-set tbl row 2 (nth 3 rec))
                  (mk:tab-set tbl row 3 (mk:tab-int (nth 4 rec)))
                  (mk:tab-set tbl row 4 (itoa cnt))
                  (mk:tab-set tbl row 5 (mk:tab-num mp))
                  (mk:tab-align tbl row 0 5)
                  (mk:tab-align tbl row 1 4)
                  (mk:tab-align tbl row 2 4)
                  (mk:tab-align tbl row 3 5)
                  (mk:tab-align tbl row 4 5)
                  (mk:tab-align tbl row 5 5)
                  (vl-catch-all-apply 'vla-SetRowHeight
                    (list tbl row *mk:tab-row-h*))
                  (setq row (1+ row)))
                (if cur
                  (progn
                    (mk:tab-set tbl row 0 "")
                    (mk:tab-set tbl row 1 (strcat "   {\\L" cur "}"))
                    (mk:tab-merge tbl row)
                    (mk:tab-align tbl row 1 4)
                    (mk:tab-set tbl row 4 (itoa grp-cnt))
                    (mk:tab-set tbl row 5 (mk:tab-num grp-mp))
                    (mk:tab-align tbl row 4 5)
                    (mk:tab-align tbl row 5 5)
                    (vl-catch-all-apply 'vla-SetRowHeight
                      (list tbl row *mk:tab-row-h*))
                    (setq row (1+ row))))
                (if (= ci total)
                  (progn
                    (mk:tab-set tbl row 0 "")
                    (mk:tab-set tbl row 1 "   {\\LИтого:}")
                    (mk:tab-merge tbl row)
                    (mk:tab-align tbl row 1 4)
                    (mk:tab-set tbl row 4 (itoa cnt-all))
                    (mk:tab-set tbl row 5 (mk:tab-num mp-all))
                    (mk:tab-align tbl row 4 5)
                    (mk:tab-align tbl row 5 5)
                    (vl-catch-all-apply 'vla-SetRowHeight
                      (list tbl row *mk:tab-row-h*))
                    (setq row (1+ row))))
                (mk:tab-fast-end tbl)
                (setq last-tbl tbl last-pt pt)
                (setq h (vl-catch-all-apply 'vlax-get-property (list tbl 'Height)))
                (if (vl-catch-all-error-p h)
                  (setq h (* 10.0 nRows)))
                (setq pt (mk:tab-next-point pt h *mk:tab-gap*)))))))
          (setq ms (- (getvar "MILLISECS") t0))
          (mk:emit (strcat "\n  [OK] Ведомость в чертеже: таблиц " (itoa total)
                          ", позиций " (itoa (length data))
                          ", всего шт. " (itoa cnt-all)
                          ", м.п. " (mk:tab-num mp-all)
                          ", время " (rtos (/ ms 1000.0) 2 2) " с"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          (if last-tbl (list last-tbl last-pt) nil))))

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

(defun mk:tab-xls (data file acc / f rec sect art mark len cnt mp cur sz
                     xl-row grp-start sub-rows n-no cnt-sub mp-sub
                     total-cnt total-mp hdr w0)
  (setq f (open file "w"))
  (if (null f)
    (progn (mk:emit (strcat "\n  [ERROR] Нет доступа: " file)) nil)
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
      ;; Excel держит файл открытым: open/close проходят, а данные не
      ;; доходят. Проверяем то, что видно: файл есть и он не пустой.
      (setq sz (vl-file-size file))
      (if (and (numberp sz) (> sz 0))
        (progn
          (mk:emit (strcat "\n  [OK] XLS: " file
                          " (" (itoa sz) " байт)"))
          t)
        (progn
          (mk:emit (strcat "\n  [WARN] XLS не записан (файл занят другой"
                          " программой или пуст): " file))
          nil)))))

;;;=====================================================================
;;; 20c. ЗАКЛАДНЫЕ: атрибуты «Пятка*» и «Кронштейн*» динамических блоков
;;;   Собираются только у вставок блоков (мультилинии игнорируются),
;;;   агрегируются по паре «атрибут + значение» и выводятся второй таблицей.
;;;=====================================================================
(setq *mk:acc-masks*    '("Пятка*" "Кронштейн*"))
(setq *mk:acc-gap*      30.0)   ; отступ второй таблицы от первой
;; Повторный прогон в том же чертеже: ведомость прошлого прогона лежит на
;; слое *mk:layer-table* и стоит там же (точка считается по витражам) — новая
;; таблица легла бы ровно поверх старой. Перед вставкой таблицы прошлого
;; прогона убираются; на слое MarkV рисует только их.
(setq *mk:reuse-table-layer* t)
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
          (mk:emit (strcat "\n  [ERROR] AddTable: " (vl-catch-all-error-message tbl)))
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          nil)
        (progn
          (mk:tab-fast-begin tbl)
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
          (mk:tab-fast-end tbl)
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          (mk:emit (strcat "\n  [OK] Таблица закладных, строк: " (itoa (length data))))
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
        ;; значение — «артикул (сечение)»: по сечению видно, почему маркер
        ;; именно такой, и таблица перестаёт быть повтором списка артикулов
        (setq out (cons (list (car labels) (car pr) (cdr pr)) out))
        (setq labels (cdr labels)))))     ; из возрастания -> убывание
  out)

;; Печать в командную строку
(defun mk:leg-print (sfx arts)
  (if sfx
    (progn
      (mk:emit (strcat "\n  " *mk:leg-h1*))
      (foreach p sfx
        (mk:emit (strcat "\n    " (mk:pad (car p) 6) " - " (cdr p))))))
  ;; Таблица «маркер -> артикул (сечение)»: единственный полный список
  ;; артикулов всего прогона — остальные строки про артикулы убраны.
  (if arts
    (progn
      (mk:emit (strcat "\n  " *mk:leg-h2*))
      (foreach p arts
        (mk:emit (strcat "\n    "
                        (mk:pad (if (> (strlen (car p)) 0) (car p) *mk:leg-none*) 16)
                        " - " (nth 1 p)
                        " (" (rtos (nth 2 p) 2 0) " мм)")))))
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
          (mk:emit (strcat "\n  [ERROR] AddTable: " (vl-catch-all-error-message tbl)))
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          nil)
        (progn
          (mk:tab-fast-begin tbl)
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
                ;; arts — список (маркер артикул сечение)
                (mk:tab-set tbl row 1
                  (strcat (nth 1 p) " (" (rtos (nth 2 p) 2 0) " мм)"))
                (mk:tab-align tbl row 0 5)
                (mk:tab-align tbl row 1 4)
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                (setq row (1+ row)))))
          (mk:tab-fast-end tbl)
          (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
          (mk:emit (strcat "\n  [OK] Таблица обозначений, строк: "
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
            (mk:xcell f "D" nil "String"
              (strcat (nth 1 p) " (" (rtos (nth 2 p) 2 0) " мм)"))
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

;; Удалить всю отрисовку модели: объекты слоя + одноимённую группу.
;; Раньше вызывалось по каждой группе витража и каждый раз сносило ВЕСЬ слой —
;; счётчик «объектов» при этом показывал остаток после первого прохода, а при
;; сбое прогона удаление не выполнялось вовсе и сетка оставалась в чертеже.
;; Теперь: сначала группа (если задана), затем весь слой — один раз.
(defun mk:erase-model (layer group / ss i n)
  (if (mk:strp group) (mk:group-delete group))
  (setq n 0)
  (if (mk:strp layer)
    (progn
      (setq ss (ssget "_X" (list (cons 8 layer))))
      (if ss
        (progn
          (setq i (sslength ss))
          (repeat i
            (setq i (1- i))
            (if (entdel (ssname ss i)) (setq n (1+ n))))))))
  n)

;; Снести сетки всех витражей прогона (слой — целиком, группы — по именам)
(defun mk:erase-all-models (/ n)
  (setq n 0)
  (foreach grp *mk:last-groups*
    (mk:group-delete (mk:group-model-name (nth 0 grp))))
  (setq n (mk:erase-model *mk:layer-model* nil))
  (mk:erase-model *mk:layer-test* *mk:group-test*)
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
(setq *mk:batch-keep-grid* nil)  ; t — оставить сетки моделей в чертеже
(setq *mk:scope-source*    nil)  ; «выборка» / «весь чертёж» — для дампа
(setq *mk:last-groups*   nil)   ; группы прогона: (TAG FRAME ENANES RECORDS COUNTS)
(setq *mk:last-extra*    nil)   ; объекты вне рамок — не маркируются
(setq *mk:last-scope*    nil)   ; полная выборка прогона (список ename)
(setq *mk:last-frames*   nil)   ; рамки чертежа
(setq *mk:last-ss*       nil)   ; набор выборки прогона (для collect)
(setq *mk:last-prefix*   nil)   ; префикс текущей группы (выноски, группы)
(setq *mk:run-scope*     nil)   ; выборка пакета МАРКАВ (список ename)
(setq *mk:last-mode*     nil)   ; режим сбора каркаса прогона (nil — ещё не задан)
(setq *mk:vitrage-none*  "<ВНЕ РАМОК>")

;; Подробный разбор сбоя витража: печатать собранные координаты.
(setq *mk:debug-stack* nil)

;; Первые несколько X (и Y) из записей — чтобы видеть, что именно собралось
(defun mk:first-coords (recs / out n pt)
  (setq out "" n 0)
  (foreach r recs
    (if (< n 5)
      (progn
        (setq pt (cdr (assoc 'INS_PT r)))
        (if pt
          (setq out (strcat out " (" (rtos (car pt) 2 0) ","
                            (rtos (cadr pt) 2 0) ")"))
          (setq out (strcat out " ?")))
        (setq n (1+ n)))))
  out)

;; Этап обработки текущего витража — чтобы [ERROR] называл место сбоя,
;; а не только текст исключения («неверная DXF-группа: (70 0)»).
(setq *mk:step* nil)

;; Спрашивать точку вставки ведомости в пакете МАРКАВ.
;; t   — после уборки сеток МАРКАВ спрашивает точку вставки таблицы
;;       (Enter — прежнее автоматическое место, под витражами);
;; nil — не спрашивать вовсе, точка всегда считается автоматически.
(setq *mk:ask-table-pt* t)

;;;--- Точка вставки таблицы в пакете ----------------------------------

;; В пакете МАРКАВ getpoint недоступен (этап идёт через vl-catch-all-apply),
;; поэтому точка вставки ведомости у него своя — начало координат.
;; *mk:table-ins-pt* можно задать заранее, тогда берётся он.
(setq *mk:table-ins-pt* nil)

;; Точка по умолчанию — (0,0). Раньше она считалась по габаритам витражей
;; (левый нижний угол минус два зазора): в одном и том же чертеже получалось
;; одно и то же место, а зависимость от состава выборки только мешала — при
;; повторном прогоне таблица вставала «куда-то туда же» и её искали руками.
(setq *mk:default-table-pt* '(0.0 0.0 0.0))

(defun mk:auto-table-point ()
  *mk:default-table-pt*)

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
  (mk:emit "\nВыберите элементы витражей (Enter — весь чертёж): ")
  (setq ss (ssget))
  (if ss
    (progn
      (setq out nil i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (not (mk:sub-entity? e)) (setq out (cons e out)))
        (setq i (1+ i)))
      (setq *mk:scope-source* "выборка" out (reverse out))
      (mk:emit (strcat "\n  Область: выборка, объектов " (itoa (length out)))))
    (progn
      (mk:emit "\n  [INFO] Выбор отменён — обрабатывается ВЕСЬ чертёж.")
      (setq all (ssget "_X" '((0 . "INSERT,MLINE,LINE,LWPOLYLINE,POLYLINE,ARC"))))
      (setq out nil i 0)
      (if all
        (repeat (sslength all)
          (setq e (ssname all i))
          (if (not (mk:sub-entity? e)) (setq out (cons e out)))
          (setq i (1+ i))))
      (setq *mk:scope-source* "чертёж" out (reverse out))
      (mk:emit (strcat "\n  Область: весь чертёж, объектов " (itoa (length out))))))
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
      ;; объект, не подошедший ни под один тип, раньше терялся молча. Помним
      ;; и вставки с незнакомым блоком (так «пропадали» окна), и подобъекты
      ;; (ATTRIB/SEQEND/VERTEX), и прочие типы (SPDSPOLYMORPHMARK, POINT) —
      ;; по ним печатается список вне типов и пишется дамп.
      (setq unknown (cons e unknown))))
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
;; Подпись объекта вне типов: имя блока, а если его нет (POINT,
;; SPDSPOLYMORPHMARK) — DXF-тип в угловых скобках. Иначе счётчик печатал
;; бессмысленное «? x30» и дублировал список ниже.
(defun mk:unknown-label (e / nm typ)
  (setq nm (mk:ent-name e))
  (if (not (mk:strp nm))
    (progn
      (setq typ (cdr (assoc 0 (entget e))))
      (setq nm (strcat "<" (mk:safe-str typ) ">"))))
  nm)

(defun mk:unknown-table (/ out hit key)
  (setq out nil)
  (foreach e *mk:last-unknown*
    (setq key (mk:unknown-label e))
    (if (setq hit (assoc key out))
      (setq out (subst (cons key (1+ (cdr hit))) hit out))
      (setq out (cons (cons key 1) out))))
  (vl-sort out '(lambda (a b) (> (cdr a) (cdr b)))))

;; Дамп объектов вне типов: DXF-тип, имя блока, слой, координаты, handle.
;; Нужен, когда в выборке встречается незнакомый тип (SPDSPOLYMORPHMARK,
;; POINT, «чужая» вставка): по дампу видно, что это и откуда.
(defun mk:dump-unknown (lst / f i e d p)
  (setq f (open *mk:out-file* "a"))
  (if f
    (progn
      (write-line "" f)
      (write-line "=== НЕ РАСПОЗНАННЫЕ ОБЪЕКТЫ (вне типов MarkV) ===" f)
      (write-line (strcat "Всего: " (itoa (length lst))) f)
      (setq i 0)
      (foreach e lst
        (if (< i 40)
          (progn
            (setq d (entget e) p (cdr (assoc 10 d)))
            (write-line
              (strcat "  " (mk:safe-str (cdr (assoc 0 d)))
                      " | блок: " (mk:safe-str (cdr (assoc 2 d)))
                      " | слой: " (mk:safe-str (cdr (assoc 8 d)))
                      " | точка: "
                      (if p (strcat (rtos (car p) 2 1) "," (rtos (cadr p) 2 1))
                        "?")
                      " | handle: " (mk:safe-str (cdr (assoc 5 d))))
              f)
            (setq i (1+ i)))))
      (if (> (length lst) 40)
        (write-line "  ... (остальные не печатаются)" f))
      (close f)
      nil)))

;; Вставки в выборке, не попавшие ни в один тип конвейера. Пакет МАРКАВ
;; разбирает выборку по рамкам витражей, поэтому *mk:last-unknown* там пуст;
;; без этого счётчика исчезала бы подсказка «окна = 0 — смотри имена блоков».
(defun mk:count-unknown (/ n)
  (setq n 0)
  (foreach e *mk:last-scope*
    (if (and e (null (mk:obj-kind e))) (setq n (1+ n))))
  n)

;; Имена объектов, не попавших ни в один тип, — по убыванию количества,
;; с DXF-типом (INSERT / ATTRIB / MLINE…). Без имён счётчик бесполезен:
;; «30 вне типов» ничего не говорит о том, что это за блоки.
(defun mk:unknown-names (/ out nm typ hit)
  (setq out nil)
  (foreach e *mk:last-scope*
    (if (and e (null (mk:obj-kind e)))
      (progn
        (setq typ (cdr (assoc 0 (entget e))))
        ;; у объекта без блока подпись уже равна <DXF-тип> — второй раз тип
        ;; в скобках не печатаем
        (setq nm (mk:unknown-label e))
        (if (/= nm (strcat "<" (mk:safe-str typ) ">"))
          (setq nm (strcat nm "  [" (mk:safe-str typ) "]")))
        (if (setq hit (assoc nm out))
          (setq out (subst (cons nm (1+ (cdr hit))) hit out))
          (setq out (cons (cons nm 1) out))))))
  (vl-sort out '(lambda (a b) (> (cdr a) (cdr b)))))

;;;--- Печать групп ----------------------------------------------------

;; Сводка «видов объектов» выборки — одна строка на тип. В пакете МАРКАВ
;; строка «из блоков N/M» по витражам не печатается (она шумит на 9 витражах),
;; но без неё не видно, что вообще попало в выборку (например, что окон в
;; чертеже нет), поэтому сводка печатается один раз за прогон.
(defun mk:print-kinds (/ kinds e k n)
  (setq kinds nil n 0)
  (foreach e *mk:last-scope*
    (setq k (if e (mk:obj-kind e) nil))
    (if k
      (progn
        (setq n (1+ n))
        (if (assoc k kinds)
          (setq kinds (subst (cons k (1+ (cdr (assoc k kinds))))
                             (assoc k kinds) kinds))
          (setq kinds (cons (cons k 1) kinds))))))
  (mk:emit (strcat "\n  Видов объектов в выборке (" (itoa n) " из "
                  (itoa (length *mk:last-scope*)) "):"))
  (foreach pr (reverse kinds)
    (mk:emit (strcat "\n    " (mk:pad (car pr) 14) (itoa (cdr pr)))))
  kinds)

(defun mk:print-groups (groups extra / n g c)
  (setq n 0)
  (mk:emit "\n=== ГРУППЫ ПО ВИТРАЖАМ ===")
  (foreach g groups
    (setq n (1+ n) c (nth 4 g))
    (mk:emit (strcat "\n  " (mk:pad (nth 0 g) 10)
                    " стойки " (itoa (nth 0 c))
                    ", ригели " (itoa (nth 1 c))
                    ", заполнения " (itoa (nth 2 c))
                    ", окна " (itoa (nth 3 c))
                    ", двери " (itoa (nth 4 c))
                    "   всего " (itoa (nth 5 c)))))
  (mk:emit (strcat "\n  " (mk:pad *mk:vitrage-none* 10)
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

(defun mk:group-cache-clear ()
  (setq *mk:group-cache* nil
        *mk:joint-scale* nil
        *mk:joint-scale-source* nil
        *mk:vitrage-scales* nil))

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
      (mk:emit (strcat "\n  Область: выборка пакета МАРКАВ, объектов "
                      (itoa (length *mk:run-scope*))))
      *mk:run-scope*)
    (mk:select-scope)))

;; Выборка (или весь чертёж) + рамки всего чертежа + группы по витражам
(defun mk:prepare-run (/ enames frames res mode n shown)
  (mk:group-cache-clear)
  (mk:diag-reset)
  (setq *mk:in-group* nil)   ; состояние предыдущего прогона не наследуем
  ;; режим сбора спрашивается один раз на прогон: в пакете его задаёт МАРКАВ,
  ;; при одиночном запуске этапа — сам mk:prepare-run
  (if (null *mk:last-mode*)
    (progn
      (initget "Блоки Мультилинии Все-типы B M A")
      (setq mode (getkword
                   "\nРежим сбора каркаса [Блоки/Мультилинии/Все-типы] <Все-типы>: "))
      (setq *mk:last-mode* (mk:norm-mode mode))
      (mk:emit (strcat "\n  Режим: " *mk:last-mode*))))
  (setq *mk:scope-mode* (mk:norm-mode *mk:last-mode*))
  (setq enames (mk:run-scope))
  (if (null enames)
    (progn (mk:emit "\n[INFO] Нечего обрабатывать — прогон прерван.") nil)
    (progn
      (setq frames (mk:frame-table))
      (mk:emit (strcat "\n  Рамок витражей в чертеже: " (itoa (length frames))))
      (if (null frames)
        (mk:emit "\n  [WARN] Рамки «Атрибуты витража» не найдены — все объекты вне рамок."))
      (setq res (mk:split-by-vitrage enames frames))
      (setq *mk:last-scope*  enames
            *mk:last-frames* frames
            *mk:last-groups* (car res)
            *mk:last-extra*  (cadr res)
            *mk:last-ss*     (mk:ss-of enames))
      (mk:print-kinds)
      (mk:print-groups *mk:last-groups* *mk:last-extra*)
      ;; Одна рамка — локальная шкала по определению. Для нескольких
      ;; витражей спрашиваем до ЭТАПА 1, чтобы все четыре этапа видели режим.
      (setq *mk:joint-scale* nil *mk:joint-scale-source* nil
            *mk:vitrage-scales* nil)
      (if (> (length *mk:last-groups*) 1)
        (progn
          (initget "Совместная Раздельная С Р")
          (setq mode
            (getkword "\nМаркировка нескольких витражей [Совместная/Раздельная] <Совместная>: "))
          (setq *mk:mark-mode*
                (if (member mode '("Раздельная" "Р")) "separate" "joint"))
          (mk:emit
            (if (= *mk:mark-mode* "joint")
              "\n  [INFO] Совместная маркировка: одна общая шкала MarkZ для всех витражей."
              "\n  [INFO] Раздельная маркировка: шкала независима внутри каждого витража.")))
        (setq *mk:mark-mode* "separate"))
      ;; «Вне типов MarkV» печатается один раз и ОДНИМ списком: подробный
      ;; список (<имя> [DXF-тип]) уже содержит имена вставок, а прежний
      ;; второй список («Вставок вне типов MarkV: N — имена блоков ниже»)
      ;; дублировал его строка в строку. Объекты без блока (SPDSPOLYMORPHMARK,
      ;; POINT) видны в нём как <DXF-тип>.
      (setq n (mk:count-unknown))
      (if (> n 0)
        (progn
          (mk:emit (strcat "\n  [INFO] Вне типов MarkV: " (itoa n)
                          " — что это за объекты (до 10 имён):"))
          (setq shown 0)
          (foreach pr (mk:unknown-names)
            (if (< shown 10)
              (progn
                (mk:emit (strcat "\n    " (car pr) " x" (itoa (cdr pr))))
                (setq shown (1+ shown)))))
          (if (> (length (mk:unknown-names)) shown)
            (mk:emit "\n    ... (остальные — в файле дампа)"))
          (mk:emit (strcat "\n    дамп: " *mk:out-file*
                          "; подробная диагностика — МАРКАВДИАГНОЗОПТОМ ("
                          *mk:diag-all-file* ")"))))
      ;; в пакете подробный дамп дописывает этап 1 (mk:dump-groups)
      (if (and *mk:last-unknown* (null *mk:batch*))
        (progn
          (mk:dump-unknown *mk:last-unknown*)
          (mk:emit (strcat "\n  [OK] Объекты вне типов записаны: " *mk:out-file*))))
      (if (null (car res))
        (progn (mk:emit "\n[WARN] Ни один объект не попал в рамки витражей.") nil)
        res))))

;; Группы для этапа: в пакете — уже собранные, иначе новый прогон
(defun mk:groups-or-prepare (/ res)
  (if (and *mk:batch* *mk:last-groups*)
    (progn (mk:emit "\n  Группы: выборка пакета МАРКАВ") *mk:last-groups*)
    (progn
      (setq res (mk:prepare-run))
      (if res (car res) nil))))

;; Имя группы на чертеже: только буквы/цифры, остальное — «_»
(defun mk:alnum-char? (c / up lo)
  ;; Проверка НЕ по коду символа: в AutoCAD (ascii c) отдаёт код системной
  ;; кодовой страницы, и диапазоны CP1251/Unicode не совпадают — раньше из-за
  ;; этого «ВН-1» превращалось в «___-_». Здесь два независимых признака:
  ;;   * ASCII-цифры и латиница — wcmatch по обычным диапазонам (они надёжны);
  ;;   * любая буква — по смене регистра: strcase меняет только буквы,
  ;;     поэтому (strcase c) и (strcase c T) различаются ровно для буквы.
  (and (mk:strp c) (= (strlen c) 1)
       (or (wcmatch c "[0-9A-Za-z]")
           (= c "-") (= c "_")
           (progn (setq up (strcase c) lo (strcase c T)) (/= up lo)))))

(defun mk:slug (s / i c out solid)
  (setq s (if (mk:strp s) s "") out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq out (strcat out (if (mk:alnum-char? c) c "_")))
    (setq i (1+ i)))
  ;; Страховка: если «твёрдых» символов не осталось (всё стало «_»), имя
  ;; собирается из исходной строки с экранированием только запрещённых
  ;; символов. Лучше рабочее имя, чем «___-_».
  (setq solid nil i 1)
  (if (> (strlen out) 0)
    (progn
      (while (<= i (strlen out))
        (setq c (substr out i 1))
        (if (/= c "_") (setq solid t))
        (setq i (1+ i)))))
  (if (or (null solid) (= out ""))
    (progn
      (setq out "" i 1)
      (while (<= i (strlen s))
        (setq c (substr s i 1))
        (setq out (strcat out (if (wcmatch c "[\\/:*?\"<>|,; ]") "_" c)))
        (setq i (1+ i)))))
  (if (> (strlen out) 0) out "ВИТРАЖ"))

(setq *mk:group-model-base* "Сетка_витража")

;; Имя группы сетки/модели текущего витража
;; (считается от базы: если брать текущее *mk:group-model*, имя накапливало бы
;;  суффиксы предыдущих витражей — Сетка_витража_ВН-1_ВН-2_…)
(defun mk:group-model-name (tag)
  (strcat *mk:group-model-base* "_" (mk:slug tag)))

;; Запись «Атрибуты витража» из рамки (для дампа)

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
  (setq fn *mk:out-file*)
  (setq f (open fn "w"))
  (if (null f)
    (mk:emit (strcat "\n  [WARN] Дамп не создан: " fn))
    (progn
      (write-line "=== ДАННЫЕ ВИТРАЖЕЙ (MarkV, многовитражный прогон) ===" f)
      (write-line (strcat "  " *mk:ver* " / " *mk:build*) f)
      (write-line (strcat "  Чертёж: " (getvar "DWGNAME")) f)
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
      ;; подробный список объектов вне типов — в конец того же файла
      ;; (mk:dump-unknown открывает его на дозапись)
      (if *mk:last-unknown* (mk:dump-unknown *mk:last-unknown*))
      (mk:emit (strcat "\n  [OK] Дамп по витражам: " fn
                      (if *mk:last-unknown* " (витражи + объекты вне типов)" ""))))))

;;;--- ЭТАП 1/4. МАРКАВГЕОМЕТРИЯ ------------------------------------

;; Один витраж: сбор -> проверки -> топология -> разметка -> 2D модель.
;; Возвращает (DATA POSTS BEAMS FILLS WINDOWS DOORS BOUNDS DUP OVERLAP OUTJ).
;; Вынесено отдельной функцией, чтобы сбой одного витража не прерывал прогон
;; (МАРКАВГЕОМЕТРИЯ вызывает её через vl-catch-all-apply).
(defun mk:process-group (grp / data posts beams fills windows doors bounds
                             all-panels dup overlap outj topo res2)
  (setq *mk:step* "сбор объектов витража")
  (setq data (mk:group-data grp))
  (setq *mk:step* "данные витража")
  (setq posts   (cdr (assoc 'POSTS data))
        beams   (cdr (assoc 'BEAMS data))
        fills   (cdr (assoc 'FILLS data))
        windows (cdr (assoc 'WINDOWS data))
        doors   (cdr (assoc 'DOORS data)))
  (mk:emit (strcat "\n  Стойки: " (itoa (length posts))
                  ", ригели: " (itoa (length beams))
                  ", заполнения: " (itoa (length fills))
                  ", окна: " (itoa (length windows))
                  ", двери: " (itoa (length doors))))
  (if (or posts beams fills)
    (progn
      (setq bounds (mk:find-vitrage-bounds posts beams))
      (mk:emit (strcat "\n  Границы: X=" (mk:safe-num (cdr (assoc 'MIN_X bounds)))
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
      (setq *mk:step* "топология, стыки")
      (setq topo (mk:build-topology posts beams fills windows doors bounds))
      (setq posts   (nth 0 topo) beams (nth 1 topo) fills (nth 2 topo)
            windows (nth 3 topo) doors (nth 4 topo))
      (setq all-panels (append fills windows doors))
      (setq *mk:step* "марки стоек")
      (setq posts (mk:annotate-posts posts (append all-panels beams)))
      (setq *mk:step* "марки ригелей")
      (setq beams (mk:annotate-beams beams all-panels doors bounds posts))
      ;; 2D модель витража — своя группа
      (setq *mk:step* "2D модель и группа")
      (mk:draw-model posts beams fills windows doors bounds)
      (setq *mk:step* "итог по витражу")
      (mk:emit (strcat "\n  Дубликатов: " (itoa dup)
                      ", перекрытий: " (itoa overlap)
                      ", вне сетки: " (itoa outj))))
    (progn
      ;; витраж без каркаса не должен ронять своё имя в лог как «сбой»
      (setq bounds (list (cons 'MIN_X nil) (cons 'MAX_X nil)
                         (cons 'MIN_Y nil) (cons 'MAX_Y nil))
            dup 0 overlap 0 outj 0)
      (mk:emit "\n  [WARN] В витраже нет элементов каркаса и заполнений.")))
  (list data posts beams fills windows doors bounds dup overlap outj))

;; Этап пакета под защитой: сбой фиксируется в *mk:run-errors* и печатается,
;; следующие этапы при этом выполняются (уборка сетки и ведомость — тоже).
;; Этап по имени команды. Имя передаётся СТРОКОЙ и превращается в символ
;; через (read) — в исходнике нет литерала-символа с кириллицей.
;; Причина: CHKALL пропускает не-ANSI слова только по маске «c:*», а знак
;; апострофа в литерал 'c:МАРКАВ… попадает в слово и маску ломает: строка
;; числится как «не-ANSI символ вне строки/комментария», хотя код верный.
(defun mk:stage-sym (cmd)
  (if (mk:strp cmd) (read (strcat "c:" cmd)) nil))

(defun mk:run-stage (no name sym / r)
  (mk:emit (strcat "\n\n===== [" no "] " name " ====="))
  (setq r (vl-catch-all-apply sym nil))
  (if (vl-catch-all-error-p r)
    (progn
      (mk:emit (strcat "\n[ERROR] " name ": сбой — "
                      (vl-catch-all-error-message r)))
      (mk:emit "\n[INFO] Переход к следующему этапу.")
      (setq *mk:run-errors* (cons (list name (vl-catch-all-error-message r))
                                 *mk:run-errors*))
      nil)
    r))

;; Итог по сбоям прогона: витражи и этапы пакета (нет сбоев — молчит)
(defun mk:print-run-errors (/ n e)
  (setq n 0)
  (foreach e *mk:run-errors* (if (cadr e) (setq n (1+ n))))
  (if (> n 0)
    (progn
      (mk:emit (strcat "\n[WARN] Сбоев в прогоне: " (itoa n) " из "
                      (itoa (length *mk:run-errors*))))
      (foreach e *mk:run-errors*
        (if (cadr e)
          (mk:emit (strcat "\n   " (car e) ": " (cadr e))))))))

(defun c:МАРКАВГЕОМЕТРИЯ (/ res groups grp data posts beams fills windows doors
                            dup overlap outj r err)
  (mk:emit "\n[МАРКАВГЕОМЕТРИЯ] Сбор данных и 2D модель — по каждому витражу отдельно...")
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)
  (setq *mk:cached-data* nil *mk:geo-summary* nil *mk:run-errors* nil)
  (if (setq res (mk:prepare-run))
    (progn
      (setq groups (car res))
      (foreach grp groups
        (setq *mk:last-prefix* (nth 0 grp))
        (setq *mk:group-model* (mk:group-model-name (nth 0 grp)))
        (setq *mk:scope-mode*  *mk:last-mode*)
        (setq *mk:step* nil)
        (mk:emit (strcat "\n\n--- Витраж " (nth 0 grp) " ---"))
        ;; один витраж не должен ронять весь прогон: сбой фиксируется и
        ;; прогон идёт дальше, остальные витражи обрабатываются
        (setq err nil)
        (setq r (vl-catch-all-apply 'mk:process-group (list grp)))
        (if (vl-catch-all-error-p r)
          (progn
            (setq err (vl-catch-all-error-message r))
            (mk:emit (strcat "\n  [ERROR] Витраж " (nth 0 grp)
                            ": сбой обработки — " err
                            (if *mk:step* (strcat " | шаг: " *mk:step*) "")))
            (setq err (strcat err (if *mk:step*
                                    (strcat " | шаг: " *mk:step*) "")))
            (if (and *mk:debug-stack* posts)
              (progn
                (mk:emit "\n  [DEBUG] Собрано до сбоя:")
                (mk:emit (strcat "\n    стойки " (itoa (length posts))
                                ", ригели " (itoa (length beams))
                                ", заполнения " (itoa (length fills))))
                (mk:emit (strcat "\n    первые X-координаты стоек: "
                                (mk:first-coords posts)))))
            (mk:emit "\n  [INFO] Прогон продолжается со следующего витража."))
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
      (mk:emit (strcat "\n[ГОТОВО] Витражей обработано: "
                      (itoa (length groups))
                      ". Модели: слой «" *mk:layer-model* "», группы «"
                      *mk:group-model-base* "_<витраж>».")))
    (mk:emit "\n[INFO] Сбор данных не выполнен."))
  (princ))

;;;--- ЭТАП 2/4. МАРКАВСТ --------------------------------------------

(defun c:МАРКАВСТ (/ groups grp plan item res n cnt sk lab doc tag tot data npost)
  (mk:emit "\n[МАРКАВСТ] Марки стоек по витражам...")
  (setq *mk:mark-map* nil)
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil)
  (if (setq groups (mk:groups-or-prepare))
    (progn
      (setq doc (mk:undo-begin) tot 0 sk 0 lab 0)
      (foreach grp groups
        (setq tag  (nth 0 grp)
              data (mk:group-data grp)
              npost (length (cdr (assoc 'POSTS data)))
              plan (mk:group-plan-posts grp))
        (if plan (mk:mark-map-put plan))
        (if (null plan)
          (mk:emit (strcat "\n  " (mk:pad tag 10) " [INFO] стоек нет"))
          (progn
            (setq *mk:last-prefix* tag cnt 0)
            (mk:emit (strcat "\n  " (mk:pad tag 10)
                            " стоек: " (itoa npost)
                            ", марок: " (itoa (length plan))))
            (foreach item plan
              (setq res (mk:write-marks (cadr item) (car item)))
              (mk:emit (strcat "\n    " (mk:pad (car item) 18)
                              " шт.: " (mk:padl (itoa (length (cadr item))) 3)))
              (setq cnt (+ cnt (car res))
                    sk  (+ sk  (cadr res))
                    lab (+ lab (caddr res))
                    tot (+ tot (- (length (cadr item)) (cadr res)))))
            ;; выноски этого витража — в свою группу
            (if (> (mk:group-labels-tag tag) 0)
              (mk:emit (strcat "\n    выноски: группа «Маркировка_"
                              (mk:slug tag) "»"))))))
      (mk:undo-end doc)
      (mk:emit (strcat "\n[ГОТОВО] Стоек промаркировано: " (itoa tot)
                      ", выносок: " (itoa lab) ", пропущено: " (itoa sk))))
    (mk:emit "\n[INFO] Группы не собраны — маркировка стоек не выполнена."))
  (princ))

;;;--- ЭТАП 3/4. МАРКАВРГ --------------------------------------------

(defun c:МАРКАВРГ (/ groups grp plan item res n cnt sk lab doc tag tot data nbeam)
  (mk:emit "\n[МАРКАВРГ] Марки ригелей по витражам...")
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
        (if plan (mk:mark-map-put plan))
        (if (null plan)
          (mk:emit (strcat "\n  " (mk:pad tag 10) " [INFO] ригелей нет"))
          (progn
            (setq *mk:last-prefix* tag cnt 0)
            (mk:emit (strcat "\n  " (mk:pad tag 10)
                            " ригелей: " (itoa nbeam)
                            ", марок: " (itoa (length plan))))
            (foreach item plan
              (setq res (mk:write-marks (cadr item) (car item)))
              (mk:emit (strcat "\n    " (mk:pad (car item) 18)
                              " шт.: " (mk:padl (itoa (length (cadr item))) 3)))
              (setq cnt (+ cnt (car res))
                    sk  (+ sk  (cadr res))
                    lab (+ lab (caddr res))
                    tot (+ tot (- (length (cadr item)) (cadr res)))))
            (if (> (mk:group-labels-tag tag) 0)
              (mk:emit (strcat "\n    выноски: группа «Маркировка_"
                              (mk:slug tag) "»"))))))
      (mk:undo-end doc)
      (mk:emit (strcat "\n[ГОТОВО] Ригелей промаркировано: " (itoa tot)
                      ", выносок: " (itoa lab) ", пропущено: " (itoa sk))))
    (mk:emit "\n[INFO] Группы не собраны — маркировка ригелей не выполнена."))
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
                      doc do-tbl do-xls file res pt1 pt2 pt3 h h2 tbl2 suffix)
  (mk:emit "\n[МАРКАВТАБЛ] Ведомость профилей — общая по всем витражам...")
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
      (mk:emit (strcat "\n  Стойки: " (itoa (length all-posts))
                      ", ригели: " (itoa (length all-beams))
                      ", витражей: " (itoa (length groups))))
      (mk:emit (strcat "\n  Уникальных позиций: " (itoa (length agg))))
      (setq acc (mk:acc-rows (append all-posts all-beams)))
      (if acc
        (mk:emit (strcat "\n  Закладных (позиций): " (itoa (length acc))))
        (mk:emit "\n  Закладные (Пятка/Кронштейн) не найдены."))
      (setq *mk:leg-sfx*  (mk:leg-suffixes agg)
            *mk:leg-arts* (mk:leg-articles all-beams))
      (mk:leg-print *mk:leg-sfx* *mk:leg-arts*)
      ;; В пакете МАРКАВ этап 4 идёт через vl-catch-all-apply: getkword/getpoint
      ;; в этот момент ввод не принимают, и команда выглядит «зависшей».
      ;; Поэтому в пакете обе таблицы и XLS строятся без вопросов, а точка
      ;; вставки берётся из *mk:table-ins-pt* (её посчитал МАРКАВ).
      ;; Одиночная МАРКАВТАБЛ по-прежнему спрашивает.
      (if *mk:batch*
        (setq do-tbl t do-xls t)
        (progn
          (initget "Да Нет")
          (setq do-tbl (getkword "\nТаблица в чертеже? [Да/Нет] <Да>: "))
          (setq do-tbl (if (= do-tbl "Нет") nil t))
          (initget "Да Нет")
          (setq do-xls (getkword "\nЭкспорт в XLS? [Да/Нет] <Да>: "))
          (setq do-xls (if (= do-xls "Нет") nil t))))
      (setq suffix (if (= (length groups) 1)
                     (nth 0 (car groups))
                     (strcat (itoa (length groups)) " витражей")))
      (setq doc (mk:undo-begin))
      ;; таблицы прошлого прогона — до вставки: иначе ведомость ляжет поверх
      (if do-tbl
        (progn
          (setq res (mk:clear-old-tables))
          (if (> res 0)
            (mk:emit (strcat "\n  [INFO] Таблиц прошлого прогона убрано: " (itoa res))))))
      (if do-xls
        (progn
          (setq file (strcat (getvar "dwgprefix")
                             (vl-filename-base (getvar "dwgname"))
                             " Стойки ригели витража "
                             (mk:slug suffix) ".xls"))
          (if (null (mk:tab-xls agg file acc))
            (mk:emit (strcat "\n  [WARN] Ведомость XLS не сохранена: " file)))))
      (if do-tbl
        (progn
          (setq pt1 (if *mk:batch* *mk:table-ins-pt* nil))
          (setq res (mk:tab-draw-all agg pt1))
          ;; Закладные («пятки») и условные обозначения ставятся СБОКУ — справа
          ;; от ведомости, столбиком (обозначения ниже закладных).
          ;; Раньше они шли под ведомостью и при большом числе таблиц
          ;; наезжали на неё.
          (if res
            (progn
              ;; (cadr res) — верх последней таблицы ведомости, но нужна точка
              ;; НАЧАЛА ведомости (там её верхний край): pt1, а если точку
              ;; не задавали — (cadr res), как было раньше.
              (setq pt2 (mk:tab-right-point (if pt1 pt1 (cadr res))
                                            *mk:acc-gap*))
              (mk:emit (strcat "\n  Закладные и обозначения: справа от ведомости, X="
                              (rtos (car pt2) 2 1) ", Y=" (rtos (cadr pt2) 2 1)))
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
      (mk:emit "\n[ГОТОВО] Ведомость сформирована (общая по всем витражам)."))
    (mk:emit "\n[INFO] Данных нет — ведомость не создана."))
  (princ))

;;;--- ПАКЕТ. МАРКАВ --------------------------------------------------
;;   Одна выборка -> по каждому витражу: сбор и модель, стойки, ригели ->
;;   удаление сеток -> одна общая ведомость. Один UNDO на весь пакет.

(defun mk:a-all (preset / ss keep doc n r pt0)
  (mk:log-start)
  (if (not *mk:cycle-tables-cleared*) (mc:remark-reset))
  (mk:emit (strcat "\n[МАРКАВ] Пакетный прогон, " *mk:ver* " / " *mk:build*))
  (mk:emit "\n  Порядок: выборка -> витражи -> (сбор, стойки, ригели) по каждому -> ведомость.")
  (mk:emit (strcat "\n  [LOG] Протокол: " (mk:log-path)))
  (if *mk:log-failed*
    (mk:emit "\n  [WARN] Файл протокола недоступен; сообщения только в командной строке."))
  (setq *mk:batch* nil *mk:batch-ss* nil *mk:batch-mode* nil
        *mk:last-groups* nil *mk:last-extra* nil *mk:last-frames* nil
        *mk:mark-mode* nil *mk:joint-scale* nil *mk:joint-scale-source* nil
        *mk:vitrage-scales* nil *mk:current-scale-source* nil)
  (mk:diag-reset)
  (setq *mk:dyn-cache* nil *mk:vitrage-cache* nil *mk:no-vitrage-prompted* nil
        *mk:manual-vitrage-prefix* nil *mk:allowance-unknown* nil)
  (setq ss (if preset preset (mk:select-scope)))
  (if (null ss)
    (mk:emit "\n[INFO] Ничего не выбрано — пакет прерван.")
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
      (setq *mk:batch* t *mk:run-errors* nil)
      ;; каждый этап под vl-catch-all-apply: раньше сбой на этапе 1 обрывал
      ;; МАРКАВ целиком — сетки моделей оставались в чертеже, ведомость
      ;; не строилась, а UNDO-транзакция не закрывалась
      (setq r (mk:run-stage "ЭТАП 1/4" "МАРКАВГЕОМЕТРИЯ"
                            (mk:stage-sym "МАРКАВГЕОМЕТРИЯ")))
      (setq r (mk:run-stage "ЭТАП 2/4" "МАРКАВСТ" (mk:stage-sym "МАРКАВСТ")))
      (setq r (mk:run-stage "ЭТАП 3/4" "МАРКАВРГ" (mk:stage-sym "МАРКАВРГ")))
      (if *mk:batch-keep-grid*
        (mk:emit (strcat "\n\n  [INFO] Сетки витражей оставлены на слое «"
                        *mk:layer-model* "» (группы «" *mk:group-model-base*
                        "_<витраж>»)."))
        (progn
          (mk:emit "\n\n  Удаление сеток витражей перед вставкой таблицы...")
          (setq n (mk:erase-all-models))
          (mk:emit (strcat "\n  [OK] Сетка удалена, объектов: " (itoa n)))))
      ;; точка вставки ведомости фиксируется один раз за пакет:
      ;; по умолчанию спрашивается, при *mk:ask-table-pt* = nil — считается сама
      (setq pt0 (mk:auto-table-point))
      (if *mk:ask-table-pt*
        (progn
          (setq *mk:table-ins-pt* nil)   ; точку прошлого прогона не берём
          (setq r (getpoint (strcat "\n  Точка вставки ведомости <Enter — X="
                                    (if pt0 (rtos (car pt0) 2 1) "?") ", Y="
                                    (if pt0 (rtos (cadr pt0) 2 1) "?") ">: ")))
          (if r (setq pt0 r))))
      (if pt0 (setq *mk:table-ins-pt* pt0))
      (if *mk:table-ins-pt*
        (mk:emit (strcat "\n  Ведомость: точка вставки X="
                        (rtos (car *mk:table-ins-pt*) 2 1)
                        ", Y=" (rtos (cadr *mk:table-ins-pt*) 2 1)))
        (mk:emit "\n\n  [WARN] Точка вставки ведомости не задана — таблицы пропущены."))
      (mk:run-stage "ЭТАП 4/4" "МАРКАВТАБЛ" (mk:stage-sym "МАРКАВТАБЛ"))
      (setq *mk:batch* nil *mk:batch-ss* nil *mk:run-scope* nil)
      (setq *mk:run-errors* (reverse *mk:run-errors*))
      (mk:print-run-errors)
      (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "EndUndoMark")))
      (setq n (length *mk:last-groups*))
      (mk:emit (strcat "\n\n[ГОТОВО] Пакет МАРКАВ завершён: витражей "
                      (itoa n) ", объектов вне рамок "
                      (itoa (length *mk:last-extra*)) "."))
      (mk:emit "\n  Откат всего пакета — один U.")))
  (mk:emit (strcat "\n  [LOG] Протокол: " (mk:log-path)))
  (if *mk:log-failed*
    (mk:emit "\n  [WARN] Файл протокола недоступен; сообщения только в командной строке."))
  (setq *mk:batch* nil)
  (princ))

;;;--- ПАКЕТ: тонкая обёртка команды -----------------------------------
(defun c:МАРКАВ () (mk:a-all nil))
;;;=====================================================================
 scale diagnostic
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



(defun c:MARKVBULKDIAG (/ ss f i e d typ counts styles blocks layers x)
  (mk:emit "\n[МАРКАВДИАГНОЗОПТОМ] Массовая диагностика выбранных объектов...")
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
      (mk:emit (strcat "\n[OK] Отчёт: " *mk:diag-all-file*)))
    (mk:emit "\n[INFO] Объекты не выбраны."))
  (princ))

(defun c:МАРКАВДИАГНОЗОПТОМ () (c:MARKVBULKDIAG))

;;;=====================================================================
;;; 21. ИНИЦИАЛИЗАЦИЯ С АВТОПРОВЕРКОЙ
;;;=====================================================================

;; (прежние mk:local-kind / mk:local-less удалены: не вызывались ниоткуда,
;;  а mk:local-kind к тому же опирался на кириллические маски в wcmatch —
;;  такой разбор ненадёжен, тип объекта даёт mk:obj-kind)


;;;=====================================================================
;;;  ЕДИНЫЙ ЦИКЛ: МАРКАВСЕ / MARKALL
;;;  Один выбор, один внешний UNDO; внутри — обычные пакеты модулей.
;;;=====================================================================
(setq *mark:cycle-ss* nil)
(setq *mk:cycle-tables-cleared* nil)

;; Набор SSGET -> список ename (MarkV ждёт список).
(defun mark:ss->enames (ss / i out)
  (setq i 0 out nil)
  (if ss
    (repeat (sslength ss)
      (setq out (cons (ssname ss i) out)
            i   (1+ i))))
  (reverse out))

;; Ближайшая длина ригеля из *mark:scale1* в пределах допуска.
(defun mark:nearest-rigel (w tol / best bd d)
  (setq best nil bd nil)
  (foreach s *mark:scale1*
    (if (numberp s)
      (progn
        (setq d (abs (- (float w) (float s))))
        (if (and (<= d tol) (or (null bd) (< d bd)))
          (setq bd d best s)))))
  best)

;; После Т-половин: одиночную ячейку, чья ширина не в шкале ригелей,
;; тянем к торцу ближайшего ригеля (край стойки 5 мм). 195 -> 200.
(defun mark:fill-single-rail-cells (boxes hs / out bb x0 y0 x1 y1 secw tierh
                                         bw L r e ey hw)
  (setq out nil)
  (foreach bb boxes
    (setq x0    (nth 0 bb)
          y0    (nth 1 bb)
          x1    (nth 2 bb)
          y1    (nth 3 bb)
          secw  (nth 4 bb)
          tierh (nth 5 bb)
          bw    (mark:round1 (- (float x1) (float x0)))
          L     nil)
    (if (and (numberp bw) (> bw 0.0)
             (not (mark:index-of *mark:scale1* bw)))
      (setq L (mark:nearest-rigel bw 5.5)))
    (if L
      (progn
        (setq r nil)
        (foreach e hs
          (if (and (null r)
                   (numberp (nth 0 e)) (numberp (nth 1 e)) (numberp (nth 2 e))
                   (<= (abs (- (- (float (nth 2 e)) (float (nth 1 e)))
                               (float L))) 0.6)
                   (<= (float (nth 1 e)) (+ (float x0) 5.6))
                   (>= (float (nth 2 e)) (- (float x1) 5.6)))
            (progn
              (setq ey (float (nth 0 e))
                    hw (if (numberp (nth 3 e)) (float (nth 3 e)) 0.0))
              (if (or (<= (abs (- ey (float y0))) 0.6)
                      (<= (abs (- ey (float y1))) 0.6)
                      (and (> hw 0.0)
                           (or (<= (abs (- (+ ey hw) (float y0))) 0.6)
                               (<= (abs (- (- ey hw) (float y1))) 0.6))))
                (setq r e)))))
        (if r
          (setq bb (list (float (nth 1 r)) y0 (float (nth 2 r)) y1 L tierh)))))
    (setq out (cons bb out)))
  (reverse out))

;; В AutoLISP `or` возвращает T/nil, а не значение: выбираем группы явно.
(defun mark:vz-split-frame-first (fills glazings / r)
  (setq r (mark:vz-frame-split fills glazings))
  (if r r (mark:vz-split-geometric fills glazings)))

;; Префикс каждого заполнения — по его собственной рамке. Приоритет у точки
;; вставки (она всегда внутри своего витража), затем площадь пересечения,
;; затем ближайшая рамка. Это разделяет витражи через общую стойку.
(defun mark:vz-map-build-by-frames
  (fills glazings / frames map e pt bb best besta ar bd d hits)
  (setq frames (mark:vz-frame-list glazings)
        map    nil)
  (foreach e fills
    (setq pt   (cdr (assoc 10 (entget e)))
          hits nil)
    (foreach fr frames
      (if (and pt
               (<= (nth 0 (cadr fr)) (car pt))
               (<= (car pt) (nth 2 (cadr fr)))
               (<= (nth 1 (cadr fr)) (cadr pt))
               (<= (cadr pt) (nth 3 (cadr fr))))
        (setq hits (cons fr hits))))
    (setq best nil besta 0.0)
    (if hits
      (progn
        (setq bb (mark:vz-bb-of e))
        (foreach fr hits
          (setq ar (mark:vz-overlap-area bb (cadr fr)))
          (if (> ar besta) (setq besta ar best fr)))
        (if (null best) (setq best (car hits))))
      (progn
        (setq bb (mark:vz-bb-of e))
        (foreach fr frames
          (setq ar (mark:vz-overlap-area bb (cadr fr)))
          (if (> ar besta) (setq besta ar best fr)))
        (if (null best)
          (progn
            (setq bd nil)
            (foreach fr frames
              (setq d (mark:vz-dist pt (cadr fr)))
              (if (or (null bd) (< d bd)) (setq bd d best fr)))))))
    (if best
      (setq map (cons (cons e (caddr best)) map))))
  (reverse map))


;; ---- локальная шкала MarkZ в раздельном режиме ------------------------
;; Геометрия ячеек общая, но длины ригелей — только текущего витража.
(defun mark:sec-local-scale (/ ss i e ed typ nm r msegs rblocks)
  (setq ss (if *mark:carc-cache* (mark:carc-ss) *mark:cycle-ss*))
  (if ss
    (progn
      (setq ss (mark:vz-carcass-filter ss)
            msegs nil rblocks nil)
      (if ss
        (progn
          (setq i (sslength ss))
          (while (> i 0)
            (setq i (1- i)
                  e (ssname ss i)
                  ed (entget e)
                  typ (if ed (cdr (assoc 0 ed)) nil))
            (cond
              ((= typ "MLINE")
               (setq r (vl-catch-all-apply 'mark:fill-extract-segs (list e)))
               (if (and r (not (vl-catch-all-error-p r)))
                 (setq msegs (append r msegs))))
              ((= typ "INSERT")
               (setq nm (mark:fill-eff-name e))
               (if (and (mark:strp nm)
                        (wcmatch (strcase nm) "*РИГЕЛЬ*"))
                 (setq rblocks (cons e rblocks))))))
          (setq *mark:scale1* (mark:sec-scale1 msegs rblocks))))))
  *mark:scale1*)

;; ---- счётчики для отчёта ---------------------------------------------
(setq *mark:run-written* nil)
(setq *mark:run-stem* nil)
(setq *mark:run-sand* nil)
(setq *mark:written-total* nil)
(setq *mark:stem-total* nil)
(setq *mark:sand-total* nil)

(defun mark:save-run-counters ()
  (setq *mark:run-written* (if (numberp *mark:written-total*)
                              *mark:written-total*
                              *mark:written*)
        *mark:run-stem*    (if (numberp *mark:stem-total*)
                              *mark:stem-total*
                              *mark:cnt-stem*)
        *mark:run-sand*    (if (numberp *mark:sand-total*)
                              *mark:sand-total*
                              *mark:cnt-sand*))
  nil)

;; ---- спецмарки Дв/Ок/Вр (только заполнениям) --------------------------
(setq *mark:special-dv*   0)
(setq *mark:special-ok*   0)
(setq *mark:special-vr*   0)
(setq *mark:special-left* 0)
(setq *mark:special-enames* nil)
(setq *mark:vent-masks*   '("*ВЕНТРЕШ*" "*ВЕНТ*"))
(setq *mark:zone-list*    nil)

(defun mark:special-mark? (m / s n tail)
  (setq s (mark:trim (if (mark:strp m) m ""))
        n (strlen s))
  ;; Проверяем ровно последние два символа. В CP1251 кириллица однобайтовая,
  ;; а wcmatch со звёздочкой в некоторых сборках не ловит кириллицу.
  (setq tail (if (> n 1) (substr s (- n 1) 2) s))
  (member tail '("Дв" "Ок" "Вр")))

(defun mark:zone-kind (name / nm)
  (setq nm (strcase (if (mark:strp name) name "")))
  (cond
    ((wcmatch nm "*ДВЕР*") "Дв")
    ((or (wcmatch nm "*ОКН*") (wcmatch nm "*СТВОР*")) "Ок")
    (t nil)))

(defun mark:opening-zones (/ doors wins kind)
  (if (not *mark:opening-loaded*) (mark:fill-load-openings))
  (setq doors nil wins nil)
  (foreach rec *mark:opening-bbs*
    (setq kind (mark:zone-kind (car rec)))
    (if (cadr rec)
      (cond
        ((= kind "Дв") (setq doors (cons (list "Дв" (cadr rec)) doors)))
        ((= kind "Ок") (setq wins  (cons (list "Ок" (cadr rec)) wins))))))
  (append doors wins))

(defun mark:vent-name? (nm)
  (or (vl-string-search "ВЕНТ" nm)
      (vl-string-search "вент" nm)
      (vl-string-search "Вент" nm)))

(defun mark:vent-zones (/ ss i e ed nm bb hit out)
  (setq out nil i 0)
  ;; Быстрый фильтр по имени блока: не перебираем все INSERT чертежа.
  (setq ss (ssget "_X" '((0 . "INSERT") (2 . "*ВЕНТ*"))))
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i) i (1+ i)
            ed (entget e)
            nm (if ed (mark:fill-eff-name e) nil)
            hit nil)
      (if (mark:strp nm)
        (progn
          (if (mark:vent-name? nm) (setq hit t))
          (foreach m *mark:vent-masks*
            (if (and (null hit) (wcmatch (strcase nm) (strcase m)))
              (setq hit t)))))
      (if hit
        (progn
          (setq bb (mark:fill-vla-bb e))
          (if bb (setq out (cons (list "Вр" bb) out)))))))
  out)

(defun mark:prefix-at (e / p)
  (setq p (cdr (assoc e *mark:vz-map*)))
  (if p p
    (if *mark:vz-current-frame*
      (mark:vz-pfx-of (list *mark:vz-current-frame*))
      *mark:prefix*)))

(defun mark:special-outside (/ zones cands obj e ins bb best besta ar kind pfx txt n)
  (setq *mark:special-left* 0)
  (setq cands nil)
  (foreach e *mark:fills*
    (if (not (assoc e *mark:fill-geo*)) (setq cands (cons e cands))))
  (setq cands (reverse cands)
        zones (append (mark:opening-zones) (mark:vent-zones)))
  (setq *mark:zone-list* zones)
  (foreach e cands
    (setq obj (mark:vla e)
          ins (cdr (assoc 10 (entget e)))
          bb  (mark:fill-e-bb e)
          best nil besta 0.0)
    (if (and obj ins bb)
      (progn
        (foreach z zones
          (setq ar (mark:vz-overlap-area bb (cadr z)))
          (if (> ar besta) (setq besta ar best z)))
        (if best
          (progn
            (setq kind (car best)
                  pfx  (mark:prefix-at e))
            (if (not (mark:strp pfx)) (setq pfx ""))
            (setq txt (if (= pfx "") kind (strcat pfx " " kind)))
            (if (mark:ar-set-attr obj *mark:attr-mark* txt)
              (progn
                (if (/= pfx "")
                  (mark:ar-set-attr obj *mark:attr-vitrage* pfx))
                (mark:ar-set-attr obj *mark:ar-attr* "")
                (mc:remark-drop "G01" e)
                (mc:remark (cond ((= kind "Дв") "D01")
                                 ((= kind "Ок") "O01")
                                 (t "R01"))
                           "INFO" (strcat "марка " txt)
                           e (car ins) (cadr ins))
                (setq *mark:special-enames*
                      (cons e *mark:special-enames*))
                (cond
                  ((= kind "Дв") (setq *mark:special-dv* (1+ *mark:special-dv*)))
                  ((= kind "Ок") (setq *mark:special-ok* (1+ *mark:special-ok*)))
                  (t (setq *mark:special-vr* (1+ *mark:special-vr*)))))))))))
  (setq n (length cands)
        *mark:special-left*
        (- n (+ *mark:special-dv* *mark:special-ok* *mark:special-vr*)))
  (if (> n 0)
    (progn
      (mark:out
        (strcat "[INFO] Вне ячеек: " (itoa n)
                " — Дв " (itoa *mark:special-dv*)
                ", Ок " (itoa *mark:special-ok*)
                ", Вр " (itoa *mark:special-vr*)))
      (if (> *mark:special-left* 0)
        (progn
          (mark:out
            (strcat "[WARN] Вне ячеек и без зоны проёма: "
                    (itoa *mark:special-left*)))
          (mark:note-warning)))))
  nil)

;; ---- отчёт ------------------------------------------------------------
(defun mark:report-path ()
  (mc:log-path
    (strcat (vl-filename-base (getvar "dwgname")) " Отчёт.txt")))

(defun mark:report-stats (/ out d posts beams z dv ok vr ngeo grp
                            written stem sand)
  (setq out nil posts 0 beams 0)
  (setq grp (if *mk:last-groups*
              (mapcar 'car *mk:last-groups*)
              (mark:vz-map-pfxs)))
  (if (and (null grp) (mark:strp *mark:prefix*) (/= *mark:prefix* ""))
    (setq grp (list *mark:prefix*)))
  (setq out (list (list "Витражи"
                        (if grp (mark:vz-join grp ", ") "—"))))
  (if *mk:last-groups*
    (progn
      (foreach g *mk:last-groups*
        (setq d (mk:group-data g))
        (setq posts (+ posts (length (cdr (assoc 'POSTS d))))
              beams (+ beams (length (cdr (assoc 'BEAMS d))))))
      (setq out (append out
                  (list (list "Стойки" (itoa posts))
                        (list "Ригели" (itoa beams)))))
      (foreach r *mk:last-acc*
        (setq out (append out
                    (list (list (strcat (if (mark:strp (nth 0 r)) (nth 0 r) "?")
                                        (if (and (mark:strp (nth 1 r))
                                                 (> (strlen (nth 1 r)) 0))
                                          (strcat " " (nth 1 r)) ""))
                                (itoa (nth 2 r)))))))))
  (setq z (if *mark:zone-list*
            *mark:zone-list*
            (append (mark:opening-zones) (mark:vent-zones)))
        dv 0 ok 0 vr 0)
  (setq *mark:zone-list* z)
  (foreach x z
    (cond
      ((= (car x) "Дв") (setq dv (1+ dv)))
      ((= (car x) "Ок") (setq ok (1+ ok)))
      (t (setq vr (1+ vr)))))
  (setq ngeo (if *mark:fill-geo* (length *mark:fill-geo*) 0))
  (setq written (if (numberp *mark:run-written*) *mark:run-written*
                    (if (numberp *mark:written*) *mark:written* 0))
        stem    (if (numberp *mark:run-stem*) *mark:run-stem*
                    (if (numberp *mark:cnt-stem*) *mark:cnt-stem* 0))
        sand    (if (numberp *mark:run-sand*) *mark:run-sand*
                    (if (numberp *mark:cnt-sand*) *mark:cnt-sand* 0)))
  (setq out (append out
    (list (list "Заполнения" (itoa (length *mark:fills*)))
          (list "  в ячейках" (itoa ngeo))
          (list "  вне ячеек" (itoa (max 0 (- (length *mark:fills*) ngeo))))
          (list "Двери" (itoa dv))
          (list "Окна" (itoa ok))
          (list "Вентрешётки" (itoa vr))
          (list "Спецмарки Дв" (itoa *mark:special-dv*))
          (list "Спецмарки Ок" (itoa *mark:special-ok*))
          (list "Спецмарки Вр" (itoa *mark:special-vr*))
          (list "Стемалит" (itoa stem))
          (list "Сэндвич" (itoa sand))
          (list "Марок записано"
                (itoa (+ written *mark:special-dv*
                         *mark:special-ok* *mark:special-vr*)))
          (list "Ошибки" (itoa (mc:remark-count-sev "ERROR")))
          (list "Предупреждения" (itoa (mc:remark-count-sev "WARN"))))))
  out)

(defun mark:report-write-lines (path lines / f)
  (setq f (vl-catch-all-apply 'open (list path "w")))
  (if (or (null f) (vl-catch-all-error-p f))
    nil
    (progn
      (foreach l lines (write-line l f))
      (close f)
      t)))

(defun mark:rec-handle (e / d)
  (setq d (if e (entget e) nil))
  (if (and d (cdr (assoc 5 d))) (cdr (assoc 5 d)) "—"))

(defun mark:report-lines (/ out r n codes c)
  (setq out (list (strcat "=== ОТЧЁТ ПРОГОНА: " (getvar "DWGNAME") " ===")
                  (strcat "MarkZ " *mark:rev* " / " *mark:build*
                          " + MarkV " *mk:ver* " / " *mk:build*)
                  ""))
  (foreach r (mark:report-stats)
    (setq out (append out (list (strcat (car r) "; " (cadr r))))))
  (setq codes nil)
  (foreach r *mc:remarks*
    (if (not (member (car r) codes)) (setq codes (cons (car r) codes))))
  (setq out (append out
              (list "" "ИТОГИ ПО КОДАМ" "Код;Количество;Пояснение")))
  (foreach c (reverse codes)
    (setq out (append out
                (list (strcat c ";"
                              (itoa (mc:remark-count c)) ";"
                              (if (mc:remark-code-name c)
                                (mc:remark-code-name c) ""))))))
  (setq out (append out
              (list "" "ЗАМЕЧАНИЯ"
                    "№;Код;Важность;Объект;X;Y;Причина")))
  (setq n 0)
  (foreach r (reverse *mc:remarks*)
    (setq n (1+ n))
    (setq out (append out
                (list (strcat (itoa n) ";"
                              (car r) ";"
                              (cadr r) ";"
                              (mark:rec-handle (nth 3 r))
                              ";"
                              (if (and (numberp (nth 4 r)) (numberp (nth 5 r)))
                                (strcat (rtos (nth 4 r) 2 0) ","
                                        (rtos (nth 5 r) 2 0))
                                "") ";"
                              (nth 2 r))))))
  out)

(defun mark:report-write (/ path)
  (setq path (mark:report-path))
  (if (mark:report-write-lines path (mark:report-lines))
    (mark:emit (strcat (chr 10) "[OK] Отчёт: " path))
    (mark:emit (strcat (chr 10) "[WARN] Отчёт не записан: " path)))
  path)

(defun mark:remarks-summary (/ errs warns infos codes stats r c)
  (setq errs 0 warns 0 infos 0 codes nil stats (mark:report-stats))
  (foreach r *mc:remarks*
    (cond
      ((= (cadr r) "ERROR") (setq errs (1+ errs)))
      ((= (cadr r) "WARN")  (setq warns (1+ warns)))
      (t (setq infos (1+ infos))))
    (if (not (member (car r) codes)) (setq codes (cons (car r) codes))))
  (setq *mc:remark-hold* t)
  (mark:emit (strcat (chr 10) "========================================"))
  (mark:emit (strcat (chr 10) " ОТЧЁТ ПРОГОНА"))
  (mark:emit (strcat (chr 10) "========================================"))
  (foreach r stats
    (mark:emit (strcat (chr 10) (mark:pad-line (car r) (cadr r)))))
  (mark:emit (strcat (chr 10) "----------------------------------------"))
  (mark:emit (strcat (chr 10) " [ERROR] " (itoa errs)
                     "   [WARN] " (itoa warns)
                     "   [INFO] " (itoa infos)))
  (foreach c (reverse codes)
    (mark:emit (strcat (chr 10) "   " (mark:rjust c 5) "  "
                       (itoa (mc:remark-count c))
                       (if (mc:remark-code-name c)
                         (strcat "  — " (mc:remark-code-name c)) ""))))
  (mark:emit (strcat (chr 10) " Отчёт: " (mark:report-path)))
  (mark:emit (strcat (chr 10) "========================================" (chr 10)))
  (setq *mc:remark-hold* nil)
  nil)

(defun mark:cycle-body (enames)
  ;; Сначала заполнения: MarkZ строит шкалу с З-ширинами (195).
  (mark:emit (strcat (chr 10) "===== [1/2] МАРКАЗАП — заполнения ====="))
  (mark:a-all "Сетка")
  ;; Добавляем вставленные заполнения в выборку MarkV: probe MarkZ должен
  ;; видеть их, иначе теряется З-ширина 195 и номера ригелей сдвигаются на 1.
  (setq enames (append enames *mark:fills*))
  (mark:emit (strcat (chr 10) "===== [2/2] МАРКАВ — каркас ====="))
  (mk:a-all enames))

(defun mark:all-cycle (/ ss enames doc r)
  (mc:remark-reset)
  (mark:emit (strcat (chr 10) "[МАРКАВСЕ] Полный цикл: каркас -> заполнения -> ведомости."))
  (setq ss (ssget))
  (if (null ss)
    (mark:emit (strcat (chr 10) "[INFO] Выбор отменён — цикл прерван."))
    (progn
      (setq *mark:cycle-ss* ss
            *mark:reuse-sel* nil
            enames (mark:ss->enames ss)
            doc (mark:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (mark:ax-invoke-ok doc "StartUndoMark" nil))
      ;; Таблицы прошлых прогонов убираем ОДИН раз. MarkV не должен
      ;; удалять таблицу заполнений, вставленную MarkZ.
      (setq *mk:reuse-table-layer* t)
      (mk:clear-old-tables)
      (setq *mk:cycle-tables-cleared* t)
      (setq r (vl-catch-all-apply 'mark:cycle-body (list enames)))
      (setq *mark:cycle-ss* nil
            *mk:cycle-tables-cleared* nil)
      (if doc (mark:ax-invoke-ok doc "EndUndoMark" nil))
      (if (vl-catch-all-error-p r)
        (mark:emit (strcat (chr 10) "[ERROR] МАРКАВСЕ: "
                           (vl-catch-all-error-message r)))
        (mark:emit (strcat (chr 10) "[OK] МАРКАВСЕ завершён. Откат всего цикла — один U.")))
      (mark:report-write)
      (mark:remarks-summary)
      ;; метка цикла в оба журнала (модули уже создали их)
      (mc:log-append
        (mark:log-path)
        (strcat "=== MARK.lsp: MarkZ " *mark:rev* " / " *mark:build*
                " + MarkV " *mk:ver* " / " *mk:build* " ==="))
      (mc:log-append
        (mk:log-path)
        (strcat "=== MARK.lsp: MarkZ " *mark:rev* " / " *mark:build*
                " + MarkV " *mk:ver* " / " *mk:build* " ==="))))
  (princ))

(defun c:МАРКАВСЕ () (mark:all-cycle))
(defun c:MARKALL () (mark:all-cycle))

;;;=====================================================================
;;;  СБОРКА: ядро и оба модуля загружены
;;;=====================================================================
(setq *mc:core-ready* t)

(mark:emit (strcat (chr 10) "Mark.lsp загружен ("
                   *mark:file-rev* ": " *mark:file-note* ")"))
(mark:help-border)
(mark:help-header)
(mark:help-row "МАРКАВСЕ / MARKALL" "Полный цикл: всё за один запуск")
(mark:help-row "МАРКАЗАП / MARKA" "Заполнения: полный цикл")
(mark:help-row "МАРКАЗАПБЛОК" "Только раскладка по сетке")
(mark:help-row "МАРКАРОВКАЗАП / MARKZ" "Только марки заполнений")
(mark:help-row "МАРКАЗАПРЯД" "Только рядовка")
(mark:help-row "МАРКАЗАПТАБЛ" "Только ведомость заполнений")
(mark:help-row "МАРКАВ" "Каркас: полный цикл")
(mark:help-row "МАРКАВГЕОМЕТРИЯ" "Этап 1: сбор и 2D-модель")
(mark:help-row "МАРКАВСТ / МАРКАВРГ" "Этапы 2 и 3: стойки / ригели")
(mark:help-row "МАРКАВТАБЛ" "Этап 4: ведомость профилей")
(mark:help-row "МАРКАВДИАГНОЗОПТОМ" "Диагностика объектов каркаса")
(mark:help-border)
(princ)
