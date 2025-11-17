# Введение в Coalton

Дата перевода: 2025/11/13
SHA1: [d5e6f2217b51113ed43afcf8daa48ff6c92b2321](https://github.com/coalton-lang/coalton/commit/d5e6f2217b51113ed43afcf8daa48ff6c92b2321)

Coalton это статически типизированный язык, встроенный и компилируемый в Common Lisp.

Этот документ предназначен для людей, которые уже знакомы с функциональными языками программирования. Если вы уже знакомы с Common Lisp, то [глоссарий](./glossary.md) может быть полезен.

## Системы

### Явное определение компонентов

Coalton использует стандартные пакеты Common Lisp (и системы ASDF) для организации кода. Если вы начинаете новый проект, добавьте `#:coalton` в список `:depends-on` вашего ASD. Для улучшения сообщений об ошибках также добавьте пакет `#:named-readtables`.

### Package Inferred System (Система созданная на основе структуры пакетов)

Для простых проектов вы можете использовать утилиту `package-inferred-system` для автоматического создания системы ASDF на основе структуры ваших пакетов. Однако этот проект, coalton, не использует эти утилиты. Из-за этой несовместимости загрузчик ASDF пропускает загрузку `#:coalton-prelude` при использовании этой утилиты в вашем собственном проекте. Следовательно, вы должны явно зарегистрировать `coalton-prelude` в вашем собственном определении системы ASDF перед указанием `#:coalton` в вашем списке `:depends-on`.

```lisp
;; В определении вашей ASDF системы; например, my-project.asd
(register-system-packages '#:coalton '(#:coalton-prelude))
```

## Пакеты

В отличие от Common Lisp, стандартная библиотека Coalton организована как большая коллекция пакетов. Например, функции, связанные со строками, находятся в пакете `#:coalton-library/string`. Полный список пакетов стандартной библиотеки можно найти в [справочнике по Coalton](https://coalton-lang.github.io/reference).

Создавая новый проект, Вам нужно определить новый пакет и установить импорты из стандартной библиотеки. *Не `:use` пакеты `#:cl` или `#:common-lisp`!* Вместо этого вам, как минимум, нужно `:use` оба пакета: `#:coalton` (для основных функций языка) и `#:coalton-prelude` (для самых распространенных функций из стандартной библиотеки):

```lisp
(defpackage #:my-package
  (:use 
   #:coalton 
   #:coalton-prelude))
```

Пакет `#:coalton-prelude` не содержит функциональности, которой нет в других стандартных библиотеках, и, следовательно, это удобно.

В идиоматическом использовании это минимальное определение пакета будет слишком ограниченным, чтобы быть полезным. Например, если мы захотим использовать функцию из стандартной библиотеки `strip-prefix` (которая удаляет префикс из строки), нам придется писать `coalton-library/string:strip-prefix` в нашей программе. Вместо этого мы можем создать локальное сокращение для этого пакета:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str #:coalton-library/string)))
```

Таким образом, теперь мы можем просто написать `str:strip-prefix`. Каждый раз используя эту новую функцию из стандартной библиотеки Coalton или сторонней библиотеки, мы можем добавить псевдонимы этого пакета в наш список `:local-nicknames`. Например:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str  #:coalton-library/string)
   (#:vec  #:coalton-library/vector)
   (#:math #:coalton-library/math)))
```

**Мы не рекомендуем использовать `:use` с любыми другими встроенными пакетами Coalton, кроме `#:coalton` и `#:coalton-prelude`.** Исторически в Common Lisp это приводило к проблемам с обратной совместимостью, когда сами пакеты определяли новые экспортируемые функции. Более того, в отличие от Common Lisp, Coalton следует шаблону использования одного и того же имени функций из разных пакетов. Например, как пакет строк, так и пакет векторов имеют функцию `length`, то есть существуют оба `str:length` и `vec:length`.

### Coalton в REPL

Если вы экспериментируете с Coalton в REPL, мы рекомендуем изменить пакет на `#:coalton-user` следующим образом:

```lisp
(in-package #:coalton-user)
```

Это пакет для удобства, который существует исключительно для Вашего интерактивного и экспериментального использования. (Ни одно программное обеспечение не должно зависеть от него, так же как и от `#:cl-user`.)

Как мы видим в этой секции, простое изменение на пакет `#:coalton-user` не означает, что Вы можете сразу начать писать код на Coalton; Вам все еще нужно использовать формы `coalton-toplevel` или `coalton`.

Поскольку `#:coalton-user` не включает в себя пакет `#:common-lisp`, Вам нужно определить любые такие использования с помощью `cl:`.

## Структура программы

После создания вашего пакета `#:my-package`, вы должны переключиться на него с помощью:

```lisp
(in-package #:my-package)

(named-readtables:in-readtable coalton:coalton)
```

Форма `named-readtables:in-readtable` является необязательной, но рекомендуется к использованию. Coalton's reader позволяет ошибкам компилятора точно ссылаться на исходный код и предоставлять правильные номера строк.

Первый основной входной пункт для кода Coalton. Определения и подобные вещи, которые находятся на верхнем уровне называются `coalton-toplevel`.

```lisp
(coalton-toplevel
  ;; <Определяющие формы Coalton>
  )
```

Сейчас, в SLIME/SLY, нет способа `C-c-c` в более мелком масштабе, чем целая форма `coalton-toplevel`.

Второй основной входной пункт - это вызов Coalton из Lisp. В этом случае используется оператор `coalton`:

```lisp
;; Lisp код
;; ...
     (coalton #|coalton expression|#)
;; ...
```

Заметьте, что в форме `coalton` нельзя делать новые определения, только вычислять выражения.

В то время как `coalton-toplevel` ожидает одно или несколько определений или объявлений на глобальном уровне, форма `coalton` принимает одно выражение, вычисляет его относительно текущего контекста и возвращает его (низлежащее) Lisp-значение. Это может быть полезно для работы с Coalton из Lisp REPL.

Помните, что пакеты Coalton, включая `#:coalton-user`, *не* используют `:use` пакет `#:common-lisp`/`#:cl`, поэтому Вам нужно добавлять зависимости Common Lisp используя `cl:`, если они вам нужны.


## Переменные и функции

Переменные и функции определяются с помощью `define`. Вот несколько вариантов определений переменных.

```lisp
(coalton-toplevel
  ;; Переменные обьявлены с помощью ключевого слова define
  (define x 5)
  (define y 6)
  (define z (+ x y))
  (define p (Tuple 1.0 2.0))

  ;; Coalton поддерживает целые числа, строки, булевы значения и unit как примитивные типы
  (define name "Alyssa P. Hacker")
  (define hungry True)
  (define data Unit))
```

Можно получить первый элемент кортежа `p`, определенного выше, из REPL, используя оператор `coalton`:
```lisp
(coalton (fst p))
```

**Заметьте**: может возникнуть желание отбросить слово `coalton` и просто вычислить `(fst p)` напрямую в Lisp REPL, но на такое поведение нельзя полагаться!

Функции определяются аналогично переменным. В отличие от Common Lisp, функции Coalton занимают то же пространство имен, что и переменные. Это облегчает функциональное программирование.

```lisp
(coalton-toplevel
  ;; Функции также определяются с помощью ключевого слова define
  (define (add2 x)
    (+ 2 x))

  ;; Функции существуют в том же пространстве имен, что и переменные
  (define addTwo add2)

  (define x (addTwo 3))

  ;; Анонимные функции могут быть определены с помощью fn
  (define z (map (fn (x) (+ 2 x)) (make-list 1 2 3 4))))
```


### Функции и каррирование

*Все* функции в Coalton принимают ровно один аргумент и возвращают ровно одно значение. Рассмотрим эту функцию:

```lisp
(coalton-toplevel
  (define (fma a b c)
    (+ c (* a b))))
```

Правда в том, что `fma` технически принимает *один аргумент*: `a`. Для человека, знакомого с Common Lisp, эта функция примерно эквивалентна

```lisp
(defun fma (a)
  (lambda (b)
    (lambda (c)
      (+ c (* a b)))))
```

Хотя Coalton скрывает эту реальность от вас, если только она вам не нужна:

```lisp
(coalton-toplevel
  (define fma1 (fma 2))      ; equiv: b -> (c -> (c + 2*b))
  (define fma2 (fma 2 3))    ; equiv: c -> (c + 6)
  (define fma3 (fma 2 3 4))) ; equiv: 10
```

Мы видим, что можно вызвать `fma`, как если бы это была функция с тремя аргументами, но это просто удобный синтаксис. Мы также можем вызвать его с меньшим количеством аргументов. Иногда это свойство называют *каррированными функциями*.

Coalton работает над оптимизацией этих функций, чтобы свести к минимуму выделение замыканий. Фактически, выражение `(fma x y z)` будет скомпилировано как простое сложение и умножение, без выделения замыканий.

Вот пример использования каррированной функции для преобразования списка.

```lisp
(coalton-toplevel
  ;; Списки могут быть созданы с помощью макроса make-list
  (define nums (make-list 2 3 4 5)))

(coalton
  ;; Функции в Coalton каррированы
  (map (+ 2) nums)) ;; 4 5 6 7
```

### Синтаксис конвееров и композиция функций

Есть удобный синтаксис для написания функций с помощью макросов `pipe` и `nest`.

```lisp
(nest f g ... h x)

;; эквивалентно

(f (g (... (h x))))

;; эквивалентно

(pipe x h ... g f)
```

Такой синтаксис делает код чище.

Заметьте, что поскольку это макросы (что видно по их вариативным аргументам), они *не могут* использоваться как функции высшего порядка. Рассмотрите возможность использования каррирования или использования `compose`, если вы думаете в этом направлении.

### Игнорирование параметров функции

Рассмотрим функцию

```lisp
(coalton-toplevel
  (define (f x y)
    x))
```

Здесь `y` не используется и вызовет предупреждение:

```
COMMON-LISP:WARNING: warn: Unused variable
  --> <unknown>:3:15
   |
 3 |    (define (f x y)
   |                 ^ variable defined here
help: prefix the variable with '_' to declare it unused
 3 |   (define (f x _y)
   |                --
```

Как предложено, можно заменить `y` на `_y`, что сообщает компилятору Coalton, что параметр может быть намеренно не использован.

**Заметьте**: переменные с префиксом `_`, такие как `_y`, все еще являются обычными переменными и могут быть прочитаны. Следующий пример является допустимым кодом Coalton:

```lisp
(define (f _x) _x)
```

Вы должны обращаться с переменными с префиксом как с игнорируемыми, когда это возможно, и использовать имя без префикса `_`, если оно может быть использовано. Чтение из переменных с префиксом `_` разрешено, чтобы сгенерированный код (например, с использованием макросов или условных считываний) не выводил предупреждений о неиспользуемых переменных, которые будут использоваться в некоторых контекстах компиляции, но не в других.

## Типы данных

Coalton позволяет определять параметрические алгебраические типы данных.

```lisp
(coalton-toplevel
  ;; Новые типы создаются с помощью оператора DEFINE-TYPE
  (define-type Point3D (Point3D Integer Integer Integer))

  ;; Coalton поддерживает типы-суммы
  (define-type Color
    Red
    Blue
    Green)

  ;; Coalton поддерживает параметрические типы
  ;;
  ;; Типы параметров определяются с помощью аргументов-ключевых слов
  (define-type (Tree :a)
    (Branch (Tree :a) :a (Tree :a))
    (Leaf :a)))
```

Определения типов представляют типизированные конструкторы. Например, мы можем создать дерево следующим образом.

```lisp
(coalton
  (Branch (Leaf Red)
          Green
          (Branch (Leaf Red) Green (Leaf Red))))
```

Мы увидим, как распаковать эти типы с помощью `match` позже в этом документе.

## Псевдонимы типов

Coalton позволяет определять параметрические псевдонимы типов. Псевдонимы типов могут быть определены для примитивных типов и типов, созданных с помощью `define-type` или `define-type-alias`.

```lisp
(coalton-toplevel
  ;; Новые псевдонимы типов создаются с помощью оператора DEFINE-TYPE-ALIAS
  (define-type-alias Coordinate Integer)
  (define-type-alias (Pair :a) (Tuple :a :a))
  (define-type-alias Translation (Pair Coordinate -> Pair Coordinate))
  
  (declare shift-right Translation)
  (define (shift-right (Tuple x y))
    (Tuple (1+ x) y))
    
  (define shifted-coordinate (shift-right (Tuple 0 0))))

  ;; Псевдонимы типов могут иметь несколько параметров
  (define-type-alias (MyTuple3 :a :b :c) (Tuple :a (Tuple :b :c)))

  ;; Псевдонимы типов могут иметь параметры, которые не имеют вида *
  (define-type-alias (IntegerCollection :col) (:col Integer))

  ;; Псевдонимы типов могут ссылаться на типы, которые не имеют вида *
  (define-type-alias MyCollection List)
```

Параметрические псевдонимы типов должны быть полностью применены.

```lisp
(coalton-toplevel

  (define-type (T :a) (ConstrT (:a Integer)))
  
  (define-type-alias (MyCollection1 :a) (List :a))
  (define-type-alias MyCollection2 List)

  ;; Эта строка не скомпилируется, потому что MyCollection1 имеет
  ;; параметр :A, который не применен
  (define-type-alias A (T MyCollection1))

  ;; Хотя эта строка скомпилируется
  (define-type-alias A (T MyCollection2)))
```

Существует несколько инструментов отладки, которые полезны при работе с псевдонимами типов. Вне выражения Coalton можно использовать `describe-type-of`, чтобы отобразить тип переменной, включая его псевдонимы, и вернуть тип. `describe-type-alias` отображает псевдоним вместе с типом, на который он ссылается, и возвращает этот тип. Кроме того, Coalton можно настроить так, чтобы он отображал только псевдонимы, только типы или оба варианта при отображении типа, связанного с переменной. Предпочтение можно установить перед компиляцией Coalton с помощью `(setf (get ':coalton-config ':type-printing-mode) mode)`, где `mode` - это один из `:types`, `:aliases` и `:types-and-aliases`. После этого режим можно изменить между этими тремя вариантами с помощью функции `set-type-printing-mode`. Режим по умолчанию - `:types`.

```lisp
COALTON-USER> (coalton-toplevel
                (define-type-alias A Integer)
                (define x (the A 5)))
; No value

COALTON-USER> (set-type-printing-mode :aliases)
:ALIASES

COALTON-USER> (type-of 'x)
A

COALTON-USER> (set-type-printing-mode :types-and-aliases)
:TYPES-AND-ALIASES

COALTON-USER> (type-of 'x)
[A := INTEGER]

COALTON-USER> (set-type-printing-mode :types)
:TYPES

COALTON-USER> shifted-coordinate ;; из предыдущего примера
#.(TUPLE 1 0)

COALTON-USER> (type-of 'shifted-coordinate)
(TUPLE INTEGER INTEGER)

COALTON-USER> (describe-type-of 'shifted-coordinate)
[(PAIR COORDINATE) := (TUPLE [COORDINATE := INTEGER] [COORDINATE := INTEGER])]

COALTON-USER> (describe-type-alias 'Pair)
[(PAIR :A) := (TUPLE :A :A)]
```

### Структуры

Также есть поддержка определения структур.

```lisp
(coalton-toplevel
  (define-struct Point
    (x Integer)
    (y Integer)))
```

Структуры похожи на ADT (АТД) с одним конструктором и создаются аналогично:

```lisp
(coalton (Point 1 2))
```

Для доступа к полям структуры используются аксессоры:

```lisp
(coalton (.x (Point 1 2)))
```

Доступ к полям структуры может быть передан по значению:

```lisp
(coalton (map .x (make-list (Point 1 2) (Point 2 3))))
```

Структуры также могут быть параметрическими:

```lisp
(coalton-toplevel
  (define-struct (Pair :a)
    (first :a)
    (second :a)))
```

## Циклы и итерации

Coalton предлагает три основных способа выполнения явных циклов:

- Хвостовая рекурсия и `rec`,
- Встроенные конструкции циклов на основе итераторов, и
- Экспериментальные макросы итерирования в стиле Lisp (например, `dotimes`).

### Хвостовая рекурсия и `rec`

Coalton стремится обеспечить устранение хвостовых вызовов. Таким образом, хвостовая рекурсия является идиоматической в Coalton.

```lisp
(coalton-toplevel
  (define (find-integer predicate limit)
    (cond
      ((negative? limit) False)
      ((predicate limit) True)
      (True              (find-integer predicate (1- limit))))))
```

Эта функция может быть выполнена для произвольно большого `limit` без влияния на стек:

```lisp
> (coalton (find-integer (fn (x) (== 65536 (* x x))) 1000000))
COMMON-LISP:T
```

Coalton имеет специальный встроенный оператор `rec` для выполнения итерации типа "named-let". Его общий синтаксис:

```lisp
(rec <name> (<binding>*)
  <body>)

;; <name> := <symbol>
;;         | (<symbol> <type>)
;;
;; <binding> := (<symbol> <value>)
```

`<name>` по сути является локальной (хвостово-рекурсивной) функцией в `<body>`. Опционально, тип этой функции может быть объявлен, что полезно для избежания нежелательного полиморфизма. Оператор `rec` не требует, чтобы вызов был в хвостовой позиции.

Идиоматично в Coalton выбирать имя `%`, когда нет другого разумного имени.

Вот первый пример `rec` для имплементации реверса списка.

```lisp
(coalton-toplevel
  (define (rev l)
    (rec % ((result Nil) (remaining l))
      (match remaining
        ((Nil) result)
        ((Cons x xs) (% (Cons x result) xs))))))
```

Вот пример функции Фибоначчи, которая работает для любого числового входящего и возвращаемого значения:

```lisp
(coalton-toplevel
  (define (fib-polymorphic n)
    (rec % ((i n) (a 0) (b 1))
      (cond
        ((== 0 i) a)
        ((== 1 i) b)
        (True (% (- i 1) b (+ a b)))))))
```

Тип функции `∀ A B. (NUM B) (NUM A) ⇒ (A → B)` и, следовательно, может быть использован для любых числовых типов:

```
> (coalton (the F32 (fib-polymorphic (the Integer 10))))
55.0
```

Мы можем сделать мономорфный вариант для `Integer`, объявив тип рекурсии.

```lisp
(define (fib-monomorphic n)
  (rec (% (Integer -> Integer -> Integer -> Integer))
       ((i n)
        (a 0)
        (b 1))
    (cond
      ((== 0 i) a)
      ((== 1 i) b)
      (True (% (- i 1) b (+ a b))))))
```

Теперь это работает без каких-либо объявлений типов при использовании:

```
> (coalton (fib-monomorphic 10))
55
```

### Встроенные конструкции циклов

Coalton поддерживает бесконечные циклы, условные циклы и итерирование типа `for`.

#### `loop`, `while`, `while-let`, and `for`

Можно зациклиться навсегда

```lisp
(loop (trace "hi"))
```

Можно зациклиться, пока некоторое условие истинно

```lisp
(coalton
 (let ((counter (cell:new 0))
       (limit 10))
   (while (< (cell:read counter) limit)
     (trace "hi") 
     (cell:increment! counter))))
```

Можно зациклиться, пока паттерн совпадает

```lisp
(coalton
 (let ((xs (vector:make 4 3 2 1)))
   (while-let (Some x) = (vector:pop! xs)
              (traceobject "x" x))))
```

Можно итерироваться по элементам `IntoIterator`

```lisp
(coalton
 (for x in "coalton"
      (traceobject "x" x)))
```


#### `break` and `continue`

Каждая из вышеупомянутых форм циклов поддерживает `break` и `continue`.

Конструкция `break` немедленно завершает итерирование. Следующий пример выводит `c`, `o` и `a`, а затем завершается.

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (break))
      (traceobject "x" x)))
```

Конструкция `continue` пропускает остальную часть тела цикла и начинает следующую итерацию. Следующий пример выводит `c`, `o`, `a`, `t`, `o` и `n`, пропуская вывод `l`.

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (continue))
      (traceobject "x" x)))
```


#### Метки циклов

Каждая из вышеупомянутых форм циклов может принимать необязательную метку цикла. Эти метки могут использоваться вместе с `break` и `continue` для достижения сложного управления итерацией.

Для каждой из форм циклов метка может следовать сразу за открывающим термином цикла:

```lisp 

(loop :outer (do-stuff))

(while :a-label (is-true?) (do-stuff))

(while-let :another-label 
   (Some thing) = (get-something)
   (do-stuff thing))

(for :iter word in words 
   (do-stuff-with word))

```

В следующем полностью вымышленном примере внешний цикл помечен как `:outer`. Эта метка передается в `break` изнутри внутреннего цикла `while`, чтобы прекратить итерацию, когда сумма аккумулятора и счетчика превысит 500. Без метки `:outer`, `break` вышел бы только из внутреннего цикла `while`.

```lisp
(coalton 
  (let ((counter (cell:new 0))
        (acc (cell:new Nil)))
    (loop :outer
          (while (< (cell:increment! counter) 10)
            (let x = (fold + (cell:read counter) (cell:read acc)))
            (when (< 500 x)
              (break :outer))
            (when (== 0 (mod (cell:read counter) 3)) 
              (continue))
            (cell:push! acc x))
          (when (< (length (cell:read acc)) 500)
            (cell:swap! counter 0)
            Unit))
    (cell:read acc)))
```

### Экспериментальные макросы итерации Lisp

> [!ВНИМАНИЕ]
> Эти конструкции являются экспериментальными и могут измениться.

Стандартная библиотека-пакет [`coalton-library/experimental/loops`](https://coalton-lang.github.io/reference/#coalton-library/experimental/loops-package) содержит множество макросов итерации Lisp, включая `dotimes` и `dolist`, которые работают аналогично своим аналогам в Common Lisp.

Эти функции обычно возвращают `Unit`, если они специально не собирают или не накапливают значение. Счётчики (как в `dotimes`) и индексы (как в `dolist-enumerated`) обычно являются `UFix`.

Предположим, что у нас есть `dotimes`, доступный из нашего пакета, мы могли бы написать функцию для вывода Пифагоровых троек следующим образом:

```lisp
(coalton-toplevel
  (define (print-pythagorean-triples limit)
    (dotimes (c limit)
      (dotimes (b c)
        (dotimes (a b)
          (when (== (^ c 2) (+ (^ a 2) (^ b 2)))
            (trace (Tuple3 a b c))))))))

;; > (coalton (print-pythagorean-triples 20))
;; #.(TUPLE3 3 4 5)
;; #.(TUPLE3 6 8 10)
;; #.(TUPLE3 5 12 13)
;; #.(TUPLE3 9 12 15)
;; #.(TUPLE3 8 15 17)
```

## Числа

Coalton поддерживает числовые типы. Основные из них - это `Integer`, `F32` и `F64`.

```lisp
(coalton-toplevel
  (define num-int 5)
  (define num-sf  5.0f0)
  (define num-df  5.0d0))
```

Можно опустить суффикс и просто написать `5.0`, который будет разрешен в зависимости от значения `cl:*read-default-float-format*` во время чтения. Эта переменная по умолчанию установлена в `cl:single-float`, что означает, что неотредактированные числа с плавающей точкой будут одинарной точности.

Существуют также другие, более ограниченные целочисленные типы, такие как целые числа с фиксированной шириной со знаком (`I32` и `I64`) и целые беззнаковые числа с фиксированной шириной (`U8`, `U32`, `U64`).

Наконец, есть тип отношения, называемый `Fraction`, который является отношением двух значений `Integer`.

Числа наследуют класс типа `Num`, который имеет методы `+`, `-`, `*` и `fromInt`.

### Деление, вкратце

Деление - это сложная тема; см. следующий раздел. Вот несколько быстрых советов.

- Оператор деления `/` и его вариантов может привести к ошибкам во время выполнения, если делитель равен нулю. Используйте `safe/`, если вы предпочитаете возвращать `Optional`.

- Если у вас есть целые числа и вы хотите получить число с плавающей точкой, используйте `inexact/`. (Для одинарной точности используйте `into`.)

- Если у вас есть целые числа и вы хотите получить целочисленный ответ, используйте `floor/`, `ceiling/` или `round/`.

- Если вы пытаетесь разделить два целых числа, чтобы получить точный рациональный ответ, используйте `exact/`.

- Если вы пишете обобщённый код, который использует деление, или хотите оставаться абстрактным, изучите `/` и как правильно его ограничить.

### Детали о делении

Почему класс типов `Num` не имеет деления, т.е. `/`?

Coalton действительно имеет оператор деления `/`, но это отдельная, немного более сложная концепция. Деление сложно по двум причинам:

1. Деление может завершиться ошибкой, если мы делим на что-то вроде нуля,

2. Деление двух чисел не обязательно приводит к тому же типу. (Фактически, деление может быть даже невозможным!)

Чтобы решить первую проблему, деление может привести к ошибке во время выполнения. Мы не используем `Optional`, потому что это довольно громоздко в математических контекстах. (Можно использовать `safe/` для варианта, который проверяет деление на ноль и возвращает `Optional`.)

Чтобы решить вторую проблему, нам нужно ввести новый класс типов, называемый `Dividable`. Выражение типа

```
(Dividable :s :t)
```

говорит о том, что деление двух элементов типа `:s` может привести к элементу типа `:t`. С учетом всего этого, мы получаем окончательный тип `/`.

```
COALTON-USER> (type-of '/)
∀ :A :B. DIVIDABLE :A :B ⇒ (:A → :A → :B)
```

Поскольку в Coalton действует [Instance Defaulting](#instance-defaulting), деление констант `Integer` без дополнительного контекста по умолчанию приводит к делению `F64`:

```
COALTON-USER> (coalton (/ 1 2))
0.5d0
```

Мы можем сообщить Coalton, что наши константы имеют другой тип, ограничив их с использованием `the` или полагаясь на вывод типов. Например, чтобы получить результат не-`F64` из входных данных `Integer`, вы должны ограничить тип результата до желаемого типа (при условии, что для этого типа определен экземпляр класса типов `Dividable`):

```
COALTON-USER> (coalton (the F32 (/ 4 2)))
2.0
COALTON-USER> (coalton (the Fraction (/ 4 2)))
#.(COALTON-LIBRARY::%FRACTION 2 1)
```

Результат `Integer` от деления с `/` невозможен, так как экземпляр `Dividable Integer Integer` не определен:

```
COALTON-USER> (coalton (the Integer (/ 4 2)))
; error: Unable to codegen
;   --> repl input:1:22
;    |
;  1 |  (COALTON (THE INTEGER (/ 4 2)))
;    |                        ^^^^^^^ expression has type ∀. (RECIPROCABLE INTEGER) => INTEGER with unresolved constraint (RECIPROCABLE INTEGER)
;    |                        ------- Add a type assertion with THE to resolve ambiguity
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

Почему это не просто `2`?! К сожалению, ответ заключается в том, что `/` *не всегда* может привести к целому числу `2`, и когда оно не делится нацело, Coalton не навязывает определенный способ округления. Таким образом, правильный способ сделать это - разделить точно, а затем округлить по вашему усмотрению с помощью `floor`, `ceiling` или `round`.

```
COALTON-USER> (coalton (floor (the Fraction (/ 4 2))))
2
```

Вот примеры того, что происходит, когда вы выбираете значения, которые не делятся нацело.

```
COALTON-USER> (coalton (floor (the Fraction (/ 3 2))))
1
COALTON-USER> (coalton (ceiling (the Fraction (/ 3 2))))
2
COALTON-USER> (coalton (round (the Fraction (/ 3 2))))
2
```

Все эти случаи достаточно распространены, поэтому мы предоставляем несколько сокращений:

- `safe/` чтобы выполнить проверку деления на ноль и вернуть `None`, если это так,

- `exact/` для деления двух целых числа с получением дроби (что заменяет все вышеприведенное использование `the Fraction`),

- `inexact/` для деления двух целых числа с получением числа двойной точности,

- `floor/`, `ceiling/`, и `round/` для деления целого числа на целое число.

Дроби могут быть преобразованы в другие делимые типы с помощью `fromfrac` (Примечание: это может привести к потере точности):

```
COALTON-LIBRARY/MATH/REAL> (coalton (the F64 (fromfrac 1/2)))
0.5d0
COALTON-LIBRARY/MATH/REAL> (coalton (the F32 (fromfrac 999/1000)))
0.999
```


## Списки

Coalton использует списки Lisp "под капотом". Списки можно создавать с помощью `make-list`.

```lisp
(coalton-toplevel
  (define x (make-list 1 2 3))
  (define y (make-list "a" "b" "c")))
```


Все элементы списка должны быть одного типа. Это означает, что следующий код приводит к ошибке типа.

```
COALTON-USER> (coalton-toplevel
                (define wut (make-list 1.0d0 2.0d0 3.0)))
; error: Type mismatch
;   --> repl input:3:4
;    |
;  3 |      (MAKE-LIST 1.0d0 2.0d0 3.0)))
;    |      ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Expected type '(LIST DOUBLE-FLOAT)' but got '(LIST SINGLE-FLOAT)' 
;    [Condition of type COALTON-IMPL/TYPECHECKER/BASE:TC-ERROR]
```

Списки также можно деконструировать с помощью `match`.

```lisp
(coalton-toplevel
  (define (is-empty lst)
    (match lst
      ((Cons _ _) "is not empty")
      ((Nil) "is empty"))))
```

## Статическая типизация

Код Coalton определяет и проверяет типы во время компиляции. Типы будут автоматически присвоены.

```lisp
(coalton-toplevel
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

Тип переменной или функции можно проверить с помощью `coalton:type-of`.

```
COALTON-USER> (type-of 'fun)
(STRING -> (OPTIONAL INTEGER)
```

Типы можно всегда добавлять вручную.

```lisp
(coalton-toplevel
  (declare fun (String -> (Optional Integer)))
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

Типы также можно объявлять в выражениях `let`.

```lisp
(coalton-toplevel
  (define (f a b)
    (let ((declare g (Integer -> Integer -> Integer))
          (g +))
      (g a b))))
```

### Преобразование, приведение и конвертация типов

Coalton управляет преобразованием типов, аналогично функции Common Lisp `cl:coerce`, с помощью класса типов `Into` (из пакета `#:coalton-library/classes`) и его единственного метода `into`. Однако метод `into` принимает только один аргумент. Как Coalton определяет, в какой тип данных преобразовывать? Он определяет это либо путем вывода типов (т.е. по окружающему контексту), как в этом примере, где `substring` ожидает `String`:

```lisp
(coalton-toplevel
  (define integer-part (substring (into 12.34d0) 0 2)))

;; ==> "12"
```

или путем явного объявления с помощью `the`:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (the (List Char) (into "mississippi")))))

;; ==> (#\m #\s #\p #\i)
```

Паттерн `the`-`into` настолько распространен, что Coalton предоставляет сокращение, называемое `as`. Приведенный выше пример можно записать более лаконично следующим образом:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (as (List Char) "mississippi"))))

;; ==> (#\m #\s #\p #\i)
```

Метод `into` используется только тогда, когда преобразование всегда может быть выполнено из одного типа в другой. Если значение не может быть преобразовано в тип, то используется другой класс `TryInto` с методом `tryInto`. Метод `tryinto` возвращает тип `Result`, который указывает, было ли преобразование успешным или нет.

**Заметка: `as` работает только для преобразований через `into`, т.е. полных преобразований.** Нет соответствующего синтаксиса для `tryInto`.

## Сопоставление с образцом

Выражение `match` может использоваться для сопоставления с образцом и деконструкции алгебраических типов данных.

```lisp
(coalton-toplevel
  (define-type Color
    Red
    Blue
    Green)

  ;; Конструкторы должны быть обернуты в круглые скобки
  (declare color-to-string (Color -> String))
  (define (color-to-string c)
    (match c
      ((Red) "Red")
      ((Blue) "Blue")
      ((Green) "Green")))

  ;; Переменные не должны быть обернуты в круглые скобки
  (declare map-optional ((:a -> :b) -> (Optional :a) -> (Optional :b)))
  (define (map-optional f x)
    (match x
      ((Some x_) (Some (f x_)))
      ((None) None)))

  ;; Паттерны могут быть вложенными, и поддерживаются шаблоны подстановки "_"
  (declare flatten-optional ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten-optional x)
    (match x
      ((Some (Some x_)) (Some x_))
      (_ None)))

  ;; Подсовпадения могут быть захвачены в переменную
  (declare dedup-head (Eq :a => List :a -> List :a))
  (define (dedup-head xs)
    "If the first and second member of list are equal, drop the first"
    (match xs
      ((Cons a (= tl1 (Cons b _))) 
       (if (== a b) tl1 xs))
      (_ xs)))

  ;; Числа или строки также могут быть сопоставлены
  (define (is-5-or-7 x)
    (match x
      (5 True)
      (7 True)
      (_ False)))
      
  (define (is-five-or-seven x)
    (match x
      ("five"  True)
      ("seven" True)
      (_       False))))
    
```

Функции могут сопоставлять свои аргументы с образцом, но шаблоны должны быть исчерпывающими.

```lisp
(coalton-toplevel
  (declare first (Tuple :a :b -> :a))
  (define (first (Tuple a _)) a)

  (declare second (Tuple :a :b -> :b))
  (define second (fn ((Tuple _ b)) b))

  ;; Патерн захвата также работает здесь
  (declare nest-right (Tuple :a :b -> (Tuple :a (Tuple :a :b))))
  (define (nest-right (= tpl (Tuple a _))) (Tuple a tpl)))

```

Оператор `coalton-library:if` может использоваться в качестве сокращения при сопоставлении с образцом на бинарных значениях:

```lisp
(coalton-toplevel
  (define (is-even x)
    (if (== 0 x)
        True
        (is-odd (- x 1))))

  (define (is-odd x)
    (if (== 0 x)
        False
        (is-even (- x 1)))))
```

Несколько выражений `if` могут быть объединены с помощью `cond`:

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (cond
      ((and (== 0 (mod n 5))
            (== 0 (mod n 3)))
            "Fizzbuzz")
      ((== 0 (mod n 3))
        "Fizz")
      ((== 0 (mod n 5))
        "Buzz")
      (True (into n)))))
```

Бинарные логические операторы `and` и `or` (из `coalton-library`) на самом деле являются вариативными макросами, которые используют вычисление по короткой схеме. Их функциональными аналогами являются `boolean-and` и `boolean-or`.

```lisp
(coalton
  (or (cheap 5) True (really-expensive (expt 2 1000000))))
```

В этом случае `really-expensive` никогда не будет вызвана из-за вычисления по короткой схеме. Также обратите внимание, что и `and`, и `or` могут принимать один или несколько аргументов.


## `COALTON-LIBRARY:PROGN`

В Coalton есть конструкция `coalton-library:progn`, похожая на Lisp.

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> (Tuple Integer Integer)))
  (define (f x y)
    (progn
      (+ x y)
      (* x y)
      (Tuple x y))))
```

Конструкция `progn` в Coalton может иметь упрощенный синтаксис `let`.

```lisp
(coalton-toplevel
 (declare f (Integer -> Integer -> String))
 (define (f x y)
   (progn
     (let x_ = (into x))
     (let y_ = (into y))
     (<> x_ y_))))
```

Упрощенные выражения `let` поддерживают сопоставление с образцом:

```lisp
(coalton-toplevel
  (declare f (Tuple Integer Integer -> Integer))
  (define (f t)
    (let (Tuple fst snd) = t)
    (+ fst snd)))

```

Упрощенные выражения `let` не являются рекурсивными и не поддерживают полиморфизм `let`. Таким образом, следующий код является недопустимым:

```
(coalton
  (progn
    (let id = (fn (x) x))
    (id Unit)
    (id "hello")))
```

Однако, этот код работает с обычными выражениями `let`:

```
(coalton
  (let ((id (fn (x) x)))
    (progn
      (id Unit)
      (id "hello"))))
```

Объявления функций создают неявный блок `progn`:

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> String))
  (define (f x y)
    (let x_ = (into x))
    (let y_ = (into y))
    (<> x_ y_)))
```

## Если не и Когда

Пакет `coalton-library` также включает `unless` и `when`, которые работают аналогично их определениям в Lisp. Мы рекомендуем использовать эти операторы только для условной обработки операций с состоянием.

```lisp
(coalton-toplevel
  (define (f x)
    (when (== x 5)
      (error "I only want the number 5"))))
```

`unless` и `when` образуют неявные блоки `progn`.

```lisp
(coalton-toplevel
  (define (f b)
    (when b
      (let x = 5)
      (let y = 7)
      (traceObject "sum" (+ x y)))))
```

## Досрочное возвращение

Функции могут быть возвращены досрочно с помощью `return`.

```lisp
(coalton-toplevel
  (define (fizz-buzz n)
    (when (== 0 (mod n 15))
      (return "fizzbuzz"))

    (when (== 0 (mod n 3))
      (return "fizz"))

    (when (== 0 (mod n 5))
      (return "buzz"))

    (into n)))
```

## Классы типов

Coalton поддерживает классы типов.

На данный момент *все* методы должны быть определены для каждого экземпляра класса типов.

```lisp
(coalton-toplevel
  ;; Типы классов определяются с помощью ключевого слова define-class
  (define-class (Eq :a)
    (== (:a -> :a -> Boolean)))

  (define-type Color
    Red
    Green
    Blue)

  ;; Типы экземпляров классов определяются с помощью ключевого слова define-instance
  (define-instance (Eq Color)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Red) (Red)) True)
        ((Tuple (Blue) (Blue)) True)
        ((Tuple (Green) (Green)) True)
        (_ False)))
    (define (/= a b) (not (== a b))))

  ;; Типы объявлений могут иметь ограничения
  (declare is-eql (Eq :a => (:a -> :a -> String)))
  (define (is-eql a b)
    (if (== a b)
      "They are equal"
      "They are not equal"))

  ;; Несколько ограничений должны быть заключены в круглые скобки
  (declare double-is-eql ((Eq :a) (Eq :b) => (:a -> :a -> :b -> :b -> String)))
  (define (double-is-eql a b c d)
    (if (and (== a b) (== c d))
      "Both pairs are equal"
      "The pairs are not both equal")))
```


## Встроенные типы классов

Основные классы типов, определенные в стандартной библиотеке.

* `Eq` - определен для сравнимых типов
* `Ord` - определен для упорядочиваемых типов
* `Num` - определен для числовых типов

* `Semigroup` - определен для типов, которые поддерживают ассоциативную бинарную операцию
* `Monoid` - определен для типов, которые являются полугруппами и имеют элемент идентичности


Каждый из следующих классов типов напоминает класс с тем же именем в Haskell, за исключением незначительных различий.

* `Functor` - `fmap` является `map` в Coalton
* `Applicative`
* `Monad` - монада не имеет `return`, вместо этого используйте `pure` из приложения
* `Alternative` - `<|>` называется `alt` в Coalton
* `Foldable`
* `Traversable`

Эти классы типов вдохновлены типажами с тем же именем в Rust:

* `Into` - полные преобразования между одним типом и другим
* `TryInto` - неполные преобразования между одним типом и другим

## Автоматическое получение типов экземпляров классов из Определений Типов

Экземпляры некоторых классов типов могут быть получены автоматически, чтобы они автоматически определялись для пользовательских типов.

```lisp
(coalton-toplevel
  (derive Eq Hash)
  (define-struct Point
    (x UFix)
    (y UFix)))
```

Теперь вы можете использовать методы `==` и `hash` для структур `Point`:

```lisp
(coalton
  ;; Проверяет равенство `Point' 
  (== (Point 1 2) (Point 3 3))

  ;; Использует `Point' в качестве ключа в хеш-карте 
  (let map = (the (hashmap:HashMap Point UFix) hashmap:empty))
  (hashmap:insert map (Point 0 0) 1))
```

Сгенерированный экземпляр будет "очевидным" в каждом случае. Например, равенство будет проверять каждое под-поле. Это может быть не всегда желательным для каждого типа, и поэтому иногда все еще нужны пользовательские экземпляры.

### Встроенные классы типов `derive`

Экземпляры следующих классов могут быть получены автоматически:

- `Eq`
- `Hash`
- `Default`

В настоящее время это единственные классы, которые можно получить автоматически в стандартной библиотеке, но в будущем могут быть добавлены и другие.

Написание `custom derivers` еще не имеет официального API, но это можно сделать относительно легко для тех, кому это интересно. Для получения руководства см. [derivers.lisp](./../library/derivers.lisp).

## Нотация `Do`

В Coalton есть макрос `do`, который работает аналогично нотации `do` в Haskell.

```lisp
(coalton-toplevel
  (define (f ax bx)
    (do
      (a <- ax)
      (b <- bx)
      (let c = (+ a b))
      (pure c)))

    ;; [6+3, 5+3, 4+3, 6+2, 5+2, 4+2, 6+1, 5+1, 4+1]
   (define xs (f (make-list 1 2 3) (make-list 4 5 6))))
```

## Встраивание аннотации типов

Встраиваемые аннотации типов могут быть добавлены для разрешения неоднозначностей при использовании типов классов.

```lisp
(coalton-toplevel
  (define f (the U32 (+ (fromInt 5) (fromInt 7)))))
```

## Сокращенный синтаксис функций

Coalton не имеет null-евых функций. Однако функция с сигнатурой типа `Unit -> *` может быть вызвана в Coalton без явной передачи `Unit`. Например, форма Coalton `(coalton-library/vector:new)` является сокращением для `(coalton-library/vector:new Unit)`.

Функции также могут быть определены с неявным параметром с помощью `(fn () 5)`. Это создает функцию с одним неявным параметром типа `Unit`.


## Инспектирование системы Coalton

Пакет `coalton` определяет несколько функций отладки.

`type-of` и `kind-of` можно использовать для проверки типов определений.

```
COALTON-USER> (type-of 'map)
∀ :A :B :C. FUNCTOR :C ⇒ ((:A → :B) → (:C :A) → (:C :B))
COALTON-USER> (kind-of 'Result)
* -> (* -> *)
```

Все следующие функции принимают необязательный параметр пакета.

* `print-type-db` - вывести все известные типы
* `print-value-db` - вывести тип каждого глобального значения
* `print-class-db` - вывести каждый класс и их методы
* `print-instance-db` - вывести экземпляры каждого класса

## Экземпляр по умолчанию

В Coalton есть система [типов по умолчанию](https://www.haskell.org/onlinereport/decls.html#sect4.3.4), аналогичная Haskell. Типы по умолчанию вызываются для неявно типизированных определений и кода, скомпилированного с помощью макроса `coalton`. Тип по умолчанию применяется к набору неоднозначных предикатов с целью разрешения неоднозначного типа переменной в допустимый тип. Coalton будет использовать тип по умолчанию только в том случае, если один или несколько предикатов являются числовыми классами типов (`Num`, `Quantizable`, `Reciprocable`, `ComplexComponent`, `Remainder`, `Integral`). Coalton установит неоднозначную переменную типа в значение `Integer`, `F32` или `F64`, выбрав первый тип, который действителен для всех предикатов, ссылающихся на эту переменную. Coalton не будет использовать тип по умолчанию, если один или несколько предикатов, содержащих неоднозначную переменную, относятся к классу типов с несколькими параметрами.

Отличия от Haskell 98. Haskell считал бы `Num (List :a)` неоднозначным, Coalton по умолчанию установил бы его в `Num Integer`. Haskell считал бы (`Num :a` `CustomTypeClass :a`) неоднозначным, Coalton по умолчанию установил бы его в (`Num Integer` `CustomTypeClass Integer`), предполагая, что `CustomTypeClass Integer` был допустимым экземпляром.

## Функциональные зависимости

Функциональные зависимости заставляют соблюдать отношение типов переменных класса для улучшения вывода типов.

Класс `C` может быть определен функциональной зависимостью `(:a -> :b)` следующим образом:

`(define-class (C :a :b (:a -> :b)))`

Выражение `(:a -> :b)` можно прочитать как: для каждого `:a` будет только один `:b` или, альтернативно, значение `:b` уникально определяется `:a`.

Если был определен экземпляр `(C String Integer)`, то было бы неверно определять `(C String Char)`, потому что для одного и того же значения `:a` существует несколько значений `:b`.

Классы могут иметь несколько функциональных зависимостей, каждая зависимость может объявлять несколько переменных класса с каждой стороны `(:a :b -> :c :d :e)`, и зависимости могут быть рекурсивными `(:a -> :b) (:b -> :a)`.

## Специализация

Coalton поддерживает оптимистичную специализацию функций на основе типов. Специализации функций объявляются с помощью формы `specialize`:

```
(coalton-toplevel
  (declare inc (Num :a => :a -> :a))
  (define (inc x)
    (trace "standard call")
    (+ x 1))

  (declare inc-int (Integer -> Integer))
  (define (inc-int x)
    (trace "int specialized call")
    (+ x 1))

  (specialize inc inc-int (Integer -> Integer)))
```

Когда `inc` вызывается с целым числом, вызов будет прозрачно переписан на вызов `inc-int`.

```
COALTON-USER> (coalton (inc 1.2))
standard call
2.2
COALTON-USER> (coalton (inc 1))
int specialized call
2
```

Специализация может применяться только тогда, когда типы аргументов в месте вызова известны. Поскольку специализация не гарантируется, специализированные функции должны иметь такое же поведение, как и их неспециализированные варианты. Специализация должна использоваться только для улучшения производительности. См. следующий пример:

```
(coalton-toplevel
  (declare inc2 (Num :a => :a -> :a))
  (define (inc2 x)
    (inc x)))
```

Поскольку тип `x` в теле `inc2` неизвестен, специализация не будет применяться.

```
COALTON-USER> (coalton (inc2 1))
standard call
2
```

Специализация может быть указана в repl с помощью `print-specializations`.

## Особенности и отличия от Common Lisp

* Coalton использует `False` (а не `t`).  Таким образом, `t` может использоваться как обычное имя переменной.
* Для определения равенства Coalton использует двойное равно `==`.
* Списки в Coalton должны быть однородными.
* Для обозначения анонимных функций Coalton использует `fn` (*а не* `lambda`).
* Числовые операторы, такие как `+`, принимают только 2 аргумента.
* Отрицание осуществляется с помощью `negate`. Форма `(- x)` является каррированной функцией, эквивалентной `(fn (z) (- x z))`.

Для получения дополнительной информации см. [глоссарий](./glossary.md).

# Незавершенный функционал

В настоящее время Coalton поддерживает эти функции, но для их улучшения требуется еще много работы.

## Обработка исключений

В Coalton есть синтаксис для определения, сигнализации, обработки и возобновления после возникновения исключительных условий.

Вкратце, соответствующие синтаксические формы:

- `define-exception`: определяет тип исключения. Помимо его имени, синтаксис идентичен `define-type`.
- `define-resumption`: определяет именованный тип возобновления.
- `catch`: выражение для перехвата и обработки исключений. Обработчики сопоставляют шаблоны с конструкторами исключений.
- `throw`: сигнализирует об исключении.
- `resumable`: выражение, которое перехватывает и обрабатывает возможное возобновление. Случаи возобновления также выполняются путем сопоставления шаблонов с конструкторами возобновления.
- `resume-to`: выражение, которое принимает экземпляр возобновления. Передает управление блоку `resumable`, который включает обработчик для указанного возобновления.

Обработка исключений в Coalton еще не завершена и развивается. Дизайн был выбран для экспериментов и обеспечения совместимости в будущем по мере развития его функций. См. раздел "Предостережения" ниже.

### Определение, вывод и перехват исключений

Если вы хотите перехватить любое исключение, включая условия ошибок Common Lisp, вы можете использовать шаблон подстановки:

```lisp
(declare divide-by-random (Integer -> Integer -> Integer))
(define (divide-by-random r m)
    "Divide `r` by a random integer between `0` and `m`. 
     If the divisor is `0`, then print the divide by zero error
     and then return `0.0`"
    (catch (lisp Integer (r m) (cl:/ r (cl:random m)))
        (_ (trace "An error was received")
           0)))
```

Более общий пример

```lisp 
(define-type Egg
  ;;     разбито? приготовлено?
  (Goose Boolean Boolean)
  (Xenomorph))

;; Определим тип исключения BadEgg с несколькими вариантами
(define-exception BadEgg
  (UnCracked Egg)
  (DeadlyEgg Egg))

;; Если мы попытаемся разбить яйцо Ксеноморфа, то произойдет ошибка DeadlyEgg
(declare crack (Egg -> Egg))
(define (crack egg)
  (match egg
    ((Goose _ cooked?)
     (Goose True cooked?))
    ((Xenomorph)
     (throw (DeadlyEgg egg)))))

;; Разбиваем яйцо безопасно.
(declare crack-safely (Egg -> (Result BadEgg Egg)))
(define (crack-safely egg)
  (catch (Ok (crack egg))
    ((DeadlyEgg _) (Err (DeadlyEgg egg)))
    ((UnCracked _) (Err (UnCracked egg)))))
```

### Определение, вызов и обработка возобновлений

Возобновления позволяют программисту Coalton восстановиться после ошибки без разворачивания стека вызовов.

Выражение `define-resumption` принимает один "Constructor". Имя конструктора также является именем типа возобновления.

Следующий код проясняет приведенный выше пример

```lisp
(define-resumption SkipEgg)
(define-resumption (ServeRaw Egg) 
  "Suggest the egg be served raw.")

(declare cook (Egg -> Egg))
(define (cook egg)
  (let ((badegg (Uncracked egg)))     ; exceptions can be constructed outside throw
    (match egg
      ((Goose (True) _)  (Goose True True))
      ((Goose (False) _) (throw badegg))
      ((Xenomorph)       (throw (DeadlyEgg egg))))))

;; Возвращаем None, если получено возобновление SkipEgg.
(declare make-breakfast-with (Egg -> (Optional Egg)))
(define (make-breakfast-with egg)
  (resumable (Some (cook (crack egg)))
    ((SkipEgg) None)))
```

Теперь определим функцию, которая готовит завтрак для `n` человек. Она пытается приготовить каждое яйцо, но если возникает ошибка из-за выбора яйца Ксеноморфа, она возобновляет `make-breakfast`, пропуская это яйцо.

```lisp 
(declare make-breakfast-for (UFix -> (Vector Egg)))
(define (make-breakfast-for n)
  (let ((eggs (vector:make))
        (skip  SkipEgg))              ; can construct outside of resume-to
    (for i in (iter:up-to n)
      (let egg = (if (== 0 (mod i 5)) Xenomorph (Goose False False)))
      (do
       (cooked <- (catch (make-breakfast-with egg)
                    ((DeadlyEgg _)    (resume-to skip))))
       (pure (vector:push! cooked eggs))))
    eggs))
```

Каждое 5-е яйцо смертоносное, поэтому приготовление завтрака для 10 человек приведет к 8 приготовленным яйцам.

Стек вызовов выглядит следующим образом:

```
make-breakfast-for 
      │
      └─ make-breakfast-with 
                │
                └─ cook  
```

Но `cook` сигнализирует об ошибке `DeadlyEgg` на яйцах `Xenomorph`. `make-breakfast-for` перехватывает эту ошибку и возобновляет `SkipEgg`, где `make-breakfast-with` получает это возобновление и обрабатывает его.


### Предостережения 

На данный момент применяются следующие оговорки;

1. Не поддерживается полиморфизм для выражений `throw` или `resume-to`. Например, следующее не скомпилируется без аннотации типа:
   - `(define (th a) (throw a))` 
   - `(define (res a) (resume-to a))`

2. Невозможно `catch` условие Lisp и привязать его к переменной в обработчике `catch`. Однако условия Lisp могут быть перехвачены с помощью шаблона подстановки.
В частности, это означает, что вы не можете повторно вызвать исключение Lisp. Более того, вы можете повторно вызвать исключение, только реконструировав его. Например:
   - `(catch (bad-thing) (_ Unit))` 
   - `(catch (bad-thing) ((MyBad x) (trace "my bad") (throw (MyBad x))))`

3. Ответвления `resumable` еще более ограничены. Вы не можете сопоставлять что-либо, _кроме_ шаблона конструктора возобновления.

4. Класс типов не может быть связан с формами сигнализации исключений. Мы изучаем различные подходы к статической проверке форм, которые могут перепрыгивать через стек вызовов. В конечном итоге может победить подход с классом типов.
Что бы мы ни делали, мы постараемся сделать его совместимым с существующим синтаксисом и семантикой.


   

