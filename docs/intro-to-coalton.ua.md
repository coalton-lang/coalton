# Введення в Coalton

Дата перекладу: 2025/11/13
SHA1: [https://github.com/coalton-lang/coalton/commit/d5e6f2217b51113ed43afcf8daa48ff6c92b2321](https://github.com/coalton-lang/coalton/commit/https://github.com/coalton-lang/coalton/commit/d5e6f2217b51113ed43afcf8daa48ff6c92b2321)

Coalton це статично типізована мова, вбудована та компільована в Common Lisp.

Цей документ призначений для людей, які вже знайомі з функціональними мовами програмування. Якщо ви вже знайомі з Common Lisp, то [глосарій](./glossary.md) може бути корисним.

## Системи

### Явне визначення компонентів

Coalton використовує стандартні пакети Common Lisp (і системи ASDF) для організації коду. Якщо ви починаєте новий проект, додайте `#:coalton` до списку `:depends-on` вашого ASD. Для покращення повідомлень про помилки також додайте пакет `#:named-readtables`.

### Package Inferred System (Система створена на основі структури пакетів)

Для простих проєктів ви можете використовувати утиліту `package-inferred-system` для автоматичного створення системи ASDF на основі структури ваших пакетів. Однак цей проект, coalton, не використовує ці утиліти. Через цю несумісність завантажувач ASDF пропускає завантаження `#:coalton-prelude` при використанні цієї утиліти у вашому власному проекті. Отже, ви повинні явно зареєструвати `coalton-prelude` у вашому власному визначенні системи ASDF перед вказанням `#:coalton` у вашому списку `:depends-on`.

```lisp
;; У вашому визначенні системи ASDF; наприклад, my-project.asd
(register-system-packages '#:coalton '(#:coalton-prelude))
```

## Пакети

На відміну від Common Lisp, стандартна бібліотека Coalton організована як велика колекція пакетів. Наприклад, функції, пов'язані з рядками, знаходяться в пакеті `#:coalton-library/string`. Повний список пакетів стандартної бібліотеки можна знайти в [довіднику по Coalton](https://coalton-lang.github.io/reference).

Створюючи новий проєкт, вам потрібно визначити новий пакет і встановити імпорти зі стандартної бібліотеки. *Не використовуйте `:use` пакети `#:cl` або `#:common-lisp`!* Замість цього вам, як мінімум, потрібно `:use` обидва пакети: `#:coalton` (для основних функцій мови) і `#:coalton-prelude` (для найпоширеніших функцій зі стандартної бібліотеки):

```lisp
(defpackage #:my-package
  (:use 
   #:coalton 
   #:coalton-prelude))
```

Пакет `#:coalton-prelude` не містить функціональності, якої немає в інших стандартних бібліотеках, і, отже, це зручно.

У ідіоматичному використанні це мінімальне визначення пакету буде занадто обмеженим, щоб бути корисним. Наприклад, якщо ми захочемо використовувати функцію зі стандартної бібліотеки `strip-prefix` (яка видаляє префікс з рядка), нам доведеться писати `coalton-library/string:strip-prefix` у нашій програмі. Замість цього ми можемо створити локальне скорочення для цього пакету:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str #:coalton-library/string)))
```

Таким чином, тепер ми можемо просто написати `str:strip-prefix`. Кожного разу, використовуючи цю нову функцію зі стандартної бібліотеки Coalton або сторонньої бібліотеки, ми можемо додати псевдоніми цього пакету до нашого списку `:local-nicknames`. Наприклад:

```lisp
(defpackage #:my-package
  (:use 
   #:coalton)
  (:local-nicknames 
   (#:str  #:coalton-library/string)
   (#:vec  #:coalton-library/vector)
   (#:math #:coalton-library/math)))
```

**Ми не радимо використовувати `:use` з будь-якими іншими вбудованими пакетами Coalton, крім `#:coalton` та `#:coalton-prelude`.** Історично в Common Lisp це призводило до проблем з зворотною сумісністю, коли самі пакети визначали нові експортовані функції. Більше того, на відміну від Common Lisp, Coalton дотримується шаблону використання однакових імен функцій з різних пакетів. Наприклад, як пакет рядків, так і пакет векторів мають функцію `length`, тобто існують обидва `str:length` та `vec:length`.

### Coalton в REPL

Якщо ви експериментуєте з Coalton у REPL, ми рекомендуємо змінити пакет на `#:coalton-user` наступним чином:

```lisp
(in-package #:coalton-user)
```

Цей пакет для зручності існує виключно для Вашого інтерактивного та експериментального використання. (Жодне програмне забезпечення не повинно залежати від нього, так само як і від `#:cl-user`.)

Як ми бачимо в цьому розділі, проста зміна на пакет `#:coalton-user` не означає, що ви можете негайно почати писати код на Coalton; вам все ще потрібно використовувати форми `coalton-toplevel` або `coalton`.

Оскільки `#:coalton-user` не включає пакет `#:common-lisp`, вам потрібно визначити будь-яке таке використання за допомогою `cl:`.

## Структура програми

Після створення вашого пакету `#:my-package`, ви повинні переключитися на нього за допомогою:

```lisp
(in-package #:my-package)

(named-readtables:in-readtable coalton:coalton)
```

Форма `named-readtables:in-readtable` є необов'язковою, але рекомендується до використання. Читач Coalton дозволяє помилкам компілятора точно посилатися на вихідний код і надавати правильні номери рядків.

Перщий основний вхідний пункт для коду Coalton. Визначення та подібні речі, які знаходяться на верхньому рівні, називаються `coalton-toplevel`.

```lisp
(coalton-toplevel
  ;; <Форми визначення Coalton>
  )
```

Зараз, у SLIME/SLY немає засобу `C-c-c` у більш дрібному масштабі, ніж ціла форма `coalton-toplevel`.

Другий основний вхідний пункт - це виклик Coalton з Lisp. У цьому випадку використовується оператор `coalton`:

```lisp
;; Lisp код
;; ...
     (coalton #|coalton expression|#)
;; ...
```

Зауважте, що в формі `coalton` не можна робити нові визначення, лише обчислювати вирази.

У той час як `coalton-toplevel` очікує одне або кілька визначень на глобальному рівні, форма `coalton` приймає один вираз, обчислює його відносно поточного контексту і повертає його (нижнє) Lisp-значення. Це може бути корисно для роботи з Coalton з Lisp REPL.

Пам'ятайте, що пакети Coalton, включаючи `#:coalton-user`, *не* використовують `:use` пакет `#:common-lisp`/`#:cl`, тому Вам потрібно додавати залежності Common Lisp, використовуючи `cl:`, якщо вони вам потрібні.


## Змінні та функції

Змінні та функції визначаються за допомогою `define`. Ось кілька варіантів визначень змінних.

```lisp
(coalton-toplevel
  ;; Змінні визначаються за допомогою ключового слова define
  (define x 5)
  (define y 6)
  (define z (+ x y))
  (define p (Tuple 1.0 2.0))

  ;; Coalton підтримує цілі числа, рядки, булеві значення та unit як примітивні типи
  (define name "Alyssa P. Hacker")
  (define hungry True)
  (define data Unit))
```

Можна отримати перший елемент кортежу `p`, визначеного вище, з REPL, використовуючи оператор `coalton`:
```lisp
(coalton (fst p))
```

**Зауважте**: може виникнути бажання відкинути слово `coalton` і просто обчислити `(fst p)` безпосередньо в Lisp REPL, але на таку поведінку не можна покладатися!

Функції визначаються аналогічно змінним. На відміну від Common Lisp, функції Coalton займають тей самий простір імен, що й змінні. Це полегшує функціональне програмування.

```lisp
(coalton-toplevel
  ;; Функції також визначаються за допомогою ключового слова define
  (define (add2 x)
    (+ 2 x))

  ;; Функції існують в тому ж просторі імен, що й змінні
  (define addTwo add2)

  (define x (addTwo 3))

  ;; Анонімні функції можна визначити за допомогою fn
  (define z (map (fn (x) (+ 2 x)) (make-list 1 2 3 4))))
```


### Функції та каррування

*Усі* функції в Coalton приймають рівно один аргумент і повертають рівно одне значення. Розглянемо цю функцію:

```lisp
(coalton-toplevel
  (define (fma a b c)
    (+ c (* a b))))
```

Правда в тому, що `fma` технічно приймає *один аргумент*: `a`. Для людини, знайомої з Common Lisp, ця функція приблизно еквівалентна:

```lisp
(defun fma (a)
  (lambda (b)
    (lambda (c)
      (+ c (* a b)))))
```

Хоча Coalton приховує цю реальність від вас, якщо тільки вона вам не потрібна:

```lisp
(coalton-toplevel
  (define fma1 (fma 2))      ; equiv: b -> (c -> (c + 2*b))
  (define fma2 (fma 2 3))    ; equiv: c -> (c + 6)
  (define fma3 (fma 2 3 4))) ; equiv: 10
```

Ми бачимо, що можна викликати `fma`, як ніби це була функція з трьома аргументами, але це просто зручний синтаксис. Ми також можемо викликати її з меншою кількістю аргументів. Іноді цю властивість називають *каррованими функціями*.

Coalton працює над оптимізацією цих функцій, щоб мінімізувати виділення замикань. Фактично, вираз `(fma x y z)` буде скомпільований як просте додавання та множення, без виділення замикань.

Ось приклад використання каррованої функції для перетворення списку.

```lisp
(coalton-toplevel
  ;; Списки можна створювати за допомогою макросу make-list
  (define nums (make-list 2 3 4 5)))

(coalton
  ;; Функції в Coalton є каррованими
  (map (+ 2) nums)) ;; 4 5 6 7
```

### Синтаксис конвейєрів і композиція функцій

Існує зручний синтаксис для написання функцій за допомогою макросів `pipe` та `nest`.

```lisp
(nest f g ... h x)

;; еквівалентно

(f (g (... (h x))))

;; еквівалентно

(pipe x h ... g f)
```

Такий синтаксис робить код чистішим.

Зауважте, що оскільки це макроси (що видно по їх варіативним аргументам), вони *не можуть* використовуватися як функції вищого порядку. Розгляньте можливість використання каррування або використання `compose`, якщо ви думаєте в цьому напрямку.

### Ігнорування параметрів функції

Розглянемо функцію:

```lisp
(coalton-toplevel
  (define (f x y)
    x))
```

Тут `y` не використовується і викличе попередження:

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

Як запропоновано, можна замінити `y` на `_y`, що повідомляє компілятору Coalton, що параметр може бути навмисно не використаний.

**Зауважте**: змінні з префіксом `_`, такі як `_y`, все ще є звичайними змінними і можуть бути прочитані. Наступний приклад є допустимим кодом Coalton:

```lisp
(define (f _x) _x)
```

Ви повинні поводитися з змінними з префіксом як з ігнорованими, коли це можливо, і використовувати ім'я без префікса `_`, якщо воно може бути використане. Читання зі змінних з префіксом `_` дозволено, щоб згенерований код (наприклад, з використанням макросів або умовних зчитувань) не виводив попереджень про невикористані змінні, які будуть використовуватися в деяких контекстах компіляції, але не в інших.

## Типи даних

Coalton дозволяє визначати параметричні алгебраїчні типи даних.

```lisp
(coalton-toplevel
  ;; Нові типи створюються за допомогою оператора DEFINE-TYPE
  (define-type Point3D (Point3D Integer Integer Integer))

  ;; Coalton підтримує сумарні типи
  (define-type Color
    Red
    Blue
    Green)

  ;; Coalton підтримує узагальнені змінні типів
  ;;
  ;; Типові параметри визначаються за допомогою аргументів ключового слова
  (define-type (Tree :a)
    (Branch (Tree :a) :a (Tree :a))
    (Leaf :a)))
```

Визначення типів представляють типізовані конструктори. Наприклад, ми можемо створити дерево наступним чином.

```lisp
(coalton
  (Branch (Leaf Red)
          Green
          (Branch (Leaf Red) Green (Leaf Red))))
```

Ми побачимо, як розпакувати ці типи за допомогою `match` пізніше в цьому документі.

## Псевдоніми типів

Coalton дозволяє визначати параметричні псевдоніми типів. Псевдоніми типів можуть бути визначені для примітивних типів і типів, створених за допомогою `define-type` або `define-type-alias`.

```lisp
(coalton-toplevel
  ;; Нові псевдоніми типів створюються за допомогою оператора DEFINE-TYPE-ALIAS
  (define-type-alias Coordinate Integer)
  (define-type-alias (Pair :a) (Tuple :a :a))
  (define-type-alias Translation (Pair Coordinate -> Pair Coordinate))
  
  (declare shift-right Translation)
  (define (shift-right (Tuple x y))
    (Tuple (1+ x) y))
    
  (define shifted-coordinate (shift-right (Tuple 0 0))))

  ;; Типові псевдоніми можуть мати кілька параметрів
  (define-type-alias (MyTuple3 :a :b :c) (Tuple :a (Tuple :b :c)))

  ;; Типові псевдоніми можуть мати параметри, які не мають виду *
  (define-type-alias (IntegerCollection :col) (:col Integer))

  ;; Типові псевдоніми можуть посилатися на типи, які не мають виду *
  (define-type-alias MyCollection List)
```

Параметричні псевдоніми типів повинні бути повністю застосовані.
```lisp
(coalton-toplevel

  (define-type (T :a) (ConstrT (:a Integer)))
  
  (define-type-alias (MyCollection1 :a) (List :a))
  (define-type-alias MyCollection2 List)

  ;; Ця лінія не скомпілюється, оскільки MyCollection1 має
  ;; параметр :A, який не застосований
  (define-type-alias A (T MyCollection1))

  ;; Проте ця лінія скомпілюється
  (define-type-alias A (T MyCollection2)))
```

Існує кілька інструментів налагодження, які корисні при роботі з псевдонімами типів. Поза виразом Coalton можна використати функцію `describe-type-of`, яка відображає тип змінної, включаючи її псевдоніми, і повертає тип. Функція `describe-type-alias` відображає псевдонім разом із типом, на який він посилається, і повертає цей тип. Крім того, Coalton можна налаштувати так, щоб він відображав лише псевдоніми, лише типи або обидва варіанти при відображенні типу, пов'язаного зі змінною. Перевагу можна встановити перед компіляцією Coalton за допомогою `(setf (get ':coalton-config ':type-printing-mode) mode)`, де `mode` - це один із `:types`, `:aliases` та `:types-and-aliases`. Після цього режим можна змінити між цими трьома варіантами за допомогою функції `set-type-printing-mode`. Режим за замовчуванням - `:types`.

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

COALTON-USER> shifted-coordinate ;; із попереднього прикладу
#.(TUPLE 1 0)

COALTON-USER> (type-of 'shifted-coordinate)
(TUPLE INTEGER INTEGER)

COALTON-USER> (describe-type-of 'shifted-coordinate)
[(PAIR COORDINATE) := (TUPLE [COORDINATE := INTEGER] [COORDINATE := INTEGER])]

COALTON-USER> (describe-type-alias 'Pair)
[(PAIR :A) := (TUPLE :A :A)]
```

### Структури

Також є підтримка визначення структур.

```lisp
(coalton-toplevel
  (define-struct Point
    (x Integer)
    (y Integer)))
```

Структуры схожі на ADT (АТД) з одним конструктором і створюються аналогічно:

```lisp
(coalton (Point 1 2))
```

Для доступу до полів структури використовуються аксесори:

```lisp
(coalton (.x (Point 1 2)))
```

Доступ до полів структури може бути переданий за значенням:

```lisp
(coalton (map .x (make-list (Point 1 2) (Point 2 3))))
```

Структури також можуть бути параметричними:

```lisp
(coalton-toplevel
  (define-struct (Pair :a)
    (first :a)
    (second :a)))
```

## Цикли та ітерації

Coalton пропонує три основні способи виконання явних циклів:

- Хвостова рекурсія и `rec`,
- Вбудовані конструкції циклів на основі ітераторів, та
- Експериментальні макроси ітерування у стилі Lisp (наприклад, `dotimes`).

### Хвостова рекурсія и `rec`

Coalton прагне забезпечити усунення хвостових викликів. Таким чином, хвостова рекурсія є ідіоматичною в Coalton.

```lisp
(coalton-toplevel
  (define (find-integer predicate limit)
    (cond
      ((negative? limit) False)
      ((predicate limit) True)
      (True              (find-integer predicate (1- limit))))))
```

Ця функція може бути виконана для довільно великого `limit` без впливу на стек:

```lisp
> (coalton (find-integer (fn (x) (== 65536 (* x x))) 1000000))
COMMON-LISP:T
```

Coalton має спеціальний вбудований оператор `rec` для виконання ітерації типу "named-let". Його загальний синтаксис:

```lisp
(rec <name> (<binding>*)
  <body>)

;; <name> := <symbol>
;;         | (<symbol> <type>)
;;
;; <binding> := (<symbol> <value>)
```

`<name>` по суті є локальною (хвостово-рекурсивною) функцією в `<body>`. Опціонально, тип цієї функції може бути оголошений, що корисно для уникнення небажаного поліморфізму. Оператор `rec` не вимагає, щоб виклик був у хвостовій позиції.

Ідіоматично в Coalton вибирають ім'я `%`, коли немає іншого розумного імені.

Ось приклад `rec` для реалізації реверсу списку.

```lisp
(coalton-toplevel
  (define (rev l)
    (rec % ((result Nil) (remaining l))
      (match remaining
        ((Nil) result)
        ((Cons x xs) (% (Cons x result) xs))))))
```

Ось приклад функції Фібоначчі, яка працює для будь-якого числового вхідного та вихідного значення:

```lisp
(coalton-toplevel
  (define (fib-polymorphic n)
    (rec % ((i n) (a 0) (b 1))
      (cond
        ((== 0 i) a)
        ((== 1 i) b)
        (True (% (- i 1) b (+ a b)))))))
```

Тип функції `∀ A B. (NUM B) (NUM A) ⇒ (A → B)` і, отже, може бути використаний для будь-яких числових типів:

```
> (coalton (the F32 (fib-polymorphic (the Integer 10))))
55.0
```

Ми можемо зробити мономорфний варіант для `Integer`, оголосивши тип рекурсії.

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

Зараз це працює без будь-яких оголошень типів при використанні:

```
> (coalton (fib-monomorphic 10))
55
```

### Вбудовані конструкції циклів

Coalton підтримує нескінченні цикли, умовні цикли та ітерації типу `for`.

#### `loop`, `while`, `while-let`, and `for`

Можна зациклитися назавжди 

```lisp
(loop (trace "hi"))
```

Можна зациклитися, доки деяка умова істинна

```lisp
(coalton
 (let ((counter (cell:new 0))
       (limit 10))
   (while (< (cell:read counter) limit)
     (trace "hi") 
     (cell:increment! counter))))
```

Можна зациклитися, доки патерн співпадає

```lisp
(coalton
 (let ((xs (vector:make 4 3 2 1)))
   (while-let (Some x) = (vector:pop! xs)
              (traceobject "x" x))))
```

Можна ітеруватися по елементах `IntoIterator`

```lisp
(coalton
 (for x in "coalton"
      (traceobject "x" x)))
```


#### `break` and `continue`

Кожна з вищезгаданих форм циклів підтримує `break` та `continue`.

Конструкція `break` негайно завершує ітерацію. Наступний приклад виводить `c`, `o` та `a`, а потім завершується.

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (break))
      (traceobject "x" x)))
```

Конструкція `continue` пропускає решту тіла циклу і починає наступну ітерацію. Наступний приклад виводить `c`, `o`, `a`, `t`, `o` та `n`, пропускаючи виведення `l`.

```lisp
(coalton
 (for x in "coalton"
      (when (== x #\l)
        (continue))
      (traceobject "x" x)))
```


#### Мітки циклів

Кожна з вищезгаданих форм циклів може приймати необов'язкову мітку циклу. Ці мітки можуть використовуватися разом із `break` та `continue` для досягнення складного керування ітерацією.

Для кожної з форм циклів мітка може слідувати безпосередньо за відкриваючим терміном циклу:

```lisp 

(loop :outer (do-stuff))

(while :a-label (is-true?) (do-stuff))

(while-let :another-label 
   (Some thing) = (get-something)
   (do-stuff thing))

(for :iter word in words 
   (do-stuff-with word))

```

У наступному повністю вигаданому прикладі зовнішній цикл позначений як `:outer`. Ця мітка передається в `break` зсередини внутрішнього циклу `while`, щоб припинити ітерацію, коли сума акумулятора та лічильника перевищить 500. Без мітки `:outer`, `break` вийшов би лише з внутрішнього циклу `while`.

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

### Експериментальні макроси ітерації Lisp

> [!УВАГА]
> Ці конструкції є експериментальними та можуть змінитися.

Стандартна бібліотека-пакет [`coalton-library/experimental/loops`](https://coalton-lang.github.io/reference/#coalton-library/experimental/loops-package) містить багато макросів ітерації Lisp, включаючи `dotimes` та `dolist`, які працюють аналогічно своїм аналогам у Common Lisp.

Ці функції зазвичай повертають `Unit`, якщо вони спеціально не збирають або не накопичують значення. Лічильники (як у `dotimes`) та індекси (як у `dolist-enumerated`) зазвичай є `UFix`.

Припустимо, у нас є `dotimes`, доступний з нашого пакету, ми могли б написати функцію для виведення Піфагорових трійок наступним чином:

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

Coalton підтримує числові типи. Основні з них - це `Integer`, `F32` та `F64`.

```lisp
(coalton-toplevel
  (define num-int 5)
  (define num-sf  5.0f0)
  (define num-df  5.0d0))
```

Можна опустити суфікс і просто написати `5.0`, який буде розпізнаний залежно від значення `cl:*read-default-float-format*` під час читання. Ця змінна за замовчуванням встановлена в `cl:single-float`, що означає, що не відредаговані числа з плаваючою точкою будуть одинарної точності.

Існують також інші, більш обмежені цілочисельні типи, такі як цілі числа з фіксованою шириною зі знаком (`I32` та `I64`) та цілі беззнакові числа з фіксованою шириною (`U8`, `U32`, `U64`).

Нарешті існує тип відношення, який називається `Fraction`, і який є відношенням двох значень `Integer`.

Числа наслідують клас типу `Num`, який має методи `+`, `-`, `*` та `fromInt`.

### Поділ, коротко

Поділ - це складна тема; див. наступний розділ. Ось кілька швидких порад.

- Оператор поділу `/` та його варіанти можуть призвести до помилок під час виконання, якщо дільник дорівнює нулю. Використовуйте `safe/`, якщо ви віддаєте перевагу поверненню `Optional`.

- Якщо у вас є цілі числа і Ви хочете отримати число з плаваючою точкою, використовуйте `inexact/`. (Для одинарної точності використовуйте `into`.)

- Якщо у вас є цілі числа і Ви хочете отримати цілочисельну відповідь, використовуйте `floor/`, `ceiling/` або `round/`.

- Якщо ви намагаєтеся поділити два цілі числа, щоб отримати точну раціональну відповідь, використовуйте `exact/`.

- Якщо ви пишете узагальнений код, який використовує ділення, або хочете залишатися абстрактним, вивчіть `/` і як правильно його обмежити.

### Деталі про поділ

Чому класс типов `Num` не має оператора поділу `/`?

Coalton дійсно має оператор ділення `/`, але це окрема, трохи складніша концепція. Ділення складне з двох причин:

1. Поділ може завершитися помилкою, якщо ми ділимо на щось на зразок нуля,

2. Поділ двох чисел не обов'язково призводить до того ж типу. (Фактично, поділ може бути навіть неможливим!)

Щоб вирішити першу проблему, ділення може призвести до помилки під час виконання. Ми не використовуємо `Optional`, тому що це досить громіздко в математичних контекстах. (Можна використовувати `safe/` для варіанту, який перевіряє ділення на нуль і повертає `Optional`.)

Щоб вирішити другу проблему, нам потрібно ввести новий клас типів, який називається `Dividable`. Вираз типу

```
(Dividable :s :t)
```

каже про те, що ділення двох елементів типу `:s` може призвести до елемента типу `:t`. З урахуванням усього цього, ми отримуємо остаточний тип `/`.

```
COALTON-USER> (type-of '/)
∀ :A :B. DIVIDABLE :A :B ⇒ (:A → :A → :B)
```

Оскільки в Coalton діє [Instance Defaulting](#instance-defaulting), ділення констант `Integer` без додаткового контексту за замовчуванням призводить до ділення `F64`:

```
COALTON-USER> (coalton (/ 1 2))
0.5d0
```

Ми можемо повідомити Coalton, що наші константи мають інший тип, обмеживши їх за допомогою `the` або покладаючись на виведення типів. Наприклад, щоб отримати результат не-`F64` з вхідних даних `Integer`, ви повинні обмежити тип результату до бажаного типу (за умови, що для цього типу визначено екземпляр класу типів `Dividable`):

```
COALTON-USER> (coalton (the F32 (/ 4 2)))
2.0
COALTON-USER> (coalton (the Fraction (/ 4 2)))
#.(COALTON-LIBRARY::%FRACTION 2 1)
```

Результат `Integer` від поділу з `/` неможливий, оскільки екземпляр `Dividable Integer Integer` не визначений:

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

Чому це не просто `2`?! На жаль, відповідь полягає в тому, що `/` *не завжди* може призвести до цілого числа `2`, і коли воно не ділиться націло, Coalton не нав'язує певний спосіб округлення. Таким чином, правильний спосіб зробити це - розділити точно, а потім округлити за вашим бажанням за допомогою `floor`, `ceiling` або `round`.

```
COALTON-USER> (coalton (floor (the Fraction (/ 4 2))))
2
```

Ось приклади того, що відбувається, коли ви вибираєте значення, які не діляться націло.

```
COALTON-USER> (coalton (floor (the Fraction (/ 3 2))))
1
COALTON-USER> (coalton (ceiling (the Fraction (/ 3 2))))
2
COALTON-USER> (coalton (round (the Fraction (/ 3 2))))
2
```

Усі ці випадки досить поширені, тому ми надаємо кілька скорочень:

- `safe/` щоб виконати перевірку поділу на нуль і повернути `None`, якщо це так,

- `exact/` для поділу двох цілих числа з отриманням дробу (що замінює все наведене вище використання `the Fraction`),

- `inexact/` для поділу двох цілих числа з одержанням числа подвійної точності

- `floor/`, `ceiling/`, та `round/` для розподілу цілого числа на ціле число.

Дроби можуть бути перетворені на інші розділені типи за допомогою `fromfrac` (Примітка: це може призвести до втрати точності):

```
COALTON-LIBRARY/MATH/REAL> (coalton (the F64 (fromfrac 1/2)))
0.5d0
COALTON-LIBRARY/MATH/REAL> (coalton (the F32 (fromfrac 999/1000)))
0.999
```

## Списки

Coalton використовує списки Lisp "під капотом". Списки можна створювати за допомогою `make-list`.

```lisp
(coalton-toplevel
  (define x (make-list 1 2 3))
  (define y (make-list "a" "b" "c")))
```


Усі елементи списку повинні бути одного типу. Це означає, що наступний код призводить до помилки типу.

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

Списки також можна деконструювати за допомогою `match`. 

```lisp
(coalton-toplevel
  (define (is-empty lst)
    (match lst
      ((Cons _ _) "is not empty")
      ((Nil) "is empty"))))
```

## Статична типізація

Код Coalton визначає та перевіряє типи під час компіляції. Типи будуть автоматично призначені.

```lisp
(coalton-toplevel
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

Тип змінної або функції можна перевірити за допомогою `coalton:type-of`.

```
COALTON-USER> (type-of 'fun)
(STRING -> (OPTIONAL INTEGER)
```

Типи завжди можна додати вручну.

```lisp
(coalton-toplevel
  (declare fun (String -> (Optional Integer)))
  (define (fun x)
    (map (+ 2) (str:parse-int x))))
```

Типи також можна оголошувати у виразах `let`.

```lisp
(coalton-toplevel
  (define (f a b)
    (let ((declare g (Integer -> Integer -> Integer))
          (g +))
      (g a b))))
```

### Перетворення, приведення та конвертація типів

Coalton керує перетворенням типів, аналогічно функції Common Lisp `cl:coerce`, за допомогою класу типів `Into` (з пакету `#:coalton-library/classes`) та його єдиного методу `into`. Однак метод `into` приймає лише один аргумент. Як Coalton визначає, у який тип даних виконувати перетворення? Він визначає це або шляхом виведення типів (тобто за допомогою навколишнього контексту), як у цьому прикладі, де `substring` очікує `String`:

```lisp
(coalton-toplevel
  (define integer-part (substring (into 12.34d0) 0 2)))

;; ==> "12"
```

або за допомогою явного оголошення за допомогою `the`:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (the (List Char) (into "mississippi")))))

;; ==> (#\m #\s #\p #\i)
```

Патерн `the`-`into` настільки поширений, що Coalton надає скорочення, яке називається `as`. Наведений вище приклад можна записати більш лаконічно наступним чином:

```lisp
(coalton-toplevel
  (define unique-letters
    (remove-duplicates (as (List Char) "mississippi"))))

;; ==> (#\m #\s #\p #\i)
```

Метод `into` використовується тільки тоді, коли перетворення завжди може бути виконане з одного типу в інший. Якщо значення не може бути перетворене в тип, то використовується інший клас `TryInto` з методом `tryInto`. Метод `tryinto` повертає тип `Result`, який вказує, чи було перетворення успішним чи ні.

**Нотатка: `as` працює лише для перетворень через `into`, тобто повних перетворень.** Немає відповідного синтаксису для `tryInto`.

## Зіставлення зі зразком

Вираз `match` може використовуватися для зіставлення з шаблоном і деконструкції алгебраїчних типів даних.

```lisp
(coalton-toplevel
  (define-type Color
    Red
    Blue
    Green)

  ;; Конструктори повинні бути обгорнуті в дужки
  (declare color-to-string (Color -> String))
  (define (color-to-string c)
    (match c
      ((Red) "Red")
      ((Blue) "Blue")
      ((Green) "Green")))

  ;; Змінні не повинні бути обгорнуті в дужки
  (declare map-optional ((:a -> :b) -> (Optional :a) -> (Optional :b)))
  (define (map-optional f x)
    (match x
      ((Some x_) (Some (f x_)))
      ((None) None)))

  ;; Патерни можуть бути вкладеними, і підтримуються шаблони підстановки "_"
  (declare flatten-optional ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten-optional x)
    (match x
      ((Some (Some x_)) (Some x_))
      (_ None)))

  ;; Підшаблони можуть бути захоплені в змінну
  (declare dedup-head (Eq :a => List :a -> List :a))
  (define (dedup-head xs)
    "If the first and second member of list are equal, drop the first"
    (match xs
      ((Cons a (= tl1 (Cons b _))) 
       (if (== a b) tl1 xs))
      (_ xs)))

  ;; Числа або рядки також можна зіставляти
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

Функції можуть зіставляти свої аргументи з шаблоном, але шаблони повинні бути вичерпними.

```lisp
(coalton-toplevel
  (declare first (Tuple :a :b -> :a))
  (define (first (Tuple a _)) a)

  (declare second (Tuple :a :b -> :b))
  (define second (fn ((Tuple _ b)) b))

  ;; паттерни підхоплення також працюють 
  (declare nest-right (Tuple :a :b -> (Tuple :a (Tuple :a :b))))
  (define (nest-right (= tpl (Tuple a _))) (Tuple a tpl)))

```

Оператор `coalton-library:if` може використовуватися як скорочення при зіставленні з шаблоном на булевих значеннях:

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

Декілька виразів `if` можна об'єднати за допомогою `cond`:

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

Бінарні логічні оператори `and` та `or` (з `coalton-library`) насправді є варіативними макросами, які використовують обчислення за короткою схемою. Їх функціональними аналогами є `boolean-and` та `boolean-or`.

```lisp
(coalton
  (or (cheap 5) True (really-expensive (expt 2 1000000))))
```

У цьому випадку `really-expensive` ніколи не буде викликана через обчислення за короткою схемою. Також зверніть увагу, що і `and`, і `or` можуть приймати один або кілька аргументів.


## `COALTON-LIBRARY:PROGN`

У Coalton є конструкція `coalton-library:progn`, схожа на Lisp.

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> (Tuple Integer Integer)))
  (define (f x y)
    (progn
      (+ x y)
      (* x y)
      (Tuple x y))))
```

Конструкція `progn` у Coalton може мати спрощений синтаксис `let`.

```lisp
(coalton-toplevel
 (declare f (Integer -> Integer -> String))
 (define (f x y)
   (progn
     (let x_ = (into x))
     (let y_ = (into y))
     (<> x_ y_))))
```

Спрощені вирази `let` підтримують зіставлення з шаблоном:

```lisp
(coalton-toplevel
  (declare f (Tuple Integer Integer -> Integer))
  (define (f t)
    (let (Tuple fst snd) = t)
    (+ fst snd)))

```

Спрощені вирази `let` не є рекурсивними і не підтримують поліморфізм `let`. Таким чином, наступний код є недопустимим:

```
(coalton
  (progn
    (let id = (fn (x) x))
    (id Unit)
    (id "hello")))
```

Проте, цей код працює зі звичайними виразами `let`:

```
(coalton
  (let ((id (fn (x) x)))
    (progn
      (id Unit)
      (id "hello"))))
```

Оголошення функцій створюють неявний блок `progn`:

```lisp
(coalton-toplevel
  (declare f (Integer -> Integer -> String))
  (define (f x y)
    (let x_ = (into x))
    (let y_ = (into y))
    (<> x_ y_)))
```

## Якщо ні та Коли

Пакет `coalton-library` також включає `unless` та `when`, які працюють аналогічно їх визначенням у Lisp. Ми рекомендуємо використовувати ці оператори лише для умовної обробки операцій зі станом.

```lisp
(coalton-toplevel
  (define (f x)
    (when (== x 5)
      (error "I only want the number 5"))))
```

`unless` та `when` створюють неявні блоки `progn`.

```lisp
(coalton-toplevel
  (define (f b)
    (when b
      (let x = 5)
      (let y = 7)
      (traceObject "sum" (+ x y)))))
```

## Дострокове повернення

Функції можуть бути повернені достроково за допомогою `return`.

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

## Класи типів

Coalton підтримує класи типів.

На цей час *усі* методи повинні бути визначені для кожного екземпляра класу типів.

```lisp
(coalton-toplevel
  ;; Типи класів визначаються за допомогою ключового слова define-class
  (define-class (Eq :a)
    (== (:a -> :a -> Boolean)))

  (define-type Color
    Red
    Green
    Blue)

  ;; Типи екземплярів класів визначаються за допомогою ключового слова define-instance
  (define-instance (Eq Color)
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Red) (Red)) True)
        ((Tuple (Blue) (Blue)) True)
        ((Tuple (Green) (Green)) True)
        (_ False)))
    (define (/= a b) (not (== a b))))

  ;; Типи оголошень можуть мати обмеження
  (declare is-eql (Eq :a => (:a -> :a -> String)))
  (define (is-eql a b)
    (if (== a b)
      "They are equal"
      "They are not equal"))

  ;; Кілька обмежень повинні бути укладені в дужки
  (declare double-is-eql ((Eq :a) (Eq :b) => (:a -> :a -> :b -> :b -> String)))
  (define (double-is-eql a b c d)
    (if (and (== a b) (== c d))
      "Both pairs are equal"
      "The pairs are not both equal")))
```


## Вбудовані типи класів

Основні класи типів, визначені в стандартній бібліотеці.

* `Eq` - визначен для порівнюємих типів
* `Ord` - визначено для типів, що впорядковуються
* `Num` - визначено для числових типів

* `Semigroup` - визначено для типів, що підтримують асоціативну бінарну операцію
* `Monoid` - визначено для типів, які є напівгрупами та мають елемент ідентичності


Кожний з наступних класів типів нагадує клас з тією ж назвою в Haskell, за винятком незначних відмінностей.

* `Functor` - `fmap` є `map` у Coalton
* `Applicative`
* `Monad` - монада не має `return`, замість цього використовуйте `pure` з додатку
* `Alternative` - `<|>` називається `alt` у Coalton
* `Foldable`
* `Traversable`

Ці класи типів натхненні типажами з тією ж назвою в Rust:

* `Into` - повні перетворення між одним типом та іншим
* `TryInto` - неповні перетворення між одним типом та іншим

## Автоматичне отримання типів екземплярів класів з Визначень Типів

Екземпляри деяких класів типів можуть бути отримані автоматично, щоб вони автоматично визначалися для користувацьких типів.

```lisp
(coalton-toplevel
  (derive Eq Hash)
  (define-struct Point
    (x UFix)
    (y UFix)))
```

Зараз ви можете використовувати методи `==` та `hash` для структур `Point`:

```lisp
(coalton
  ;; Тестує рівність `Point'
  (== (Point 1 2) (Point 3 3))

  ;; Викристовує `Point` як ключ у хеш-мапі
  (let map = (the (hashmap:HashMap Point UFix) hashmap:empty))
  (hashmap:insert map (Point 0 0) 1))
```

Сгенеровані екземпляри будуть "очевидним" у кожному випадку. Наприклад, рівність буде перевіряти кожне під-поле. Це може бути не завжди бажаним для кожного типу, і тому іноді все ще потрібні користувацькі екземпляри.

### Вбудовані класи типів `derive`

Екземпляри наступних класів можуть бути отримані автоматично:

- `Eq`
- `Hash`
- `Default`

Зараз це єдині класи, які можна отримати автоматично в стандартній бібліотеці, але в майбутньому можуть бути додані й інші.

Написання `custom derivers` ще не має офіційного API, але це можна зробити відносно легко для тих, кому це цікаво. Для отримання керівництва див. [derivers.lisp](./../library/derivers.lisp).

## Нотація `Do`

У Coalton є макрос `do`, який працює аналогічно нотації `do` в Haskell.

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

## Вбудовування анотації типів

Вбудовувані анотації типів можуть бути додані для розв'язання неоднозначностей при використанні типів класів.

```lisp
(coalton-toplevel
  (define f (the U32 (+ (fromInt 5) (fromInt 7)))))
```

## Скорочений синтаксис функцій

Coalton не має null-функцій. Однак функція з сигнатурою типу `Unit -> *` може бути викликана в Coalton без явної передачі `Unit`. Наприклад, форма Coalton `(coalton-library/vector:new)` є скороченням для `(coalton-library/vector:new Unit)`.

Функції також можуть бути визначені з неявним параметром за допомогою `(fn () 5)`. Це створює функцію з одним неявним параметром типу `Unit`.

## Інспектування системи Coalton

Пакет `coalton` визначає кілька функцій налагодження.

`type-of` та `kind-of` можна використовувати для перевірки типів визначень.

```
COALTON-USER> (type-of 'map)
∀ :A :B :C. FUNCTOR :C ⇒ ((:A → :B) → (:C :A) → (:C :B))
COALTON-USER> (kind-of 'Result)
* -> (* -> *)
```

Усі наступні функції приймають необов'язковий параметр пакету.

* `print-type-db` - вивести всі відомі типи
* `print-value-db` - вивести тип кожного глобального значення
* `print-class-db` - вивести кожен клас та їх методи
* `print-instance-db` - вивести екземпляри кожного класу

## Екземпляр за замовчуванням

В Coalton є система [типів за замовчуванням](https://www.haskell.org/onlinereport/decls.html#sect4.3.4), аналогічна Haskell. Типи за замовчуванням викликаються для неявно типізованих визначень і коду, скомпільованого за допомогою макросу `coalton`. Тип за замовчуванням застосовується до набору неоднозначних предикатів з метою розв'язання неоднозначного типу змінної в допустимий тип. Coalton використовуватиме тип за замовчуванням лише у тому випадку, якщо один або кілька предикатів є числовими класами типів (`Num`, `Quantizable`, `Reciprocable`, `ComplexComponent`, `Remainder`, `Integral`). Coalton встановить неоднозначну змінну типу в значення `Integer`, `F32` або `F64`, вибираючи перший тип, який дійсний для всіх предикатів, що посилаються на цю змінну. Coalton не використовуватиме тип за замовчуванням, якщо один або кілька предикатів, що містять неоднозначну змінну, відносяться до класу типів з кількома параметрами.

Відмінності від Haskell 98. Haskell вважав би `Num (List :a)` неоднозначним, Coalton за замовчуванням встановив би його в `Num Integer`. Haskell вважав би (`Num :a` `CustomTypeClass :a`) неоднозначним, Coalton за замовчуванням встановив би його в (`Num Integer` `CustomTypeClass Integer`), припускаючи, що `CustomTypeClass Integer` був допустимим екземпляром.

## Функціональні залежності

Функціональні залежності змушують дотримуватися відношення типів змінних класу для покращення виведення типів.

Клас `C` може бути визначений функціональною залежністю `(:a -> :b)` наступним чином:

`(define-class (C :a :b (:a -> :b)))`

Вираз `(:a -> :b)` можна прочитати як: для кожного `:a` існує лише один `:b` або, альтернативно, значення `:b` унікально визначається `:a`.

Якщо визначено екземпляр `(C String Integer)`, то визначення `(C String Char)` було б неправильним, оскільки для одного й того ж значення `:a` існує кілька значень `:b`.

Класи можуть мати кілька функціональних залежностей, кожна залежність може оголошувати кілька змінних класу з кожного боку `(:a :b -> :c :d :e)`, і залежності можуть бути рекурсивними `(:a -> :b) (:b -> :a)`.

## Спеціалізація

Coalton підтримує оптимістичну спеціалізацію функцій на основі типів. Спеціалізації функцій оголошуються за допомогою форми `specialize`:

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

Коли `inc` викликається з цілим числом, виклик буде прозоро переписаний на виклик `inc-int`.

```
COALTON-USER> (coalton (inc 1.2))
standard call
2.2
COALTON-USER> (coalton (inc 1))
int specialized call
2
```

Спеціалізація може застосовуватися лише тоді, коли типи аргументів у місці виклику відомі. Оскільки спеціалізація не гарантується, спеціалізовані функції повинні мати таку ж поведінку, як і їх неспеціалізовані варіанти. Спеціалізація повинна використовуватися лише для покращення продуктивності. Див. наступний приклад:

```
(coalton-toplevel
  (declare inc2 (Num :a => :a -> :a))
  (define (inc2 x)
    (inc x)))
```

Оскільки тип `x` у тілі `inc2` невідомий, спеціалізація не буде застосовуватися.

```
COALTON-USER> (coalton (inc2 1))
standard call
2
```

Спеціалізація може бути вказана в repl за допомогою `print-specializations`.


## Особливості та відмінності від Common Lisp

* Coalton використовує `False` (а не `t`). Таким чином, `t` може використовуватися як звичайне ім'я змінної.
* Для визначення рівності Coalton використовує подвійне рівно `==`.
* Списки в Coalton мають бути однорідними.
* Для позначення анонімних функцій Coalton використовує `fn` (*а не* `lambda`).
* Числові оператори, такі як `+`, приймають лише 2 аргументи.
* Заперечення здійснюється за допомогою `negate`. Форма `(- x)` є каррірованою функцією, еквівалентною `(fn (z) (- x z))`.

Щодо отримання додаткової інформації див. [глосарій](./glossary.md).

# Незавершений функціонал

Зараз Coalton підтримує ці функції, але для їх покращення потрібна ще велика робота.

## Обробка винятків

У Coalton є синтаксис для визначення, сигналізації, обробки та відновлення після виникнення виняткових умов.

Коротко, відповідні синтаксичні форми:

- `define-exception`: визначає тип винятка. Крім його імені, синтаксис ідентичний `define-type`.
- `define-resumption`: визначає іменований тип поновлення.
- `catch`: вираз для перехоплення та обробки винятків. Обробники зіставляють шаблони з конструкторами винятків.
- `throw`: сигналізує про виняток.
- `resumable`: вираз, який перехоплює та обробляє можливе поновлення. Випадки поновлення також виконуються шляхом зіставлення шаблонів з конструкторами поновлення.
- `resume-to`: вираз, який приймає екземпляр поновлення. Передає керування блоку `resumable`, який включає обробник для зазначеного поновлення.

Обробка винятків у Coalton ще не завершена і розвивається. Дизайн був обраний для експериментів і забезпечення сумісності в майбутньому в міру розвитку його функцій. Див. розділ "Попередження" нижче.

### Визначення, виведення та перехоплення винятків

Якщо ви хочете перехопити будь-який виняток, включаючи умови помилок Common Lisp, ви можете використовувати шаблон підстанови:

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

Більш загальний приклад

```lisp 
(define-type Egg
  ;;     розбите?  приготоване?
  (Goose Boolean Boolean)
  (Xenomorph))

;; Ми визначаємо тип винятка BadEgg з кількома варіантами
(define-exception BadEgg
  (UnCracked Egg)
  (DeadlyEgg Egg))

;; Якщо ми намагаємося розбити яйце Ксеноморфа, то виникне помилка DeadlyEgg
(declare crack (Egg -> Egg))
(define (crack egg)
  (match egg
    ((Goose _ cooked?)
     (Goose True cooked?))
    ((Xenomorph)
     (throw (DeadlyEgg egg)))))

;; розбиваємо яйце безпечно. 
(declare crack-safely (Egg -> (Result BadEgg Egg)))
(define (crack-safely egg)
  (catch (Ok (crack egg))
    ((DeadlyEgg _) (Err (DeadlyEgg egg)))
    ((UnCracked _) (Err (UnCracked egg)))))
```

### Визначення, виклик та обробка поновлень

Поновлення дозволяють програмісту Coalton відновитися після помилки без розгортання стеку викликів.

Вираз `define-resumption` приймає один "Constructor". Ім'я конструктора також є ім'ям типу поновлення.

Наступний код прояснює наведений вище приклад.

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

;; Повертаємо None якщо отримано відновлення SkipEgg.
(declare make-breakfast-with (Egg -> (Optional Egg)))
(define (make-breakfast-with egg)
  (resumable (Some (cook (crack egg)))
    ((SkipEgg) None)))
```

Тепер визначимо функцію, яка готує сніданок для `n` людей. Вона намагається приготувати кожне яйце, але якщо виникає помилка через вибір яйця Ксеноморфа, вона відновлює `make-breakfast`, пропускаючи це яйце.

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

Кожне 5-те яйце - це яйце Ксеноморфа, тому приготування сніданку для 10 осіб призведе до 8 яєць. 

Стек викликів виглядвє наступним чином:

```
make-breakfast-for 
      │
      └─ make-breakfast-with 
                │
                └─ cook  
```

Але `cook` сигналізує про помилку `DeadlyEgg` на яйцях `Xenomorph`. `make-breakfast-for` перехоплює цю помилку і відновлює `SkipEgg`, де `make-breakfast-with` отримує це відновлення та обробляє його.

### Застереження 

На цей час застосовуються такі застереження;

1. Не підтримується поліморфізм для виразів `throw` або `resume-to`. Наприклад, наступне не скомпілюється без інструкції типу:
   - `(define (th a) (throw a))` 
   - `(define (res a) (resume-to a))`

2. Неможливо `catch` умову Lisp і прив'язати її до змінної в обробнику `catch`. Проте, умови Lisp можуть бути перехоплені за допомогою шаблону підставляння.
Зокрема, це означає, що ви не можете повторно викликати виняток Lisp. Щобільше, ви можете повторно викликати виняток лише реконструювавши його. Наприклад:
   - `(catch (bad-thing) (_ Unit))` 
   - `(catch (bad-thing) ((MyBad x) (trace "my bad") (throw (MyBad x))))`

3. Відгалуження `resumable` ще більш обмежені. Ви не можете зіставляти будь-що, _крім_ шаблону конструктора відновлення.

4. Клас типів не може бути пов'язані з формами сигналізації винятків. Ми вивчаємо різні підходи до статичної перевірки форм, які можуть перестрибувати через стек викликів. Зрештою може перемогти підхід із класом типів.
   Що б ми не робили, ми постараємося зробити його сумісним із чинним синтаксисом та семантикою.


   

