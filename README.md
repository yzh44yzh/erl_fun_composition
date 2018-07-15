В данном учебном проекте я рассматриваю способы композиции функций в Elixir и Erlang.

На примере конкретной прикладной задачи я предлагаю 5 вариантов композиции, начиная с простых и понятных, двигаюсь к более сложным способам, характерным для функционального программирования. Анализирую плюсы и минусы каждого варианта, рекомендую, что лучше использовать в реальном проекте.


# Задача

У нас есть книжный магазин для котов. Он принимает заказы и доставляет книги.

У магазина есть API для создания заказа.

На входе API принимает json-данные, содержащие информацию о коте-заказчике, его адрес, и книги, которые кот хочет заказать.

Например:
```
{
  "cat": "Tihon",
  "address": "Coolcat str 7/42 Minsk Belarus",
  "books": [
    {"title": "Domain Modeling Made Functional", "author": "Scott Wlaschin"},
    {"title": "Удовольствие от Х", "author": "Стивен Строгац"},
    {"title": "Distributed systems for fun and profit", "author": "Mikito Takada"}
  ]
}
```

На выходе из API мы имеем валидный бизнес-объект **Order**, который передается дальше в систему для обработки, или ошибку валидации.

Для проверки данных и создания валидного объекта Order нужно выполнить следующие шаги:
- проверить кота по имени
- проверить его адрес
- проверить каждую книгу в списке
- создать Order

Для этого у нас есть следующие функции:
```
@spec validate_incoming_data(map) :: {:ok, map} | {:error, :invalid_incoming_data}

@spec validate_cat(binary) :: {:ok, cat} | {:error, :cat_not_found}

@spec validate_address(binary) :: {:ok, address} | {:error, :invalid_address}

@spec get_book(binary, binary) :: {:ok, Book.t} | {:error, {:book_not_found, binary}}

@spec create_order(cat, address, [Book.t]) :: Order.t
```

То есть, у нас есть 3 функции, которые могут вернуть успешный результат, либо ошибку. Четвертая функция, которую нужно применить несколько раз к элементам списка. И, наконец, пятая функция, которая всегда возвращает успешный результат.

Нужно выполнить композицию этих функций.

[BookShop.ex](./src_ex/BookShop.ex)


# Вариант 1. Решение в лоб -- вложенные case.

[Main1.ex](./src_ex/Main1.ex)

Здесь получилось 4 уровня вложенности. Пока что это не так страшно. Но что, если понадобится добавить еще один шаг валидации? Два шага? Десять? Или переставить некоторые шаги местами?

Такой код -- явный пример, как не надо делать. Тем не менее, тут есть пару плюсов. Во-первых, он работает. Во-вторых, он хорошо проверяется dialyzer.

Я не поленился создать пользовательские типы данных и написать spec ко всем функциям. Так что теперь dialyzer может проверить правильность композиции функций.


# Вариант 2. Каждый case в отдельную функцию.

[Main2.ex](./src_ex/Main2.ex)

У нас получилось 5 небольших функций, вызывающих друг друга по очереди. И некий общий **State**, который проходит через все эти вызовы. State нужен, чтобы накапливать промежуточные результаты и передавать их дальше.

Каждая функция маленькая и понятная. Тут легко добавить два, пять, десять, сколько угодно новых шагов валидации. Легко менять их местами.

dialyzer по-прежнему контролирует правильность композиции. Но за правильностью использования State разработчику придется следить самому. Тут появляются возможности для ошибок.

Кроме того, функции похожи, они повторяют одинаковый шаблон. И это наводит на мысль, что можно что-то обобщить, сократить количество кода.


# Вариант 3. Решение с использованием исключений.

[Main3.ex](./src_ex/Main3.ex)

Шаблонность кода вызвана тем, что результат каждого вызова нужно проверить на ошибку. Попробуем переделать модуль BookShop, чтобы его функции сообщали об ошибках через исключения, а не через возвращаемое значение.

Получилось очень просто, лаконично. Код пишем только для happy path, **try..catch** решает все остальные проблемы. Красота!

В таком простом примере это решение может показаться самым лучшим. Но в больших проектах исключения создают некоторые проблемы.

Во-первых, понадобится много разных типов исключений и стратегия их использования -- какой тип для чего применять. Если разработчиков на проекте больше одного, то таких стратегий может оказаться больше одной. И тогда неизвестные исключения вдруг прилетают из неожиданных мест.

Во-вторых, вызывая функцию, разработчик не может знать всех возможных вариантов ее завершения. Разве что прочитает ее код и проследит внутренние вызовы на всю глубину. В решениях 1 и 2 все возможные варианты завершения функции описаны в ее спецификации. По-хорошему, спецификация могла бы содержать информацию, какие исключения могут возникнуть (как в Java). Но в Erlang этого нет.

В использовании исключений ничего плохого нет (даже если некоторые ФП программисты будут говорить вам обратное). Но в мире Erlang исключения не очень популярны.


# Вариант 4. Pipeline, bind и sequence.

Исключения дали нам возможность сосредоточиться на happy path и не мучиться с шаблонной обработкой ошибок. В функциональном программировании есть другие инструменты с таким же эффектом.

Сначала пару слов про Haskell. Дело в том, что Haskell может показать нужные нам идеи в эталонном виде. А в Elixir/Erlang мы может реализовать только что-то похожее, с некоторым приближением. Так что прежде, чем смотреть на искаженную копию, сперва посмотрим на оригинал.

Нам нужны те же функции валидации, реализованные на Haskell: [BookShop.hs](./src_hs/BookShop.hs)

Тип данных **Either ErrResult SuccessResult** -- это аналог нашего **{:ok, success_result} | {:error, error_result}**.

Теперь мы будем соединять эти функции в цепочку. Это легко, когда результат одной функции совпадает с аргументом другой. Но что делать, если не совпадает? Собственно, вокруг этого и строится все ФП :)

Если на выходе из функции нужное нам значение завернуто в Either, а на входе другой функции это значение нужно в чистом виде, то соединить эти две функции можно оператором **bind**.

```
fun1 >>= fun2
```

Это оператор делает именно то, что делали наши маленькие функции во 2-м варианте -- с помощью case проверяет результат первой функции, и либо вызывает следующую, либо возвращает ошибку.

Имея несколько таких функций, их можно соединить в цепочку оператором bind:
```
fun1 >>= fun2 >>= fun3 >>= fun4
```

Получится то же самое, что во 2-м варианте, но без явных case.

Решение на Haskell выглядит так: [Main4.hs](./src_hs/Main4.hs).

С Elixir нас ожидает трудность -- нет оператора bind. Но способ убрать явный case есть. Мы можем цепочку функций представить как список функций, и выполнить свертку над этим списком.
```
Pipeline.bind data, [
    &fun1/1,
    &fun2/1,
    &fun3/1,
    &fun4/1
]).
```

Свертка реализуется тривиально: [Pipeline.ex](./src_ex/Pipeline.ex)
и позволяет получить лаконичный код: [Main4.ex](./src_ex/Main4.ex).

Отдельно посмотрим на валидацию списка книг. Прогнав книги через **BookShop.get_book/2** мы получим:
```
[{:ok, Book1}, {:ok, Book2}, {:ok, Book3}]
```

Но нам нужно другое. Нам нужно:
```
{:ok, [Book1, Book2, Book3]}
```

Первое во второе легко превратить с помощью **sequence**. Это стандартная функция в Haskell, и ее легко реализовать в Elixir.

Есть еще одна проблема в Elixir, которой нет в Haskell. Если предыдущие варианты хорошо контролировались dialyzer, то 4-й вариант уже нет. Dialyzer ничего не может сказать о том, подходят ли функции в списке друг к другу. Поэтому лучше всего, чтобы все функции были одинаковые по сигнатуре:
```
@spec fun(my_state) -> {:ok, my_state} | {:error, some_error}.
```
Что, собственно, и сделано в 4-м варианте (кроме последней функции).

4-й вариант лаконичнее 2-го варианта. Но выигрыш не такой большой, потому что задача для него не самая подходящая. Функции **BookShop** не ложатся в pipeline непосредственно, их нужно оборачивать. Другое дело, если бы BookShop специально писался под использование в pipeline, тогда выигрыш был бы больше.

На самом деле, эту задачу я составлял для 5-го варианта :)


# Вариант 5. do-нотация для Elixir.

Давайте посмотрим, как это выглядит в Haskell: [Main5.hs](./src_hs/Main5.hs)

А выглядит это очень похоже на вариант 3 с исключениями. Только здесь нет исключений :)

Мы видим два способа получить результат выполнения функции. Стрелка влево (**<-**) извлекает результат из Either, знак присваивания (**=**) извлекает обычное значение. Полученными значениями можно пользоваться ниже. Если какая-то функция вернет ошибку, то выполнение блока **do** прерывается, и ошибка возвращается как результат.

Для Haskell это самый простой и лаконичный вариант. А что с Elixir? У нас есть специальная форма [with](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#with/1), которая работает похожим образом. Мы можем точно так же извлекать значения стрелкой влево, или пользоваться обычными значениями. И точно также вычисления прекращаются, если шаблон слева от стрелки не совпал.

Получается такой код: [Main5.ex](./src_ex/Main5.ex). Здесь почти все хорошо, только аргументы функций обязательно нужно окружать скобками. Увы, любимый мною безскобочный стиль в Elixir работает далеко не везде :(

Можно попробовать пойти дальше, и взять настоящие монады, а не их упрощенные имитации. Для Elixir есть библиотека [Monad](https://hexdocs.pm/monad/Monad.html), и в ней монада [Error](https://hexdocs.pm/monad/Monad.Error.html), которая подходит для нашего случая.

Концептуально это все тот же тип данных **{:ok, success_result} | {:error, error_result}**, но обернутый в более сложную сущность. Для нас сейчас важно, что эта сущность поддерживает do-нотацию.

И получается такой код: [Main6.ex](./src_ex/Main6.ex). Разница с Main5 не большая. Синтаксис ближе к Haskell, и можно вызывать функции без скобок.

Интересно, что в разных языках эта монада называется по-разному: в Haskell -- Either, в OCaml -- Result, в Elixir -- Error. Название Result мне кажется самым подходящим.


# Вариант 5. do-нотация для Erlang.

С Erlang ситуация сложнее, встроенных средств языка нет.
Но можно попробовать придумать некий DSL, реализующий ту же идею: [main_5.erl](./src_erl/main_5.erl).

Получилось заметно сложнее, и вряд ли понятно без дополнительный пояснений. Мы опять видим список. Здесь элементы списка не просто функции, а кортежи из трех элементов, где посередине находится функция, слева атом, а справа либо атом, либо что-то другое. Можно догадаться, что справа -- аргументы функции, а слева ее результат.

Где-то в недрах реализации прячется контекст -- обыкновенная **map**. Атомы слева и справа -- это ключи, по которым читаются и записываются значения в контекст. Аргументом функции может быть либо конкретное значение, либо атом, и тогда значение берется из контекста. Результат функции по заданному ключу сохраняется в контексте. Последний сохраненный результат -- это результат всего блока **do**. Если какая-то функция вернет ошибку, то выполнение всего блока прекращается, и возвращается ошибка.

DSL на самом деле простой, и его реализация тоже простая: [wannabe_haskell.erl](./src_erl/wannabe_haskell.erl).

Для эксперимента это неплохо. При желании можно как-то развивать такой DSL, но брать в реальные проекты вряд ли стоит.

С DSL понятно, но нет ли для Erlang библиотек с монадами? Есть, конечно. Для нашего случая подойдет библиотека [erlando](https://github.com/rabbitmq/erlando) от создателей rabbitmq. У Erlang, конечно, не такие возможности для метапрограммирования, как у Elixir, но авторам удалось сделать неплохую штуку. Они взяли синтаксис lists comprehension и превратили его в do-нотацию. Получилось неплохо.

C монадой **error_m** и **{parse_transform, do}** получается такой вариант: [main_6.erl](./src_erl/main_6.erl).


# В чем принципиальна разница между pipeline и do-notation?

pipeline лучше всего подходит там, где нужно просто передавать выход одной функции на вход другой. Но у нас есть промежуточные результаты, которые нужно где-то сохранить, чтобы использовать позже. Из-за этого появляется некое состояние.

Для pipeline это состояние приходиться прокидывать через все функции. Значит, функции должны знать про состояние и уметь с ним работать. Таким образом, не любую функцию можно положить в pipeline. Функции BookShop напрямую использовать нельзя, приходиться делать для них обертки, поддерживающие состояние.

В случае с do-нотацией состояние существует отдельно. Можно использовать любые функции, и мы напрямую используем BookShop, без всяких оберток.


# Выводы

1-й вариант -- это очевидный пример того, как делать не надо.

Остальные варианты вполне годятся для использования в реальных проектах.

5-й вариант на Elixir получился лучше, на Erlang хуже. На Elixir лучше просто взять **with**, т.к. монады добавят мало чего полезного в рамках данной задачи. Для Erlang лучше взять **erlando**, чем кастомный DSL.

Напоследок порекомендую книгу [Domain Modeling Made Functional. Scott Wlaschin](https://pragprog.com/book/swdddf/domain-modeling-made-functional). Прекрасное введение в функциональное программирование.
