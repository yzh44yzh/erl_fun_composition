Учебный проект, где рассматриваются различные способы композиции функций в Erlang.

# Intro

Эрланг -- язык с динамической типизаций.

С одной стороны это просто, не нужно заморачиваться со сложными вариантами композиции, как в Haskell пиши что хочешь, компилятор всем доволен.

С другой стороны сложно. Если типы и правильность композиции не проверяет компилятор, то эти приходится делать программисту в уме.

С третьей стороны, у нас есть dialyzer. Он весьма крут во многих случаях. И хорошо бы получать от него пользу, где это возможно.


# Задача

У нас есть книжный магазин для котов. Он принимает заказы и доставляет книги.

У магазина есть API для создания заказа.

На входе API принимает json-данные, содержащие информацию о коте-заказчике, его адресс и книги, которые кот хочет заказать.

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

На выходе валидный бизнес-объект типа Order, который передается дальше в систему для обработки; или ошибка, которая возвращается клиенту API, если с данными что-то не так.

Для проверки данных и создания валидного объекта Order нужно выполнить следующие шаги:
- проверить кота по имени
- проверить его адрес
- проверить каждую книгу в списке
- создать Order

Для этого у нас есть следущие функции:
```
-spec validate_incoming_data(JsonData :: map()) -> {ok, map()} | {error, term()}.

-spec validate_cat(CatName :: binary()) -> {ok, cat()} | {error, cat_not_found}.

-spec validate_address(Address :: binary()) -> {ok, address()} | {error, invalid_address}.

-spec get_book(Title :: binary(), Author :: binary()) -> {ok, book()} | {error, not_found}.

-spec create_order(Cat :: cat(), Address :: address(), Books :: [book()]) -> order().
```

Нужно выполнить композицию этих функций.



# Вариант 1. Решение в лоб

Плюсы: очевидное
Минусы: трудно читать, трудно менять. А что, если шагов не 4, а 15?


# Вариант 2. Решение на исключениях

Работает. Читаемо. Расширяемо. Имеет полное право на существование.
Но трудно отследить все точки выхода, найти все варианты, что может пойти не так.
ну и это не ФП путь


# Вариант 3. Прагматично, без этих ваших монад

Плюсы: понятное, читаемое, расширяемое
Минусы: копипаста, шаблонный код. Можно бы подсунуть сюда макросы, но нет, я предпочитаю копипастить.


# Вариант 4. Pipeline. Список функций и fold по этому списку.

Плюсы: понятное, читаемое, расширяемое
Минус: типы проверяются диалайзером только поверхносно. {ok, term()} | {error, term()}.
а что там в term(), не проверяется.

Два способа:
- монада Eigher
- State на входе, State на выходе. Содержит все, что нужно всему pipeline.

В первом варианте можно составлять композицию из более общих функций.
о втором варианте это специализированые функции под конкретную задачу.
В любом случае нам нужно делать обертки к имеющимся 4-м функциям, чтобы адаптировать их вход-выход

частичного применения нет, но можно делать такие обертки:
validate_by_schema(JsonData) ->
    validate_by_schema(JsonData, <<"my_schema">>).
с помощью этих оберток можно как-то адаптировать выход-выход функций, если нужно


# Вариант 5. Компоизиция как в Haskell: bind, fmap, вот это вот все.

Увы, для красивого читаемого кода нужна инфиксная нотация вызова функции:
arg1 fun arg2
и это дает
fun1 bind fun2 bind fun3 bind fun4
Но в эрланге этого нет. Можно сделать
bind(bind(bind(fun1, fun2), fun3), fun4)
но это не сильно лучше варианта 1 с вложеными case.

Вместо этого можно придумать DSL

validate_something(SomeData) ->
    Composition = {{bind, fun1},
                   {fmap, fun2},
                   {bind, fun3},
                   {fmap, fun4}},
    execure(Composition).

Для такого DSL наверняка можно написать реализацию так, чтобы типы проверялись диалайзером.
Но я не уверен, что получится понятно и просто.

В реальности на практике я применяю варианты 3 и 4.
4й вариант подразумевает, что все функции соединяются одинаково.
В 3м варианте можно выбрать способ композиции для каждой функции отдельно.
5-й годится для экспериментов.


# Либы

https://github.com/fogfish/datum
кажется, тут больше про структуры данных, чем про композицию функций

https://github.com/habibutsu/erlz
https://github.com/yzh44yzh/erlz
а вот тут как раз про композицию. Но эта библиотека без ориентации на dialyzer, так что пользы от нее меньше, чем хотелось бы.
