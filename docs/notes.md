# Какие вообще есть способы композиции?

точка, $, & или |> (pipe)
<$>, <*>
>>= bind, >> fmap
=<<
<=< >=> рыба


В эликсире
только |>

В эрланге ничего

Реализация в Erlang работает только для одного типа возвращаемого значения: {ok, _} | {error, _}.
В Haskell это работает для любой монады.


# elixir

mix compile
iex -S mix
r MyModule

http://elixirschool.com/ru/lessons/specifics/debugging/