-module(pipeline).

-export_type([result/0, pipeline_fun/0]).

-export([
    bind/2,
    sequence/1,
    sample/1,
    sample_2/0
]).


%%% Types

-type result() :: {ok, term()} | {error, term()}.

-type pipeline_fun() :: fun((term()) -> result()).

%% This doesn't help:
%%
%% -type result(Ok, Error) :: {ok, Ok} | {error, Error}.
%%
%% -type pipeline_fun(InArg, OkRes, ErrRes) :: fun((InArg) -> result(OkRes, ErrRes)).
%%
%% -spec sample(integer()) ->
%%     result(
%%         [{integer(), integer()}],
%%         {atom(), integer()}
%%     ).


%%% Module API

-spec bind(term(), [pipeline_fun()]) -> result().
bind(Arg, Funs) ->
    lists:foldl(
        fun
            (Fun, {ok, PrevRes}) -> Fun(PrevRes);
            (_, {error, Error}) -> {error, Error}
        end,
        {ok, Arg},
        Funs).


-spec sequence([result()]) -> result().
sequence([]) -> {ok, []};
sequence([{error, Error} | _]) -> {error, Error};
sequence([{ok, Value} | Tail]) ->
    case sequence(Tail) of
        {ok, List} -> {ok, [Value | List]};
        {error, Error} -> {error, Error}
    end.


%%% Samples

-spec sample(integer()) -> result().
sample(In) ->
    bind(In, [
        fun(A) -> {ok, A + 10} end,
        fun(A) ->
            if
                A > 0 -> {ok, A * 2};
                %% A > 0 -> {ok, <<"some">>}; % dialyzer can't find this
                A =< 0 -> {error, {invalid_arg, A}}
            end
        end,
        fun(A) -> {ok, [A, A, A]} end,
        fun(AList) -> {ok, lists:map(fun(A) -> {A, A+1} end, AList)} end
    ]).


-spec sample_2() -> ok.
sample_2() ->
    List1 = [{ok, 1}, {ok, 2}, {ok, 3}, {ok, 4}],
    Res1 = sequence(List1),
    io:format("List1:~p, Res1:~p~n", [List1, Res1]),

    List2 = [{ok, 1}, {error, something_wrong_2}, {error, something_wrong_3}, {ok, 4}],
    Res2 = sequence(List2),
    io:format("List2:~p, Res2:~p~n", [List2, Res2]),
    ok.