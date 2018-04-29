-module(pipeline).

-export_type([result/0, pipeline_fun/0]).
-export([pipeline/2, sample/1]).

-type result() :: {ok, term()} | {error, term()}.

-type pipeline_fun() :: fun((term()) -> result()).

-spec pipeline(term(), [pipeline_fun()]) -> result().
pipeline(Arg, Funs) ->
    lists:foldl(
        fun
            (Fun, {ok, PrevRes}) -> Fun(PrevRes);
            (_, {error, Error}) -> {error, Error}
        end,
        {ok, Arg},
        Funs).


-spec sample(integer()) -> result().
sample(In) ->
    pipeline(In, [
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


%% This doesn't help:

%% -type result(Ok, Error) :: {ok, Ok} | {error, Error}.

%% -type pipeline_fun(InArg, OkRes, ErrRes) :: fun((InArg) -> result(OkRes, ErrRes)).

%% -spec sample(integer()) ->
%%     result(
%%         [{integer(), integer()}],
%%         {atom(), integer()}
%%     ).