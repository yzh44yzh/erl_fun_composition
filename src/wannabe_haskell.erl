-module(wannabe_haskell).

-export([do/1]).

-type step() :: {Key :: atom(), StepFun :: fun(), Args :: term()}.

-spec do([step()]) -> {ok, term()} | {error, term()}.
do(Steps) ->
    State = #{last_result => undefined},
    State2 = lists:foldl(fun do_step/2, State, Steps),
    #{last_result := Reply} = State2,
    case Reply of
        {error, Error} -> {error, Error};
        Res -> {ok, Res}
    end.


do_step(_, #{last_result := {error, _}} = State) -> State;

do_step({Key, StepFun, Args}, State) when is_list(Args) ->
    Args2 = lists:map(
        fun
            (Arg) when is_atom(Arg) -> maps:get(Arg, State);
            (Arg) -> Arg
        end,
        Args),
    case erlang:apply(StepFun, Args2) of
        {ok, Res} -> State#{Key => Res, last_result => Res};
        {error, Error} -> State#{last_result => {error, Error}};
        Res -> State#{Key => Res, last_result => Res}
    end;

do_step({Key, StepFun, Arg}, State) ->
    do_step({Key, StepFun, [Arg]}, State).