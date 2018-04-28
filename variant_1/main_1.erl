-module(main_1).

-export([
]).

-spec validate_something(term()) -> ok | {error, term()}.
validate_something(SomeData) ->
    case check_rule_1(SomeData) of
        false -> {error, rule_1_error};
        true ->
            case check_rule_2(SomeData) of
                false -> {error, rule_2_error};
                true ->
                    case check_rule_3(SomeData) of
                        false -> {error, rule_3_error};
                        true ->
                            case check_rule_4(SomeData) of
                                false -> {error, rule_4_error};
                                true -> ok
                            end
                    end
            end
    end.
