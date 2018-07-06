-module(main_1).

-export([main/0]).


-spec main() ->
    {ok, books_shop:order()} | {error, term()}.

main() ->
    handle_create_order(books_shop:test_data()).


-spec handle_create_order(map()) ->
    {ok, books_shop:order()} | {error, term()}.

handle_create_order(Data0) ->
    case books_shop:validate_incoming_data(Data0) of
        {error, Reason} -> {error, Reason};
        {ok, Data} ->
            #{
                <<"cat">> := Cat0,
                <<"address">> := Address0,
                <<"books">> := Books0
            } = Data,
            case books_shop:validate_cat(Cat0) of
                {error, Reason} -> {error, Reason};
                {ok, Cat} ->
                    case books_shop:validate_address(Address0) of
                        {error, Reason} -> {error, Reason};
                        {ok, Address} ->
                            Books1 = lists:map(
                                fun(#{<<"title">> := Title, <<"author">> := Author}) ->
                                    books_shop:get_book(Title, Author)
                                end,
                                Books0
                            ),
                            InvalidBooks = lists:filter(
                                fun
                                    ({error, _}) -> true;
                                    ({ok, _}) -> false
                                end,
                                Books1
                            ),
                            case InvalidBooks of
                                [Error | _] -> Error;
                                [] ->
                                    Books2 = lists:map(
                                        fun({ok, Book}) -> Book end,
                                        Books1
                                    ),
                                    books_shop:create_order(Cat, Address, Books2)
                            end
                    end
            end
    end.
