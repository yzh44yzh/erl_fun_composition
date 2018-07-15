-module(main_6).
-compile({parse_transform, do}).

-export([main/0]).


-spec main() -> {ok, books_shop:order()} | {error, term()}.
main() ->
    handle_create_order(books_shop:test_data()).


-spec handle_create_order(map()) -> {ok, books_shop:order()} | {error, term()}.
handle_create_order(Data0) ->
    do([error_m ||
        Data <- books_shop:validate_incoming_data(Data0),
        #{<<"cat">> := Cat0, <<"address">> := Address0, <<"books">> := Books0} = Data,
        Cat <- books_shop:validate_cat(Cat0),
        Address <- books_shop:validate_address(Address0),
        Books1 = lists:map(
            fun(#{<<"title">> := Title, <<"author">> := Author}) ->
                books_shop:get_book(Title, Author)
            end,
            Books0),
        Books2 <- pipeline:sequence(Books1),
        books_shop:create_order(Cat, Address, Books2)
    ]).

