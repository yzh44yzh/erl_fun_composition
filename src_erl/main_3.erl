-module(main_3).

-export([main/0]).


-spec main() -> {ok, books_shop:order()} | {error, term()}.
main() ->
    handle_create_order(books_shop:test_data()).


-spec handle_create_order(map()) ->
    {ok, books_shop:order()} | {error, term()}.

handle_create_order(Data0) ->
    try
        Data = books_shop:validate_incoming_data_ex(Data0),
        #{
            <<"cat">> := Cat0,
            <<"address">> := Address0,
            <<"books">> := Books0
        } = Data,
        Cat = books_shop:validate_cat_ex(Cat0),
        Address = books_shop:validate_address_ex(Address0),
        Books = lists:map(
            fun(#{<<"title">> := Title, <<"author">> := Author}) ->
                books_shop:get_book_ex(Title, Author)
            end,
            Books0
        ),
        Order = books_shop:create_order(Cat, Address, Books),
        {ok, Order}
    catch
        throw:Error -> Error
    end.

