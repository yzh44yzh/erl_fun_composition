-module(main_5).

-export([main/0]).


-spec main() -> {ok, books_shop:order()} | {error, term()}.
main() ->
    handle_create_order(books_shop:test_data()).


-spec handle_create_order(map()) -> {ok, books_shop:order()} | {error, term()}.
handle_create_order(Data) ->
    wannabe_haskell:do([
        {data, fun books_shop:validate_incoming_data/1, Data},

        {cat_0, fun(#{<<"cat">> := Cat0}) -> {ok, Cat0} end, data},
        {cat, fun books_shop:validate_cat/1, cat_0},

        {address_0, fun(#{<<"address">> := Address0}) -> {ok, Address0} end, data},
        {address, fun books_shop:validate_address/1, address_0},

        {books_0, fun(#{<<"books">> := Books0}) -> {ok, Books0} end, data},
        {books,
            fun(Books0) ->
                pipeline:sequence(
                    lists:map(
                        fun(#{<<"title">> := Title, <<"author">> := Author}) ->
                            books_shop:get_book(Title, Author)
                        end,
                        Books0
                    ))
            end,
            books_0},

        {order, fun books_shop:create_order/3, [cat, address, books]}
    ]).

