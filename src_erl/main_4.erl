-module(main_4).


-export([main/0]).


-spec main() -> {ok, books_shop:order()} | {error, term()}.
main() ->
    handle_create_order(books_shop:test_data()).


-spec handle_create_order(map()) -> {ok, books_shop:order()} | {error, term()}.
handle_create_order(Data) ->
    pipeline:bind(Data, [
        fun books_shop:validate_incoming_data/1,
        fun validate_cat/1,
        fun validate_address/1,
        fun validate_books/1,
        fun create_order/1
        ]).


-spec validate_cat(map()) -> {ok, map()} | {error, term()}.
validate_cat(#{<<"cat">> := Cat0} = State) ->
    case books_shop:validate_cat(Cat0) of
        {ok, Cat} -> {ok, State#{cat => Cat}};
        Error -> Error
    end.


-spec validate_address(map()) -> {ok, map()} | {error, term()}.
validate_address(#{<<"address">> := Address0} = State) ->
    case books_shop:validate_address(Address0) of
        {ok, Address} -> {ok, State#{address => Address}};
        Error -> Error
    end.


-spec validate_books(map()) -> {ok, map()} | {error, term()}.
validate_books(#{<<"books">> := Books0} = State) ->
    Books1 = lists:map(
        fun(#{<<"title">> := Title, <<"author">> := Author}) ->
            books_shop:get_book(Title, Author)
        end,
        Books0
    ),
    case pipeline:sequence(Books1) of
        {ok, Books2} -> {ok, State#{books => Books2}};
        Error -> Error
    end.


-spec create_order(map()) -> {ok, books_shop:order()}.
create_order(#{cat := Cat, address := Address, books := Books}) ->
    Order = books_shop:create_order(Cat, Address, Books),
    {ok, Order}.
