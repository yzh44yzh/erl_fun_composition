-module(main_2).

-export([main/0]).


-spec main() -> {ok, books_shop:order()} | {error, term()}.
main() ->
    handle_create_order(books_shop:test_data(), #{}).


%% don`t care to describe type for State
-spec handle_create_order(map(), map()) -> {ok, books_shop:order()} | {error, term()}.
handle_create_order(Data0, State) ->
    case books_shop:validate_incoming_data(Data0) of
        {error, Reason} -> {error, Reason};
        {ok, Data} -> validate_cat(Data, State)
    end.


-spec validate_cat(map(), map()) -> {ok, books_shop:order()} | {error, term()}.
validate_cat(#{<<"cat">> := Cat0} = Data, State) ->
    case books_shop:validate_cat(Cat0) of
        {error, Reason} -> {error, Reason};
        {ok, Cat} -> validate_address(Data, State#{cat => Cat})
    end.


-spec validate_address(map(), map()) -> {ok, books_shop:order()} | {error, term()}.
validate_address(#{<<"address">> := Address0} = Data, State) ->
    case books_shop:validate_address(Address0) of
        {error, Reason} -> {error, Reason};
        {ok, Address} -> check_books(Data, State#{address => Address})
    end.


-spec check_books(map(), map()) -> {ok, books_shop:order()} | {error, term()}.
check_books(#{<<"books">> := Books0}, State) ->
    Books = lists:map(
        fun(#{<<"title">> := Title, <<"author">> := Author}) ->
            books_shop:get_book(Title, Author)
        end,
        Books0
    ),
    validate_books(Books, State).


%% don`t care to describe type for Books
-spec validate_books(list(), map()) -> {ok, books_shop:order()} | {error, term()}.
validate_books(Books, State) ->
    InvalidBooks = lists:filter(
        fun
            ({error, _}) -> true;
            ({ok, _}) -> false
        end,
        Books
    ),
    case InvalidBooks of
        [Error | _] -> Error;
        [] -> create_order(State#{books => Books})
    end.


-spec create_order(map()) -> {ok, books_shop:order()}.
create_order(#{cat := Cat, address := Address, books := Books0}) ->
    Books = lists:map(
        fun({ok, Book}) -> Book end,
        Books0
    ),
    Order = books_shop:create_order(Cat, Address, Books),
    {ok, Order}.
