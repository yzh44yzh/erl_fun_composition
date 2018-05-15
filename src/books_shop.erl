-module(books_shop).

-export_type([cat/0, address/0, book/0, order/0]).

-export([
    test_data/0,
    validate_incoming_data/1,
    validate_cat/1,
    validate_address/1,
    get_book/2,
    validate_incoming_data_ex/1,
    validate_cat_ex/1,
    validate_address_ex/1,
    get_book_ex/2,
    create_order/3
]).


%%% Types

-type cat() :: {cat, binary()}.

-type address() :: {address, binary()}.

-record(book, {
    id :: binary(),
    title :: binary(),
    author :: binary()
}).
-type book() :: #book{}.

-record(order, {
    customer :: cat(),
    shipping_address :: address(),
    books :: [book()]
}).
-type order() :: #order{}.


%%% Test Data

-spec test_data() -> map().
test_data() ->
    #{
        <<"cat">> => <<"Tihon">>,
        <<"address">> => <<"Coolcat str 7/42 Minsk Belarus">>,
        <<"books">> => [
            #{
                <<"title">> => <<"Domain Modeling Made Functional">>,
                <<"author">> => <<"Scott Wlaschin">>
            },
            #{
                <<"title">> => <<"Удовольствие от Х"/utf8>>,
                <<"author">> => <<"Стивен Строгац"/utf8>>
            },
            #{
                <<"title">> => <<"Distributed systems for fun and profit">>,
                <<"author">> => <<"Mikito Takada">>
            }
        ]
    }.


%%% Module API

-spec validate_incoming_data(JsonData :: map()) ->
    {ok, map()} | {error, invalid_incoming_data}.

validate_incoming_data(JsonData) ->
    %% jesse:validate("order", JsonData)
    case rand_success() of
        true -> {ok, JsonData};
        false -> {error, invalid_incoming_data}
    end.


-spec validate_cat(CatName :: binary()) ->
    {ok, cat()} | {error, cat_not_found}.

validate_cat(CatName) ->
    %% find cat in database
    case rand_success() of
        true -> {ok, {cat, CatName}};
        false -> {error, cat_not_found}
    end.


-spec validate_address(Address :: binary()) ->
    {ok, address()} | {error, invalid_address}.

validate_address(Address) ->
    %% send request to third-party service
    case rand_success() of
        true -> {ok, {address, Address}};
        false -> {error, invalid_address}
    end.


-spec get_book(Title :: binary(), Author :: binary()) ->
    {ok, book()} | {error, {book_not_found, binary()}}.

get_book(Title, Author) ->
    %% find book in database
    case rand_success() of
        true ->
            {ok, #book{
                id = <<"ISBN 978-5-00057-917-6">>,
                title = Title,
                author = Author
            }};
        false -> {error, {book_not_found, Title}}
    end.


-spec create_order(Cat :: cat(), Address :: address(), Books :: [book()]) ->
    order().

create_order(Cat, Address, Books) ->
    #order{
        customer = Cat,
        shipping_address = Address,
        books = Books
    }.


%%% Module API with Exceptions

-spec validate_incoming_data_ex(JsonData :: map()) -> map().
validate_incoming_data_ex(JsonData) ->
    %% jesse:validate("order", JsonData)
    case rand_success() of
        true -> JsonData;
        false -> throw({error, invalid_incoming_data})
    end.


-spec validate_cat_ex(CatName :: binary()) -> cat().
validate_cat_ex(CatName) ->
    %% find cat in database
    case rand_success() of
        true -> {cat, CatName};
        false -> throw({error, cat_not_found})
    end.


-spec validate_address_ex(Address :: binary()) -> address().
validate_address_ex(Address) ->
    %% send request to third-party service
    case rand_success() of
        true -> {address, Address};
        false -> throw({error, invalid_address})
    end.


-spec get_book_ex(Title :: binary(), Author :: binary()) -> book().
get_book_ex(Title, Author) ->
    %% find book in database
    case rand_success() of
        true ->
            #book{
                id = <<"ISBN 978-5-00057-917-6">>,
                title = Title,
                author = Author
            };
        false -> throw({error, {book_not_found, Title}})
    end.


%%% Internal functions

-spec rand_success() -> boolean().
rand_success() ->
    Rand = rand:uniform(10),
    if
        Rand > 1 -> true;
        true -> false
    end.
