-module(books_shop_for_cats).

-export_type([cat/0, address/0, book/0, order/0]).

-export([
    validate_incoming_data/2,
    validate_cat/1,
    validate_address/1,
    get_book/2,
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


%%% Module API

-spec validate_incoming_data(SchemaName :: binary(), JsonData :: map()) ->
    {ok, map()} | {error, term()}.
validate_incoming_data(_SchemaName, JsonData) ->
    %% jesse:validate(SchemaName, JsonData)
    {ok, JsonData}.


-spec validate_cat(CatName :: binary()) -> {ok, cat()} | {error, cat_not_found}.
validate_cat(CatName) ->
    %% find cat in database
    {ok, {cat, CatName}}.


-spec validate_address(Address :: binary()) -> {ok, address()} | {error, invalid_address}.
validate_address(Address) ->
    %% send request to third-party service
    {ok, {address, Address}}.


-spec get_book(Title :: binary(), Author :: binary()) -> {ok, book()} | {error, not_found}.
get_book(Title, Author) ->
    %% find book in database
    #book{
        id = <<"ISBN 978-5-00057-917-6">>,
        title = Title,
        author = Author
    }.

-spec create_order(Cat :: cat(), Address :: address(), Books :: [book()]) -> order().
create_order(Cat, Address, Books) ->
    #order{
        customer = Cat,
        shipping_address = Address,
        books = Books
    }.

