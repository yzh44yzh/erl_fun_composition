defmodule Main5 do

    @spec main :: {:ok, BookShop.Order.t} | {:error, term}
    def main do
        BookShop.test_data |> handle_create_order
    end


    @spec handle_create_order(map) :: {:ok, BookShop.Order.t} | {:error, term}
    def handle_create_order data0 do
        with {:ok, data} <- BookShop.validate_incoming_data(data0),
             %{"cat" => cat0, "address" => address0, "books" => books0} = data,
             {:ok, cat} <- BookShop.validate_cat(cat0),
             {:ok, address} <- BookShop.validate_address(address0),
             books1 = Enum.map(books0,
                 fn %{"title" => title, "author" => author} ->
                     BookShop.get_book title, author
                 end),
             {:ok, books2} <- Pipeline.sequence(books1),
             do: {:ok, BookShop.create_order(cat, address, books2)}
    end

end
