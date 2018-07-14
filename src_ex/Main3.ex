defmodule Main3 do

    @spec main :: {:ok, BookShop.Order.t} | {:error, term}
    def main do
        BookShop.test_data |> handle_create_order
    end


    @spec handle_create_order(map) :: {:ok, BookShop.Order.t} | {:error, term}
    def handle_create_order data0 do
        try do
            data = BookShop.validate_incoming_data_ex data0
            %{
                "cat" => cat0,
                "address" => address0,
                "books" => books0
            } = data
            cat = BookShop.validate_cat_ex cat0
            address = BookShop.validate_address_ex address0
            books = Enum.map books0, fn %{"title" => title, "author" => author } ->
                BookShop.get_book_ex title, author
            end
            order = BookShop.create_order cat, address, books
            {:ok, order}
        catch
            {:error, reason} -> {:error, reason}
        end
    end

end