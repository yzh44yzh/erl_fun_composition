defmodule Main1 do

    @spec main :: {:ok, BookShop.Order.t} | {:error, term}
    def main do
        BookShop.test_data |> handle_create_order
    end


    @spec handle_create_order(map) :: {:ok, BookShop.Order.t} | {:error, term}
    def handle_create_order data do
        case BookShop.validate_incoming_data data do
            {:error, reason} -> {:error, reason}
            {:ok, %{
                "cat" => cat0,
                "address" => address0,
                "books" => books0
            }} ->
                case BookShop.validate_cat cat0 do
                    {:error, reason} -> {:error, reason}
                    {:ok, cat} ->
                        case BookShop.validate_address address0 do
                            {:error, reason} -> {:error, reason}
                            {:ok, address} -> {:error, :some}
                        end
                end
        end
    end

end
