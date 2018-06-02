defmodule BookShop do

  # Types

  @type cat :: {:cat, binary}
  @type address :: {:address, binary}

  defmodule Book do
    defstruct [:id, :title, :author]
    @enforce_keys [:id, :title, :author]
    @type t :: %Book{id: binary, title: binary, author: binary}
  end

  defmodule Order do
    alias BookShop, as: BS
    defstruct [:customer, :shipping_address, :books]
    @enforce_keys [:customer, :shipping_address, :books]
    @type t :: %Order{customer: BS.cat, shipping_address: BS.address, books: [Book.t]}
  end


  # Test Data

  @spec test_data() :: map
  def test_data do
    %{
      "cat" => "Tihon",
      "address" => "Coolcat str 7/42 Minsk Belarus",
      "books" => [
        %{
          "title" => "Domain Modeling Made Functional",
          "author" => "Scott Wlaschin"
        },
        %{
          "title" => "Удовольствие от Х",
          "author" => "Стивен Строгац"
        },
        %{
          "title" => "Distributed systems for fun and profit",
          "author" => "Mikito Takada"
        }
      ]
    }
  end


  # Module API

  @spec validate_incoming_data(map) :: {:ok, map} | {:error, :invalid_incoming_data}
  def validate_incoming_data json_data do
    case rand_success() do
      true -> {:ok, json_data}
      false -> {:error, :invalid_incoming_data}
    end
  end


  @spec validate_cat(binary) :: {:ok, cat} | {:error, :cat_not_found}
  def validate_cat cat_name do
    case rand_success() do
      true -> {:ok, {:cat, cat_name}}
      false -> {:error, :cat_not_found}
    end
  end


  # Internal functions

  @spec rand_success() :: boolean
  def rand_success do
    rand = :rand.uniform 10
    rand > 1
  end

end
