module Main where

import qualified BookShop as BS


handle_create_order :: BS.JsonData -> Either BS.ValidationError BS.Order
handle_create_order json_data =
  do
    json_data2 <- BS.validate_incoming_data json_data
    let cat_name = get_cat_name json_data2
    let addr_str = get_addr_str json_data2
    let book_strs = get_book_strs json_data2
    cat <- BS.validate_cat cat_name
    address <- BS.validate_address addr_str
    books <- sequence $ map (\(t, a) -> BS.get_book t a) book_strs
    let order = BS.create_order cat address books
    Right order


get_cat_name :: BS.JsonData -> String
get_cat_name _ = "Tihon"


get_addr_str :: BS.JsonData -> String
get_addr_str _ = "Coolcat str 7/42 Minsk Belarus"


get_book_strs :: BS.JsonData -> [(String, String)]
get_book_strs _ = [ ("Domain Modeling Made Functional", "Scott Wlaschin")
                  , ("Distributed systems for fun and profit", "Mikito Takada")
                  ]


main :: IO()
main =
  case handle_create_order "My JSON Data" of
    Right order -> print order
    Left error -> print error
