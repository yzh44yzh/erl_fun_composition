module Main where

import qualified BookShop as BS
import qualified System.Random as SR

handle_create_order :: BS.JsonData -> Either BS.ValidationError BS.Order
handle_create_order json_data =
  do
    (cat_name, addr_str, book_strs) <- BS.validate_incoming_data json_data
    cat <- BS.validate_cat cat_name
    address <- BS.validate_address addr_str
    books <- sequence $ map (\(t, a) -> BS.get_book t a) book_strs
    Right $ BS.create_order cat address books


main :: IO()
main =
  do
    rand <- SR.randomRIO (1 :: Integer, 2 :: Integer)
    let test_data = if rand == 1 then BS.test_data else BS.test_data'
    case handle_create_order test_data of
      Right order -> print order
      Left error -> print error
