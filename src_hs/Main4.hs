module Main where

import qualified BookShop as BS
import qualified System.Random as SR


handle_create_order :: BS.JsonData -> Either BS.ValidationError BS.Order
handle_create_order json_data =
  return json_data
  >>= validate_cat
  >>= validate_address
  >>= validate_books
  >>= \(c, a, bs) -> Right $ BS.create_order c a bs


validate_cat :: BS.JsonData -> Either BS.ValidationError (BS.JsonData, BS.Cat)
validate_cat json_data =
  let (cat, _, _) = json_data in
    BS.validate_cat cat
    >>= \cat -> Right (json_data, cat)


validate_address :: (BS.JsonData, BS.Cat) -> Either BS.ValidationError (BS.JsonData, BS.Cat, BS.Address)
validate_address (json_data, cat) =
  let (_, address, _) = json_data in
    BS.validate_address "Coolcat str 7/42 Minsk Belarus"
    >>= \addr -> Right (json_data, cat, addr)


validate_books :: (BS.JsonData, BS.Cat, BS.Address) -> Either BS.ValidationError (BS.Cat, BS.Address, [BS.Book])
validate_books (json_data, cat, addr) =
  get_books book_str >>= \books -> Right (cat, addr, books)
  where
    (_, _, book_str) = json_data
    get_books bs = sequence $ map (\(t, a) -> BS.get_book t a) bs


main :: IO()
main =
  do
    rand <- SR.randomRIO (1 :: Integer, 2 :: Integer)
    let test_data = if rand == 1 then BS.test_data else BS.test_data'
    case handle_create_order test_data of
      Right order -> print order
      Left error -> print error
