module Main where

import qualified BookShop as BS


handle_create_order :: BS.JsonData -> Either BS.ValidationError BS.Order
handle_create_order json_data =
  return json_data
  >>= validate_cat
  >>= validate_address
  >>= validate_books
  >>= \(c, a, bs) -> BS.create_order c a bs
  |> Right


validate_cat :: String -> Either BS.ValidationError (String, BS.Cat)
validate_cat json_data =
  case BS.validate_cat "Tihon" of
    Left err -> Left err
    Right cat -> Right (json_data, cat)


validate_address :: (String, BS.Cat) -> Either BS.ValidationError (String, BS.Cat, BS.Address)
validate_address (json_data, cat) =
  case BS.validate_address "Coolcat str 7/42 Minsk Belarus" of
    Left err -> Left err
    Right addr -> Right (json_data, cat, addr)


validate_books :: (String, BS.Cat, BS.Address) -> Either BS.ValidationError (BS.Cat, BS.Address, [BS.Book])
validate_books (json_data, cat, addr) =
  case get_books of
    Left err -> Left err
    Right books -> Right (cat, addr, books)
  where
    get_books = sequence $ map (\(t, a) -> BS.get_book t a) book_strs
    book_strs = [ ("Domain Modeling Made Functional", "Scott Wlaschin")
                , ("Distributed systems for fun and profit", "Mikito Takada")
                ]


(|>) :: a -> (a -> b) -> b
(|>) a f = f a


main :: IO()
main =
  case handle_create_order "My JSON Data" of
    Right order -> putStrLn $ show order
    Left error -> putStrLn $ show error
