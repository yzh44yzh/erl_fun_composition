module BookShop where

import qualified System.Random as SR

data Cat = Cat String deriving Show

data Address = Address String deriving Show

data Book =
  Book { id :: String
       , title :: String
       , author :: String
       } deriving Show

data Order =
  Order { customer :: Cat
        , shipping_address :: Address
        , books :: [Book]
        } deriving Show

data ValidationError
  = InvalidIncomingData
  | CatNotFound
  | InvalidAddress
  | BookNotFound
  deriving Show


type JsonData = (String, String, [(String, String)])

test_data :: JsonData
test_data =
    ("Tihon", "Coolcat str 7/42 Minsk Belarus",
    [ ("Scott Wlaschin", "Domain Modeling Made Functional")
    , ("Стивен Строгац", "Удовольствие от Х")
    , ("Mikito Takada", "Distributed systems for fun and profit")
    ])


test_data' :: JsonData
test_data' =
    ("Marfa", "Coolcat str 7/42 Minsk Belarus",
    [ ("Scott Wlaschin", "Domain Modeling Made Functional")
    , ("Mikito Takada", "Distributed systems for fun and profit")
    ])


validate_incoming_data :: JsonData -> Either ValidationError JsonData
validate_incoming_data json_data =
  Right json_data


validate_cat :: String -> Either ValidationError Cat
validate_cat cat_name =
  case cat_name of
    "Tihon" -> Right $ Cat cat_name
    _ -> Left CatNotFound


validate_address :: String -> Either ValidationError Address
validate_address addr_str =
  Right $ Address addr_str


get_book :: String -> String -> Either ValidationError Book
get_book title author =
  Right $ Book  "ISBN 978-5-00057-917-6" title author


create_order :: Cat -> Address -> [Book] -> Order
create_order cat address books =
  Order cat address books
