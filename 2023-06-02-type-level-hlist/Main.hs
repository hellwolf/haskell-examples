-- -*- fill-column: 100; -*-

import           Data.Proxy    (Proxy (..))

import           CSVDecoderAPI

main :: IO ()
main = do
  -- Use "sugarfree" hlist syntax
  print $ csvDecode @TNil ""
  print $ csvDecode @(IntVal :> ())
    "42"
  print $ csvDecode @(IntVal :> StringVal :> IntVal :> ())
    "42,\"france\",41"
  print $ csvDecode @(IntVal :> (StringVal :> IntVal :> ()) :> ())
    "42,(\"estonia\",14)"
  print $ csvDecode @(IntVal :> (StringVal :> IntVal :> ()) :> StringVal :> ())
    "42,(\"estonia\",14),\"finland\""
  print $ csvDecode @(IntVal :> (StringVal :> IntVal :> ()) :> (StringVal :> IntVal :> ()) :> ())
    "42,(\"estonia\",14),(\"finland\",50)"
  -- Use (nicer) tuple syntax
  print $ csvDecode @(OneVal IntVal) -- FIXME is there way to avoid such workaround?
    "42"
  print $ csvDecode @(IntVal, (StringVal, IntVal))
    "42,(\"france\",41)"
  print $ csvDecode @(StringVal, [IntVal])
    "\"germany\",[1,3,5,7,11]"
  print $ csvDecode @(StringVal, [(IntVal, IntVal)])
    "\"germany\",[(1,3),(5,7),(11,13)]"
  print $ csvDecode @(StringVal, [[IntVal]])
    "\"germany\",[[1],[3,5,7]]"
  -- Use patter matching to consume the parsed values
  case csvDecode @(IntVal :> IntVal :> ()) "4,2" of
    (_, Just (a :> b :> ())) -> print (a, b)
    _                        -> error "not possible"
