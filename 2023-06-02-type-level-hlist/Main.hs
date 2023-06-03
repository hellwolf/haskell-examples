-- -*- fill-column: 100; -*-

import           Data.Proxy    (Proxy (..))

import           TSVDecoderAPI

main :: IO ()
main = do
  -- Use "sugarfree" hlist syntax
  print $ tsvDecode @TNil ""
  print $ tsvDecode @(IntVal :> ())
    "42"
  print $ tsvDecode @(IntVal :> StringVal :> IntVal :> ())
    "42,\"france\",41"
  print $ tsvDecode @(IntVal :> (StringVal :> IntVal :> ()) :> ())
    "42,(\"estonia\",14)"
  print $ tsvDecode @(IntVal :> (StringVal :> IntVal :> ()) :> StringVal :> ())
    "42,(\"estonia\",14),\"finland\""
  print $ tsvDecode @(IntVal :> (StringVal :> IntVal :> ()) :> (StringVal :> IntVal :> ()) :> ())
    "42,(\"estonia\",14),(\"finland\",50)"
  -- Use (nicer) tuple syntax
  print $ tsvDecode @(OneVal IntVal) -- FIXME is there way to avoid such workaround?
    "42"
  print $ tsvDecode @(IntVal, (StringVal, IntVal))
    "42,(\"france\",41)"
  print $ tsvDecode @(StringVal, [IntVal])
    "\"germany\",[1,3,5,7,11]"
  print $ tsvDecode @(StringVal, [(IntVal, IntVal)])
    "\"germany\",[(1,3),(5,7),(11,13)]"
  print $ tsvDecode @(StringVal, [[IntVal]])
    "\"germany\",[[1],[3,5,7]]"
  -- Use patter matching to consume the parsed values
  case tsvDecode @(IntVal :> IntVal :> ()) "4,2" of
    (_, Just (a :> b :> ())) -> print (a, b)
    _                        -> error "not possible"
