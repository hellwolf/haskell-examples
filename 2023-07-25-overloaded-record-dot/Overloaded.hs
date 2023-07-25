{-# LANGUAGE OverloadedRecordDot #-}

import           Records (Company (name, owner), Person (name), mkCompany)

main = do
  -- let c' = MkCompany "hehe" "" -- Nope
  let c = mkCompany "DECENTRAL.EE OÜ" "Miao, ZhiCheng"
  putStrLn $ c.name ++ " is run by " ++ c.owner.name
