#!/usr/bin/env -S cabal run -v1
{- cabal:
build-depends: base
default-language: GHC2024
ghc-options: -Wall
-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import           Control.Monad (replicateM)
import           Data.List     (intercalate, sortBy)
import           Text.Printf   (printf)

{- nCr maths -}

factorial n = product [n, n-1 .. 1]

nCr n r = n' `div` r'
    where
    -- unroll just what you need and nothing more
    n' = product [n, n-1 .. n-r+1]
    r' = factorial r

{- election utilities -}

data State = State String {- state name -} Int {- votes -} Float {- probability -}

flip_state flipBlue s@(State n v p) = if flipBlue then State n v (1.0 - p) else s

votes_to_win = 270 :: Int

-- Data as of 2024-10-26
-- Sources:
-- - https://polymarket.com/elections
-- - https://www.270towin.com/road-to-270-combinations/
blue_votes = 226 :: Int
red_votes  = 219 :: Int
undecided_states =
  [ State "nc" 16 0.71
  , State "nv" 6 0.66
  , State "az" 11 0.72
  , State "pa" 19 0.61
  , State "mi" 15 0.55
  , State "wi" 10 0.58
  , State "ga" 16 0.72
  ]

total_combos = let n = length undecided_states
               in foldr (\r t -> nCr n r + t) 0 [0 .. n]

winning_combos redWins =
    filter (\(v, _, _) -> v >= votes_to_win) $
    fmap tally combis
  where
    nstates = length undecided_states
    combis  = replicateM nstates [True, False]
    votes   = if redWins then red_votes else blue_votes
    tally c = foldr
      (\ (i, w) (v, p, ws) -> let (State n v' p') = flip_state (not redWins) (undecided_states !! i)
                              in if w then (v + v', p * p', ws <> [n]) else (v, p, ws))
      (votes, 1.0, [])
      (zip [0 .. nstates] c)

highest_chances redWins =
    sortBy (\(_, p1, _) (_, p2, _) -> compare p1 p2) $
    winning_combos redWins

print_combo (v, p, ws) = putStrLn $ printf "%0.3f%% %d " (100 * p) v ++ intercalate " " ws

total_chance redWins = go (fmap (flip_state (not redWins)) undecided_states) votes 1
  where
    votes = if redWins then red_votes else blue_votes
    go ((State _ v' p'):as) v p = go as v ((1 - p') * p)
      + if v + v' < votes_to_win then go as (v + v') (p' * p) else p' * p
    go [] v p = if v >= votes_to_win then p else 0

print_total_chance = putStrLn . printf "Total chance: %0.4f%%" . (* 100) . total_chance

main = do
  let totalCombos = total_combos
      redCombos = highest_chances True
      blueCombos = highest_chances False
  putStrLn $ "Total combos: " ++ show totalCombos
  putStrLn $ "Tie combos: " ++ show (totalCombos - length redCombos - length blueCombos)
  putStrLn "***** Red Winnings *****"
  mapM_ print_combo redCombos
  putStrLn $ "Number of combos: " ++ show (length redCombos)
  print_total_chance True
  putStrLn "***** Blue Winnings *****"
  mapM_ print_combo blueCombos
  putStrLn $ "Number of combos: " ++ show (length blueCombos)
  print_total_chance False

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}

