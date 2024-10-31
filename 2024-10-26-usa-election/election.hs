#!/usr/bin/env -S cabal run -v1
{- cabal:
build-depends: base
default-language: GHC2024
ghc-options: -Wall
-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import           Control.Exception (assert)
import           Control.Monad     (replicateM)
import           Data.List         (intercalate, sortBy)
import           Text.Printf       (printf)

{- nCr maths -}

factorial n = product [n, n-1 .. 1]

nCr n r = n' `div` r'
    where
    -- unroll just what you need and nothing more
    n' = product [n, n-1 .. n-r+1]
    r' = factorial r

{- election utilities -}

data State = State String {- state name -} Int {- votes -} Float {- probability -}

total_votes = 538 :: Int

votes_to_win = 270 :: Int

total_combos states = let n = length states in foldr (\r t -> nCr n r + t) 1 [0 .. n]

flip_state flipBlue s@(State n v p) = if flipBlue then State n v (1.0 - p) else s

set_state_to_win name states = fmap (\s@(State n v _) -> if n == name then State n v 1.0 else s) states

winning_combos states votes =
    filter (\(v, _, _) -> v >= votes_to_win) $
    fmap tally combis
  where
    nstates = length states
    combis  = replicateM nstates [True, False]
    tally c = foldr
      (\ (i, w) (v, p, ws) -> let (State n v' p') = states !! i
                              in if w then (v + v', p * p', ws <> [n]) else (v, p, ws))
      (votes, 1.0, [])
      (zip [0 .. nstates] c)

highest_chances states votes =
    sortBy (\(_, p1, _) (_, p2, _) -> compare p2 p1) $
    winning_combos states votes

print_combo (v, p, ws) = putStrLn $ printf "%0.2f%% %d " (100 * p) v ++ intercalate " " ws

total_chance states votes = go states votes 1
  where
    go ((State _ v' p'):as) v p = go as v ((1 - p') * p)
      + if v + v' < votes_to_win then go as (v + v') (p' * p) else p' * p
    go [] v p = if v >= votes_to_win then p else 0

print_total_chances states votes = do
  go "Total chance" states
  mapM_ (\n -> go ("Total chance if " ++ n ++ " is won") (set_state_to_win n states)) names
  where
    names = fmap (\(State n _ _) -> n) states
    go title states' = putStrLn $ printf (title ++ ": %0.2f%%") (total_chance states' votes * 100)

-- Data as of 2024-10-31
-- Sources:
-- - https://polymarket.com/elections
-- - https://www.270towin.com/road-to-270-combinations/
red_votes  = 219 :: Int
blue_votes = 226 :: Int
undecided_states_in_red =
  [ State "nc" 16 0.71
  , State "nv"  6 0.67
  , State "az" 11 0.76
  , State "pa" 19 0.61
  , State "mi" 15 0.46
  , State "wi" 10 0.54
  , State "ga" 16 0.73
  ]

undecided_states_in_blue = fmap (flip_state True) undecided_states_in_red

undecided_votes = let n = sum $ fmap (\(State _ v _) -> v) undecided_states_in_red
                  in assert (blue_votes + red_votes + n == total_votes) n

main = do
  let totalCombos = total_combos undecided_states_in_red
      redCombos = highest_chances undecided_states_in_red red_votes
      blueCombos = highest_chances undecided_states_in_blue blue_votes
  putStrLn $ "Total electoral votes: " ++ show total_votes
  putStrLn $ "Electoral votes to win: " ++ show votes_to_win
  putStrLn $ "Undecided votes: " ++ show undecided_votes
  putStrLn $ "Total combos: " ++ show totalCombos
  putStrLn $ "Tie combos: " ++ show (totalCombos - length redCombos - length blueCombos)
  putStrLn "***** Red Winnings *****"
  mapM_ print_combo redCombos
  putStrLn $ "Number of combos: " ++ show (length redCombos)
  print_total_chances undecided_states_in_red red_votes
  putStrLn "***** Blue Winnings *****"
  mapM_ print_combo blueCombos
  putStrLn $ "Number of combos: " ++ show (length blueCombos)
  print_total_chances undecided_states_in_blue blue_votes

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}

