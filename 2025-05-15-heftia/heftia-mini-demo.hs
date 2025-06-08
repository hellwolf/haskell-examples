#!/usr/bin/env -S cabal run -v1
{- cabal:
build-depends: base, heftia
ghc-options: -Wall
-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Prelude hiding (log, span)
import Control.Monad.Hefty

data Log :: Effect where
  Log :: String -> Log f ()
makeEffectF ''Log

data Span :: Effect where
  Span :: String -> f a -> Span f a
makeEffectH ''Span

runLog :: (Emb IO :> es) => Eff (Log : es) ~> Eff es
runLog = interpret \(Log msg) -> liftIO $ putStrLn $ "[LOG] " <> msg

runSpan :: (Emb IO :> es) => Eff (Span : es) ~> Eff es
runSpan = interpret \(Span name m) -> do
  liftIO $ putStrLn $ "[Start span '" <> name <> "']"
  r <- m
  liftIO $ putStrLn $ "[End span '" <> name <> "']"
  pure r

main :: IO ()
main = runEff . runLog . runSpan $ do
  span "example program" do
    log "foo"

    span "greeting" do
      log "hello"
      log "world"

    log "bar"

{-
Local Variables:
fill-column: 100
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}

