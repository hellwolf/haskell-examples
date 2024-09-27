#!/usr/bin/env cabal
{- cabal:
build-depends: base
default-language: GHC2024
-}
{-|
Synopsis: a simple "cat" program that supports both line-buffering and binary input modes
-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad         (unless)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr           (Ptr)
import           System.IO             (BufferMode (..), hGetBuf, hGetBuffering, hGetLine, hIsEOF, hPutBuf, hPutStrLn,
                                        stdin, stdout)

-- https://stackoverflow.com/questions/68639266/size-of-buffered-input-in-c
-- By many accounts, it seems "gnu cat" uses 128 KB as buffer size
cBUFFER_SIZE :: Int
cBUFFER_SIZE = 128 * 1024

line_buffering_cat :: IO ()
line_buffering_cat = go where
  go = do
    isEOF <- hIsEOF stdin
    unless isEOF $ hGetLine stdin >>= hPutStrLn stdout >> go

binary_cat :: Ptr a -> IO ()
binary_cat ptr = go where
  go = do
    n <- hGetBuf stdin ptr cBUFFER_SIZE
    unless (n == 0) $ hPutBuf stdout ptr n >> go

main :: IO ()
main = do
  hGetBuffering stdin >>= \case
    -- use text input out for line-buffering stdin
    LineBuffering -> line_buffering_cat
   -- use binary input output for non line-buffering stdin
    _ -> allocaBytes cBUFFER_SIZE binary_cat

{-
Local Variables:
fill-column: 120
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
