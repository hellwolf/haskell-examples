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
import           System.IO             (BufferMode (..), Handle, hGetBuf, hGetBuffering, hGetLine, hIsEOF, hPutBuf,
                                        hPutStrLn, stdin, stdout)

-- https://stackoverflow.com/questions/68639266/size-of-buffered-input-in-c
-- By many accounts, it seems "gnu cat" uses 128 KB as buffer size
cBUFFER_SIZE :: Int
cBUFFER_SIZE = 128 * 1024

line_buffering_cat :: Handle -> Handle -> IO ()
line_buffering_cat hIn hOut = go where
  go = do
    isEOF <- hIsEOF hIn
    unless isEOF $ hGetLine hIn >>= hPutStrLn hOut >> go

binary_cat :: Handle -> Handle -> Ptr a -> IO ()
binary_cat hIn hOut ptr = go where
  go = do
    n <- hGetBuf hIn ptr cBUFFER_SIZE
    unless (n == 0) $ hPutBuf hOut ptr n >> go

main :: IO ()
main = do
  hGetBuffering stdin >>= \case
    -- use text input out for line-buffering stdin
    LineBuffering -> line_buffering_cat stdin stdout
   -- use binary input output for non line-buffering stdin
    _ -> allocaBytes cBUFFER_SIZE (binary_cat stdin stdout)

{-
Local Variables:
fill-column: 120
haskell-process-type: cabal-repl
haskell-process-load-or-reload-prompt: t
End:
-}
