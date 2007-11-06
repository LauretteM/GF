module Main where

import GF.Command.Interpreter
import GF.Command.Commands
import GF.GFCC.API
import System (getArgs)
import Data.Char (isDigit)

-- Simple translation application built on GFCC. AR 7/9/2006 -- 19/9/2007

main :: IO ()
main = do
  file:_  <- getArgs
  grammar <- file2grammar file
  let env = CommandEnv grammar (allCommands grammar)
  printHelp grammar
  loop env

loop :: CommandEnv -> IO ()
loop env = do
  s <- getLine
  if s == "q" then return () else do
    interpretCommandLine env s
    loop env

printHelp grammar = do
  putStrLn $ "languages:  " ++ unwords (languages grammar)
  putStrLn $ "categories: " ++ unwords (categories grammar)
---  putStrLn commands

{- obsolete

commands = unlines [
  "Commands:",
  "  (gt | gtt | gr | grt) Cat Num - generate all or random",
  "  p Lang Cat String             - parse (unquoted) string",
  "  l Tree                        - linearize in all languages",
  "  h                             - help",
  "  q                             - quit"
  ]

treat :: MultiGrammar -> String -> IO ()
treat mgr s = case words s of
  "gt" :cat:n:_ -> mapM_ prlinonly $ take (read1 n) $ generateAll mgr cat
  "gtt":cat:n:_ -> mapM_ prlin $ take (read1 n) $ generateAll mgr cat
  "gr" :cat:n:_ -> generateRandom mgr cat >>= mapM_ prlinonly . take (read1 n) 
  "grt":cat:n:_ -> generateRandom mgr cat >>= mapM_ prlin . take (read1 n) 
  "p":lang:cat:ws -> do
    let ts = parse mgr lang cat $ unwords ws
    mapM_ (putStrLn . showTree) ts 
  "h":_ -> printHelp mgr
  "l" : ws -> lins $ readTree mgr $ unwords ws
 where
  grammar = gfcc mgr
  langs = languages mgr
  lins t = mapM_ (lint t) $ langs 
  lint t lang = do
----    putStrLn $ showTree $ linExp grammar lang t 
    lin t lang
  lin t lang = do
    putStrLn $ linearize mgr lang t
  prlins t = do
    putStrLn $ showTree t
    lins t
  prlin t = do
    putStrLn $ showTree t
    prlinonly t
  prlinonly t = mapM_ (lin t) $ langs
  read1 s = if all isDigit s then read s else 1
-}

