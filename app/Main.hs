module Main where

import System.Environment
import System.Exit

import Analyser


main :: IO ()
main = getArgs >>= runCommand

runCommand :: [String] -> IO ()
runCommand ("-h":_) = putStrLn usage
runCommand [catalogPath] = runAnalysis catalogPath Nothing >>= exitWith
runCommand [catalogPath, queryPath] = runAnalysis catalogPath (Just queryPath) >>= exitWith
runCommand [catalogPath, "-d", dirPath] = runAnalysisOnDir catalogPath dirPath >>= exitWith
runCommand args = putStrLn (unlines [invalidArgs args, usage]) >> exitFailure

usage :: String
usage = unlines ["Usage:"
        , "\tvq-analyser catalog_file [query_file]"
        , "\tvq-analyser catalog_file -d directory"
        ]

invalidArgs :: [String] -> String
invalidArgs args = "Invalid arguments: \'" ++ unwords args ++ "\'"
