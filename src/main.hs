module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print $ calc' args
  where
    calc' args = calc (args !! 0) (args !! 1) (args !! 2)

calc :: String -> String -> String -> Float
calc lhs' op rhs' =
  let lhs = read lhs'
      rhs = read rhs'
   in case op of
        "+" -> lhs + rhs
        "-" -> lhs - rhs
        "*" -> lhs * rhs
        "/" -> lhs / rhs
        _ -> error $ "invalid op: " ++ op
