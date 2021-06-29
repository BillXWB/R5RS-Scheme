module Main where

main :: IO ()
main = do
  lhs <- input "lhs: "
  op <- input "op: "
  rhs <- input "rhs: "
  print $ calc lhs op rhs

input :: String -> IO String
input hint = do
  putStr hint
  getLine

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
