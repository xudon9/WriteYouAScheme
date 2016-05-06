module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn "Name: "
    name <- getLine
    putStrLn $ "Hello, " ++ name
