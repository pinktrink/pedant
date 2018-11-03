{-# LANGUAGE OverloadedStrings #-}

module Lib where


import Control.Monad (unless)
import Data.Char (isSpace)
import Options.Applicative
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import System.IO (stdin)


data ShouldFix = FixIt | DONTFixIt deriving Show

data Options = Options
    { ofile :: Maybe FilePath
    , ofix :: ShouldFix }
    deriving Show

newtype Errors = Errors [Int]


prettyErrors :: Errors -> String
prettyErrors (Errors []) = ""
prettyErrors (Errors ints) = "Trailing whitespace on line" ++ (if length ints == 1 then "" else "s") ++ " " ++ intercalate ", " (show <$> ints)

main :: IO ()
main = do
    ops <- execParser $ info (helper <*> options)
                             (fullDesc)
    case ops of
        Options (Just path) _ -> detectNPrint =<< readFile path
        Options Nothing _     -> detectNPrint =<< getContents

    where

    detectNPrint file = do
        putStrLn . prettyErrors $ detectTrailing (lines file)
        unless (endNL file) $ putStrLn "No newline at the end of the file."

options :: Parser Options
options = Options
    <$> argument (maybeReader $ \x -> Just $ if x == "" then Nothing else Just x) (
        metavar "FILENAME" <>
        help "The filename from which to read (defaults to stdin)." <>
        value Nothing
    )
    <*> flag DONTFixIt FixIt (
        long "fix" <>
        short 'f' <>
        help "Whether or not to fix detected issues."
    )

detectTrailing :: [String] -> Errors
detectTrailing xs = Errors . catMaybes $ fmap f $ zip xs [0..]
    where
    f :: (String, Int) -> Maybe Int
    f ("",_) = Nothing
    f (l, n) = if isSpace (last l) then Just $ n + 1 else Nothing

endNL :: String -> Bool
endNL = ('\n' ==) . last

trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse

trimRightAll :: [String] -> [String]
trimRightAll = fmap trimRight
