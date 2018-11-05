{-# LANGUAGE OverloadedStrings #-}

module Lib where


import Control.Monad (unless)
import Data.Char (isSpace)
import Options.Applicative
import Data.List (intercalate, isInfixOf)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import System.IO (stdin)


data ShouldFix = FixIt | DontFixIt deriving Show

data Options = Options
    { ofile :: Maybe FilePath
    , ofix :: ShouldFix }
    deriving Show

newtype Errors = Errors [Int]


prettyTrailingErrors :: Errors -> String
prettyTrailingErrors (Errors []) = ""
prettyTrailingErrors (Errors ints) = "Trailing whitespace on line" ++ (if length ints == 1 then "" else "s") ++ " " ++ intercalate ", " (show <$> ints) ++ "."

prettyTabErrors :: Errors -> String
prettyTabErrors (Errors []) = ""
prettyTabErrors (Errors ints) = "Literal tab characters on line" ++ (if length ints == 1 then "" else "s") ++ " " ++ intercalate ", " (show <$> ints) ++ ". Please use \\t."

main :: IO ()
main = do
    ops <- execParser $ info (helper <*> options)
                             (fullDesc)
    case ops of
        Options (Just path) f -> do
            case f of
                DontFixIt ->
                    detectNPrint =<< readFile path
                FixIt -> do
                    c <- readFile path
                    seq (length c) (return ())
                    writeFile path (intercalate "\n" (trimRightAll (lines c))++ "\n")

        Options Nothing _ -> detectNPrint =<< getContents

    where

    detectNPrint file = do
        putStrLn . prettyTrailingErrors $ detectTrailing (lines file)
        putStrLn . prettyTabErrors $ detectTabs (lines file)
        unless (endNL file) $ putStrLn "No newline at the end of the file."

options :: Parser Options
options = Options
    <$> argument (maybeReader $ \x -> Just $ if x == "" then Nothing else Just x) (
        metavar "FILENAME" <>
        help "The filename from which to read (defaults to stdin)." <>
        value Nothing
    )
    <*> flag DontFixIt FixIt (
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

detectTabs :: [String] -> Errors
detectTabs xs = Errors . catMaybes $ fmap f $ zip xs [0..]
    where
    f :: (String, Int) -> Maybe Int
    f ("",_) = Nothing
    f (l, n) = if isInfixOf "\t" l then Just $ n + 1 else Nothing

endNL :: String -> Bool
endNL = ('\n' ==) . last

trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse

trimRightAll :: [String] -> [String]
trimRightAll = fmap trimRight
