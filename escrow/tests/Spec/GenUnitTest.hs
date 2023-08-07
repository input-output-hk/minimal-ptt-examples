
module Spec.GenUnitTest where

import Text.Parsec.String
-- import Data.Char
import Data.List.Split (splitOn)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Functor (void)
import Data.Bifunctor (second)
import Replace.Megaparsec
import System.IO
import Control.Monad

{-
https://github.com/haskell-github-trust/replace-megaparsec

failingTestCase = ["do action $ Pay (Wallet 4) 20\n   pure ()\n","do action $ WaitUntil (SlotNo 41)\n   action $ Refund (Wallet 4)\n   pure ()\n","Main strategy","Actions \n [Pay (Wallet 4) 20,\n  WaitUntil (SlotNo 41),\n  Refund (Wallet 4)]


failingTestCase = ["Actions \n [Pay (Wallet 2) 27,\n  Pay (Wallet 4) 22,\n  Redeem (Wallet 5)]",

failingTestCase = ["Actions \n [Pay (Wallet 2) 27,\n  Pay (Wallet 4) 22,\n  Redeem (Wallet 5)]",

-}

bigFailString = "failingTestCase = [\"Actions \n [Pay (Wallet 2) 27,\n  Pay (Wallet 4) 22,\n  Redeem (Wallet 5)]\",\"Balance changes don't match:\n  Predicted symbolic balance changes:\n    Wallet 4: {-22000000 Lovelace}\n    Wallet 2: {-7000000 Lovelace}\n"

parens :: Parsec Void String ()
parens = do
        char '['
        manyTill
            (void (noneOf "[]") <|> void parens)
            (char ']')
        pure ()

-- You can get then get the matching output with: splitCap (match parens) bigFailString

failString = "[Pay (Wallet 2) 27,\n  Pay (Wallet 4) 22,\n  Redeem (Wallet 5)]"

failString1 = "(Pay (Wallet 2) 27)"

failString2 = "[Pay (Wallet 4) 20,\n  WaitUntil (SlotNo 41),\n  Refund (Wallet 4)]"


-- remove [] from string and add space
fixString :: String -> String
fixString s = let s' = drop 1 s
              in " " ++ (take ((length s') - 1) s')

-- replace (Wallet Int) with testwallets such as w1
fixWallet :: String -> String
fixWallet s = streamEdit wllt (const "w") s
  where wllt = chunk "Wallet " :: Parsec Void String String

-- replace WaitUntil actions to WaitUntilDL actions
fixWaitUntil :: String -> String
fixWaitUntil s = streamEdit wuntil (const "(waitUntilDL") s
  where wuntil = chunk "action $ WaitUntil (SlotNo" :: Parsec Void String String

-- Generate list of actions from string
genActions :: String -> [String]
genActions s = map (\z -> "    action $" ++ z) (splitOn ",\n " s)

-- apply all fixes to a list of actions
fixActions :: String -> [String]
fixActions s = map (\ z -> fixWaitUntil $ fixWallet $ z) (genActions (fixString s))

genString = " Pay (w4) 20,\n  (waitUntilDL 41),\n  Refund (w4)"

-- genActions :: String -> String
genActions' = let ac = chunk ",\n" :: Parsec Void String String
             in splitCap ac genString


-- Give a unit test name, model name and a failString to produce a unit test
genUnitTest :: String -> String -> String -> [String]
genUnitTest n m s = [n ++ " :: DL " ++ m ++ " ()", n ++ " = ", "  do"]
                    ++
                    fixActions s

main :: IO ()
main = sequence_ (map putStrLn (genUnitTest "unitTest1" "EscrowModel" failString2))

{-Output:
unitTest1 :: DL EscrowModel ()
unitTest1 =
  do
    action $ Pay (w4) 20
    (waitUntilDL 41)
    action $ Refund (w4)
-}

readExample :: IO String
readExample = do
          s <- readFile "output.txt"
          return s


output = Right ("[\"Actions \\n [Pay (Wallet 2) 27,\\n  Pay (Wallet 4) 22,\\n  Redeem (Wallet 5)]\",\"Balance changes don't match:\\n  Predicted symbolic balance changes:\\n    Wallet 4: {-22000000 Lovelace}\\n    Wallet 2: {-7000000 Lovelace}\\n    Wallet 1: {10000000 Lovelace}\\n    Wallet 5: {19000000 Lovelace}\\n  Predicted actual balance changes:\\n    Wallet 4: {-22000000 Lovelace}\\n    Wallet 2: {-7000000 Lovelace}\\n    Wallet 1: {10000000 Lovelace}\\n    Wallet 5: {19000000 Lovelace}\\n  Actual balance changes:\\n    Wallet 4: {-22000000 Lovelace}\\n    Wallet 2: {-27000000 Lovelace}\\n  Sum of min Lovelace: Lovelace 3784180\"]",())

fixOutput :: Either String ([Char], ()) -> Maybe String
fixOutput es = case es of
                Left s -> Nothing
                Right (s,c) -> if (take 9 s) == "[\"Actions"
                               then Just "Success"
                               else Nothing
