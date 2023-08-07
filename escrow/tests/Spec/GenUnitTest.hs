
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

fixWallet :: String -> String
fixWallet s = streamEdit wllt (const "w") s
  where wllt = chunk "Wallet " :: Parsec Void String String

fixWaitUntil :: String -> String
fixWaitUntil s = streamEdit wuntil (const "(waitUntilDL") s
  where wuntil = chunk "WaitUntil (SlotNo" :: Parsec Void String String

-- We assume we get to here at some point
-- need to replace first [ with a space
genString = " Pay (w4) 20,\n  (waitUntilDL 41),\n  Refund (w4)"

-- genActions :: String -> String
genActions = let ac = chunk ",\n" :: Parsec Void String String
             in splitCap ac genString


genActions' = map (\z -> "    action $" ++ z) (splitOn ",\n " genString)


