{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Spec.EscrowSpec qualified

import Test.Tasty
import Cooked.Wallet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "use cases" [
    Spec.EscrowSpec.tests
    ]
