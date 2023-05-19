{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Spec.Vesting qualified

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "use cases" [
    Spec.Vesting.tests
    , Spec.Vesting.modelTests
    ]
