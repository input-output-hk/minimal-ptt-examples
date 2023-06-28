module Main(main) where

import Spec.Auction qualified

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "cooked-contractmodel"
  [ Spec.Auction.tests
  ]
