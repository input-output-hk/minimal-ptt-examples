# Cooked ContractModel

This repository provides the instances and API necessary to use
`cooked-contracts` and `quickcheck-contractmodel` together, allowing users of
`cooked-contracts` to write model-based property tests for their smart
contracts. An example of using `quickcheck-contractmodel` to test a `cooked-contracts`
contract can be found in the [Spec.Auction](cooked-contractmodel/test/Spec/Auction.hs)
example that showcases how to write a contract model for the `cooked-contracts`' auction
contract example found [here](https://github.com/tweag/cooked-smart-contracts/tree/0612c438c5a603f820557fd2c994e308a7136c64/auction).

We hope that this repository can serve as an example of how to implement
`quickcheck-contractmodel` bindings for any custom `Cardano` emulator.  Feel
free to check out the implementation of
[Test.QuickCheck.ContractModel.Cooked](cooked-contractmodel/src/Test/QuickCheck/ContractModel/Cooked.hs)
for an example of how to use `quickcheck-contractmodel` with your emulator.
