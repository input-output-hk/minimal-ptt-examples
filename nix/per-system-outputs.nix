# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ inputs, inputs', pkgs, projects }:
{
  # iog.dapp = projects.ghc8107;
  dapp = projects.ghc8107;
  iog = dapp;
  # packages = { };
  # checks = { };
  # apps = { };
  # operables = { };
  # oci-images = { };
  # nomadTasks = { };
  # foobar = { };
}
