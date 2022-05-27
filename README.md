[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit&logoColor=white)](https://github.com/pre-commit/pre-commit)

| **`Windows`** | **`Linux(x86/s390x)`** |
|:-----------------:|:-----------------:|
[![Build status](https://ci.appveyor.com/api/projects/status/tcx4nbu4yb3qubhu/branch/develop?svg=true)](https://ci.appveyor.com/project/rokoDev/rabbit/branch/develop)|[![CircleCI](https://circleci.com/gh/rokoDev/rabbit/tree/develop.svg?style=shield)](https://circleci.com/gh/rokoDev/rabbit/tree/develop)|

# rabbit

## Prerequisites
Install `pre-commit` package. For instructions see: [pre-commit installation](https://pre-commit.com/#install)

In order to be able to make commits with proper formatting and other checks you should run this commands after clonning the repo:
  1. `cd rabbit`
  2. `pre-commit install`
  3. `pre-commit install --hook-type prepare-commit-msg`
