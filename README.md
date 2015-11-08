# Pers
Ideas for persistent. Investigations of working with Named Records.

## Introduction

Current `persistent` package is good but have some disadvantages:
* Code is generated by TH which reduce composability
* No decision for projections of `entity`
* No instruments for working with views/stored procedures/direct select
* Others?....

Here I want to try to check some ideas in this field.

The first one is storing data in some kind of Named Records using `Symbols` for type-level checking.
I was excited with using `Symbols` in [Servant](http://haskell-servant.github.io/) library. 
And I want to try something similar for Persistency.
Another source I saw is [Record](github.com/nikita-volkov/record) library.

## Named Records
