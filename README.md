# `template-haskell-jailbreak`

[![CircleCI](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master.svg?style=shield)](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master)

Black magic to workaround the Template Haskell stage restriction. Work in progress. Doesn't work on Windows yet.

## Black magic No.1

Retrieve `LocalBuildInfo` in a Template Haskell splice. No need for custom `Setup.hs` script!

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Jailbreak.Internals

main :: IO ()
main = print $(lbiQ)
```

## Black magic No.2

Run Template Haskell splice within a Template Haskell splice, which can be arbitrarily nested. WIP
