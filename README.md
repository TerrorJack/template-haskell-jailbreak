# `template-haskell-jailbreak`

[![Build Status](https://ci.appveyor.com/api/projects/status/github/TerrorJack/template-haskell-jailbreak?branch=master&svg=true)](https://ci.appveyor.com/project/TerrorJack/template-haskell-jailbreak?branch=master)

Black magic to workaround the Template Haskell stage restriction. Work in progress.

## Black magic No.1

Retrieve `LocalBuildInfo` in a Template Haskell splice. No need for custom `Setup.hs` script!

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Jailbreak.Internals

main :: IO ()
main = print $(lbiQ)
```

Doesn't work on Windows yet.

## Black magic No.2

Run Template Haskell splice within a Template Haskell splice, which can be arbitrarily nested. WIP
