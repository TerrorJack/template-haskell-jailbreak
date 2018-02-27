# `template-haskell-jailbreak`

[![CircleCI](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master.svg?style=shield)](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master)

Black magic to workaround a Template Haskell stage restriction.

## Demo

Evaluate any Template Haskell `Exp`, any time you want.

```haskell
-- eval :: Q Exp -> Q a

{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Jailbreak
import Language.Haskell.TH.Syntax

main :: IO ()
main =
  print
    $(do r <- eval [|2 + 2 :: Int|]
         lift (r :: Int))
```

With `eval`, you are granted the power of jailbreaking. Here comes one example usage: Suppose you want to query the `Storable` instance of a `Type` and calculate its size via calling `sizeOf`. You need something like:

```haskell
sizeOfType :: Type -> Q Int
```

After obtaining the `sizeOfType` result in the `Q` monad, you can generate subsequent `Dec`s you need. What would its implementation look like?

```haskell
sizeOfType :: Type -> Q Int
sizeOfType t = pure $([| sizeOf (undefined :: $(pure t)) |])
```

Unfortunately `ghc` is protesting:

```
    * GHC stage restriction:
        `t' is used in a top-level splice, quasi-quote, or annotation,
        and must be imported, not defined locally
    * In the untyped splice: $(pure t)
      In the Template Haskell quotation
        [| sizeOf undefined :: $(pure t) |]
      In the untyped splice: $([| sizeOf (undefined :: $(pure t)) |])
```

It's time to jailbreak!

```haskell
sizeOfType :: Type -> Q Int
sizeOfType t = eval [|sizeOf (undefined :: $(pure t))|]
```

This version works.

There's probably more idiomatic way to achieve this though, do tell me if something ain't right..

## How it works

### Implementing `eval`

The core idea is simple: provide `eval :: Q Exp -> Q a`, so we can pass an expression splice to `eval`, and acquire the (explicitly typed) result in the `Q` monad. Due to stage restriction, we simply cannot use the Template Haskell mechanism to evaluate the passed in `Q Exp`, so we're going to use GHC API to perform evaluation.

Using the utilities in the `GHC` module, we can fire up a GHC session like this:

```haskell
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just ghcLibDir) $ do
      dflags <- GHC.getSessionDynFlags
      _ <- GHC.setSessionDynFlags dflags
      subsequent_computation
```

The type of `subsequent_computation` is `Ghc a`, and in the `Ghc` monad we can use the functions in `GHC` to perform tasks like parsing, type-checking, desugaring or interactive evaluation. For our own task here, we can either:

* Pretty-print the binding from a symbol to the `Exp` to a temporary `.hs`, compile it and link it
* Pass the `Exp` directly for interactive evaluation

The first approach generates higher-quality code, yet we're going to run the generated code only once so we're heading for the simpler & faster second approach.

The `Convert` module provides utility functions to marshal `template-haskell` AST types to `ghc` AST types. Using `convertToHsExpr` we can get a parsed `ghc` AST from `Exp`, then we can feed it to `compileParsedExpr` and retrieve the result. The result type is `HValue` which is a wrapper of `Any`, and `unsafeCoerce` is required to cast it back to the right type.

There may be multiple `eval` invocations in a single module, and the overhead of one GHC session for each invocation is high. Thankfully, Template Haskell supports maintaining states between different splices in one module, so we'll simply make our `subsequent_computation` do one thing: receive a `Maybe (Ghc ())` value from an `MVar` (used as a channel), if it contains a computation of type `Ghc ()`, we perform it; it it is `Nothing` then it means the channel is closed and we quit. It runs in a forked thread, so the initialization code can return immediately.

The `Language.Haskell.TH.Syntax` module provides `getQ`, `putQ` and `addModFinalizer` which can be used to cache a GHC session for all `eval` invocations in a module. In each `eval` call, we check if there's a value of `Ghc () -> IO ()` available, if not, we initialize a GHC session first. When evaluating an `Exp`, we send the computation of "evaluate it and put it in an `MVar`", and get the result in the same `MVar`. The finalizer is simply sending `Nothing` to close the channel and quit the evaluation thread.

There is one remaining problem though. The default `DynFlags` doesn't share the same GHC flags with the module which calls `eval`. If the splice passed to `eval` references something out of global/user package database it won't work. This can be solved by running `getArgs` in the Q monad to acquire current GHC flags, and use `parseDynamicFlagsCmdLine` in the `DynFlags` module to adjust a fresh `DynFlags` value.
