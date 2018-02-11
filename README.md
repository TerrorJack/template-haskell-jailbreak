# `template-haskell-jailbreak`

[![CircleCI](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master.svg?style=shield)](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master)

Black magic to workaround a Template Haskell stage restriction. Doesn't work on Windows yet.

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

There is one remaining problem though. We don't know `ghcLibDir`, and the default `DynFlags` doesn't share the same GHC flags with the module which calls `eval`. If the splice passed to `eval` references something out of global/user package database it won't work.

Assuming we're not invoking `ghc` by hand but using `Cabal`-based tools to compile our modules, our remaining task is: find out the `Cabal` configuration of the current module and apply it to our GHC evaluation session.

### Acquiring `LocalBuildInfo`

After `Setup configure` completes, `Setup` generates a `LocalBuildInfo` value which contains all information about subsequent builds. It is serialized to `$builddir/setup-config`, where `$builddir` is specified by `--builddir` option of `Setup` (by default it's `dist`).

To obtain `LocalBuildInfo` from `ghc`, we can either:

* Require the user to write a custom `Setup.hs` script, which caches `LocalBuildInfo` to a file at a known path, then read it from that path. My previous package `cabal-toolkit` provides utilities to do this.
* Figure out `$builddir` and read it from `$builddir/setup-config`, just like what `Setup` does.

For the convenience of users, the second approach is more preferrable. We just need to:

* Acquire the parent process id of `ghc`, which is the `Setup` process performing `Setup build`
* Acquire the command lines of that process id, find out `$builddir`

There isn't a cross-platform way for those tasks as of now. On Unix it's easier, we can use `getppid` system call for parent process id, and inspect the `/proc` filesystem to get the command lines. After grepping through the command lines and finding out `$builddir`, we can read the persisted `LocalBuildInfo` with `getPersistBuildConfig` from `Cabal`.

### Applying `LocalBuildInfo` to `DynFlags`

Ideally, we want to apply `LocalBuildInfo` to `DynFlags` with the same effect of the bunch of `ghc` flags generated by `Cabal`. To achieve that degree of precision, we need to:

* Implement a frontend plugin
* Use `Cabal` to generate `ghc` flags from `LocalBuildInfo`, but modify the flags slightly so the major mode of `ghc` becomes our frontend plugin. Invoke `ghc` with modified flags
* In the frontend plugin, the `DynFlags` is already properly configured, we just need some IPC mechanism to pump the `Exp`s there and get results back

This is going to take a huge amount of work. So we resort a bit and focus on adjusting `DynFlags` by hand. For now, we just modify the package db flags, so packages in the `stack` snapshot, `cabal` sandbox or whatever will be available for use.
