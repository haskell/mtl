# Migrating to `mtl-2.3.x`

In `mtl-2.3.x`, [re-exports were removed](https://github.com/haskell/mtl/pull/108) from the library in order to achieve PVP compliance, as well as disentangle some historical warts in its design. This was a breaking change that maintainers decided was in the `mtl`'s best interest due to the re-exports overlapping with `base`, as well as generally being confusing and outdated for modern GHC versions. While all changes were detailed in the changelog, navigating the removal of re-exports can be tricky. Below is a detailed review of what needs to be done to use the new `mtl`.

## Removed re-exports

The following `base`-specific re-exports were cut from many modules:

- `Control.Monad`
- `Control.Monad.Fix`
- `Data.Monoid`

In `mtl-2.3`, the following export was erroneously removed from `mtl`-specific modules:

- `ExceptT` and related functions from `Control.Monad.Except`

## What do I need to do?

First, update your `*.cabal` file to point to the new `mtl-2.3.1` release (currently on Hackage), or add the following to your `cabal.project`:


```
packages: .

source-repository-package
  type: git
  location: https://github.com/haskell/mtl
  tag: v2.3.1

allow-newer:
  *:mtl
```

Now, if upon recompile you see anything to the effect that particular functions from any of the above re-exports are missing, then you'll need to import the modules explicitly in order to expose the functions.
