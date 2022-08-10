2.3.1 -- 2022-09-10
-----
* Add `modifyError` to `Control.Monad.Error.Class`, and re-export from
  `Control.Monad.Except`.
* Make the `MonadCont` instance for `ContT` more polykinded; now, `r` is allowed
  to be of an arbitrary kind `k`, rather than only `Type`.
* Add a generic `liftCallCC` for use with any `MonadTrans`.
* Add `modifyError` to `Control.Monad.Error.Class`
* Return re-export of `ExceptT` and related functions to `Control.Monad.Except`.
* Add `label` function to `MonadCont`

2.3 -- 2022-05-07
---
* Add instances for `Control.Monad.Trans.Writer.CPS` and
  `Control.Monad.Trans.RWS.CPS` from `transformers` 0.5.6 and add
  `Control.Monad.Writer.CPS` and `Control.Monad.RWS.CPS`.
* `Control.Monad.Cont` now re-exports `evalCont` and `evalContT`.
* Add `tryError`, `withError`, `handleError`, and `mapError` to
  `Control.Monad.Error.Class`, and re-export from `Control.Monad.Except`.
* Remove `Control.Monad.List` and `Control.Monad.Error`.
* Remove instances of deprecated `ListT` and `ErrorT`.
* Remove re-exports of `Error`.
* Add instances for `Control.Monad.Trans.Accum` and
  `Control.Monad.Trans.Select`.
* Require GHC 8.6 or higher, and `cabal-install` 3.0 or higher.
* Require `transformers-0.5.6` or higher.
* Add `Control.Monad.Accum` for the `MonadAccum` type class, as well as the
  `LiftingAccum` deriving helper.
* Add `Control.Monad.Select` for the `MonadSelect` type class, as well as the
  `LiftingSelect` deriving helper.
* Remove re-exports of `Control.Monad`, `Control.Monad.Fix` and `Data.Monoid` modules

2.2.2 -- 2018-02-24
-----
* `Control.Monad.Identity` now re-exports `Control.Monad.Trans.Identity`
* Fix a bug in which `Control.Monad.State.Class.modify'` was not as strict in
  the new state as its counterparts in `transformers`
* Add a `MonadError () Maybe` instance
* Add `liftEither :: MonadError e m => Either e a -> m a` to
  `Control.Monad.Except{.Class}`
* Add a `MonadWriter w ((,) w)` instance (when built against `base-4.9` or later)

2.2.1 -- 2014-06-02
-------
* Provide MINIMAL pragmas for `MonadState`, `MonadWriter`, `MonadReader`
* Added a cyclic definition of `ask` in terms of `reader` for consistency with `get`/`put` vs. `state` and `tell` vs. `writer`
* Fix deprecation warnings caused by `transformers` 0.4 deprecating `ErrorT`.
* Added `Control.Monad.Except` in the style of the other `mtl` re-export modules

2.2.0.1 -- 2014-05-05
-------
* Fixed a bug caused by the change in how `transformers` 0.4 exports its data types. We will now export `runFooT` for each transformer again!

2.2 -- 2014-05-05
---
* `transformers` 0.4 support
* Added instances for `ExceptT`
* Added `modify'` to `Control.Monad.State.*`

2.1.3.1 -- 2014-03-24
-------
* Avoid importing `Control.Monad.Instances` on GHC 7.8 to build without deprecation warnings.

2.1.3
-----
* Removed the now-irrelevant `Error` constraint from the `MonadError` instance for `Either e`.
