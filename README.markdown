# `mtl` [![Hackage](https://img.shields.io/hackage/v/mtl.svg)](https://hackage.haskell.org/package/mtl) [![Build Status](https://travis-ci.org/haskell/mtl.svg)](https://travis-ci.org/haskell/mtl)

MTL is a collection of monad classes, extending the `transformers`
package, using functional dependencies for generic lifting of monadic
actions.

## Structure

Transformers in MTL are divided into classes and data types. Classes
define the monadic operations of transformers. Data types, generally
from the `transformers` package, implement transformers, and MTL
provides instances for all the transformer type classes.

MTL and `transformers` use a common module, data type, and function
naming scheme. As an example, let's imagine we have a transformer
`Foo`.

In the `Control.Monad.Foo` module, we'd find:

* A type class `MonadFoo` with the transformer operations.
* A data type `FooT` with instances for all monad transformer classes.
* Functions to run the transformed computation, e.g. `runFooT`. For
  the actual transformers, there are usually a number of useful runner
  functions.

### Lifting

When using monad transformers, you often need to "lift" a monadic
action into your transformed monadic action. This is done using the
`lift` function from `MonadTrans` in the `Control.Monad.Trans.Class`
module:

``` haskell
lift :: (Monad m, MonadTrans t) => m a -> t m a
```

The action `m a` is lifted into the transformer action `t m a`.

As an example, here we lift an action of type `IO a` into an action of
type `ExceptT MyError IO a`:

``` haskell
data MyError = EmptyLine

mightFail :: ExceptT MyError IO ()
mightFail = do
  l <- lift getLine
  when (null l) (throwError EmptyLine)
```

### Transformers

The following outlines the available monad classes and transformers in
MTL and `transformers`. For more details, and the corresponding
documentation of the `mtl` version you are using, see [the
documentation on Hackage](https://hackage.haskell.org/package/mtl).

* `Control.Monad.Cont`

    The Continuation monad transformer adds the ability to use
    [continuation-passing style
    (CPS)](https://en.wikipedia.org/wiki/Continuation-passing_style)
    in a monadic computation. Continuations can be used to manipulate
    the control flow of a program, e.g. early exit, error handling, or
    suspending a computation.

    - Class: `Control.Monad.Cont.Class.MonadCont`
    - Transformer: `Control.Monad.Cont.ContT`

* `Control.Monad.Error` (deprecated!)

    The Error monad transformer has been deprecated in favor of
    `Control.Monad.Except`.

* `Control.Monad.Except`

    The Except monad transformer adds the ability to fail with an
    error in a monadic computation.

    - Class: `Control.Monad.Except.Class.MonadError`
    - Transformer: `Control.Monad.Except.ExceptT`

* `Control.Monad.Identity`

    The Identity monad transformer does not add any abilities to a
    monad. It simply applies the bound function to its inner monad
    without any modification.

    - Transformer: `Control.Monad.Trans.Identity.IdentityT` (in the `transformers` package)
    - Identity functor and monad: `Data.Functor.Identity.Identity` (in the `base` package)

* `Control.Monad.RWS`

    A convenient transformer that combines the Reader, Writer, and
    State monad transformers.

    - Lazy transformer: `Control.Monad.RWS.Lazy.RWST` (which is the default, exported by `Control.Monad.RWS`)
    - Strict transformer: `Control.Monad.RWS.Strict.RWST`

* `Control.Monad.Reader`

    The Reader monad transformer represents a computation which can
    read values from an environment.

    - Class: `Control.Monad.Reader.Class.MonadReader`
    - Transformer: `Control.Monad.Reader.ReaderT`

* `Control.Monad.State`

    The State monad transformer represents a computation which can
    read and write internal state values. If you only need to _read_
    values, you might want to use
    [Reader](http://hackage.haskell.org/package/mtl/docs/Control-Monad-Reader.html)
    instead.

    - Class: `Control.Monad.State.Class.MonadState`
    - Lazy transformer: `Control.Monad.State.Lazy.StateT` (the default, exported by `Control.Monad.State`)
    - Strict transformer: `Control.Monad.State.Strict.StateT`

* `Control.Monad.Writer`

    The Writer monad transformer represents a computation that can
    produce a stream of data in addition to the computed values. This
    can be used to collect values in some data structure with a
    `Monoid` instance. This can be used for things like logging and
    accumulating values throughout a computation.

    - Class: `Control.Monad.Writer.Class.MonadWriter`
    - Lazy transformers: `Control.Monad.Writer.Lazy.WriterT`
    - Strict transformers: `Control.Monad.Writer.Strict.WriterT`

* `Control.Monad.Accum`

    The `Accum` monad transformer represents a computation which
    manages append-only state, or a writer that can read all
    previous inputs. It binds a function to a monadic value by
    lazily accumulating subcomputations via `(<>)`. For more general
    access, use [State](https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-State.html) instead.

    - Class: `Control.Monad.Accum`
    - Transformer: `Control.Monad.Trans.Accum.AccumT`

* `Control.Monad.Select`

    The `Select` monad transformer represents a computation which
    can do backtracking search using a 'ranked' evaluation strategy.
    Binding a function to a monad value chains together evaluation
    strategies in the sense that the results of previous strategies
    may influence subsequent rank and evaluation strategies in
    subcomputations.

    - Class: `Control.Monad.Select`
    - Transformer: `Control.Monad.Trans.Select.SelectT`

## Resources

* [`mtl` on Hackage](http://hackage.haskell.org/package/mtl)
* The [Monad Transformers](http://dev.stephendiehl.com/hask/#monad-transformers)
  chapter in "What I Wish I Knew When Learning Haskell".
* References:
    - This package is inspired by the paper _Functional Programming
      with Overloading and Higher-Order Polymorphism_, by Mark P
      Jones, in _Advanced School of Functional Programming_, 1995
      (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).
