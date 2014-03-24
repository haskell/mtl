2.1.3.1
-------
* Avoid importing `Control.Monad.Instances` on GHC 7.8 to build without deprecation warnings.

2.1.3
-----
* Removed the now-irrelevant `Error` constraint from the `MonadError` instance for `Either e`.
