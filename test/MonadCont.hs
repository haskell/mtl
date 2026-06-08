{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}

module Main (main) where

import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Cont.Class (MonadCont (callCC))
import Data.Proxy (Proxy)

#ifdef __GLASGOW_HASKELL__
-- Test that MonadCont is possible with polykinded ContT
x :: (ContT 42 Proxy) ()
x = callCC $ \_ -> return ()
#endif

main :: IO ()
main = return ()
