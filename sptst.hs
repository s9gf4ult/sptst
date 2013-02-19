{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}


module Main where
{-|
To get this work just add needed traces to some packages, then
use `cabal-dev add-source` to install them inside the sandbox of this package and install
the deps. When you run you will see what happens
-}

import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist
import Data.Fixed
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace(trace)

share [mkPersist sqlSettings, mkMigrate "mgrt"] [persistLowerCase|
Persist
  pico Pico
  deriving Eq Show
|]


-- main = withSqlitePool ":memory:" 1 $ \pool -> runResourceT $ (flip runSqlPool) pool $ do -- this will not work because withSqlPool close connection
                                                                                            -- even earlier than runResourceT finalize statements
-- main = runResourceT $ withSqliteConn ":memory:" $ \conn -> (flip runSqlConn) conn $ do -- this will not work
main = withSqliteConn ":memory:" $ \conn -> runResourceT $ (flip runSqlConn) conn $ do -- this works, runResourceT close statements earlier
  runMigration mgrt
  let a = Persist 24.3333335877
  k <- trace "insert !!!!" $ insert a
  b <- trace "select !!!!" $ get k
  case b of
    Just bb -> 
      liftIO $ print $ a == bb
    Nothing -> return ()
  return ()
  