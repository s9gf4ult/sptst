{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}


module Main where


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


-- main = withSqlitePool ":memory:" 1 $ \pool -> runResourceT $ (flip runSqlPool) pool $ do -- this will not work
main = runResourceT $ withSqliteConn ":memory:" $ \conn -> (flip runSqlConn) conn $ do
  runMigration mgrt
  let a = Persist 24.3333335877
  k <- trace "insert !!!!" $ insert a
  b <- trace "select !!!!" $ get k
  case b of
    Just bb -> 
      liftIO $ print $ a == bb
    Nothing -> return ()
  return ()
  