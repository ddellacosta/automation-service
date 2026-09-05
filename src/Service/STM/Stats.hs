{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.Stats
-- Copyright   :  (c) 2011 David Leuschner
--                (c) 2011 Joachim Breitner
-- License     :  BSD3
-- 
-- Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>
-- Portability :  non-portable (requires STM)
--
-- This module provides variants to the function 'atomically' from
-- "Control.Concurrent.STM" which keep track of how often the transaction is
-- initiated and how often it was retried.
--
-----------------------------------------------------------------------------


module Control.Concurrent.STM.Stats 
    (
    -- * Example usage
    --
    -- $intro

    -- * Generating statistics
    --
      trackSTM 
    , trackNamedSTM
    , trackThisSTM
    , trackSTMConf
    -- * Configuring TrackSTM
    , TrackSTMConf(..)
    , defaultTrackSTMConf
    -- * More helpful exceptions
    , BlockedIndefinitelyOnNamedSTM(..)
    -- * Reading the statistics
    , getSTMStats
    , dumpSTMStats
    ) where

import Prelude hiding (catch)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IORef
import Data.Time
import Data.Traversable (for)
import Data.Typeable ( Typeable )
import Control.Exception.Base
import Control.Monad
import Text.Printf
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM
import Language.Haskell.TH (Q, Exp(AppE,VarE,LitE), Lit(StringL), Loc, location, loc_filename, loc_start, mkName)
import GHC.Conc (unsafeIOToSTM)

-- | Global state, seems to be unavoidable here.
globalRetryCountMap :: IORef (Map String (Int,Int))
globalRetryCountMap = unsafePerformIO (newIORef M.empty)
{-# NOINLINE globalRetryCountMap #-}

-- | For the most general transaction tracking function, 'trackSTMConf', all
-- settings can be configured using a 'TrackSTMConf' value.
data TrackSTMConf = TrackSTMConf 
    { tryThreshold :: Maybe Int
        -- ^ If the number of retries of one transaction run reaches this
        -- count, a warning is issued at runtime. If set to @Nothing@, disables the warnings completely.
    , globalThreshold :: Maybe Int
        -- ^ If the total number of retries of one named transaction reaches
        -- this count, a warning is issued. If set to @Nothing@, disables the
        -- warnings completely.
    , extendException :: Bool
        -- ^ If this is set, a 'BlockedIndefinitelyOnSTM' exception is replaced
        -- by a 'BlockedIndefinitelyOnNamedSTM' exception, carrying the name of
        -- the exception.
    , warnFunction :: String -> IO ()
        -- ^ Function to call when a warning is to be emitted.
    , warnInSTMFunction :: String -> IO ()
        -- ^ Function to call when a warning is to be emitted during an STM
        -- transaction. This is possibly dangerous, see the documentation to
        -- 'unsafeIOToSTM', but can be useful to detect transactions that keep
        -- retrying forever.
    }

-- | The default settings are:
--
-- > defaultTrackSTMConf = TrackSTMConf
-- >    { tryThreshold =      Just 10
-- >    , globalThreshold =    Just 3000
-- >    , exception =         True
-- >    , warnFunction =      hPutStrLn stderr
-- >    , warnInSTMFunction = \_ -> return ()
-- >    }
defaultTrackSTMConf :: TrackSTMConf
defaultTrackSTMConf = TrackSTMConf 
    { tryThreshold = Just 10
    , globalThreshold = Just 3000
    , extendException = True
    , warnFunction = hPutStrLn stderr
    , warnInSTMFunction = \_ -> return ()
    }

-- | A drop-in replacement for 'atomically'. The statistics will list this, and
-- all other unnamed transactions, as \"@_anonymous_@\" and
-- 'BlockedIndefinitelyOnSTM' exceptions will not be replaced.
-- See below for variants that give more control over the statistics and
-- generated warnings.
trackSTM :: STM a -> IO a
trackSTM = trackSTMConf defaultTrackSTMConf { extendException = False } "_anonymous_"

-- | Run 'atomically' and collect the retry statistics under the given name and using the default configuration, 'defaultTrackSTMConf'.
trackNamedSTM :: String -> STM a -> IO a
trackNamedSTM = trackSTMConf defaultTrackSTMConf

-- | This, when used as @$trackThisSTM@ in a module with @-XTemplateHaskell@ enabled,
-- will call 'trackNamedSTM' with a name automatically derived from the source
-- file name and position, e.g. \"@Test.hs:6:21@\".
trackThisSTM :: Q Exp
trackThisSTM = do
    name <- formatLoc <$> location
    -- We don't use ''trackNamedSTM here, so that this module can be used on a
    -- compile that does not support TH.
    return $ AppE (VarE (mkName "Control.Concurrent.STM.Stats.trackNamedSTM"))
                  (LitE (StringL name))
  where formatLoc :: Loc -> String
        formatLoc loc = let file = loc_filename loc
                            (line, col) = loc_start loc
                        in  printf "%s:%d:%d" file line col

-- | Run 'atomically' and collect the retry statistics under the given name,
-- while issuing warnings when the configured thresholds are exceeded.
trackSTMConf :: TrackSTMConf -> String -> STM a -> IO a
trackSTMConf (TrackSTMConf {..}) name txm = do
    counter <- newIORef 0
    let wrappedTx =
            do  unsafeIOToSTM $ do
                    i <- atomicModifyIORef counter incCounter
                    when (warnPred i) $
                        warnInSTMFunction $ msgPrefix ++ " reached try count of " ++ show i
                txm
    res <- if extendException
          then atomically wrappedTx
              `catch` (\(_::BlockedIndefinitelyOnSTM) ->
                       throwIO (BlockedIndefinitelyOnNamedSTM name))
          else atomically wrappedTx
    i <- readIORef counter
    void $ for tryThreshold $ \threshold ->
       when (i > threshold) $
            warnFunction $ msgPrefix ++ " finished after " ++ show (i-1) ++ " retries"
    void $ incGlobalRetryCount (i - 1)
    return res
  where
    incCounter i = let j = i + 1 in (j, j)
    warnPred j = case tryThreshold of
        Nothing -> False
        Just n  -> j >= 2*n && (j >= 4 * n || j `mod` (2 * n) == 0)
    msgPrefix = "STM transaction " ++ name
    incGlobalRetryCount i = do
        (k,k') <- atomicModifyIORef globalRetryCountMap $ \m -> 
                let (oldVal, m') = M.insertLookupWithKey
                                    (\_ (a1,b1) (a2,b2) -> ((,) $! a1+a2) $! b1+b2)
                                    name
                                    (1,i)
                                    m
                in (m', let j = maybe 0 snd oldVal in (j,j+i))
        for globalThreshold $ \globalRetryThreshold -> 
            when (k `div` globalRetryThreshold /= k' `div` globalRetryThreshold) $
                warnFunction $ msgPrefix ++ " reached global retry count of " ++ show k'

-- | If 'extendException' is set (which is the case with 'trackNamedSTM'), an
-- occurrence of 'BlockedIndefinitelyOnSTM' is replaced by
-- 'BlockedIndefinitelyOnNamedSTM', carrying the name of the transaction and
-- thus giving more helpful error messages.
data BlockedIndefinitelyOnNamedSTM = BlockedIndefinitelyOnNamedSTM String
    deriving (Typeable)

instance Show BlockedIndefinitelyOnNamedSTM where
    showsPrec _ (BlockedIndefinitelyOnNamedSTM name) =
        showString $ "thread blocked indefinitely in STM transaction" ++ name

instance Exception BlockedIndefinitelyOnNamedSTM



-- | Fetches the current transaction statistics data.
--
-- The map maps transaction names to counts of transaction commits and
-- transaction retries.
getSTMStats :: IO (Map String (Int, Int))
getSTMStats = readIORef globalRetryCountMap

-- | Dumps the current transaction statistics data to 'System.IO.stderr'.
dumpSTMStats :: IO ()
dumpSTMStats = do
    stats <- getSTMStats
    time <- show <$> getCurrentTime
    hPutStrLn stderr $ "STM transaction statistics (" ++ time ++ "):"
    sequence_ $
        hPrintf stderr "%-12s %10s %10s %10s\n" "Transaction" "Commits" "Retries" "Ratio" :
        [ hPrintf stderr "%-12s %10d %10d %10.2f\n" name commits retries ratio
        | (name,(commits,retries)) <- M.toList stats
        , commits > 0 -- safeguard
        , let ratio = fromIntegral retries / fromIntegral commits :: Double
        ]

-- $intro
--
-- The following example code shows how to use the module:
--
-- @
--import Control.Concurrent
--import Control.Concurrent.STM
--import Control.Monad
-- 
--import Control.Concurrent.STM.Stats
-- 
--main = do
--     var <- 'trackSTM' $ newTVar 0
--     forkIO $ forM_ [1..23] $ \i -> do
--         threadDelay (100*1000)
--         'trackNamedSTM' \"writer\" $ writeTVar var i
--     putStrLn \"Starting reader...\"
--     'trackNamedSTM' \"reader\" $ do
--         i <- readTVar var
--         when (i < 23) retry
--     putStrLn \"Reader finished.\"
--     'dumpSTMStats'
-- @
--
-- Running this program will result in this output:
-- 
-- >Starting reader...
-- >STM transaction reader finished after 23 retries
-- >Reader finished.
-- >STM transaction statistics (2011-10-09 12:28:37.188951 UTC):
-- >Transaction     Commits    Retries      Ratio
-- >_anonymous_           1          0       0.00
-- >reader                1         23      23.00
-- >writer               23          0       0.00
--
-- The function 'trackSTM' is a direct replacement for 'atomically', while
-- 'trackNamedSTM' and 'trackSTMConf' provide more control and @$@'trackThisSTM'
-- uses Template Haskell to automatically generate a good name. 
