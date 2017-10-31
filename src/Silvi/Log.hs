{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}

module Silvi.Log where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Kind (Type)
import Data.Proxy
import Data.Semigroup (Semigroup((<>)))

import Silvi.Tuples
import Silvi.Types

import qualified Data.SemiGroup as S

--log :: (MonadRecord r m, Provider p)
--    =>  RecordBuilder r
--    -> p
--    -> m ()
--log rec p = do
--  appendRecord $ doink p mempty $ rec

-- log :: (MonadRecord rm, Provider p)
--     => RecordBuilder r 
--     -> [Type] 
--     -> m ()
-- log rec ts = do
-- appendRecord $ doink <$> ts $ rec
--
--
-- doink :: (a ~ ProviderOf b) => b -> a -> RecordBuilder as -> RecordBuilder (Provider b, as)
-- doink b a = fmap (Provider b a, )
--
--

newtype Log a = Log { fromLog :: a } deriving (Show, Functor)

mapLog f (Log a) = Log (f a)

type family LogUnit (k :: * -> *) where
  LogUnit (ExceptT  e k) = LogUnit k
  LogUnit (ListT      k) = LogUnit k
  LogUnit (MaybeT     k) = LogUnit k
  LogUnit (ReaderT  r k) = LogUnit k
  LogUnit (RWST r w s k) = LogUnit k
  LogUnit (StateT   s k) = LogUnit k
  LogUnit (WriterT  w k) = LogUnit k
  LogUnit (LoggerT  l m) = l

class (Monad m, Applicative m) => MonadLogger m where
  appendLog :: Log (LogUnit m) -> m ()
  
newtype RecordBuilder a = RecordBuilder { fromRecordBuilder :: a } deriving (Show, Functor)
empty = RecordBuilder ()

class (Monad m) => MonadRecord d m where
  appendRecord :: RecordBuilder d -> m ()

  default appendRecord :: (MonadLogger m, LogBuilder d m) => RecordBuilder d -> m ()
  appendRecord d = do
    l <- buildLog d
    appendLog l

instance (Monad m, MonadRecord d m) => MonadRecord d (ExceptT e m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (ListT m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (MaybeT m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (ReaderT s m) where
    appendRecord = lift . appendRecord

instance (Monad m, Monoid w, MonadRecord d m) => MonadRecord d (RWST r w s m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (StateT s m) where
    appendRecord = lift . appendRecord

instance (Monad m, Monoid w, MonadRecord d m) => MonadRecord d (WriterT w m) where
    appendRecord = lift . appendRecord

class LogBuilderProto a m b where
  buildLogProto :: RecordBuilder a -> m (Log b)

type LogBuilder a m = LogBuilderProto a m (LogUnit m)

buildLog :: (Monad m, Applicative m, LogBuilder a m) => RecordBuilder a -> m (Log (LogUnit m))
buildLog = buildLogProto

instance (LogBuilderProto xs m ys, Functor m) => LogBuilderProto (Provider x, xs) m (Provider x, ys) where
  buildLogProto b = (fmap . fmap) (x, ) $ buildLogProto $ RecordBuilder xs where
    (x, xs) = fromRecordBuilder b

instance (LogBuilderProto (Provider x, xs) m ys, LogBuilderProto xs m (Provider y, ()), Monad m) => LogBuilderProto (Provider x, xs) m (Provider y, ys) where
  buildLogProto b = do
    let (x, xs) = fromRecordBuilder b
    Log ys      <- buildLogProto b
    Log (y, ()) <- buildLogProto $ RecordBuilder xs
    pure $ Log (y, ys)

instance (Monad m) => LogBuilderProto a m () where
  buildLogProto _ = pure $ Log ()

instance (Functor m, Applicative m, ProvGetter y m, LogBuilderProto () m ys) => LogBuilderProto () m (Provider y, ys) where
  buildLogProto b = fmap Log $ (,) <$> getProv <*> (fromLog <$> buildLogProto b)

doink :: (a ~ ProviderOf b) => b -> a -> RecordBuilder as -> RecordBuilder (Provider b, as)
doink b a = fmap (Provider b a, )

data Provider b = Provider { recBase :: b
                           , recProv :: ProviderOf b
                           }

type family ProviderOf a :: * where
  ProviderOf IPv4           = IPv4
  ProviderOf OffsetDatetime = OffsetDatetime
  ProviderOf HttpMethod     = HttpMethod
  ProviderOf HttpStatus     = HttpStatus
  ProviderOf HttpProtocol   = HttpProtocolVersion
  ProviderOf Url            = Url
  ProviderOf UserId         = UserId
  ProviderOf ObjSize        = ObjSize
  ProviderOf LogLevel       = LogLevel
  
class ProvGetter b m where
  getProv :: m (Provider b)

newtype LoggerT l m a = LoggerT { runRawLoggerT :: m a } deriving (Monad, MonadIO, Applicative, Functor)

runLoggerT :: l -> LoggerT (MapRTuple Provider (Tuple2RTuple l)) m a -> m a
runLoggerT _ = runRawLoggerT

runLogger d = runIdentity . runLoggerT d

instance (Applicative m, Monad m) => MonadLogger (LoggerT l m) where
  appendLog _ = pure ()

instance MonadTrans (LoggerT l) where
  lift = LoggerT

instance Monad m => MonadRecord d (LoggerT l m) where
  appendRecord _ = pure ()

data LogLevel = Debug     -- ^ Debug logs
              | Info      -- ^ Information
              | Notice    -- ^ Normal runtime conditions
              | Warning   -- ^ General warning(s)
              | Error     -- ^ General error(s)
              | Critical  -- ^ Severe situation(s)
              | Alert     -- ^ Take immediate action
              | Panic     -- ^ System is unusable
              | Other     -- ^ Other
              deriving (Eq, Show, Read)

data 
newtype Format a = Format { runFormatter :: (Monoid b) => Log a ->  }

mapFormat f (Format a) = Format (f a)

class FormatBuilder a b where
  buildFormat :: a -> Format b

(<:>) :: (FormatBuilder a c, FormatBuilder b c) => a -> b -> Format c
(<:>) a b = concatFormats (buildFormat a) (buildFormat b)

concatFormat :: Format a -> Format a -> Format a
concatFormat (Format f) (Format g) = (\s -> f s <> g s)

(<++>) :: Format a -> Format a -> Format a
(<++>) = concatFormat

instance (a ~ b) => FormatBuilder (Format a) b where
  buildFormat = id
