{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE DeriveDataTypeable #-}
#endif

-- |
-- Provides a data type similar to 'Data.Dynamic.Dynamic'
-- from @base:Data.Dynamic@, but with a 'Binary' instance.
-- This of course means that only instances of @Binary@ can
-- be encapsulated in this @Dynamic@.
module Data.Dynamic.Binary
  (
    -- re-exported for convenience
    module RT

    -- * The @Dynamic@ type
  , Dynamic

    -- * Converting to and from @Dynamic@
  , toDyn
  , fromDyn
  , fromDynamic

    -- * Accessor for the contained @TypeRep@
  , dynTypeRep
  )
where



import qualified Data.Rank1Typeable as RT
import qualified Data.ByteString.Lazy as BSL

import GHC.Exception    ( Exception )

import Data.IORef       ( IORef, newIORef, readIORef, writeIORef )
import Data.Binary      ( Binary(..), encode, decode )
import Data.Typeable    ( Typeable )

import System.IO.Unsafe ( unsafePerformIO )
import Unsafe.Coerce    ( unsafeCoerce )



-- |
-- This @Dynamic@ is a variant of the 'Data.Dynamic.Dynamic'
-- from @base:Data.Dynamic@ with a 'Binary' instance.
-- It encapsulates a value of an arbitrary
-- type, provided that the type is instance of both
-- @Typeable@ and @Binary@.
-- 
-- The advantage over just using a 'ByteString' is the type safety:
-- Raw @ByteString@s have no associated type, and the @Binary@
-- interface makes no guarantee that the representations for
-- values of different types are different.
--
-- The advantage over using a type-tagged @ByteString@ is that
-- @Dynamic@ avoids unnecessary encoding/decoding by internally
-- containing either a @ByteString@ or a decoded value.
data Dynamic = Dynamic
  RT.TypeRep
  -- the reason why we cannot simply use a `forall a . a` here
  -- is that when decoding, we have no access to the Binary
  -- instance, so the decoding needs to be postponed until the
  -- user provides an instance when calling fromDyn(amic).
  (IORef (Either BSL.ByteString LoadedContent))
#if !MIN_VERSION_base(4,8,0)
 deriving ( Typeable )
#endif

data LoadedContent = forall a . LoadedContent a (a -> BSL.ByteString)

instance Show Dynamic where
  -- the instance just prints the type representation.
  showsPrec _ (Dynamic tRep _) = showString "<<"
                                  . showsPrec 0 tRep
                                  . showString ">>"

-- here so that it isn't an orphan:
instance Exception Dynamic

instance Binary Dynamic where
  put (Dynamic tRep vRef) = do
    put tRep
    put $ unsafePerformIO $ flip fmap (readIORef vRef) $ \eith ->
      case eith of
        Left bs -> bs
        Right (LoadedContent v enc) -> enc v
  get = do
    tRep <- get
    bs <- get
    return $ Dynamic tRep
           $ unsafePerformIO
           $ newIORef
           $ Left
           $ bs

-- |
-- Converts an arbitrary value into an object of type 'Dynamic'.
toDyn :: (Typeable a, Binary a) => a -> Dynamic
toDyn v = Dynamic (RT.typeOf v) e
  where
    e = unsafePerformIO
      $ newIORef
      $ Right
      $ LoadedContent v encode

-- |
-- Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.
fromDyn :: (Typeable a, Binary a)
        => Dynamic
        -> a -- ^ default value
        -> a -- ^ if types match, the value contained in the @Dynamic@,
             --   otherwise the default value.
fromDyn (Dynamic tRep vRef) def
  | RT.typeOf def == tRep = readFromIORef vRef
  | otherwise             = def

-- |
-- Converts a 'Dynamic' object back into an ordinary Haskell value of
-- the correct type.
fromDynamic :: forall a . (Typeable a, Binary a)
            => Dynamic
            -> Maybe a -- ^ if types match, @Just@ the contained value,
                       --   otherwise @Nothing@.
fromDynamic (Dynamic tRep vRef) = if tRep == RT.typeOf (undefined::a)
  then Just $ readFromIORef vRef
  else Nothing

readFromIORef :: forall a . Binary a
              => IORef (Either BSL.ByteString LoadedContent)
              -> a
readFromIORef vRef = unsafePerformIO $ do
  eith <- readIORef vRef
  case eith of
    Left bs -> do
      let v = decode bs :: a -- this type signature is necessary
                             -- solely to make ghci work for this
                             -- module. probably some bug?
      writeIORef
        vRef
        (Right $ LoadedContent v encode)
      return $ v
    Right (LoadedContent v _) ->
      return $ unsafeCoerce v

-- |
-- Getter for the 'TypeRep' of this @Dynamic@.
dynTypeRep :: Dynamic -> RT.TypeRep
dynTypeRep (Dynamic tRep _) = tRep

-- _test :: IO ()
-- _test = do
--   let v1 = toDyn "hello, world"
--       v2 = toDyn v1
--   print v1
--   print v2
--   let b1 = encode v1
--       b2 = encode v2
--   print b1
--   print b2
--   let c1 :: Dynamic
--       c1 = decode b1
--       c2 :: Dynamic
--       c2 = decode b2
--   print c1
--   print c2
--   let v3 :: String
--       v3 = fromDyn c1 "problem"
--       v4 :: Dynamic
--       v4 = fromDyn c2 (toDyn "problem")
--       v5 :: String
--       v5 = fromDyn v4 "problem"
--       v6 :: String
--       v6 = fromDyn v4 "problem"
--   print v3
--   print v4
--   print v5
--   print v6
