{-# LANGUAGE BangPatterns #-}
module GaussElim where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic (Vector)
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as V

newtype RowI = RowI Int
newtype ColI = ColI Int
newtype LinI = LinI { unLinI :: Int }
newtype NRows = NRows { unNRows :: Int }
newtype NCols = NCols { unNCols :: Int }


data Matrix v a
    = Matrix
      { nCols   :: NCols
      , nRows   :: NRows
      , backing :: v a
      }


colOK :: Matrix v a -> ColI -> Bool
colOK matrix (ColI j) = j < unNCols (nCols matrix)


incCol :: ColI -> ColI
incCol (ColI j) = ColI (j + 1)


rowOK :: Matrix v a -> RowI -> Bool
rowOK matrix (RowI i) = i < unNRows (nRows matrix)


incRow :: RowI -> RowI
incRow (RowI i) = RowI (i + 1)

    
index
    :: RowI
    -> ColI
    -> NCols
    -> LinI
index (RowI i) (ColI j) (NCols n) = LinI (j * n + i)


getElem
    :: ( MVector v a
       , PrimMonad m )
    => Matrix (v (PrimState m)) a
    -> RowI
    -> ColI
    -> m a
getElem matrix i j = M.unsafeRead (backing matrix) ii
  where
    ii = unLinI (index i j (nCols matrix))


setElem
    :: ( MVector v a
       , PrimMonad m )
    => Matrix (v (PrimState m)) a
    -> RowI
    -> ColI
    -> a
    -> m ()
setElem matrix i j = M.unsafeWrite (backing matrix) ii
  where
    ii = unLinI (index i j (nCols matrix))


swapElem
    :: ( MVector v a
       , PrimMonad m )
    => Matrix (v (PrimState m)) a
    -> (RowI, ColI)
    -> (RowI, ColI)
    -> m ()
swapElem matrix (i1, j1) (i2, j2) = do
    e1 <- getElem matrix i1 j1
    e2 <- getElem matrix i2 j2
    setElem matrix i1 j1 e2
    setElem matrix i2 j2 e1


swapRows
    :: ( MVector v a
       , PrimMonad m )
    => Matrix (v (PrimState m)) a
    -> RowI
    -> RowI
    -> m ()
swapRows matrix i1 i2 =
    loopCols matrix (ColI 0) $ \j -> swapElem matrix (i1, j) (i2, j)


colMax
    :: ( Ord a
       , MVector v a
       , PrimMonad m )
    => Matrix (v (PrimState m)) a
    -> ColI
    -> m RowI
colMax matrix j = do
    e0  <- getElem matrix (RowI 0) j
    cur <- newMutVar (RowI 0, e0)
    loopRows matrix (RowI 0) $ \i -> do
        e      <- getElem matrix i j
        curMax <- snd <$> readMutVar cur
        if e > curMax
            then writeMutVar cur (i, e)
            else pure ()
    fst <$> readMutVar cur


loopRows
    :: (Monad m)
    => Matrix v a
    -> RowI
    -> (RowI -> m ())
    -> m ()
loopRows matrix start = loop start (rowOK matrix) incRow


loopCols
    :: (Monad m)
    => Matrix v a
    -> ColI
    -> (ColI -> m ())
    -> m ()
loopCols matrix start = loop start (colOK matrix) incCol
    

loop
    :: (Monad m)
    => a
    -> (a -> Bool)
    -> (a -> a)
    -> (a -> m ())
    -> m ()
loop !start !while !step !body = go start
  where
    go !x | while x   = body x >> go (step x)
          | otherwise = pure ()
