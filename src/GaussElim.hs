{-# LANGUAGE BangPatterns #-}
module GaussElim where

import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic (Vector, Mutable)
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

newtype RowI = RowI Int
newtype ColI = ColI Int
newtype LinI = LinI { unLinI :: Int }
newtype NRows = NRows { unNRows :: Int } deriving Show
newtype NCols = NCols { unNCols :: Int } deriving Show


data Matrix v a
    = Matrix
      { nCols   :: NCols
      , nRows   :: NRows
      , backing :: v a
      } deriving Show


m :: Matrix U.Vector Float
m = case fromList (NCols 3) [ 1, 2, 4, 5, 2, 4, 8, 9, 1 ] of
        Just m  -> m
        Nothing -> error "fromList failed for example"

    
thaw
    :: ( Vector v a
       , PrimMonad m )
    => Matrix v a
    -> m (Matrix (Mutable v (PrimState m)) a)
thaw matrix = do
    backing' <- V.thaw (backing matrix)
    pure (matrix { backing = backing' })


freeze
    :: ( Vector v a
       , PrimMonad m )
    => Matrix (Mutable v (PrimState m)) a
    -> m (Matrix v a)
freeze matrix = do
    backing' <- V.freeze (backing matrix)
    pure (matrix { backing = backing' })


fromList
    :: (Vector v a)
    => NCols
    -> [a]
    -> Maybe (Matrix v a)
fromList nc xs =
    if V.length v `mod` unNCols nc == 0
    then Just (Matrix nc (NRows nr) v)
    else Nothing
  where
    v = V.fromList xs
    nr = V.length v `quot` unNCols nc


colOK :: Matrix v a -> ColI -> Bool
colOK matrix (ColI j) = j < unNCols (nCols matrix)


incCol :: ColI -> ColI
incCol (ColI j) = ColI (j + 1)


rowOK :: Matrix v a -> RowI -> Bool
rowOK matrix (RowI i) = i < unNRows (nRows matrix)


incRow :: RowI -> RowI
incRow (RowI i) = RowI (i + 1)

rowToCol :: RowI -> ColI
rowToCol (RowI i) = ColI i

    
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
    -> RowI
    -> m RowI
colMax matrix j from_i = do
    e0  <- getElem matrix from_i j
    cur <- newMutVar (from_i, e0)
    loopRows matrix from_i $ \i -> do
        e      <- getElem matrix i j
        curMax <- snd <$> readMutVar cur
        if e > curMax
            then writeMutVar cur (i, e)
            else pure ()
    fst <$> readMutVar cur


-- | @e(ib, j) += c * e(ia, j)@
mulAddRows
    :: ( Num a
       , MVector v a
       , PrimMonad m )
    => Matrix (v (PrimState m)) a
    -> (RowI, a)                   -- ^ (ia, c) : source row and coefficient
    -> RowI                        -- ^ ib      : destination row
    -> m ()
mulAddRows matrix (ia, c) ib = 
    loopCols matrix (ColI 0) $ \j -> do
        eDest <- getElem matrix ia j
        eSrc  <- getElem matrix ib j
        let eDest' = eDest + c * eSrc
        setElem matrix ib j eDest'


upperTriangular
    :: ( Num a
       , Fractional a
       , Ord a
       , MVector v a
       , PrimMonad m )
    => Matrix (v (PrimState m)) a
    -> m ()
upperTriangular matrix =
    loopRows matrix (RowI 0) $ \i -> do
        pivotRow <- colMax matrix (rowToCol i) i
        pivotEl  <- getElem matrix pivotRow (rowToCol i)
        swapRows matrix i pivotRow
        loopRows matrix (incRow i) $ \i' -> do
            initEl <- getElem matrix i' (rowToCol i)
            let coeff = (-1) * initEl / pivotEl
            mulAddRows matrix (i, coeff) i'


upperTriangular'
    :: ( Num a
       , Fractional a
       , Ord a
       , Vector v a )
    => Matrix v a
    -> Matrix v a
upperTriangular' matrix = runST $ do
    matrix' <- thaw matrix
    upperTriangular matrix'
    freeze matrix'
    

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
