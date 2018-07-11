{-# LANGUAGE ScopedTypeVariables #-}
module GaussElimSlow where

import qualified Data.Vector         as V
import qualified Data.Vector.Generic as G
import qualified Text.PrettyPrint.Boxes as B


-- | Matrix type.
--
--   The outer vector contains each row of the matrix as an element. The inner
--   vector (type @v@) contains the column values for that row. Once
--   constructed, it can be assumed that the row vectors all have the same
--   length.
newtype Matrix v a = Matrix (V.Vector (v a))


-- | Pretty poor show instance.
--
--   TODO: Improve me!
instance forall a v. (Show a, G.Vector v a) => Show (Matrix v a) where
    show (Matrix rows) =
        B.render (B.emptyBox 1 2
                  B.<> B.vcat B.center1 (V.toList boxRows)
                  B.<> B.emptyBox 1 2)
      where
        boxRows :: V.Vector B.Box
        boxRows = B.hsep 3 B.right <$> (fmap (B.text . show) . G.toList <$> rows)


-- | Constructs a Matrix from a row-major list of elements.
--
--   The number of columns in the matrix must be a factor of the length of the
--   list; otherwise the matrix cannot be constructed.
fromList
    :: (G.Vector v a)
    => Int                 -- ^ Number of columns.
    -> [a]                 -- ^ Row-major list of elements.
    -> Maybe (Matrix v a)  -- ^ Constructed matrix.
fromList n xs =
    if vecLen `mod` n == 0
    then Just . Matrix . V.fromList . fmap sliceRow $ [0..(m - 1)]
    else Nothing
  where
    sliceRow i = G.slice (i * n) n fullVec
    m          = vecLen `quot` n
    vecLen     = G.length fullVec
    fullVec    = G.fromList xs


-- | Constructs a Matrix from a row-major list of elements.
--
--   The number of columns in the matrix must be a factor of the length of the
--   list; otherwise the matrix cannot be constructed.
--
--   If the matrix cannot be constructed then an error is produced.
fromList'
    :: (G.Vector v a)
    => Int              -- ^ Number of columns.
    -> [a]              -- ^ Row-major list of elements.
    -> Matrix v a       -- ^ Constructed matrix.
fromList' n xs =
    case fromList n xs of
        Just m  -> m
        Nothing -> error "fromList: invalid number of matrix columns."

