{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module GaussElimSimple where

import Data.List (foldl', maximumBy)
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as G
import qualified Text.PrettyPrint.Boxes as B


-- | Matrix type.
--
--   The outer vector contains each row of the matrix as an element. The inner
--   vector (type @v@) contains the column values for that row. Once
--   constructed, it can be assumed that the row vectors all have the same
--   length.
newtype Matrix v a = Matrix (V.Vector (v a)) deriving (Eq, Show)


-- | Empty matrix.
empty :: Matrix v a
empty = Matrix V.empty


-- | Checks if a matrix is empty.
isEmpty :: Matrix v a -> Bool
isEmpty (Matrix rows) = V.null rows


-- | Converts a vector into a column matrix.
toColMat
    :: ( G.Vector v a )
    => v a
    -> Matrix v a
toColMat = Matrix . V.fromList . fmap G.singleton . G.toList


-- | Horizontally concatenate matrices.
--
--   This only works if both matrices have the same number of rows.
hcat
    :: ( G.Vector v a )
    => Matrix v a
    -> Matrix v a
    -> Maybe (Matrix v a)
hcat m1@(Matrix r1) m2@(Matrix r2)
    | nRows m1 /= nRows m2 = Nothing
    | otherwise            = Just (Matrix rows)
  where
    rows = V.map (uncurry (G.++)) (V.zip r1 r2)


-- | Returns the number of rows in a matrix.
nRows
    :: Matrix v a
    -> Int
nRows (Matrix rows) = V.length rows


-- | Returns the number of columns in a matrix.
nCols
    :: (G.Vector v a)
    => Matrix v a
    -> Int
nCols (Matrix rows) =
    if V.length rows > 0
    then G.length (rows V.! 0)
    else 0


-- | Fetches an element from a matrix.
el
    :: (G.Vector v a)
    => Matrix v a   -- ^ Matrix to fetch element from.
    -> Int          -- ^ Row of element (i).
    -> Int          -- ^ Column of element (j).
    -> a            -- ^ Element.
el (Matrix rows) i j = (rows V.! i) G.! j


-- | Extracts a sub-matrix.
--
--   This fetches a sub-matrix from inside a larger matrix. The region to be
--   extracted is specified using vector-package-style slices; these consist of
--   the starting element index and the length of the slice.
subMatrix
    :: (G.Vector v a)
    => Matrix v a     -- ^ Input matrix.
    -> (Int, Int)     -- ^ Row slice (starting element, length).
    -> (Int, Int)     -- ^ Column slice (starting element, length).
    -> Matrix v a     -- ^ Output sub-matrix.
subMatrix (Matrix rows) (i, m) (j, n)
    | m <= 0 || n <= 0 = empty
    | otherwise        = Matrix subvec
  where
    subvec = V.slice i m (G.slice j n <$> rows)


-- | Pretty poor show instance.
--
--   TODO: Improve me!
{-
instance forall a v. (Show a, G.Vector v a) => Show (Matrix v a) where
    show (Matrix rows) =
        B.render (B.emptyBox 1 2
                  B.<> B.vcat B.center1 (V.toList boxRows)
                  B.<> B.emptyBox 1 2)
      where
        boxRows :: V.Vector B.Box
        boxRows = B.hsep 3 B.right <$> (fmap (B.text . show) . G.toList <$> rows)
-}


-- | Constructs a Matrix from a row-major list of elements.
--
--   The number of columns in the matrix must be a factor of the length of the
--   list; otherwise the matrix cannot be constructed.
--
--   If an empty list is passed-in, or n <= 0, then an empty matrix is
--   constructed.
fromList
    :: (G.Vector v a)
    => Int                 -- ^ Number of columns.
    -> [a]                 -- ^ Row-major list of elements.
    -> Maybe (Matrix v a)  -- ^ Constructed matrix.
fromList n xs
    | n <= 0 || vecLen == 0 = Just empty
    | vecLen `mod` n == 0   = Just . Matrix . V.fromList . fmap sliceRow
                              $ [0..(m-1)]
    | otherwise             = Nothing
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


-- | Converts a Matrix to a row-major list of elements.
toList
    :: (G.Vector v a)
    => Matrix v a      -- ^ Matrix to convert.
    -> [a]             -- ^ Produced list.
toList (Matrix rows) = concat . V.toList . fmap G.toList $ rows


-- | Performs a pivot operation.
--
--   @
--   pivot i m
--   @
--
--   Pivoting examines column @i@ of the matrix, and the rows of that column
--   from row @i@ until the end. For example, in the following 3x4 matrix, if
--   @i=1@ then we are concerned with elements at @(1,1)@ and @(2,1)@, indicated
--   in square brackets:
--
--   @
--          column i = 1 (zero-based indexing)
--          |
--      1   4   8   2
--      0  [1]  9   1   -- row 1, and
--      0  [3]  2   4   -- row 2
--   @
--
--   During pivoting, we find the element with the largest absolute value from
--   the indicated sub-column. This is called the "pivot element". In the above
--   example, 3 is the pivot element. We then swap the row containing the pivot
--   element with row @i@. Thus, in the example above, after pivoting, we would
--   obtain the following matrix:
--
--   @
--      1   4   8   2
--      0   3   2   4
--      0   1   9   1
--   @
--
--   An error is raised if an attempt is made to pivot an empty matrix, since
--   this doesn't make much sense.
--
--   This kind of pivoting is referred to as "partial pivoting", and is
--   performed for reasons of numerical stability. By using the element with the
--   largest absolute value in later steps of Gaussian Eliminiation, we avoid
--   dividing by small numbers, which may approach zero and introduce large
--   errors.
pivot
    :: forall a v.
       ( Ord a
       , Num a
       , G.Vector v a )
    => Int               -- ^ The current row to pivot.
    -> Matrix v a        -- ^ Un-pivoted matrix.
    -> Matrix v a        -- ^ Pivoted matrix and pivot element.
pivot pivotRow matrix@(Matrix rows)
    | isEmpty matrix = error "Cannot pivot an empty matrix."
    | otherwise      = Matrix rows'
  where
    rows' :: V.Vector (v a)
    rows' = V.backpermute rows (V.fromList (swapFn <$> [0..(m-1)]))

    swapFn :: Int -> Int
    swapFn ii
        | ii == pivotEl  = pivotRow
        | ii == pivotRow = pivotEl
        | otherwise      = ii
    
    pivotEl :: Int
    pivotEl = snd $ maximumBy (\(x, a) (y, b) -> compare (abs x, b) (abs y, a)) candidates
    
    candidates :: [(a, Int)]
    candidates = [ (el matrix i pivotRow, i) | i <- [pivotRow..(m-1)] ] 

    m, n :: Int
    m = nRows matrix
    n = nCols matrix

    
-- | Processes rows in Gaussian Eliminiation, following pivoting.
--
--   Processing rows involves:
--
--     1. For each row @i@ below the pivot row (@i > j@), finding a coefficient
--        @c@ according to @c_i = (-1) * (el matrix i j) / (el matrix j j)@.
--        This coefficient is chosen so that step 2 zeroes-out the element at
--        @(i,j)@.
--
--     2. Adding to every element in each row @i > j@, @c@ times the
--        corresponding element in the pivot row.
--
--   An additional optimisation may be performed here: because the elements to
--   the left of column @j@ in the region of interest should all already be
--   zero, these elements will not need to be processed.
--
--   Consider the example matrix from above.
--
--   @
--      1   4   8   2
--      0   3   2   4
--      0   1   9   1
--   @
--
--   If we process this matrix with @processRows 1 matrix@ then we should
--   obtain the following (using rationals):
--
--   @
--      1        4        8         2
--      0        3        2         4
--      0        0     25 % 3  (-1) % 3
--   @
processRows
    :: forall v a.
       ( Fractional a
       , G.Vector v a )
    => Int              -- ^ Pivot row @j@; also the first non-zero column.
    -> Matrix v a       -- ^ Un-processed matrix.
    -> Matrix v a       -- ^ Processed matrix.
processRows j matrix@(Matrix rows) = Matrix (row' <$> V.fromList [0..(m-1)])
  where
    row' :: Int -> v a
    row' i
        | i <= j    = rows V.! i
        | otherwise = processRow i

    processRow :: Int -> v a
    processRow i = G.generate n el'
      where
        el' jj
            | jj < j    = 0
            | otherwise = curRow G.! jj + c * pvtRow G.! jj
        curRow = rows V.! i
        pvtRow = rows V.! j
        c = (-1) * (curRow G.! j) / (pvtRow G.! j)
    
    m, n :: Int
    m = nRows matrix
    n = nCols matrix


{-
data RowEchelonResult a
    = RESingular
    | OK a
    deriving (Eq, Show, Functor)


instance Applicative RowEchelonResult where
    pure = OK 
    RESingular <*> _        = RESingular
    _        <*> RESingular = Singular
    (OK f)   <*> (OK x)   = OK (f x)


instance Monad RowEchelonResult where
    Singular >>= _ = Singular
    OK a     >>= f = f a
-}
    

-- | Converts a matrix to row echelon (upper triangular) form.
toRowEchelon
    :: forall a v.
       ( Ord a
       , Num a
       , Fractional a
       , G.Vector v a )
    => (a -> Bool)         -- ^ Singularity check for the pivot element.
    -> Matrix v a          -- ^ Un-processed matrix.
    -> Maybe (Matrix v a)  -- ^ Matrix in row-echelon form.
toRowEchelon isPivotSingular matrix = foldl' step (pure matrix) [0..(m-1)]
  where
    step :: Maybe (Matrix v a) -> Int -> Maybe (Matrix v a)
    step mat i = do
        m <- pivot i <$> mat
        if isSingular i m
            then Nothing
            else Just (processRows i m)
    
    isSingular :: Int -> Matrix v a -> Bool
    isSingular i mat = isPivotSingular (el mat i i)
    
    m, n :: Int
    m = nRows matrix
    n = nCols matrix


backSubstitute
    :: forall a v.
       ( Num a
       , G.Vector v a )
    => Matrix v a
    -> Matrix v a
backSubstitute = undefined
    

data LinSolResult a
    = Singular
    | DimensionErr
    | LinSol a
    

-- | Solves @Ax = b@ for @x@.
linSol
    :: forall a v.
       ( Ord a
       , Num a
       , Fractional a
       , G.Vector v a )
    => (a -> Bool)                     -- ^ Singularity check for the pivot element.
    -> Matrix v a                      -- ^ Matrix @A@.
    -> V.Vector (v a)                  -- ^ Vector containing multiple @b@ values.
    -> LinSolResult (V.Vector (v a))   -- ^ Vector containing solutions @x@; one for each @b@.
linSol isPivotSingular aMat bVec =
    case maybeAugMat of
        Nothing -> DimensionErr
        Just _  ->
            case maybeRE of
                Nothing -> Singular
                Just re -> LinSol undefined -- (backSubstitute re) -- need conversion back to vectors
  where
    maybeRE :: Maybe (Matrix v a)
    maybeRE = maybeAugMat >>= toRowEchelon isPivotSingular
    
    maybeAugMat :: Maybe (Matrix v a)
    maybeAugMat = foldl' augmentColVec (Just aMat) bVec

    augmentColVec :: Maybe (Matrix v a) -> v a -> Maybe (Matrix v a)
    augmentColVec m v = m >>= flip hcat (toColMat v)
