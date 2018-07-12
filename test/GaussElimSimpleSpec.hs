{-# LANGUAGE ScopedTypeVariables #-}
module GaussElimSimpleSpec (tests) where

import           GaussElimSimple

import Data.Ratio (Ratio, (%))
import Data.List (maximumBy)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import           Hedgehog            ((===))
import qualified Hedgehog            as H
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Tasty          as T
import           Test.Tasty.Hedgehog as TH


tests :: T.TestTree
tests = T.testGroup "GaussElimSimple"
    [ TH.testProperty
        "list should round-trip through a Matrix"
        prop_listRoundTrip
    , TH.testProperty
        "fromList should fail for invalid numbers of elements"
        prop_fromListInvalidElements
    , TH.testProperty
        "fromList should handle an empty list"
        prop_fromListEmpty
    , TH.testProperty
        "toColMat should construct a column matrix"
        prop_toColMat
    , TH.testProperty
        "hcat should concatenate matrices horizontally"
        prop_hcat
    , TH.testProperty
        "el should be able to access all elements"
        prop_el
    , TH.testProperty
        "subMatrix should correctly extract a sub-matrix"
        prop_subMatrix
    , TH.testProperty
        "pivot should not change elements before the pivot row"
        prop_pivotNoChangeBeforePivotRow
    , TH.testProperty
        "pivot should pick the largest candidate element"
        prop_pivotLargestElement
    , TH.testProperty
        ("pivot should only change the order of elements in each column in "
        ++ "the pivot region")
        prop_pivotSameElements
    , TH.testProperty
        "the pivot example in the documentation should work"
        unit_pivotExample
    , TH.testProperty
        "the processRows example in the documentation should work"
        unit_processRows
    , TH.testProperty
        "a toRowEchelon example should work"
        unit_toRowEchelon
    ]


-- | Generates a random matrix.
randomMatrix
    :: ( H.MonadGen m
       , G.Vector v a )
    => H.Range Int      -- ^ Range for number of rows.
    -> H.Range Int      -- ^ Range for number of columns.
    -> m a              -- ^ Generator for elements.
    -> m (Matrix v a)   -- ^ Resulting matrix.
randomMatrix range_m range_n gen_a = do
    m  <- Gen.int range_m
    n  <- Gen.int range_n
    xs <- Gen.list (Range.singleton (m * n)) gen_a
    pure (fromList' n xs)


-- | Generates a small matrix with rows of unboxed integers.
--
--   The matrix can have 0 to 7 rows and 0 to 7 columns.
smallIntMatrix :: (H.MonadGen m) => m (Matrix U.Vector Int)
smallIntMatrix =
    randomMatrix
        (Range.linear 0 7)
        (Range.linear 0 7)
        (Gen.int Range.linearBounded)


-- | Generates a small non-empty matrix with rows of unboxed integers.
--
--   The matrix can have 0 to 7 rows and 0 to 7 columns.
smallIntMatrixNonEmpty :: (H.MonadGen m) => m (Matrix U.Vector Int)
smallIntMatrixNonEmpty =
    randomMatrix
        (Range.linear 1 7)
        (Range.linear 1 7)
        (Gen.int Range.linearBounded)
    

-- | Matrices can be constructed from lists in row-major form. This test checks
--   that matrices can round-trip correctly through lists.
prop_listRoundTrip :: H.Property
prop_listRoundTrip = H.property $ do
    matrix <- H.forAll $ smallIntMatrix
    H.tripping matrix toList (fromList (nCols matrix))


-- | When constructing a matrix from a list, we rely on the list to have a
--   number of elements which is evenly-divisible by the number of columns. If
--   that's not the case, then construction should fail.
prop_fromListInvalidElements :: H.Property
prop_fromListInvalidElements = H.property $ do
    m  <- H.forAll $ Gen.int (Range.linear 3 7) -- m rows
    n  <- H.forAll $ Gen.int (Range.linear 3 7) -- n cols
    xs <- H.forAll $ Gen.list (Range.singleton ((m * n) + 1))  --elements
                              (Gen.int Range.linearBounded)
    case fromList n xs :: Maybe (Matrix U.Vector Int) of
        Just _  -> H.failure
        Nothing -> H.success


-- | We should be able to construct an empty matrix. It's a bit of an edge-case,
--   but could still perhaps occur. Special-cased unit test.
prop_fromListEmpty :: H.Property
prop_fromListEmpty = H.withTests 1 $ H.property $ do
    matrix :: Matrix U.Vector Int
           <- case fromList 0 [] of
                  Just mat -> pure mat
                  Nothing  -> H.failure
    nCols matrix   === 0
    nRows matrix   === 0
    isEmpty matrix === True


-- | Constructing a column matrix from a regular vector.
prop_toColMat :: H.Property
prop_toColMat = H.property $ do
    v :: U.Vector Int <- H.forAll $ U.fromList
                         <$> Gen.list (Range.linear 0 10)
                                      (Gen.int Range.linearBounded)
    let matrix = toColMat v
    let els = [ el matrix i j
              | i <- [0 .. (nRows matrix - 1)]
              , j <- [0 .. (nCols matrix - 1)] ]
    if U.null v
        then do
            nCols matrix   === 0
            isEmpty matrix === True
        else do
            nCols matrix  === 1
            nRows matrix  === U.length v
            toList matrix === U.toList v
            toList matrix === els


-- | We can concatenate matrices horizontally, but only if they have the same
--   number of rows.
prop_hcat :: H.Property
prop_hcat = H.property $ do
    m1 <- H.forAll $ smallIntMatrix
    m2 <- H.forAll $ smallIntMatrix
    let hc = hcat m1 m2
    if nRows m1 == nRows m2
        then do
            m <- case hc of
                     Just mat -> pure mat
                     Nothing  -> H.failure
            let el' i j
                    | j < nCols m1 = el m1 i j
                    | otherwise    = el m2 i (j - nCols m1)
                els = [ el' i j
                      | i <- [0 .. (nRows m1 - 1)]
                      , j <- [0 .. (nCols m1 + nCols m2 - 1)] ]
            nRows m  === nRows m1
            nCols m  === nCols m1 + nCols m2
            toList m === els
        else hc === Nothing
        
    
-- | The 'el' function should be able to access all elements of a matrix.
--
--   We iterate over the rows and columns of a matrix using a list
--   comprehension, to produce a row-major list of element. This should
--   precisely match the output from 'toList'.
prop_el :: H.Property
prop_el = H.property $ do
    matrix <- H.forAll $ smallIntMatrix
    let els = [ el matrix i j
              | i <- [0..(nRows matrix - 1)]
              , j <- [0..(nCols matrix - 1)]
              ]
    els === toList matrix


-- | The 'subMatrix' function extracts a sub-region of the matrix.
--
--   In this test, we extract a random sub-region and compare it to the same
--   region obtained by indexing explicitly over the specified slices.
prop_subMatrix :: H.Property
prop_subMatrix = H.property $ do
    matrix <- H.forAll $ smallIntMatrix
    let
        m = nRows matrix
        n = nCols matrix
    i' <- H.forAll $ Gen.int (Range.linear 0 (m - 1))
    m' <- H.forAll $ Gen.int (Range.linear 0 (m - 1 - i'))
    j' <- H.forAll $ Gen.int (Range.linear 0 (n - 1))
    n' <- H.forAll $ Gen.int (Range.linear 0 (n - 1 - j'))
    let
        s = subMatrix matrix (i', m') (j', n')
        els = [ el matrix i j
              | i <- [i' .. (i' + m' - 1)]
              , j <- [j' .. (j' + n' - 1)]
              ]
    els === toList s
    s   === fromList' n' els
    if null els
        then isEmpty s === True
        else isEmpty s === False

    
-- | Pivoting should not change the elements before the pivot row.
prop_pivotNoChangeBeforePivotRow :: H.Property
prop_pivotNoChangeBeforePivotRow = H.property $ do
    matrix <- H.forAll $ smallIntMatrixNonEmpty
    let
        m = nRows matrix
        n = nCols matrix
        maxPivotRow = min m n
    pivotRow <- H.forAll $ Gen.int (Range.linear 0 (maxPivotRow - 1))
    let
        matrix' = pivot pivotRow matrix
        sub     = subMatrix matrix  (0, pivotRow-1) (0, n)
        sub'    = subMatrix matrix' (0, pivotRow-1) (0, n)
    H.annotateShow sub
    H.annotateShow sub'
    sub === sub'


-- | Pivoting should pick the element with the largest absolute value in
--   pivot-sub-column region.
prop_pivotLargestElement :: H.Property
prop_pivotLargestElement = H.property $ do
    matrix <- H.forAll $ smallIntMatrixNonEmpty
    let
        m = nRows matrix
        n = nCols matrix
        maxPivotRow = min m n
    pivotRow <- H.forAll $ Gen.int (Range.linear 0 (maxPivotRow - 1))
    let
        matrix' = pivot pivotRow matrix
        pivotEl = el matrix' pivotRow pivotRow
        subEls  = toList
                  $ subMatrix matrix (pivotRow, (m - pivotRow)) (pivotRow, 1)
    H.annotateShow subEls
    pivotEl === maximumBy (\x y -> compare (abs x) (abs y)) subEls


-- | Checks if two lists are permutations of each other.
listsArePermutations :: (Eq a, Ord a) => [a] -> [a] -> Bool
listsArePermutations xs ys = all countsIdentical xSet
  where
    countsIdentical x = filter (== x) xs == filter (== x) ys
    xSet = Set.toList . Set.fromList $ xs
    

-- | Pivoting should not change the elements in each column after the
--   pivot row; only their order.
prop_pivotSameElements :: H.Property
prop_pivotSameElements = H.property $ do
    matrix <- H.forAll $ smallIntMatrixNonEmpty
    let
        m = nRows matrix
        n = nCols matrix
        maxPivotRow = min m n
    pivotRow <- H.forAll $ Gen.int (Range.linear 0 (maxPivotRow - 1))
    let
        matrix'    = pivot pivotRow matrix
        checkCol j = listsArePermutations a b === True
          where
            a = toList (subMatrix matrix  (pivotRow, (m - pivotRow)) (j, 1))
            b = toList (subMatrix matrix' (pivotRow, (m - pivotRow)) (j, 1))
    sequence_ (checkCol <$> [0..(n - 1)])


-- | Check the simple example shown in the documentation for pivot. This is more
--   a sanity-check than anything else.
unit_pivotExample :: H.Property
unit_pivotExample = H.withTests 1 $ H.property $ do
    let
        matrix :: Matrix U.Vector Int
            = fromList' 4
                 [ 1, 4, 8, 2
                 , 0, 1, 9, 1
                 , 0, 3, 2, 4 ]
        matrix' :: Matrix U.Vector Int
            = fromList' 4
                 [ 1, 4, 8, 2
                 , 0, 3, 2, 4
                 , 0, 1, 9, 1 ]
    pivot 1 matrix === matrix'


-- | Check the processRows example in the documentation.
unit_processRows :: H.Property
unit_processRows = H.withTests 1 $ H.property $ do
    let
        matrix :: Matrix V.Vector (Ratio Int)
            = fromList' 4
              [ 1, 4, 8, 2
              , 0, 3, 2, 4
              , 0, 1, 9, 1 ]
        matrix' :: Matrix V.Vector (Ratio Int)
            = fromList' 4
              [ 1, 4,      8,        2
              , 0, 3,      2,        4
              , 0, 0, 25 % 3, (-1) % 3 ]
    processRows 1 matrix === matrix'


-- | Simple example for row echelon form.
unit_toRowEchelon :: H.Property
unit_toRowEchelon = H.withTests 1 $ H.property $ do
    let
        matrix :: Matrix V.Vector (Ratio Int)
            = fromList' 4
              [ 1, 1, 1, 4
              , 2, 1, 3, 7
              , 3, 1, 6, 2 ]
        matrix' :: Matrix V.Vector (Ratio Int)
            = fromList' 4
              [ 3,     1,        6,      2
              , 0, 2 % 3,       -1, 10 % 3
              , 0,     0, (-1) % 2,      4 ]
    toRowEchelon (const False) matrix === Just matrix'
