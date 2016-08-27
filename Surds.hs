module Surds where

import Data.List
import qualified Data.Map.Lazy as Map
import Math.NumberTheory.Primes.Factorisation

-- A surd can be represented as a * sqrt(b)
-- data OneSurd = Surd Int Int
type Root = Integer
type Coefficient = Integer
data Surd = Surd { surd :: Map.Map Root Coefficient} deriving (Eq)

addSurds :: Surd -> Surd -> Surd
addSurds (Surd s1) (Surd s2) =
  Surd (Map.foldlWithKey (\s k v -> Map.alter (f v) k s) s1 s2)
  where f v Nothing = Just v
        f v (Just v') = let x = v + v' in
                          case x of
                            0 -> Nothing
                            _ -> Just x

instance Monoid Surd where
  mempty = Surd Map.empty
  mappend = addSurds

mkOneSurd :: (Coefficient, Root) -> Surd
mkOneSurd (a, 0) = mempty
mkOneSurd (a, b) =
  let factorisation = factorise b
      reducer (a', b') (m, 1) = (a', b') -- cant factor this out of the root
      reducer (a', b') (m, n) =
        let n' = toInteger n
        in
          case n `mod` 2 of
            0 -> ((a * (m * (n' `div` 2))), b' `div`  (m ^ n))
            _ -> ((a * (m * ((n' - 1) `div` 2))), b' `div` (m ^ (n - 1)))
      (newA, newB) = foldl reducer (a, b) factorisation
  in
      Surd (Map.fromList [(newB, newA)])


mkSurd :: [(Coefficient, Root)] -> Surd
mkSurd seeds = mconcat $ map mkOneSurd seeds

surdToFloating :: Floating a => Surd -> a
surdToFloating (Surd s) = sum $ Map.mapWithKey (\r c -> (fromInteger c) * (sqrt (fromInteger r))) s


instance Show Surd where
  show (Surd s) =
    intercalate " + " $ map (\(k, v) -> show v ++ "√" ++ show k) (Map.toList s)


testSurd1 = Surd (Map.fromList [(1, 3), (2, 5), (3, -3)])
testSurd2 = Surd (Map.fromList [(1, 7), (5, 9), (3, 3)])

-- num filth

negateSurd :: Surd -> Surd
negateSurd (Surd s1) = Surd $ Map.map negate s1

instance Num Surd where
      negate        = negateSurd
      (+)           = addSurds
      (*)           = undefined
      fromInteger n = mkSurd [(n, 1)]
      abs           = undefined
      signum        = undefined

-- ord

instance Ord Surd where
  compare s1 s2 = compare (surdToFloating s1) (surdToFloating s2)
