module Vector.Vector
  ( Vector(..)
  , times
  , plus
  , dot
  , vlen
  , isTheSameTypeOfVector
  ) where

data Vector = Vector  [Double]
            | Vector2 (Double,Double)
            | Vector3 (Double,Double,Double)
            | Vector4 (Double,Double,Double,Double) deriving (Show, Read)

times :: Double -> Vector -> Vector
times k (Vector2 (v1, v2))         = Vector2 (v1 * k, v2 * k)
times k (Vector3 (v1, v2, v3))     = Vector3 (v1 * k, v2 * k, v3 * k)
times k (Vector4 (v1, v2, v3, v4)) = Vector4 (v1 * k, v2 * k, v3 * k, v4 * k)
times k (Vector  xs)               = Vector  (map (*k) xs)

plus :: Vector -> Vector -> Maybe Vector
plus (Vector2 (i,j))     (Vector2 (i',j'))       = Just (Vector2 (i + i', j + j'))
plus (Vector3 (i,j,k))   (Vector3 (i',j',k'))    = Just (Vector3 (i + i', j + j', k + k'))
plus (Vector4 (i,j,k,l)) (Vector4 (i',j',k',l')) = Just (Vector4 (i + i', j + j', k + k', l + l'))
plus (Vector  xs)        (Vector  ys)            = if length xs == length ys
                                                      then Just (Vector (zipWith (+) xs ys))
                                                      else Nothing
plus _                   _                       = Nothing

dot :: Vector -> Vector -> Maybe Double
dot (Vector2 (i,j))     (Vector2 (i',j'))       = Just (i * i' + j * j')
dot (Vector3 (i,j,k))   (Vector3 (i',j',k'))    = Just (i * i' + j * j' + k * k')
dot (Vector4 (i,j,k,l)) (Vector4 (i',j',k',l')) = Just (i * i' + j * j' + k * k' + l * l')
dot (Vector  xs)        (Vector  ys)            = Just (sum $ zipWith (*) xs ys)
dot _                   _                       = Nothing

vlen :: Vector -> Int
vlen (Vector2 _)  = 2
vlen (Vector3 _)  = 3
vlen (Vector4 _)  = 4
vlen (Vector  xs) = length xs

isTheSameTypeOfVector :: Vector -> Vector -> Bool
isTheSameTypeOfVector (Vector2  _) (Vector2  _) = True
isTheSameTypeOfVector (Vector3  _) (Vector3  _) = True
isTheSameTypeOfVector (Vector4  _) (Vector4  _) = True
isTheSameTypeOfVector (Vector  v1) (Vector  v2) = vlen (Vector v1) == vlen (Vector v2)
isTheSameTypeOfVector _            _            = False
