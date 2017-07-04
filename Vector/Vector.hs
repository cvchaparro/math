module Vector.Vector
  ( Vector(..)
  , times
  , plus
  , dot
  , vlength
  , isTheSameTypeOfVector
  ) where

data Vector = Vector  { elements :: [Double] }
            | Vector2 { i :: Double, j :: Double }
            | Vector3 { i :: Double, j :: Double, k :: Double }
            | Vector4 { i :: Double, j :: Double, k :: Double, l :: Double }
            deriving (Show, Read)

times :: Double -> Vector -> Vector
times c (Vector2 i j)     = Vector2 (i * c) (j * c)
times c (Vector3 i j k)   = Vector3 (i * c) (j * c) (k * c)
times c (Vector4 i j k l) = Vector4 (i * c) (j * c) (k * c) (l * c)
times c (Vector  v)       = Vector  (map (* c) v)

plus :: Vector -> Vector -> Maybe Vector
plus (Vector2 i j)     (Vector2 i' j')       = Just (Vector2 (i + i') (j + j'))
plus (Vector3 i j k)   (Vector3 i' j' k')    = Just (Vector3 (i + i') (j + j') (k + k'))
plus (Vector4 i j k l) (Vector4 i' j' k' l') = Just (Vector4 (i + i') (j + j') (k + k') (l + l'))
plus (Vector  v1)      (Vector  v2)          = if length v1 == length v2
                                                  then Just (Vector (zipWith (+) v1 v2))
                                                  else Nothing
plus _                   _                   = Nothing

dot :: Vector -> Vector -> Maybe Double
dot (Vector2 i j)     (Vector2 i' j')       = Just (i * i' + j * j')
dot (Vector3 i j k)   (Vector3 i' j' k')    = Just (i * i' + j * j' + k * k')
dot (Vector4 i j k l) (Vector4 i' j' k' l') = Just (i * i' + j * j' + k * k' + l * l')
dot (Vector  v1)      (Vector  v2)          = Just (sum $ zipWith (*) v1 v2)
dot _                   _                   = Nothing

vlength :: Vector -> Int
vlength (Vector2 _ _)     = 2
vlength (Vector3 _ _ _)   = 3
vlength (Vector4 _ _ _ _) = 4
vlength (Vector  v)       = length v

isTheSameTypeOfVector :: Vector -> Vector -> Bool
isTheSameTypeOfVector (Vector2 _ _)     (Vector2 _ _)     = True
isTheSameTypeOfVector (Vector3 _ _ _)   (Vector3  _ _ _)  = True
isTheSameTypeOfVector (Vector4 _ _ _ _) (Vector4 _ _ _ _) = True
isTheSameTypeOfVector (Vector  v1)      (Vector  v2)      = vlength (Vector v1) == vlength (Vector v2)
isTheSameTypeOfVector _            _                      = False
