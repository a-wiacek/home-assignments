module Lib where
import Data.Ratio
import Data.Fixed
import Data.List
import Mon

-- Basic stuff ----------------------------------------------------------------

type R = Rational
type R2 = (R, R)

mapPair f (a, b) = (f a, f b)
rsin x
    | 0 <= x  && x <= 180 = 4 * x * (180 - x) / (40500 - x * (180 - x))
    | 180 < x && x <= 360 = negate $ rsin (360 - x)
    | otherwise           = rsin $ mod' x 360 
rcos x = rsin (x + 90)
rot a (x, y) = (x * rcos a - y * rsin a, x * rsin a + y * rcos a)

-- Vectors and points ---------------------------------------------------------

data Vec = Vec R2            -- 2D vector
data Point = Point R2        -- 2D point

vecplus :: Vec -> Vec -> Vec
vecplus (Vec (a, b)) (Vec (x, y)) = Vec (a + x, b + y)
pointplus :: Point -> Point -> Point
pointplus (Point (a, b)) (Point (x, y)) = Point (a + x, b + y)

instance Eq Vec where
    Vec (a, b) == Vec (x, y) = a == x && b == y
instance Eq Point where
    Point (a, b) == Point (x, y) = a == x && b == y
instance Show Vec where
    show (Vec (a, b)) = "[" ++ show a ++ ", " ++ show b ++ "]"
instance Show Point where
    show (Point (a, b)) = "(" ++ show a ++ ", " ++ show b ++ ")"

point :: R2 -> Point
point x = Point x
extractPoint :: Point -> R2
extractPoint (Point x) = x
vec :: R2 -> Vec
vec x = Vec x

instance Mon Vec where
    m1 = Vec (0 % 1, 0 % 1)
    (><) = vecplus

-- Picture --------------------------------------------------------------------

data Picture = Picture [(Point, Point)] -- list of lines
instance Show Picture where
    show (Picture p) = "Picture " ++ show p

line :: (R, R) -> (R, R) -> Picture
line x y = Picture [(Point x, Point y)]

rectangle :: R -> R -> Picture
rectangle w h = Picture [(Point (0, 0), Point (w, 0)),
                         (Point (w, 0), Point (w, h)),
                         (Point (w, h), Point (0, h)),
                         (Point (0, h), Point (0, 0))]

(&) :: Picture -> Picture -> Picture
(Picture a) & (Picture b) = Picture (a ++ b)

type IntLine = ((Int, Int), (Int, Int))
type IntRendering = [IntLine]

renderScaled :: Int -> Picture -> IntRendering
renderScaled n (Picture p) = map scaleOneLine p where
    scaleOneR = round . (*((toInteger n) % 1))
    scaleOneLine = mapPair $ (mapPair scaleOneR) . extractPoint

-- Transformations ------------------------------------------------------------

data SingleTransform = Translation Vec | Rotation R
instance Show SingleTransform where
    show (Translation v) = "Translation by " ++ show v ++ " vector" 
    show (Rotation r) = "Rotation by " ++ show r ++ " degrees"
instance Eq SingleTransform where
    Translation v1 == Translation v2 = v1 == v2
    Rotation r1 == Rotation r2 = r1 == r2
    _ == _ = False

-- Invariant: list consists of alfternating rotations and translations
data Transform = Transform [SingleTransform]
instance Show Transform where
    show (Transform tr) = intercalate ", " $ map show tr
instance Eq Transform where
    Transform tr1 == Transform tr2 = tr1 == tr2

translate :: Vec -> Transform
translate v = Transform [Translation v]
rotate :: R -> Transform
rotate a = Transform [Rotation a]
fullCircle :: R
fullCircle = 360

instance Mon Transform where
    m1 = Transform []
    (Transform f) >< (Transform []) = Transform f
    (Transform []) >< (Transform g) = Transform g
    (Transform f) >< (Transform g) = case (head g, last f) of
        (Rotation a, Rotation b) ->
            Transform (init f ++ (Rotation (a + b)):(tail g))
        (Translation v, Translation w) ->
            Transform (init f ++ (Translation (vecplus v w)):(tail g))
        _ -> Transform (f ++ g)

singletrpoint :: SingleTransform -> Point -> Point
singletrpoint (Translation (Vec v)) p = pointplus p (Point v)
singletrpoint (Rotation a) (Point p) = Point (rot a p)
trpoint :: Transform -> Point -> Point
trpoint (Transform tr) p = foldl (flip singletrpoint) p tr

singletrvec :: SingleTransform -> Vec -> Vec
singletrvec (Translation _) v = v
singletrvec (Rotation a) (Vec v) = Vec (rot a v)
trvec :: Transform -> Vec -> Vec
trvec (Transform tr) v = foldl (flip singletrvec) v tr

transform :: Transform -> Picture -> Picture
transform tr (Picture p) = Picture (map (mapPair $ trpoint tr) p)
