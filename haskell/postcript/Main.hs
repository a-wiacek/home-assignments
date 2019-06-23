module Main(main) where
import System.Environment
import Text.Read
import qualified Control.Monad.State.Strict as S
import Control.Exception
import Data.Ratio
import Data.Maybe

import Lib
import Mon

invalidArgs = "Pass one parameter N: positive integer or none for N = 1"
prologue = "300 400 translate"
epilogue = "stroke showpage"
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"

extReadMaybe :: String -> Maybe Integer
extReadMaybe "" = Nothing
extReadMaybe (h:t) = case readMaybe (h:t) of
    Just r -> Just r
    Nothing -> if h == '+' -- +40 is valid string,
        then case readMaybe t of 
            Nothing -> Nothing
            Just r -> if r >= 0
                then Just r
                else Nothing -- but +-40 not.
        else Nothing

data Lexem = LNum R | SafeMatOp (R -> R -> R) | Div
           | MoveTo | LineTo | ClosePath
           | Translate | Rotate
recognizeLexem :: String -> Lexem
recognizeLexem lexem = case extReadMaybe lexem of
    Just r -> LNum (r % 1)
    Nothing -> case lexem of
        "add" -> SafeMatOp (+)
        "sub" -> SafeMatOp (-)
        "mul" -> SafeMatOp (*)
        "div" -> Div 
        "moveto" -> MoveTo
        "lineto" -> LineTo
        "closepath" -> ClosePath
        "translate" -> Translate
        "rotate" -> Rotate 

getN :: [String] -> Maybe Int
getN [] = Just 1
getN (h:[]) = case readMaybe h of
    Just k -> if k > 0 then Just k else Nothing 
    Nothing -> Nothing
getN _ = Nothing

printRendering :: IntRendering -> [String]
printRendering l = reverse $ map f l where
    f ((a, b), (c, d)) = show a ++ " " ++ show b ++ " moveto " ++
                         show c ++ " " ++ show d ++ " lineto"

data Path = Empty | Only Point | AtLeast Point
pathToPoint :: Path -> Point
pathToPoint (Only p) = p
pathToPoint (AtLeast p) = p
-- pathToPoint Empty throws exception
-- PSState = (stack, current point, first point of current path,
--            current transformation, picture)
type PSState = ([R], Maybe Point, Path, Transform, Picture)
initState = ([], Nothing, Empty, Transform [], Picture [])
type PSValue = Picture

processInput :: [String] -> S.State PSState PSValue
processInput (lexem:lexems) = do
    (stack, currpt, path, currtr, pic) <- S.get
    case recognizeLexem lexem of
        LNum r -> S.put (r:stack, currpt, path, currtr, pic)
        SafeMatOp f -> case stack of
            h:n:t -> S.put ((f n h):t, currpt, path, currtr, pic)
        -- Lazy evaluation forces to manually throw exception.
        Div -> case stack of
            h:n:t -> if h == 0
                then throw DivideByZero
                else S.put ((n / h):t, currpt, path, currtr, pic)
        MoveTo -> case stack of
            h:n:t -> S.put (t, Just p, Only p, currtr, pic) where
                p = trpoint currtr $ Point (n, h)
        LineTo -> case stack of
            h:n:t -> S.put (t, Just $ Point p, AtLeast r, currtr, l & pic) where
                l = line q p
                p = extractPoint $ trpoint currtr $ Point (n, h)
                q = extractPoint $ fromJust currpt
                r = pathToPoint path
        ClosePath -> case path of
            AtLeast p -> S.put (stack, Just p, path, currtr, l & pic) where
                l = line (extractPoint $ fromJust currpt) (extractPoint p)
            _ -> S.modify id
        Translate -> case stack of 
            h:n:t -> S.put (t, currpt, path, newtr, pic) where
                newtr = translate (vec (n, h)) >< currtr
        Rotate -> case stack of
            h:t -> S.put (t, currpt, path, rotate h >< currtr, pic)
    processInput lexems 

processInput [] = do
    (_, _, _, _, p) <- S.get
    return p

catchExc :: SomeException -> IO ()
catchExc _ = putStrLn errorMessage

main = do
    args <- getArgs
    case getN args of
        Nothing -> putStrLn invalidArgs
        Just n -> do
            input <- getContents
            putStrLn prologue
            -- We don't differentiate errors.
            catch
                (mapM_ putStrLn $ printRendering $ renderScaled n $
                 S.evalState (processInput $ words input) initState)
                catchExc
            putStrLn epilogue
