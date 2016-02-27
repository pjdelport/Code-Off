-- Minor convenience libraries:
--
-- monadlist for mapAccumM
-- split for splitOn

import           Control.Monad       (replicateM)
import           Control.Monad.ListM (mapAccumM)
import           Data.Foldable       (for_)
import           Data.List           (minimumBy)
import           Data.List.Split     (splitOn)
import qualified Data.Map.Strict     as M
import           Data.Ord            (comparing)


-- Type synonyms for easier reading.
type Litre = Integer
type LiquidType = Integer
type Reservoir = M.Map LiquidType Litre

-- A jar and its compatible liquid types.
data Jar = Jar {capacity :: Litre, compatibleTypes :: [LiquidType]}
    deriving (Eq, Ord, Read, Show)

-- A jar filled with some liquid type.
data FilledJar = FilledJar {filledAmount :: Litre, filledType :: LiquidType}
    deriving (Eq, Ord, Read, Show)


-- Parse a jar specification. For example:
--
-- >>> parseJar "100,0,1"
-- Jar {capacity = 100, compatibleTypes = [0,1]}
--
parseJar :: String -> Jar
parseJar s | liters:types <- read <$> splitOn "," s
           = Jar liters types
           | otherwise = error ("parseJar: bad spec: " ++ show s)


-- Helper: Remove liquid from a reservoir.
drain :: Litre -> LiquidType -> Reservoir -> Reservoir
drain amount = M.update $ \v ->
    case v - amount of r | 0 < r -> Just r
                       0         -> Nothing
                       _         -> error ("drain: negative remainder!")


-- Fill a jar with each compatible liquid type.
--
-- This returns a list of filled jars and drained reservoirs, one for each
-- compatible liquid type. For example:
--
-- >>> fillJar (fromList [(0,10),(1,4)]) (Jar 5 [0,1,2])
-- [(fromList [(0,5),(1,4)],  FilledJar {filledAmount = 5, filledType = 0})
-- ,(fromList [(0,10)],       FilledJar {filledAmount = 4, filledType = 1})
-- ,(fromList [(0,10),(1,4)], FilledJar {filledAmount = 0, filledType = 2})]
--
fillJar :: Reservoir -> Jar -> [(Reservoir, FilledJar)]
fillJar reservoir jar =
    [ (drain amount t reservoir, FilledJar amount t)
    | t <- compatibleTypes jar
    , let amount = capacity jar `min` M.findWithDefault 0 t reservoir
    ]


-- Fill a list of jars in sequence. This combines the lists of alternative
-- fillings for each individual jar into a list of alternative fillings for the
-- whole input list of jars.
--
-- (mapAccumM is like Data.List.mapAccumL, but also combines monadic results.)
--
fillJars :: Reservoir -> [Jar] -> [(Reservoir, [FilledJar])]
fillJars = mapAccumM fillJar


main :: IO ()
main = do
    -- Read the available liquids.
    numLiquids <- readLn
    liquids <- replicateM numLiquids readLn
    let initialReservoir = M.fromAscList (zip [0..] liquids)

    -- Read the input jars.
    numJars <- readLn
    jars <- replicateM numJars (parseJar <$> getLine)

    -- Get a solution with minimum remaining liquid.
    let solutions = fillJars initialReservoir jars
    let scored = [(sum r,js) | (r,js) <- solutions]
    let (remainder, filledJars) = minimumBy (comparing fst) scored

    -- Format the output.
    if null solutions
    then error "No solutions!"
    else do
        print remainder
        for_ filledJars $ \(FilledJar amount liquid) ->
            putStrLn (show liquid ++ "," ++ show amount)
