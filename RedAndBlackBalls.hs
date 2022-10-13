import System.Random
import Control.Monad

main = do
    --Generate 1Mil boxes
    boxes <- sequence $ makeBoxes 1000000
    --Prompt specifies one is red as a given, so remove all where both are black
    boxesWithAtLeastOneRed <- filterM (pure . (/= Box (Black, Black))) boxes
    --Since it's a given that we didn't have any boxes where both where black, this is our divisor
    print $ "How many boxes we have without both being black: " ++ show (length boxesWithAtLeastOneRed)
    --Having only boxes where at least one was red, we remove boxes where they were both red
    boxesWhereOneIsBlack <- filterM (pure . (/= Box (Red, Red))) boxesWithAtLeastOneRed
    print $ "Number of boxes where one was red and the other black: " ++ show (length boxesWhereOneIsBlack)
    print $ "Probability is: " ++ show (fromIntegral (length boxesWhereOneIsBlack)
                                        / fromIntegral (length boxesWithAtLeastOneRed))

--type Jar = [Ball]
newtype Box = Box (Ball, Ball) deriving (Show, Eq)

data Ball = Red | Black deriving (Show, Eq)

makeJar :: [Ball]
makeJar = replicate 50 Red <> replicate 50 Black

makeBox :: IO Box
makeBox = do drawFromJar makeJar

drawFromJar jar = do
    ball1 <- randomRIO (0,99)
    ball2 <- randomRIO (0,99)
    let select ball = jar !! ball
    return $ Box (select ball1, select ball2)

makeBoxes :: Int -> [IO Box]
makeBoxes 0 = []
makeBoxes count = makeBox : makeBoxes (count - 1)

