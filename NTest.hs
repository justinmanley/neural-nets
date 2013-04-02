--implemented by Elliot Ding

module NTest where

import Data.List (sort,group)
import Data.Char (toLower,isAlpha)
import System.Random
import Control.Monad.State

data Neuron = Neuron {
    function :: (Double -> Double),
    threshold :: Double,
    weights :: [Double]
}

-- Transfer functions
cutoff, sigmoid :: Double -> Double
cutoff = \x -> if x > 0 then 1 else 0
-- Logistic sigmoid function with -2 offset
sigmoid = \x -> 1 / (1 + exp ((-x) - 2))

-- Feeds inputs into a neuron and returns the output.
fireNeuron :: Neuron -> [Double] -> Double
fireNeuron (Neuron f t ws) inputs = f $ sum (zipWith (*) ws inputs) - t


{- Randomness -}

type RandState = State StdGen

randGaussian :: Double -> Double -> RandState Double
randGaussian m s = do
    r1 <- randR (0,1)
    r2 <- randR (0,1)
    return $ boxMuller m s (r1,r2)
    where
        -- Pseudo-random approximation of a Gaussian distribution based on
        -- Box-Muller transformation.
        -- Mean, StDev, Rand (0,1]
        boxMuller :: Double -> Double -> (Double, Double) -> Double
        boxMuller m s (r1,r2) = m + s * sqrt (-2 * log r1) * cos (2 * pi * r2)

        -- Generates a random number within a range.
        randR :: Random a => (a, a) -> RandState a
        randR (lo,hi) = do
            gen <- get
            let (x, gen2) = randomR (lo,hi) gen
            put gen2
            return x

-- Returns a random number, based on a quasi-Gaussian distribution, given a
-- mean value and a standard deviation.
gaussianR :: Double -> Double -> IO Double
gaussianR m s = do
    gen <- newStdGen
    return $ fst $ runState (randGaussian m s) gen


{- Language Detection -}

-- Returns the frequency (as a percentage) of each of the 26 characters of the
-- standard alphabet in a given string.
freqMap :: String -> [(Char, Double)]
freqMap string = map (charFreq string2) ['a'..'z'] where

    charFreq :: String -> Char -> (Char, Double)
    charFreq s c = (c,freq) where
        count = fromIntegral $ length $ filter (==c) s
        freq = 100 * count / total

    total = fromIntegral $ length string2
    string2 = map toLower $ filter isAlpha string

-- Outputs 1 if English, outputs 0 if French or Spanish
initNeuronEn :: Neuron
initNeuronEn = Neuron cutoff 110 (replicate 19 1 ++ [2] ++ (replicate 6 1))

-- Outputs 1 if Spanish, outputs 0 if French or English
initNeuronSp :: Neuron
initNeuronSp = Neuron cutoff 112 (2 : (replicate 25 1))

initEnCount = [
    8.88, 1.22, 2,70, 5.05, 14.36,
    2.35, 2.44, 6,96, 5.92, 0.00,
    0.26, 3.66, 1.13, 6.70, 8.10,
    1.31, 8.70, 6.88, 3.83, 10.97,
    1.83, 2.09, 2.44, 0.00, 0.87, 0.00]

initSpCount = [
    12.06, 1.86, 3.24, 4.02, 14.22,
    1.57, 1.27, 0.98, 8.14, 0.29,
    0.00, 6.37, 2.25, 7.16, 7.25,
    2.35, 1.37, 5.88, 8.73, 3.33,
    4.51, 0.98, 0.00, 0.39, 0.98, 0.78]

initFrCount = [
    6.69, 0.70, 2.64, 3.50, 17.34,
    0.93, 1.94, 0.77, 7.31, 0.31,
    0.00, 6.30, 1.79, 7.31, 5.13,
    2.02, 1.63, 7.08, 8.63, 6.84,
    8.86, 1.48, 0.00, 0.08, 0.70, 0.00]
