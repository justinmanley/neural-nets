module JustinNTest where

-- a module extending NTest to include functions for training and validating neurons and single-layer networks of neurons

import NTest
import System.IO
import Data.List (transpose)

type Layer = [Neuron] -- this algoritm is complex enough to train a single-layer network (no hidden layers)

instance Show Neuron where
	show (Neuron f t ws) = "Neuron" ++ " " ++ show t ++ " " ++ show ws

--this function came from http://stackoverflow.com/a/2471396/1642618 
zipAllWith :: ([a] -> b) -> [[a]] -> [b]
zipAllWith _ [] = []
zipAllWith f xss = map f $ transpose $ xss

zipAll = zipAllWith id

--train a single neuron using the WolframAlpha algorithm for a single-perceptron system
--inputs is a matrix with N rows (each row is an input vector).  answers is a list of length N.
trainNeuron :: Neuron -> [[Double]] -> Double -> [Double] -> Neuron
trainNeuron (Neuron f t ws) inputs n answers
    | all (==0) errorvector = Neuron f t ws
    | otherwise = trainNeuron (Neuron f t_new ws_new) inputs n answers where
    	ws_new = zipWith (+) ws $ map (n*) [foldr (+) 0 $ zipWith (*) i errorvector | i <- zipAll inputs]
    	t_new = t + n * (foldr (+) 0 errorvector)
    	errorvector = zipWith (-) answers $ map (fireNeuron (Neuron f t ws)) inputs

-- need to zip layer and results because I will otherwise get all possible combinations of neurons and results
trainLayer :: Layer -> [[Double]] -> Double -> [[Double]] -> Layer
trainLayer layer inputs n answerss = [trainNeuron neuron inputs n answers | (neuron,answers) <- zip layer answerss]

{- validation layer -}
-- after we train the neurons, we need to test them separately

validateNeuron :: Neuron -> [Double] -> Double -> Bool
validateNeuron neuron inputs result = (fireNeuron neuron inputs) == result

-- need to zip layer and results because I will otherwise get all possible combinations of neurons and results
validateLayer :: Layer -> [Double] -> [Double] -> Bool
validateLayer layer inputs results = all (==True) [validateNeuron neuron inputs result | (neuron,result) <- zip layer results ]

{- test suite -}

-- I have a file called baudelaire.txt in this directory, 
--a file of baudelaire's poems acquired from the Project Gutenberg website.
baudelaire :: IO String
baudelaire = readFile "baudelaire.txt" 

baudmap :: IO String -> IO [(Char,Double)]
baudmap iostring = do
	s <- iostring
	return $ freqMap s

train :: Neuron -> IO ()
train n = do
	baudfreq <- baudmap baudelaire
	print $ trainNeuron n [[b | (a,b) <- baudfreq]] 0.1 [1]