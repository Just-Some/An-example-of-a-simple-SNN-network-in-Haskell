module Main (main) where
import Data.List
import Data.Ord (comparing)
import Control.Monad (replicateM)

data NeuronType = E | I deriving (Eq, Show) -- Возбуждающий | Тормозящий

newtype Weight = Weight Double
newtype SpikeVal = SpikeVal Double
newtype VMemb = VMemb Double
newtype Trace = Trace Double
newtype Beta = Beta Double
newtype Threshold = Threshold Double
newtype Amplitude = Amplitude Double

data Neuron = Neuron {
    nType :: NeuronType,
    nWeights :: [Weight],
    nVMemb :: VMemb,
    postTrace :: Trace,
    nCount :: Int,
    nRefr :: Int
}

mkNeuron :: NeuronType -> Double -> [Weight] -> Neuron
mkNeuron ntype vmemb w = Neuron {
    nType = ntype,
    nWeights = w,
    nVMemb = VMemb vmemb,
    postTrace = Trace 0,
    nCount = 0,
    nRefr = 0
}

data SpikeRes = SpikeRes {
    preTrace :: Trace,
    sSpikeVal :: SpikeVal,
    preType :: NeuronType
}

data LayerCfg = LayerCfg {
    cfgBeta :: Beta,
    cfgVThresh :: Threshold,

    cfgEAPlus :: Amplitude,
    cfgEAMinus :: Amplitude,

    cfgIAPlus   :: Amplitude, 
    cfgIAMinus  :: Amplitude,

    cfgBetaTrace :: Beta,
    cfgWMax :: Weight,
    cfgWMin :: Weight,
    cfgRefract :: Int
}

data Layer = Layer {
    layerCfg     :: LayerCfg,
    layerNeurons :: [Neuron]
}

mkLayer :: LayerCfg -> [Neuron] -> Layer
mkLayer cfg neurons = Layer cfg neurons 

newtype WeightSum = WeightSum Double

weightSum :: [Weight] -> [SpikeRes] -> WeightSum
weightSum weights spikeRes = WeightSum $ sum [ sign * w * s 
    | (Weight w, SpikeRes _ (SpikeVal s) pType) <- zip weights spikeRes
    , let sign = if pType == E then 1.0 else -1.0 
    ]

membraneCharge :: LayerCfg -> WeightSum -> Neuron -> Double
membraneCharge cfg (WeightSum ws) neuron
    | nRefr neuron > 0 = 0
    | otherwise        = beta * vOld + (1 - beta) * ws
    where
        Beta beta = cfgBeta cfg
        VMemb vOld = nVMemb neuron


stdp :: LayerCfg -> NeuronType -> SpikeVal -> SpikeVal -> Weight -> Trace -> Trace -> Weight
stdp cfg preNeuronType (SpikeVal preValSpike) (SpikeVal postValSpike) (Weight weight) (Trace pre_Trace) (Trace post_Trace) = newWeight 
    where
        magnitude = abs weight

        Amplitude aMinus = if preNeuronType == E then cfgEAMinus cfg else cfgIAMinus cfg
        Amplitude aPlus = if preNeuronType == E then cfgEAPlus cfg else cfgIAPlus cfg
            
        Weight wMin = cfgWMin cfg
        Weight wMax = cfgWMax cfg

        dLTP = case preNeuronType of
            E -> if postValSpike >= 1.0 then aPlus * (wMax - magnitude) * pre_Trace else 0
            I -> if preValSpike >= 1.0 then aPlus * (wMax - magnitude) * post_Trace else 0
        dLTD = case preNeuronType of 
            E -> if preValSpike >= 1.0 then aMinus * magnitude * post_Trace else 0
            I -> if postValSpike >= 1.0 then aMinus * magnitude * pre_Trace else 0

        clamped = max wMin (min wMax (weight + dLTP - dLTD))
        newWeight = Weight clamped

stepNeuron :: LayerCfg -> [SpikeRes] -> Bool -> Neuron -> (Neuron, SpikeRes)
stepNeuron cfg inSpikeRes fires neuron = (newNeuron, outSpikeRes)
    where
        preSpikeVals = map sSpikeVal inSpikeRes
        preTraces = map preTrace inSpikeRes
        weights = nWeights neuron

        preWsum = weightSum weights inSpikeRes

        -- [ИЗМЕНЕНО] вместо lif
        chrg         = membraneCharge cfg preWsum neuron
        postSpikeVal = SpikeVal (if fires then 1.0 else 0.0)
        membraneNew  = VMemb (if fires then 0 else chrg)   -- сброс строго в ноль
        newRefr      = if fires then cfgRefract cfg else max 0 (nRefr neuron - 1)

        Trace oldPostTrace = postTrace neuron
        Beta betaTrace  = cfgBetaTrace cfg
        postTraceBeforeSpike = Trace $ oldPostTrace * betaTrace

        preTypes = map preType inSpikeRes
        
        newWeights = [stdp cfg preType' preSpikeVal' postSpikeVal weights' preTrace' postTraceBeforeSpike
            | (preType', preSpikeVal', preTrace', weights') <- zip4 preTypes preSpikeVals preTraces weights]

        Trace floatPostTraceBeforeSpike = postTraceBeforeSpike
        SpikeVal floatPostSpikeVal = postSpikeVal
        newPostTrace = Trace $ floatPostTraceBeforeSpike + floatPostSpikeVal

        neuronCounter = nCount neuron

        newNeuron = Neuron {
            nType  = nType neuron,
            nWeights = newWeights,
            nVMemb  = membraneNew,
            postTrace = newPostTrace,
            nCount = neuronCounter + if floatPostSpikeVal >= 1.0 then 1 else 0,
            nRefr = newRefr -- [НОВОЕ]
        }
        outSpikeRes = SpikeRes {
            preTrace  = newPostTrace,
            sSpikeVal = postSpikeVal,
            preType   = nType neuron
        }

stepLayer :: Layer -> [SpikeRes] -> (Layer, [SpikeRes])
stepLayer (Layer cfg neurons) spikeRes = (newLayer, outSpikes)
  where
    Threshold th = cfgVThresh cfg

    charges = [ membraneCharge cfg (weightSum (nWeights n) spikeRes) n | n <- neurons ]

    cands = [ (c - th, i, nType n)
            | (i, c, n) <- zip3 [0 :: Int ..] charges neurons
            , nRefr n == 0
            , c >= th ]

    winnerOf ty = case [ (m, i) | (m, i, ty') <- cands, ty' == ty ] of
        [] -> []
        xs -> [ snd (maximumBy (comparing (\(m, i) -> (m, negate i))) xs) ]
    winners = winnerOf E ++ winnerOf I
    losers  = [ i | (_, i, _) <- cands, i `notElem` winners ]

    res = [ stepNeuron cfg spikeRes (i `elem` winners) n
          | (i, n) <- zip [0 :: Int ..] neurons ]
    (stepped, outSpikes) = unzip res

    newNeurons = [ if i `elem` losers then n { nVMemb = VMemb 0 } else n
                 | (i, n) <- zip [0 :: Int ..] stepped ]

    newLayer = Layer cfg newNeurons

data Input = Input {
    iVals :: [Double],
    iTrace :: [Trace],
    iBeta :: Beta
}

mkInput :: Int -> Double -> Input
mkInput nIn beta = Input [] (replicate nIn (Trace 0)) (Beta beta)

stepInput :: Input -> (Input, [SpikeRes])
stepInput (Input vals trace (Beta beta)) = (newInput, spikeRes)
    where
        traces = zipWith (\(Trace tr) sp -> Trace $ tr * beta + sp) trace vals
        spikeRes = [ 
            SpikeRes {
                preTrace = Trace (tr * beta + v),
                sSpikeVal = SpikeVal v,
                preType = E
            }
            | (Trace tr, v) <- zip trace vals]
        newInput = Input {
            iVals = vals,
            iTrace = traces,
            iBeta = Beta beta
        }

stepLayers :: [Layer] -> [SpikeRes] -> ([Layer], [SpikeRes])
stepLayers [] inpSpikes = ([], inpSpikes)
stepLayers (layer:rest) inputSpikes = (newLayer : newLayers, spikeValRes)
    where
        (newLayer, resSpikes) = stepLayer layer inputSpikes
        (newLayers, spikeValRes) = stepLayers rest resSpikes

data Network = Network {
    netInput :: Input,
    netLayers :: [Layer]
}

mkNetwork :: Input -> [Layer] -> Network
mkNetwork inp lay = Network inp lay

setInputNetwork :: [Double] -> Network -> Network
setInputNetwork vals net = net { netInput = (netInput net) { iVals = vals } }

stepNetwork :: Network -> (Network, [SpikeRes])
stepNetwork (Network input layers) = (newNetwork, res)
    where
        (newInput, inpRes) = stepInput input
        (newLayers, res) = stepLayers layers inpRes
        newNetwork = Network {
            netInput = newInput,
            netLayers = newLayers
        }

data History = History {
    hisInp :: Input,
    hisN :: Int, 
    hisNet :: Network,
    hisRes :: [SpikeRes]
}

printHistory :: History -> IO ()
printHistory (History inp n net res) = do
    putStrLn $ "t=" ++ show n
    putStrLn $ "  inp=" ++ show (iVals inp)
    mapM_ printLayer (zip [1..] (netLayers net))
    putStrLn $ "  out=" ++ show [ s | SpikeRes _ (SpikeVal s) _ <- res ]
    where
        printLayer (ln, (Layer _ ns)) =
            mapM_ (\(i, Neuron t ws (VMemb v) (Trace tr) c _) ->  -- [ИЗМЕНЕНО] +1 поле
                    putStrLn $ "  L"++show ln ++" N" ++ show i ++ " [" ++ show t ++ "]"
                            ++ " w=" ++ show [ w | Weight w <- ws ]
                            ++ " c=" ++ show c)
                    (zip [1..] ns)

runDebug :: [[Double]] -> Network -> [History]
runDebug vals network = go vals network 0 []
    where
        go :: [[Double]] -> Network -> Int -> [History] -> [History]
        go [] net c hist = reverse hist
        go (vs:rest) net c hist = 
            let net' = setInputNetwork vs net
                (newNet, newRes) = stepNetwork net'
                newHist = History {
                    hisInp = netInput net' ,
                    hisN = c,
                    hisNet = newNet,
                    hisRes = newRes
                }
            in go rest newNet (c+1) (newHist : hist) 

spikesByInput :: [History] -> Int -> Int -> (Int, Int)
spikesByInput hist layerIdx neuronIdx = go 0 hist (0, 0)
  where
    go _ [] acc = acc
    go prev (h:rest) (a, b) =
      let cur  = nCountAt h
          inp  = iVals (hisInp h)
          acc' | cur > prev = case inp of
                   (x:y:_) | x >= 1.0 -> (a + 1, b)
                           | y >= 1.0 -> (a, b + 1)
                   _ -> (a, b)
               | otherwise = (a, b)
      in go cur rest acc'
    nCountAt h = nCount (neuronAt layerIdx neuronIdx (hisNet h))
    neuronAt li ni net = layerNeurons (netLayers net !! (li-1)) !! (ni-1)

main :: IO ()
main = do
    let cfg = LayerCfg {
        cfgBeta      = Beta 0.6,
        cfgVThresh   = Threshold 0.20,
        cfgEAPlus    = Amplitude 0.010,
        cfgEAMinus   = Amplitude 0.020,
        cfgIAPlus    = Amplitude 0.005,
        cfgIAMinus   = Amplitude 0.005,
        cfgBetaTrace = Beta 0.5,
        cfgWMax      = Weight 0.8,
        cfgWMin      = Weight 0.0,
        cfgRefract   = 2
    }

    let nIn = 2
    let nSteps = 1000

    let cfgL1 = cfg
    let cfgL2 = cfg
    let cfgL3 = cfg { cfgVThresh = Threshold 0.08 }

    let l1 = mkLayer cfgL1 [ mkNeuron E 0 [Weight 0.6, Weight 0.2]
                           , mkNeuron E 0 [Weight 0.2, Weight 0.6] ]

    let l2 = mkLayer cfgL2 [ mkNeuron E 0 [Weight 0.6, Weight 0.2]
                           , mkNeuron I 0 [Weight 0.2, Weight 0.6] ]

    let l3 = mkLayer cfgL3 [ mkNeuron E 0 [Weight 0.4, Weight 0.4] ]

    let net = mkNetwork (mkInput nIn 0.5) [l1, l2, l3]

    let inputs = [ [ if t `mod` 5 == 0 then 1.0 else 0.0
                , if t `mod` 5 == 3 then 1.0 else 0.0
                ]
                | t <- [0 .. nSteps - 1] ]

    let hist = runDebug inputs net

    mapM_ printHistory [hist !! t | t <- [0, 100, 500, 1000-1], t < length hist]

    let report name l n = do
            let (a, b) = spikesByInput hist l n
            putStrLn $ name ++ ": input1=" ++ show a ++ " input2=" ++ show b
    report "L1 N1" 1 1
    report "L1 N2" 1 2
    report "L2 N1" 2 1
    report "L2 N2" 2 2
    report "L3 N1" 3 1
