
type Signal = Double
type Inflow = (Neuron, Signal)

data Neuron = Neuron {
    weights :: [Double],
    vMem  :: Double,
    trace :: Double
}

data LayerCfg = LayerCfg {
    beta   :: Double, -- Коф забывания
    vThresh :: Double, -- Порог срабатывания
    -- Амплитуды STDP (LTP / LTD)
    aPlus :: Double,
    aMinus  :: Double,
    -- Коф забывания трейса
    betaTrace :: Double 
}

type Layer = ([Neuron], LayerCfg)

-- Границы весов
wMax = 1.0 :: Double
wMin = 0.0 :: Double

lif :: LayerCfg -> Double -> Double -> (Double, Double)
lif cfg wsum old = (newChrg, spike)
    where
        beta' = beta cfg
        tresh' = vThresh cfg
        chrg = beta' * old + (1 - beta') * wsum
        spike = if chrg >= tresh' then 1.0 else 0.0
        newChrg = chrg - tresh' * spike 

stdp :: LayerCfg -> Signal -> Signal -> Double -> Double -> Double -> Double
stdp cfg postSpike preSpike weight preTrace postTrace = max wMin (min wMax wLTP)
    where
        wLTD = if preSpike >= 1.0 then weight - aMinus cfg * postTrace else weight
        wLTP = if postSpike >= 1.0 then wLTD + aPlus cfg * preTrace else wLTD

stepNeuron :: LayerCfg -> [Inflow] -> Neuron -> (Neuron, Double)
stepNeuron cfg inflows neuron = (Neuron newWeights vNew postTrace, spike)
    where
        (preNeurons, preSpikes) = unzip inflows
        preTraces = zipWith (\n sp -> trace n - sp) preNeurons preSpikes
        weights' = weights neuron
        wsum = sum (zipWith (*) preSpikes weights')
        (vNew, spike) = lif cfg wsum (vMem neuron)

        postTrace' = trace neuron * betaTrace cfg
        newWeights = [stdp cfg spike preSpike weight' preTrace' postTrace' 
                            | (weight', preTrace', preSpike) <- zip3 weights' preTraces preSpikes]
        
        postTrace = postTrace' + spike

stepLayer :: Layer -> [Inflow] -> (Layer, [Inflow])
stepLayer (neurons, cfg) inflows = ((neurons', cfg), outflows)
    where 
        results = map (stepNeuron cfg inflows) neurons
        (neurons', spikes) = unzip results
        outflows = zip neurons' spikes

stepInput :: Double -> [Double] -> [Double] -> ([Inflow], [Double])
stepInput betaTr inTraces spikes = (inflows, traces')
    where
        traces' = zipWith (\tr sp -> tr * betaTr + sp) inTraces spikes
        inflows = zipWith (\tr sp -> (Neuron [] 0.0 tr, sp)) traces' spikes
        
feedAll :: [Layer] -> [Inflow] -> ([Layer], [Double])
feedAll [] inflows = ([], map snd inflows)
feedAll (l:ls) inflows = (l' : ls', outSpikes)
    where
        (l', outflows) = stepLayer l inflows
        (ls', outSpikes) = feedAll ls outflows
    
step :: [Layer] -> [Double] -> [Double] -> ([Layer], [Double], [Double])
step [] inTraces spikes = ([], inTraces, spikes)
step (l:ls) inTraces spikes =(layers', inTraces', outSpikes)
    where
        betaTr = betaTrace (snd l)
        (inflows, inTraces') = stepInput betaTr inTraces spikes
        (layers', outSpikes) = feedAll (l:ls) inflows

run :: [Layer] -> [[Double]] -> [(Int, [Layer], [Double])]
run layers0 inputs = go 0 layers0 (replicate nIn 0.0) inputs
  where
    nIn = length (head inputs)
    go _  _  _    []         = []
    go t  ls inTr (sp:rest)  =
        let (ls', inTr', outSp) = step ls inTr sp
        in (t, ls', outSp) : go (t+1) ls' inTr' rest


-- Результат
mkLayer :: [[Double]] -> LayerCfg -> Layer
mkLayer wss cfg = (map (\ws -> Neuron ws 0.0 0.0) wss, cfg)

main :: IO ()
main = do
  let cfg = LayerCfg
        { beta      = 0.9
        , vThresh   = 0.15      -- ← было 0.4
        , aPlus     = 0.02      -- ← было 0.05
        , aMinus    = 0.022     -- ← было 0.06
        , betaTrace = 0.95
        }

      l1  = mkLayer [[0.9, 0.7], [0.7, 0.9]] cfg
      l2  = mkLayer [[0.5, 0.5]]              cfg
      net = [l1, l2]

      nSteps = 500
      inputs = [ [ if t `mod` 4 == 0 then 1.0 else 0.0
                 , if t `mod` 4 == 2 then 1.0 else 0.0
                 ]
               | t <- [0 .. nSteps - 1] ]
      history = run net inputs

  putStrLn "=== Веса и счётчики спайков ==="
  mapM_ (\t -> case history !! t of
                 (_, ls, _) -> do
                   putStrLn ("--- t=" ++ show t ++ " ---")
                   printWeights ls)
        [0, 100, 200, 300, 499]

  putStrLn ""
  putStrLn "=== Сколько спайков сгенерировал каждый нейрон ==="

printWeights :: [Layer] -> IO ()
printWeights ls =
  mapM_ (\(li, (ns, _)) ->
           mapM_ (\(ni, n) ->
                    putStrLn ("  L" ++ show li ++ ".N" ++ show ni
                              ++ "  weights=" ++ show (weights n)))
                 (zip [1 :: Int ..] ns))
        (zip [1 :: Int ..] ls)