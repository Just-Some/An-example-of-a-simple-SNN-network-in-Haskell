
-- wsum (Взвешаная сумма) :: X -> Веса -> Σ(X * Веса)
wsum :: [Double] -> [Double] -> Double
wsum i w = sum (zipWith (*) i w)

-- ileak (Интеграция с утечкой без сброса) :: Коф забывания (где b 0,0..1,0, n*b) -> Старый заряд (прошлый рез ileak) -> Взвешеная сумма всех входов -> Заряд (без проверки порога) 
ileak :: Double -> Double -> Double -> Double
ileak b v i = b * v + (1 - b) * i

-- active (Активировать ли спайк) :: Порог для активации -> Текущий заряд -> Заряд спайка
active :: Double -> Double -> Double
active t v = if v >= t then 1.0 else 0.0 

-- reset (Сброс с сохранением излишка) :: Порог для активации -> Заряд нейрона (v) -> Заряд спайка (s) -> Типо если s = 0 тогда v иначе v - t
reset :: Double -> Double -> Double -> Double
reset t v s = v - t * s

-- stepNeuron (Шаг нейрона) :: Коф забывания (где b 0,0..1,0, n*b) -> Вход нейрона -> Веса нейрона -> Порог для активации -> Старый заряд -> (Заряд спайка, Свой заряд)
stepNeuron :: Double -> [Double] -> [Double] -> Double -> Double -> (Double, Double)
stepNeuron b i w t old = (s, v)
    where
        wsumIW = wsum i w -- Взвешеная сумма
        v' = ileak b old wsumIW -- Без сброса заряд
        s = active t v' -- Заряд спайка
        v = reset t v' s -- После сброса заряд


-- stepLayer (Шаг слоя) :: Коф забывания -> Все входные спайки (Спайки прошлого слоя, единые для всех) -> Список весов для каждого нейрона -> Список зарядов в тек слое -> Порог активации -> (Список спайков, Список потенциалов)
stepLayer :: Double -> [Double] -> [[Double]] -> [Double] -> Double -> ([Double], [Double])
stepLayer b i wss vs t = (ss, vs')
    where
        res = zipWith (\w v -> stepNeuron b i w t v) wss vs
        ss = map fst res
        vs' = map snd res

-- run (Запуск 1 слой) :: Коф забывания -> Все входные спайки -> Список весов для каждого нейрона -> Порог для активации -> Число шагов -> Истроия спайков в слое
run :: Double -> [Double] -> [[Double]] -> Double -> Int -> [[Double]]
run b i wss t n = go n (replicate (length wss) 0.0) [] -- replicate (length wss) 0.0 типо заполнить нулями
    where 
        go 0 _ acc = reverse acc -- Возвращаем истроию
        go k vs acc =   let (ss, vs') = stepLayer b i wss vs t -- Запускаем слой и запоминаем новое сотояние и спайки
                        in go (k-1) vs' (ss : acc) -- Уменьшаем шаг вызываем рекурсию и передаем новое сотояние слоя

main :: IO ()
main = do
  let b     = 0.9
      i     = [1.0, 1.0]
      wss   = [[0.5, 0.5], [0.1, 0.1]]   -- было [0.3,0.2] и [0.1,0.1]
      t     = 0.5
      steps = 30                          -- было 20
  mapM_ print (run b i wss t steps)