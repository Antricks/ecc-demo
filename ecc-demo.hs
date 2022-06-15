type Point = (Double, Double)

o :: Point
o = (1 / 0, 1 / 0) -- 1/0 ist hier als `Infinity` definiert, da wir mit floating-Point Zahlen arbeiten.
               -- Aber das ist ein Thema für wann anders.

-- Kurvenparameter: y^2 = x^3 + a*x + b
a = 2

b = 3

mod_p = 97 -- Das Modul für unseren begrenzten Körper F_p, Primzahl

-- Positiven Punkt auf Kurve in R mit gegbenem x finden
pointOnCurve :: Double -> Point
pointOnCurve x = (x, sqrt (x ** 3 + a * x + b))

-- Addition von Punkten auf der elliptischen Kurve in R
add :: Point -> Point -> Point
add p@(x_p, y_p) q@(x_q, y_q)
  | p == o = q -- Reminder: o ist das neutrale Element
  | q == o = p
  | x_p == x_q && y_p == -y_q = o -- Hier haben wir symmetrische Punkte, also p + (-p) = p-p = o
  | otherwise = (x_r, y_r)
  where
    x_r = m * m - x_p - x_q
    y_r = -y_p - m * (x_r - x_p)
    m
      | p == q = (3 * x_p ** 2 + a) / (2 * y_p) -- Bei identischen Punkten müssen wir auf die Bestimmung der Tangentensteigung durch Ableiten zurückgreifen.
      | otherwise = (y_q - y_p) / (x_q - x_p) -- Bei zwei verschiedenen, nicht symmetrischen Punkten ist die Steigung einfach zu berechnen.

-- Reihe der Vielfachen von p in R
multiplesOf :: Point -> [Point]
multiplesOf p = multiplesOfHelper p
  where
    multiplesOfHelper buf = buf : multiplesOfHelper (add buf p)

-- Ineffiziente Multiplikation in R
naiveMultiply :: Int -> Point -> Point
naiveMultiply n p = (multiplesOf p) !! (n - 1)

-- Effizientere Multiplikation in R
multiply :: Int -> Point -> Point
multiply n p =
  let (_, result) = multiply_ 0 p
   in result
  where
    multiply_ i p_i
      | 2 ^ (i + 1) > n = (n - 2 ^ i, p_i)
      | 2 ^ i > left = (left, buf)
      | 2 ^ i <= left = (left - 2 ^ i, add p_i buf)
      where
        (left, buf) = multiply_ (i + 1) (add p_i p_i)

-- Hilfsfunktion für mod mit Doubles
fmod :: Double -> Int -> Double
fmod a b
    | f < 0 && n `mod` b == 0 = fromIntegral(b)+f
    | otherwise = fromIntegral (n `mod` b) + f
    where (n, f) = properFraction (a) 