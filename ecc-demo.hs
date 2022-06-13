type Point = (Float, Float)

o :: Point
o = (1/0, 1/0) -- 1/0 ist hier als `Infinity` definiert, da wir mit Floating-Point Zahlen arbeiten.
               -- Aber das ist ein Thema für wann anders. 

-- Kurvenparameter: y^2 = x^3 + a*x + b
a = -7
b = 10

mod_p = 127 -- Das Modul für unseren begrenzten Körper F_p, Primzahl

-- Addition von Punkten auf der elliptischen Kurve in R
addInR :: Point -> Point -> Point
addInR p@(x_p, y_p) q@(x_q, y_q)
  | p == o = q -- Reminder: o ist das neutrale Element
  | q == o = p
  | x_p == x_q && y_p == -y_q = o -- Hier haben wir symmetrische Punkte, also p + (-p) = p-p = o
  | x_p == x_q && y_p == y_q = -- Bei identischen Punkten müssen wir auf die Bestimmung der Tangentensteigung durch Ableiten zurückgreifen.
      let m = ((3*x_p*x_p + a) * x_p ) / (2 * y_p)
          x_r = m*m - 2*x_p
          y_r = m * (x_p - x_r) - y_p
      in (x_r, y_r)
  | otherwise = -- Bei zwei verschiedenen, nicht symmetrischen Punkten ist die Steigung einfach zu berechnen.
      let m = (y_q - y_p) / (x_q - x_p)
          x_r = m*m - x_p - x_q
          y_r = m * (x_q-x_r) - y_q
      in (x_r, y_r)

-- Addition von Punkten auf der elliptischen Kurve in F_p
addInF :: Point -> Point -> Point
addInF p@(x_p, y_p) q@(x_q, y_q)
  | p == o = q -- Reminder: o ist das neutrale Element
  | q == o = p
  | x_p == x_q && y_p == -y_q = o -- Hier haben wir symmetrische Punkte, also p + (-p) = p-p = o
  | x_p == x_q && y_p == y_q = -- Bei identischen Punkten müssen wir auf die Bestimmung der Tangentensteigung durch Ableiten zurückgreifen.
      let m = (((3*x_p*x_p + a) * x_p ) / (2 * y_p)) `fmod` mod_p
          x_r = (m*m - 2*x_p) `fmod` mod_p
          y_r = (m * (x_p - x_r) - y_p) `fmod` mod_p
      in (x_r, y_r)
  | otherwise = -- Bei zwei verschiedenen, nicht symmetrischen Punkten ist die Steigung einfach zu berechnen.
      let m = ((y_q - y_p) / (x_q - x_p)) `fmod` mod_p
          x_r = (m*m - x_p - x_q) `fmod` mod_p
          y_r = (m * (x_q-x_r) - y_q) `fmod` mod_p
      in (x_r, y_r)


-- Reihe der Vielfachen von p in R
multiplesOfInR :: Point -> [Point]
multiplesOfInR p = multiplesOfHelper p where multiplesOfHelper buf = buf : multiplesOfHelper (addInR buf p)

-- Reihe der Vielfachen von p in Z_p
multiplesOfInF :: Point -> [Point]
multiplesOfInF p = multiplesOfHelper p where multiplesOfHelper buf = buf : multiplesOfHelper (addInF buf p)









-- Hilfsfunktion für mod mit Floats

fmod :: Float -> Int -> Float
fmod a b = let
  (n, f) = properFraction(a)
  in fromIntegral (mod n b) + f