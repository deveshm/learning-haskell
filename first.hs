double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

last xs = head (reverse xs)
init xs = tail (reverse xs)

qsort [] = []
qsort (x : xs) = reverse (reverse (qsort smaller) ++ [x] ++ reverse (qsort larger))
   where smaller = [a | a <- xs, a <= x]
         larger = [b | b <- xs, b > x]