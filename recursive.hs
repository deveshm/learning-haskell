m ^ 0 = 1
m ^ n = m * (Main.^) m (n - 1)

and [] = True
and (b : bs) = b && Main.and bs

concat [] = []
concat (xs : xss) = xs ++ Main.concat xss

replicate 0 _ = []
replicate n x = x : Main.replicate (n - 1) x

(x : _) !! 1 = x
(_ : xs) !! n = xs Main.!! (n - 1)

elem _ [] = False
elem x (y : ys) | x == y = True | otherwise = Main.elem x ys

merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

halve xs = splitAt (length xs `div` 2) xs

msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs) where (ys, zs) = halve xs

eval xs = foldl (\x y -> y + (10 * x)) 0 xs
