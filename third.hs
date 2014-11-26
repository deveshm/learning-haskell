sum100 = sum [x ^ 2 | x <- [1 .. 100]]
replicate n a = [a | _ <- [1 ..n]]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]
factors n = [x | x <- [1 .. n], n `mod` x == 0]
perfects n = [x | x <- [1 .. n], isPerfect x] where isPerfect num = sum (init (factors num)) == num

find k t = [v | (k',v) <- t, k == k']
positions x xs = find x (zip xs [0 .. n]) where n = length xs - 1

scalarproduct xs ys = sum [x*y | (x, y) <- xs `zip` ys]

rifle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]