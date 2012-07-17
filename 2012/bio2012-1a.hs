factors :: Int -> [Int] -- Find a list of prime factors of n.
factors n | n <= 0       = error "Cannot find factors of a number less than 1."
          | n <= 3       = [n] -- Return n if n is smaller than 3.
          | mod n 2 == 0 = 2 : factors (div n 2) -- Special case for even numbers.
          | mod n 3 == 0 = 3 : factors (div n 3) -- Special case for multiples of 3.
          | otherwise    = p : (factors (div n p))
  where
    p = fac 5 2 -- Check numbers in a pattern: 5, 7, 11, 13, 17...
    fac :: Int -> Int -> Int -- Quick prime checking.
    fac i j | mod n i == 0 = i -- If i is a factor, return i.
            | i >= s       = n -- If we've checked this far, it's prime.
            | otherwise    = fac (i+j) (6-j) -- Increment and try again.
      where
        s = (round . sqrt . fromIntegral) n -- Square root of n.

bio20121a :: Int -> Int
bio20121a n = foldr (*) 1 (filtDup (factors n)) -- Multiply the unique factors together.
  where
    filtDup :: (Eq a) => [a] -> [a] -- Filter duplicates out of the list.
    filtDup []         = []
    filtDup [x]        = [x]
    filtDup (x1:x2:xs) | x1 == x2  = filtDup (x2:xs) -- Remove duplicate.
                       | otherwise = x1 : filtDup (x2:xs) -- Include distinct.
