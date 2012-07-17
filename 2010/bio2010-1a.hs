import Data.List (sort) -- For working out if the numbers are anagrams.
bio20101a :: Int -> String
bio20101a n = noOrAnswer (foldr f "" [2..9])
  where
    f :: Int -> String -> String -- Append all found answers to the output.
    f x e
      | anagram (n*x) n = (toEnum (x+48) : " ") ++ e -- If this digit makes an anagram, include it.
      | otherwise                            = e -- If it doesn't, leave it out of the answer.
    anagram :: Int -> Int -> Bool -- Check if two numbers are anagrams.
    anagram n1 n2 = (sort . digits) n1 == (sort . digits) n2
    digits :: Int -> [Int] -- Convert the number into a list of digits.
    digits n
      | n==0      = []
      | otherwise = (mod n 10):(digits (div n 10))
    noOrAnswer :: String -> String -- If there is an answer, trim the last space off, else print NO.
    noOrAnswer a | a==""     = "NO"
                 | otherwise = take ((length a)-1) a