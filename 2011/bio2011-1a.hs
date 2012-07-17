($$) :: Char -> Char -> Char -- Calculate the next letter in the sequence.
($$) c1 c2 = charAtPos (mod (alphaPos c1 + alphaPos c2) 26) -- Convert the letters to their place in the alphabet, add them, then convert back to a letter.
  where
    alphaPos :: Char -> Int -- Get character's position in alphabet.
    alphaPos c = fromEnum c - 64
    charAtPos :: Int -> Char -- Get character at a given position in alphabet.
    charAtPos n = toEnum (fixZero n + 64)
    fixZero :: Int -> Int -- Make sure that Z handles correctly.
    fixZero n | n==0      = 26
              | otherwise = n

bio20111a :: Char -> Char -> Int -> Char
bio20111a c1 c2 n | n==1      = c1 -- Return the first element.
                  | n==2      = c2 -- Return the second element.
                  | otherwise = bio20111a c2 (c1 $$ c2) (n-1) -- Calculate the next element in the series.