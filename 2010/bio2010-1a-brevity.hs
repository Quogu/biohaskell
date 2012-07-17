import Data.List
b n=g(foldr f""[2..9])
 where
  f x e|anagram(n*x)n=toEnum(x+48):' ':e
   |otherwise=e
anagram y z=e y==e z
e=sort.d
d n|n==0=[]
 |otherwise=(mod n 10):(d(div n 10))
g a|a==""="NO"
 |otherwise=take((length a)-1)a