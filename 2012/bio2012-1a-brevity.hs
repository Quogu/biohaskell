g n|n<=3=[n]
 |mod n 2==0=2:g(div n 2)
 |mod n 3==0=3:g(div n 3)
 |otherwise=p:(g(div n p))
 where 
  p=f 5 2
  f i j|mod n i==0=i
   |i>=(round.sqrt.fromIntegral)n=n
   |otherwise=f(i+j)(6-j)
b n=foldr(*)1(h(g n))
h[]=[]
h[x]=[x]
h(x1:x2:xs)|x1==x2=h(x2:xs)
 |otherwise=x1:h(x2:xs)