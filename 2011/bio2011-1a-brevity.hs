c1 $$ c2=toEnum(f(mod(fromEnum c1-64+fromEnum c2-64)26)+64)
f 0=26
f n=n
b c1 c2 n|n==1=c1
 |n==2=c2
 |otherwise=b c2(c1 $$ c2)(n-1)