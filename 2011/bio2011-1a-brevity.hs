x $$ y=toEnum(f(mod(fromEnum x-64+fromEnum y-64)26)+64)
f 0=26
f n=n
b x y n|n==1=x
 |n==2=y
 |otherwise=b y(x $$ y)(n-1)