library(pracma)
library(PolynomF)
library(Matrix)

rm(list=ls())

fx<-function(x) exp(x)
gx<-function(x) 1/x
pf<-taylor(fx, 0, 5)
pf
intertaylorf<-function(x)0.008334245*x^5 +0.04166657*x^4 +0.16666673*x^3 +0.50000000*x^2 +1.00000000*x +1.00000000
pg<-taylor(f=gx,x0=1,n=4)
pg
intertaylorg<-function(x)1.000029*x^4 -5.000119*x^3 +10.000182*x^2 -10.000124*x +5.000032

intertaylorf(3)
errorf<-abs(intertaylorf(3)-fx(3))/fx(3)
cat("el error para e^x donde x=3 es de: ",errorf*100,"%")
intertaylorg(3)
errorg<-abs(intertaylorg(3)-gx(3)/gx(3))
cat("el error para 1/x donde x=3 es de: ",errorg*100,"%")
