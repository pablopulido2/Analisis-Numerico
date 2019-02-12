#Número a conocer la raiz
numero = 5



#Valor inicial de n
valorini= 4

Func = function(x) ((x^valorini)-numero)
gx = function(x) (valorini*x^(valorini-1))

newton <- function(a,b) {
  seqx = seq(0,1,0.1)
  matplot(seqx,cbind(gx(seqx),Func(seqx)),type="l",col=c("blue","red"))
  ptoMedio = 1
  i = 0
  x = 0.7
  r<-gx(x)
  error = 0
  while(Func(r) != 0 )
  {
    error<-abs(r-x)
    if(error >= 1.e-8)
    {
      x<-r
    }
    r<-gx(x)
    i = i+1
    points(rbind(c(x,Func(x))),pch=15,cex=0.4,col="green")
    cat("Iteracion=",i,"\tFunc(x)=",Func(x),"\tX=",x,"\tError=",error,"\n")
  }
  cat("Resultado = ", x , " es la raiz ", valorini, "de el numero", numero, "\n")
}

newton(0,1)