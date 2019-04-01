#Puntos originales

y=c(2.5,3,3.7,3.9,4.2,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                      
x=c(0.62,1,2,5,6,7,8,10,13,17,20,23,24,25,27,27.7,28,29,30)
xb1 = c(0.62,6.9,8.38)
yb1 = c(2.5,2.4,2.95)
xb12 =c(8.38,8.38,7.7)
yb12 =c(2.95,2.95,1.84)
xb13 = c(7.7,8.16,8.8,10.7,11.75,13.39,14.25,17.2,18.5,20,22,23.2,23.5,24.2,24.8,26,26.5,27.39,27.78,28.56,28.96,28.19,29.17,29.31,29.57,30.12)
yb13 = c(1.84,1.4,1.25,1.65,1.25,1.28,1.42,1.39,1.3,1.17,0.9,0.7,0.8,0.9,1,1.10,1.23,1.77,2.2,2.22,2.02,2.18,2.15,2.36,2.25,2.64)

plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Pupperino ",ylim = c(0,10))
points(xb1,yb1,col = "red",pch =19,cex = 0.5)
points(xb12,yb12,col = "red",pch =19,cex = 0.5)



#Puntos interpolados
y2  = c(2.5,3.7,4.2,6.69,6.7,4.45,7,6.1,5.87,5.15,4.3,4.1,2.64)                                                                                                      
x2  = c(0.62,2,6,8,13,17,20,23,25,27,27.7,29,30.12)      
xb21 = c(0.62,6.9,8.38)
yb21 = c(2.5,2.4,2.95)
xb22 =c(8.38,8.38,7.7)
yb22 =c(2.95,2.95,1.84)
xb23 = c(7.7,8.8,10.7,11.75,14.25,17.1)
yb23 = c(1.84,1.25,1.65,1.25,1.42,1.39)
xb2o = c(17.1,17.74,18.7)
yb2o = c(1.39,1,1.28)
xb4  = c(18.7,23.2,24.2,26,27.78,28.56,28.96,30.12)
yb4  = c(1.28,0.9,0.9,1.10,2.2,2.22,2.02,2.64)

#puntos volteados
#y2 <- c(2.64 , 4.1 , 4.3 , 5.15 , 5.87 , 6.1 , 7 , 4.45 , 6.7 , 6.69 , 4.2 , 3.7 , 2.5 )
#x2 <- c(30.12, 29 , 27.7 , 27 , 25 , 23 , 20 , 17 , 13 , 8 , 6 , 2 , 0.62)
#xb2 <- c(17.1,15.8, 14.25 , 11.75 , 10.7 , 8.8 , 7.7 , 8.38 , 6.9 , 0.62)
#yb2 <- c(1.39,1.4 , 1.42 , 1.25 , 1.65 , 1.25 , 1.84 , 2.95 , 2.4 , 2.5 )
#xb2o <- c(18.7, 17.74 , 17.1)
#yb2o <- c(1.28 , 1 , 1.39)
#xb3 <- c(30.12, 28.96 , 28.56 , 27.78 , 26 , 24.2 , 23.2 , 18.7)
#yb3 <- c(2.64, 2.02 , 2.22 , 2.2 , 1.10 , 0.9 , 0.9 , 1.28)


#Se imprimen los puntos interpolados
points(x2,y2,col = "black",pch =19,cex = 0.5)
points(xb21,yb21,col = "black",pch =19,cex = 0.5)
points(xb22,yb22,col = "black",pch =19,cex = 0.5)
points(xb23,yb23,col = "black",pch =19,cex = 0.5)
points(xb2o,yb2o,col = "black",pch =19,cex = 0.5)
points(xb4,yb4,col = "black",pch =19,cex = 0.5)



#Inicio codigo

calculoError1<-function(x,f,s,n)
{
  err<-0
  sol<-0
  ans<-0
  for(i in 1:n)
  {
    h<-max(x[i+1]-x[i])
    err=h^(3/2)
    ans = f(x[i])-s[i]
    if(!is.null((ans< err))||!is.null((ans==err)))
    {
      sol[i]=abs(ans)
    }
  }
  return(sol)
}
calculoError<-function(x,f,s,n)
{
  err<-0
  sol<-0
  ans<-0
  for(i in 1:n)
  {
    h<-max(x[i+1]-x[i])
    err=h^(3/2)
    ans = f(x[i])-s[i]
    if(!is.null((ans< err))||!is.null((ans==err)))
    {
      sol[i]=ans
    }
  }
  return(sol)
}

impresionCompError <- function(x,y,datx,daty)
{
  fSplinesmonoHFC <- splinefun(x,y, method = "monoH.FC" )
  errorMonoHFC <- calculoError(datx,fSplinesmonoHFC,daty,length(datx))
  datosMHFC <- data.frame(cbind(datx,daty,fSplinesmonoHFC(datx),errorMonoHFC))
  colnames(datosMHFC)<-c('X','Y','Spline MonoH.FC','Error')
  print(datosMHFC)
  
}

sumaCompError <- function(x,y,datx,daty)
{
  fSplinesmonoHFC <- splinefun(x,y, method = "monoH.FC" )
  errorMonoHFC <- calculoError1(datx,fSplinesmonoHFC,daty,length(datx))
  return(sum(errorMonoHFC))
}

SplineCubico_Aux<- function(x,y){
  a=rep(y)
  n=length(x)
  h<-(c(x,0)-c(0,x))[2:n]
  #Cuando (i,j) i = j = 2(hi+hi+1)
  #Cuando (i,j-1) = hi
  #Cuando (i,j+1) = hi+1
  A <- c(1,rep(0,times=n-1))
  for (i in 1:(n-2)) {
    # rep(0,times = i-1) y rep (0,times = n-i-2) son para poner 0 en elementos que no compongan la diagonal
    A <- rbind(A,c( rep(0,times=i-1) , c(h[i],2*(h[i]+h[i+1]),h[i+1]) , rep(0,times=n-i-2) ) )
  }
  A <- rbind(A,c(rep(0,times=n-1),1))
  casoBase<-(3/c(1,h,1,1)*(c(a,1,1) - c(1,a,1)) - 3/c(1,1,h,1)*(c(1,a,1)-c(1,1,a)))[3:n]
  b <- c(0,casoBase,0)
  #Despeje de la ecuacion de la forma Ax = b
  c <- solve(A, b)
  #Se calculan los coeficientes del polinomio de Spline
  b <- ((c(a,0) - c(0,a))/c(1,h,1) - c(1,h,1)/3*(c(c,0) + 2*c(0,c)))[2:n]
  d <- ((c(c,0) - c(0,c))/(3*c(1,h,1)))[2:n]
  return( rbind(a[1:n-1],b,c[1:n-1],d) )
}

SplineCubico<-function(x,y){
  t = 1:length(x)
  sx = SplineCubico_Aux(t,x)
  sy = SplineCubico_Aux(t,y)
  cont = 0
  for (i in 1:(length(t)-1)) {
    dat<- data.frame(t=seq(t[i],t[i+1], by=0.1) )
    fx <- function(x) (sx[1,i] + sx[2,i]*(x-t[i]) + sx[3,i]*(x-t[i])^2 + sx[4,i]*(x-t[i])^3)
    fy <- function(x) (sy[1,i] + sy[2,i]*(x-t[i]) + sy[3,i]*(x-t[i])^2 + sy[4,i]*(x-t[i])^3)
    dat$y=fy(dat$t)
    dat$x=fx(dat$t)
    points(dat$x,dat$y,type='l', col='brown',lwd=1)
    impresionCompError(x,y,dat$x,dat$y)
    cont = cont + sumaCompError(x,y,dat$x,dat$y)
  }
  return(cont)
}

#Llamadas para los intervalos
cont = 0
cont = cont + SplineCubico(x2,y2)
cont = cont + SplineCubico(xb21,yb21)
cont = cont + SplineCubico(xb22,yb22)
cont = cont + SplineCubico(xb23,yb23)
cont = cont + SplineCubico(xb2o,yb2o)
cont = cont + SplineCubico(xb4, yb4)

print("Suma de error para los puntos interpolados escogidos")
print(cont)

#funciones usadas para pruebas de las preguntas

trasladar = function(arreglo,porcentaje)
{
  arreglo = arreglo + arreglo*porcentaje
  return(arreglo)
}

desplazar = function(arreglo,cantidad)
{
  arreglo = arreglo + cantidad
  return(arreglo)
}


aleatorio = function(arreglo, margenError)
{
  signo=1
  i=0
  for (i in 1:length(arreglo)) {
    ale=runif(1)
    if(ale>0.5){
      signo = -1  
    }
    arreglo[i]= arreglo[i] +(signo*margenError*arreglo[i])
  }
  return(arreglo)
}

cotaError = function(arreglo, margenError)
{
  signo=1
  i=0
  for (i in 1:length(arreglo)) {
    ale=runif(1)
    if(ale>0.5){
      signo = -1  
    }
    arreglo[i]= arreglo[i] +(signo*margenError)
  }
  return(arreglo)
}

#pruebas primera pregunta

cantx= 1
canty= 1

x2 = desplazar(x2,cantx)
y2 = desplazar(y2,canty)
xb21 = desplazar(xb21,cantx) 
yb21 = desplazar(yb21,canty)
xb22 = desplazar(xb22,cantx)
yb22 = desplazar(yb22,canty)
xb23 = desplazar(xb23,cantx)
yb23 = desplazar(yb23,canty)
xb2o = desplazar(xb2o,cantx)
yb2o = desplazar(yb2o,canty)
xb4 = desplazar(xb4,cantx)
yb4 = desplazar(yb4,canty)

#dibujar perrito desplazado
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Pupperino desplazado ")
points(xb1,yb1,col = "red",pch =19,cex = 0.5)
points(xb12,yb12,col = "red",pch =19,cex = 0.5)
points(xb13,yb13,col = "red",pch =19,cex = 0.5)
points(x2,y2,col = "black",pch =19,cex = 0.5)
points(xb21,yb21,col = "black",pch =19,cex = 0.5)
points(xb22,yb22,col = "black",pch =19,cex = 0.5)
points(xb23,yb23,col = "black",pch =19,cex = 0.5)
points(xb2o,yb2o,col = "black",pch =19,cex = 0.5)
points(xb4,yb4,col = "black",pch =19,cex = 0.5)

cont = 0
cont = cont + SplineCubico(x2,y2)
cont = cont + SplineCubico(xb21,yb21)
cont = cont + SplineCubico(xb22,yb22)
cont = cont + SplineCubico(xb23,yb23)
cont = cont + SplineCubico(xb2o,yb2o)
cont = cont + SplineCubico(xb4, yb4)

print("Suma de error para los puntos interpolados desplazados")
print(cont)

#resetear valores

y2  = c(2.5,3.7,4.2,6.69,6.7,4.45,7,6.1,5.87,5.15,4.3,4.1,2.64)                                                                                                      
x2  = c(0.62,2,6,8,13,17,20,23,25,27,27.7,29,30.12)      
xb21 = c(0.62,6.9,8.38)
yb21 = c(2.5,2.4,2.95)
xb22 =c(8.38,8.38,7.7)
yb22 =c(2.95,2.95,1.84)
xb23 = c(7.7,8.8,10.7,11.75,14.25,17.1)
yb23 = c(1.84,1.25,1.65,1.25,1.42,1.39)
xb2o = c(17.1,17.74,18.7)
yb2o = c(1.39,1,1.28)
xb4  = c(18.7,23.2,24.2,26,27.78,28.56,28.96,30.12)
yb4  = c(1.28,0.9,0.9,1.10,2.2,2.22,2.02,2.64)

#cambiar escala (estaba lindo y no lo quise borrar :v)

cantx=-0.2
canty=-0.2

x2 = trasladar(x2,cantx)
y2 = trasladar(y2,canty)
xb21 = trasladar(xb21,cantx) 
yb21 = trasladar(yb21,canty)
xb22 = trasladar(xb22,cantx)
yb22 = trasladar(yb22,canty)
xb23 = trasladar(xb23,cantx)
yb23 = trasladar(yb23,canty)
xb2o = trasladar(xb2o,cantx)
yb2o = trasladar(yb2o,canty)
xb4 = trasladar(xb4,cantx)
yb4 = trasladar(yb4,canty)

#dibujar perrito escalado
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Pupperino escalado ")
points(xb1,yb1,col = "red",pch =19,cex = 0.5)
points(xb12,yb12,col = "red",pch =19,cex = 0.5)
points(xb13,yb13,col = "red",pch =19,cex = 0.5)
points(x2,y2,col = "black",pch =19,cex = 0.5)
points(xb21,yb21,col = "black",pch =19,cex = 0.5)
points(xb22,yb22,col = "black",pch =19,cex = 0.5)
points(xb23,yb23,col = "black",pch =19,cex = 0.5)
points(xb2o,yb2o,col = "black",pch =19,cex = 0.5)
points(xb4,yb4,col = "black",pch =19,cex = 0.5)

cont = 0
cont = cont + SplineCubico(x2,y2)
cont = cont + SplineCubico(xb21,yb21)
cont = cont + SplineCubico(xb22,yb22)
cont = cont + SplineCubico(xb23,yb23)
cont = cont + SplineCubico(xb2o,yb2o)
cont = cont + SplineCubico(xb4, yb4)

print("Suma de error para los puntos interpolados escalados")
print(cont)

#resetear valores
y2  = c(2.5,3.7,4.2,6.69,6.7,4.45,7,6.1,5.87,5.15,4.3,4.1,2.64)                                                                                                      
x2  = c(0.62,2,6,8,13,17,20,23,25,27,27.7,29,30.12)      
xb21 = c(0.62,6.9,8.38)
yb21 = c(2.5,2.4,2.95)
xb22 =c(8.38,8.38,7.7)
yb22 =c(2.95,2.95,1.84)
xb23 = c(7.7,8.8,10.7,11.75,14.25,17.1)
yb23 = c(1.84,1.25,1.65,1.25,1.42,1.39)
xb2o = c(17.1,17.74,18.7)
yb2o = c(1.39,1,1.28)
xb4  = c(18.7,23.2,24.2,26,27.78,28.56,28.96,30.12)
yb4  = c(1.28,0.9,0.9,1.10,2.2,2.22,2.02,2.64)

#Pruebas segunda pregunta


cantx= 2.524
canty= 2.524

x2 = cotaError(x2,cantx)
y2 = cotaError(y2,canty)
xb21 = cotaError(xb21,cantx) 
yb21 = cotaError(yb21,canty)
xb22 = cotaError(xb22,cantx)
yb22 = cotaError(yb22,canty)
xb23 = cotaError(xb23,cantx)
yb23 = cotaError(yb23,canty)
xb2o = cotaError(xb2o,cantx)
yb2o = cotaError(yb2o,canty)
xb4 = cotaError(xb4,cantx)
yb4 = cotaError(yb4,canty)

#dibujar perrito erroneo
plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Pupperino erroneo ")
points(xb1,yb1,col = "red",pch =19,cex = 0.5)
points(xb12,yb12,col = "red",pch =19,cex = 0.5)
points(xb13,yb13,col = "red",pch =19,cex = 0.5)
points(x2,y2,col = "black",pch =19,cex = 0.5)
points(xb21,yb21,col = "black",pch =19,cex = 0.5)
points(xb22,yb22,col = "black",pch =19,cex = 0.5)
points(xb23,yb23,col = "black",pch =19,cex = 0.5)
points(xb2o,yb2o,col = "black",pch =19,cex = 0.5)
points(xb4,yb4,col = "black",pch =19,cex = 0.5)
cont = 0
cont = cont + SplineCubico(x2,y2)
cont = cont + SplineCubico(xb21,yb21)
cont = cont + SplineCubico(xb22,yb22)
cont = cont + SplineCubico(xb23,yb23)
cont = cont + SplineCubico(xb2o,yb2o)
cont = cont + SplineCubico(xb4, yb4)

print("Suma de error para los puntos interpolados puestos de forma erronea")
print(cont)



plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Pupperino normal completo ")
points(xb1,yb1,col = "red",pch =19,cex = 0.5)
points(xb12,yb12,col = "red",pch =19,cex = 0.5)
points(xb13,yb13,col = "red",pch =19,cex = 0.5)
cont = 0
cont = cont + SplineCubico(x,y)
cont = cont + SplineCubico(xb1,yb1)
cont = cont + SplineCubico(xb12,yb12)
cont = cont + SplineCubico(xb13,yb13)

print("Suma de error para el uso de splines con todos los puntos")
print(cont)