#Puntos originales

y=c(3,3.7,3.9,4.2,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                      
x=c(1,2,5,6,7,8,10,13,17,20,23,24,25,27,27.7,28,29,30)      
xb = c(0.62,6.9,8.38,7.7,8.16,8.8,10.7,11.75,13.39,14.25,17.2,18.5,20,22,23.2,23.5,24.2,24.8,26,26.5,27.39,27.78,28.56,28.96,28.19,29.17,29.31,29.57,30.12)
yb = c(2.5,2.4,2.95,1.84,1.4,1.25,1.65,1.25,1.28,1.42,1.39,1.3,1.17,0.9,0.7,0.8,0.9,1,1.10,1.23,1.77,2.2,2.22,2.02,2.18,2.15,2.36,2.25,2.64)

plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Pupperino ",ylim = c(0,10))
points(xb,yb,col = "red",pch =19,cex = 0.5)



#Puntos interpolados
y2  = c(2.5,3.7,4.2,6.69,6.7,4.45,7,6.1,5.87,5.15,4.3,4.1,2.64)                                                                                                      
x2  = c(0.62,2,6,8,13,17,20,23,25,27,27.7,29,30.12)      
xb2 = c(0.62,6.9,8.38,7.7,8.8,10.7,11.75,14.25,17.1)
yb2 = c(2.5,2.4,2.95,1.84,1.25,1.65,1.25,1.42,1.39)
xb2o = c(17.1,17.74,18.7)
yb2o = c(1.39,1,1.28)
xb3  = c(18.7,23.2,24.2,26,27.78,28.56,28.96,30.12)
yb3  = c(1.28,0.9,0.9,1.10,2.2,2.22,2.02,2.64)


#Se imprimen los puntos interpolados
points(x2,y2,col = "black",pch =19,cex = 0.5)
points(xb2,yb2,col = "black",pch =19,cex = 0.5)
points(xb2o,yb2o,col = "black",pch =19,cex = 0.5)
points(xb3,yb3,col = "black",pch =19,cex = 0.5)


#Llamadas para los intervalos
SplineCubico(x2,y2)
SplineCubico(xb2,yb2)
SplineCubico(xb2o,yb2o)
SplineCubico(xb3, yb3)





#Inicio codigo
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
  fSplinesNatural <- splinefun(x,y,method = "natural")
  fSplinesmonoHFC <- splinefun(x,y, method = "monoH.FC" )
  errorNatural <- calculoError(datx,fSplinesNatural,daty,length(datx))
  errorMonoHFC <- calculoError(datx,fSplinesmonoHFC,daty,length(datx))
  datosN <- data.frame(cbind(datx,daty,errorNatural))
  colnames(datosN)<-c('X','Spline Natural','Error')
  print(datosN)
  datosMHFC <- data.frame(cbind(datx,daty,errorNatural))
  colnames(datosMHFC)<-c('X','Spline MonoH.FC','Error')
  print(datosMHFC)
  
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
  for (i in 1:(length(t)-1)) {
    dat<- data.frame(t=seq(t[i],t[i+1], by=0.1) )
    fx <- function(x) (sx[1,i] + sx[2,i]*(x-t[i]) + sx[3,i]*(x-t[i])^2 + sx[4,i]*(x-t[i])^3)
    fy <- function(x) (sy[1,i] + sy[2,i]*(x-t[i]) + sy[3,i]*(x-t[i])^2 + sy[4,i]*(x-t[i])^3)
    dat$y=fy(dat$t)
    dat$x=fx(dat$t)
    points(dat$x,dat$y,type='l', col='brown',lwd=1)
    impresionCompError(x,y,dat$x,dat$y)
  }
}


