Fx <- function(t) 3*sin(t)*cos(t)-4*sin(t)+cos(t)
Dx <- function(t) -sin(t) -4*cos(t) -3*cos(2*t)
Hx <- function(t) t - (Fx(t)/Dx(t))

newton <- function(a,b)
{
  x = seq(a,b,0.1)
  
 plot(x,Fx(x),type="l",col="red")
 if((Fx(a)-a)*(Fx(b)-b)<0)
 {
 
     t<-0
     r<-Hx(t)
     i<-0
     while (Fx(r) != 0 )
     {    
          error<-abs(Fx(t)/Dx(t))
          if(error >  1.e-4)
            t<-r
          else break
          r<-Hx(t)  
          i<-i+1
          points(rbind(c(t,Fx(t))),pch=15,cex=0.4,col="blue")
          cat("I=",i,"\tF(t) =",Fx(t),"\tT=",signif(t, digits = 4),"\tE=",error,"\n")
      }
  }
  else
  {
      cat("No tiene raíz la funcion en ese intervalo.\n")
  }
        
}
  

newton(0,1)