Gx = function (t) cos(3*t) +  exp(t)



fSecante <- function(x0,x1) {
  
  x = seq(x0,x1,0.1)
  
  plot(x,Gx(x),type="l",col="red")
  
  abline(h=0)
  
  x2 = x1-(((x1-x0)*(Gx(x1)))/(Gx(x1)-Gx(x0)))
  
  error = abs(x2-x1)/x2
  
  i = 0
  
  aux = Gx(x2)
  
  points(rbind(c(x2,aux)),pch=15,cex=0.4,col="blue")
  
  cat("Iteracion=",i,"\tFunc(x)=",Gx(x2),"\tX=",x2,"\tError=",error,"\n")
  
  while (error > 1.e-8) {
    
    x0 = x1
    
    x1 = x2
    
    x2 = x1-(((x1-x0)*(Gx(x1)))/(Gx(x1)-Gx(x0)))
    
    i = i+1
    
    error = abs(x2-x1)/x2
    
    aux = Gx(x2)
    
    points(rbind(c(x2,aux)),pch=15,cex=0.4,col="blue")
    
    cat("Iteracion=",i,"\tFunc(x)=",Gx(x2),"\tX=",x2,"\tError=",error,"\n")
    
  }
  
}

fSecante(-15,-14)