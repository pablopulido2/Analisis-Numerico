library(pracma)
library(Matrix)
#1A


#1B(i)

A=matrix(c(4,-1,-1,-1,
           -1,4,-1,-1,
           -1,-1,4,-1,
           -1,-1,-1,4),nrow=4,byrow=TRUE)
print(A)
Modi=matrix(c(4,-1,-1,-1,
           -1.15,4,-1,-1,
           -1,-1,4,-1,
           -1,-1,-1,4),nrow=4,byrow=TRUE)
print(Modi)
E=Modi-A
cat("error en la matriz: ",E)
b=matrix(c(-exp(1),5,6,0),nrow = 4)
cat(b)
errorR=norm(E)/norm(A)
print(errorR)
condicion=norm(A)* norm(solve(A))
cat("numero condicion: ",condicion)
solve(A,b,tol=0.001)
solve(Modi,b,tol=0.01)
#cota<= condicion* abs|error relativo|
cota= condicion*errorR
cat("el error relativo puede variar hasta en(%): ")
cat(cota*100)

#2a

F=function(x) tan(x* pi )-sin(x* pi )
recursiva =  function(F,xin,tole,xn)#parametros: F= funcion, xin=valor anterior(xn-2),tole=tolerancia al error, xn valor actual(xn-1)
  {
  niter=1
  contpos=0
  while(TRUE)
    xnk=xn-(F(xn)*(xn-xin)/F(xn)-F(xin))
    conv=(xnk-xn/xnk)
    cat("iteracion: ", niter)
    cat("error: ", conv)
    if(conv< tole)
      return(xnk)
    if(niter>=1000)
      cat("no converge:")
      break
    niter=niter+1
      
}
  
#se deja comentado por no saber los valores anteriores ni actuales
#xanterior
#xactual
#recursiva(F,xactual,10^-9,xanterior)

#2b(ii)
newton(F,0,tol = 10^-9 )

#el metodo de newton requiere de menos iteraciones(1 en 0), para encontrar el resultado, ademas de que no requiere conocer dos valores(el anterior y "actual", a diferencia del plantedo en el punto 2a)
