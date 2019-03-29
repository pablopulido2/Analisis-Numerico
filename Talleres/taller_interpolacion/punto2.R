library(pracma)
library(PolynomF)
library(Matrix)
library(rSymPy)
rm(list=ls())

x= c(100,200,300,400,500,600)
y= c(-160,-35,-4.2,9.0,16.9,21.3)

plot(x = x,y = y,type = "l", col="red", main = "Gases no ideales nitrogeno" )

datosx1=x[1:6]; datosy1=y[1:6]
Polinomio1= poly.calc(datosx1,datosy1)
Polinomio1
#curve(Polinomio1,from = x[1],to = x[5], add = TRUE,col="yellow")
points(x,y,col="red",main="Gases no ideales nitrogeno")

funcionInterpolacion<- function(a,b)
{
  sequencia = seq(x[a], x[b], length=90)
  Interpol <- barylag(x[a:b], y[a:b], sequencia)
  lines(sequencia, Interpol, col="black")
  cat(Interpol[67])
}
funcionInterpolacion(1,6)

lagrange.poly <- function(x, y) 
  {
  
  l <- list() # List to store Lagrangian polynomials L_{1,2,3,4}
  k <- 1
  
  for (i in x) {
    # Set the numerator and denominator of the Lagrangian polynomials to 1 and build them up
    num <- 1
    denom <- 1
    
    # Remove the current x value from the iterated list
    p <- x[! x %in% i]
    
    # For the remaining points, construct the Lagrangian polynomial by successively 
    # appending each x value
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = "", collapse = "")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = "", collapse = "")
    }
    
    # Set each Lagrangian polynomial in rSymPy to simplify later.
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = "", collapse = "")
    k <- k + 1
  }
  
  # Similar to before, we construct the final Lagrangian polynomial by successively building 
  # up the equation by iterating through the polynomials L_{1,2,3,4} and the y values 
  # corresponding to the x values.
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = "", collapse = "")
  }
  
  # Define x variable for rSymPy to simplify
  x <- Var('x')
  
  # Simplify the result with rSymPy and return the polynomial
  return(sympy(paste("simplify(", eq, ")")))
}
polinomioLagrange<-lagrange.poly(x,y)
polinomioLagrange
pl<-function(x)-573.9 + 6.63535*x - 0.0318345833333333*x^2 + 7.76666666666666e-5*x^3 - 9.40416666666667e-8*x^4 + 4.48333333333333e-11*x^5
pl(450)
plot(x = x,y = y,type = "l", col="red", main = "Gases no ideales nitrogeno, lagrange" )
curve(pl,add=TRUE,from =x[1],to =x[6])



