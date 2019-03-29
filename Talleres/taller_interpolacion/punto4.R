library(pracma)
library(PolynomF)
library(Matrix)
library(rSymPy)
rm(list=ls())

x=c(40,50,60,70,80)
y=c(35,83,153,193,215)

plot(x = x,y = y,type = "l", col="red", main = "notas del curso. Frecuencias acumuladas" )

polinomico=poly.calc(x,y)
polinomico
polinomico(55)

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
pl<-function(x)3343 - 7181*x/30 + 371*x^2/60 - 101*x^3/1500 + x^4/3750
pl(55)
plot(x = x,y = y,type = "l", col="red", main = "frecuencias acumuladas lagrange" )
curve(pl,add=TRUE,from =0,to =1)