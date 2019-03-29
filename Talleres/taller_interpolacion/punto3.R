library(pracma)
library(PolynomF)
library(Matrix)
library(rSymPy)
rm(list=ls())

x=c(seq(0,1,by=0.1))
y=exp(x)

plot(x = x,y = y,type = "l", col="red", main = "tabular" )

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
pl<-function(x)1 + 0.999999999985818*x + 0.500000000402906*x^2 + 0.166666661873023*x^3 + 0.0416666985838674*x^4 + 0.00833320448873565*x^5 + 0.0013892303686589*x^6 + 0.000197824032511562*x^7 + 2.54621845670044e-5*x^8 + 2.28951103053987e-6*x^9 + 4.55445842817426e-7*x^10
plot(x = x,y = y,type = "l", col="red", main = "tabular lagrange" )
curve(pl,add=TRUE,from =0,to =1)

