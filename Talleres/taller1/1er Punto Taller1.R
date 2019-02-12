func1 = c(2,0,-3,3,-4)
func2 = c(7,6,-6,0,3,-4)
func3 = c(-5,0,3,0,2,-4,0)

horner <- function(p,n,x)
{
  y = p[1]
  i = 2
  cont = 0
  while(i <= n)
  {
    y = y*x + p[i]
    i = i + 1
    cont  = 1 + cont
  }
  cat("El numero de operaciones fue de:",cont)
  return (y)
}

eval <- function(p,n,x)
{
  s = 0
  n = n +1 
  i = 1
  while(i < n)
  {
    s = s + (p[i])*(x**(n-i-1))
    i = i + 1
  }
  return(s)
}
eva = eval(func1,5,-2)
print(eva)
ho = horner(func1,5,-2)
cat("El valor del primer P(x) es igual a: ",eva,"Por el metodo de horner fue: ",ho)
eva = eval(func2,6,3)
ho = horner(func2,6,3)
cat("El valor del segundo P(x) es igual a: ",eva,"Por el metodo de horner fue: ",ho)
eva = eval(func3,7,-1)
ho = horner(func3,7,-1)
cat("El valor del tercer P(x) es igual a: ",eva,"Por el metodo de horner fue: ",ho)