library(pracma)
library(plot3D)


#David Herrera Caicedo
#Pablo Alejandro Pulido
#Febrero de 2019



#PRIMER PUNTO
n=4

D1<-eye(n, m = n)
D2<-ones(n, m = n)
D3<-zeros(n, m = n)

print("D1")
print(D1)
print("D2")
print(D2)
print("D3")
print(D3)

#b.
 num = 4
A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=num, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=num, byrow=TRUE)
print("b")
print(b)


#Se obtiene el vector de la diagonal
D = diag(A)
#Se obtiene la matriz que solo contiene a la diagonal
Didi = diag(D)

print(A)

#Se separan en dos matrices(triangular inferior y superior respectivamente)
L = A
L[upper.tri(L)] <- 0
L[!lower.tri(L)] <- 0

U = A
U[lower.tri(U)] <- 0
U[!upper.tri(U)] <- 0


#Matriz de Transicion para Jacobi
T = (-solve(Didi))%*%(L+U)
print (T)
#Matriz de Transicion para Gauss-Seidel
T = ((-solve(Didi))%*%U)%*%solve(D2+(L%*%(solve(Didi))))
print (T)
#Matriz de Transicion para SOR
w = 1.2
T = solve(Didi - w * L) %*% ( (1 - w) * D + w * U)
print(T)


# Segundo Punto

#a.
x=c(1,1,1,1)
sol = itersolve(A, b,x0 = x, tol = 1e-9 , method = "Gauss-Seidel")
cat("Gauss- Seidel")
print(sol)
jacobi <- function(M, b, x0, repeticiones)
{
  x = matrix(x0) 
  iteracion = 1
  while(iteracion <= repeticiones)
  {
    x1 = solve(M) %*% matrix(b-((L+U)%*%x))
    error = norm(x1-x)/norm(x)
    cat ("Error = ", error, "Iteracion = ", iteracion, "\n")
    x = x1
    iteracion =iteracion + 1
    
  }
}
jacobi(A,b,x,5)

Aja <- charpoly(A, info = FALSE)

print(length(Aja))


#Punto Tres



tril1 <- function(M, k) {
  if (k == 0) {
    M[upper.tri(M, diag = FALSE)] <- 0
    M[!lower.tri(M)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
   M[col(M) == row(M)] <- 0
  }
  return(M)
}

a3 = magic(3)
print(tril1(a3,1))

#Punto Cuatro
gauss = function(A, b){  # Se supone det(A) != 0
  n = nrow(A) 
  Ab = cbind(A,b)
  mult = 0
  for (k in 1:(n-1))
    {   
      if(Ab[k,k]==0)
        {     
          fila = which(Ab[k, ]!=0)[1]
          Ab[c(k, fila),  ] = Ab[c(fila, k),  ]
        }
      for (i in (k+1):n)
      {
          Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
          mult = mult + 2*(n)
      }
    } 
    x = rep(NA, times=n)
    x[n] =  Ab[n, n+1]/Ab[n,n]     
    for(i in (n-1):1 )
    {
      x[i]= (Ab[i, n+1] -sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
      mult = mult + 2*(n-2)
    }
    cat("Numero de multiplicaciones es:", mult, "\n")
   return(x)
}
A5 = magic(5)
b = c(1,0,0,0,0)
cat("Ejercicio 4:")
gauss(A5,b)
cat("\n")

#Punto 5

#inventado
beta = 0
alpha = 3
Matriz = matrix(c(2, 0, 1,
                      beta,2 , -1,
                      -1, 1, 1), nrow=3, byrow=TRUE)
b5 = c(1,2,3)

jacobi2 <- function(M, b, x0, repeticiones)
{
  x = matrix(x0) 
  iteracion = 1
  L = M
  L[upper.tri(L)] <- 0
  L[!lower.tri(L)] <- 0
  
  U = M
  U[lower.tri(U)] <- 0
  U[!upper.tri(U)] <- 0
  ptosX = 0
  ptosY = 0
  ptosZ = 0
  while(iteracion <= repeticiones)
  {
    x1 = solve(M) %*% matrix(b-((L+U)%*%x))
    error = norm(x1-x)/norm(x)
    cat ("Error = ", error, "Iteracion = ", iteracion, "\n")
    x = x1
    iteracion =iteracion + 1
    ptosX[iteracion] = x[1]
    ptosY[iteracion] = x[2]
    ptosZ[iteracion] = x[3]
    
  }
  cat("Solucion (", ptosX[iteracion],",",ptosY[iteracion],",",ptosZ[iteracion],")")
  lines3D(ptosX,ptosY,ptosZ)
}
x = c(0,0,0)
jacobi2(Matriz,b5,x,10)


#Punto 6

A6 = A
b6 = c(1,2,3,4)



L = A6
L[upper.tri(L)] <- 0
L[!lower.tri(L)] <- 0

U = A6

U[lower.tri(U)] <- 0
U[!upper.tri(U)] <- 0

cat("Lower:")
print(L)
cat("Upper:")
print(U)


gs <- gramSchmidt(A6)

(Q <- gs$Q);(R <- gs$R)

cat("Q:")
print(Q)
cat("R")
print(R)

Ab = cbind(A6,b6)
print(Ab)
holi = Q %*% R
print (holi)

#PUNTO 7
#7A
punto7 = function(x) {
  n = length(x)
  F = rep(NA, n)
  F[1] = x[1] - x[2]
  F[2] = x[1]^2 + x[2]^2 -1
  F
}
p0 = c(1,1) 
sol = BBsolve(par=p0, fn=punto7)
sol$par

plot(sol$par)
plot(ecuaciones)
#7B
trigexp = function(x) {
n = length(x)
F = rep(NA, n)
F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
tn1 = 2:(n-1)
F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
F
}
n = 10000
p0 = runif(n) # n initial random starting guesses
sol = BBsolve(par=p0, fn=trigexp)
sol$par

#8



