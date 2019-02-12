punto2 = function(n)
{
  repeticiones=0
  numeros= n/2 
  while(n>0){
    d = n%%2
    n = as.integer(n/2)
    repeticiones=repeticiones+1
  } 
  return (repeticiones)
}
y= seq(1,73, by =1)
for(i in 1:73){
  y[i]=punto2(i)
}
plot(y, type='l')