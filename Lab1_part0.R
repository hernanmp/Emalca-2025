
######Radamcher random variables

## Numero de simulaciones Monte Carlo
N = 1000
n = 1000
sigma = 1

max_gaussian = rep(0,N)

for(iter in 1:N)
{
  ## generamos n variables Gaussianas
  x = abs(rnorm(n,sigma))
  ## Guardamos el maximo
  max_gaussian[iter] = max(x)
}

hist(max_gaussian)

### La teoria dice que con probabilidad al menos 1- 2/sqrt(n) el maximo es menor o igual que 2sigma*sqrt(log n)
### probabilidad
1- 2/sqrt(n) 

### Cota superior
Cota_superior = 2*sigma*sqrt(log(n))

### proporcion mas grande que la cantidad
length(which(  max_gaussian >Cota_superior ))/N


#### Radamcher random variables
N = 1000
n = 10000

average = rep(0,N)

for(iter in 1:N)
{
  x = sample(c(-1,1),n,replace= TRUE)
  average[iter] = mean(x)
}

hist(average)

### theory says P( |sum X_i/n| > t) <= 2*exp(- n t^2/2)
### t 
t = sqrt(2 *log(n)/n)
t
max(abs(average))

##propotion exceeding t is
length(which(abs(average) > t  ))/N

#probability of excedding t is at most:
2/n


