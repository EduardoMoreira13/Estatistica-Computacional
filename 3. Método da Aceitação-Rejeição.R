# Metodo da Aceitação - Rejeição

library(tidyverse)

# Gerar uma normal -- N(0,1) ----

# -log(1-u)/lambda gera a exponencial --- -log(u)/lambda tbm

c = sqrt( (2*exp(1)) / pi )
c

n = 1000000

for (i in 1:n) {
  u1_exponencial = runif(1, min = 0, max = 1)
  Y = -log(u1_exponencial)
  uniforme = runif(1, min = 0, max = 1)
  while (uniforme > exp(-0.5*(Y-1)*(Y-1))) {
    u1_exponencial = runif(1, min = 0, max = 1)
    Y = -log(u1_exponencial)
    uniforme = runif(1, min = 0, max = 1)
  }	
  u3 = runif(1, min = 0, max = 1)
  if(u3 < 0.5){
    X[i] = Y
  } 
  else {
    X[i] = -Y
  }
}

hist(X,freq=F) # Desenhar a curva em cima do histograma

Z = sort(X)
densidade = (2 * exp(-0.5*Z^2)) / sqrt(2*pi)
lines(Z,densidade,lwd=2)



# Aula 2: Método da rejeicao 

# Calcular o valor da constante k 

f=function(x)
	{res=x^2*exp(-x)}

k=1/0.1606

# Algoritmo  

for (i in 1:n) 
	{y=runif(1)
	 u=runif(1)
	 while (u>y^2*exp(-y))
		 {y=runif(1)
         u=runif(1)}	
	 x[i]=y}

hist(x,freq=F)

# Desenhar a curva em cima do histograma

w=seq(0,1,0.01)
v=6.22*w^2*exp(-w)
lines(w,v,lwd=2)

# Algoritmo de Box-Miller 
n=10000
for (i in 1:(n/2))
	{u1=runif(1)
	 u2=runif(1)
	 x[2*i-1]=(-2*log(u1))^0.5*cos(2*pi*u2)
	 x[2*i]=(-2*log(u1))^0.5*sin(2*pi*u2)}

mean(x)
var(x)
hist(x,freq=F,lwd=2)
w=seq(-4,4,0.01)
lines(w,dnorm(w,0,1),lwd=2)










