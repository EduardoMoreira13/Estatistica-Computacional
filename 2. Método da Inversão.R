# Metodo da inversao para dist. discreta

library(tidyverse)

p1 = 0.2
p2 = 0.15
p3 = 0.25
p4 = 0.4
n = 100000

u = runif(n, min = 0, max = 1)

dados <- data.frame(u = u)
  
  
dados$valores <- cut(x = dados$u,
                     breaks = c(0, 0.2, 0.35, 0.60, 1.1),
                     labels = c(1:4), right = FALSE)

dados %>% 
  group_by(valores) %>% 
  summarise(contagem = n() / n)


# Metodo da inversao para dist. exp lambda

#Algoritmo 1

lambda = 1
n = 100000

u = runif(n, min = 0, max = 1)
x = -log(1-u)/lambda


#Algoritmo 2

lambda=1
n=100000
x=array(0,c(n))
u=array(0,c(n))

for(i in 1:n){
  u[i]=runif(1)
  x[i]=-log(1-u[i])/lambda
  } 

# Diagnostico 

mean(x)
# Histograma da densidade 
hist(x,freq=F)

# Calcular a curva de densidade 
# Criar uma sequencia no eixo x 
w=seq(0,10,0.01)
w
v=lambda*exp(-lambda*w)
hist(x,freq=F)
lines(w,v,lwd=2)



# ---------------------------------------------------------- #



# Exemplo 2: Gerar pontos da U(a,b)

# Algoritmo 1

a = 2
b = 5
n = 100000

u = runif(n, min = 0, max = 1)
x = a+(b-a)*u

mean(x)
hist(x,freq=F)
abline(h=1/(b-a),lwd=2)


# Algoritmo 2

a=2
b=5
n=10000
x=array(0,c(n))
u=array(0,c(n))

for (i in 1:n){
  u[i]=runif(1)
  x[i]=a+(b-a)*u[i]
  }

mean(x)
hist(x,freq=F)
abline(h=1/(b-a),lwd=2)




# ---------------------------------------------------------- #

# Exemplo 3: Metodo da inversao para dist. geométrica

n = 1000000
p = 0.5

u = runif(n, min = 0, max = 1)
x = log(1-u)/log(1-p) - 1
x = ceiling(log(1-u)/log(1-p) - 1) # ruim
x = floor(log(1-u)/log(1-p) - 1)   # ruim
x = ceiling(log(u)/log(1-p))       # ok
x = floor(log(u)/log(1-p))         # ok

# Histograma da densidade 
mean(x)
hist(x,freq=F)

# Calcular a curva de densidade 
# Criar uma sequencia no eixo x 
w=seq(0,10,0.01)
w
v=p*(1-p)^w
hist(x,freq=F)
lines(w,v,lwd=2)

