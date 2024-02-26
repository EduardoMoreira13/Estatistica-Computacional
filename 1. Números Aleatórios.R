# GERAÇÃO DE NÚMEROS ALEATÓRIOS - INTEGRAÇÃO


# Integral de 2X - intervalo (0,1)

k = 10000000
x <- runif(k, min = 0, max = 1) 

funcao <- 2*x
integral <- sum(funcao) / k
integral # exato = 1



# Integral da exponencial com lambda igual a 1 - intervalo (0,1)
lambda = 1

k = 1000000
x <- runif(k, min = 0, max = 1) 

funcao <- lambda*exp(-x*lambda)
integral <- sum(funcao) / k
integral # exato = -1/exp(1) + 1



# Integral da exponencial com lambda igual a 1 - intervalo (0,inf)

k = 1000000
y <- runif(k, min = 0, max = 1) 
# y = 1 / (1 + x) foi a transformação usada, feita fora do R

funcao <- exp( -(1/y - 1) )*( 1/(y^2) )
integral <- sum(funcao) / k
integral # exato = 1



# Integral dupla de xy - no retângulo [0,1]x[0,1]
k = 1000000
x <- runif(k, min = 0, max = 1) 
y <- runif(k, min = 0, max = 1) 
funcao <- x*y
integral <- sum(funcao) / k
integral 



# Estimação de pi com base em Monte Carlo
k = 1000000
raio = 1

area_qua <- (2*raio)*(2*raio)
area_qua

# Círculo = pi r^2 = pi

prob_circulo = pi / 4
prob_circulo

x <- runif(n.pontos, -raio, raio)  
y <- runif(n.pontos, -raio, raio)  
dentro <- x^2 + y^2 < raio^2
dados <- data.frame(x=x, y=y, Dentro=dentro)
integral <- mean(dados$Dentro)
integral

area_qua*mean(dados$Dentro)
pi*raio^2
