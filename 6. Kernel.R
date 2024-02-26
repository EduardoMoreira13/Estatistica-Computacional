# Estimador de Kernel - estimação de densidade (não paramétrico)

# Exemplo 1: uma amostra da gamma simulada por 
# um Kernel Gaussiano (distribuição normal)

require(tidyverse) 
set.seed(123) # Definindo a semente 
n <- 20 # Tamanho da mostra 
x <- rgamma(n, 5, 2) # Gerando uma amostra de tamanho n da Gamma

hist(x, freq = F, breaks = 12)
lines(density(x), col = "red", lwd =2)
lines(density(rgamma(10000, 5, 2)), col = "blue", lwd =2)

### Calculando o estimador de kernel para a distribuição que gerou os dados x
h <- .5 # Bandwidth, taxa de suavização.
m <- 1000 # Número de pontos que usaremos para construir o grid
x.grid <- seq(0, 8, length.out=m) # Grid sobre o qual queremos estimar a função
y.grid <- numeric(m) # Vetor que receberá os valores estimados em cada ponto
for(i in 1:length(y.grid)) {
  y.grid[i] <- sum(dnorm(x.grid[i], x, h)/n)
}

1-pgamma(5, 5, 2) # Probabilidade exata P (X > 5)
mean(y.grid[x.grid>5])*(max(x.grid) - 5) # Estimada pela nossa implementação


# Exemplo 2: uma amostra da normal simulada por 
# um Kernel Gaussiano (distribuição normal)

set.seed(1000) # Definindo a semente 
n <- 2000 # Tamanho da mostra 
x <- rnorm(n, 10, 2) # Gerando uma amostra de tamanho n da Gamma

hist(x, freq = F, breaks = 12)
lines(density(x, kernel = c("gaussian"), bw = "nrd0"), 
      col = "red", lwd =2)

?density

h <- .5 # Bandwidth, taxa de suavização.
m <- 1000 # Número de pontos que usaremos para construir o grid
x.grid <- seq(4, 16, length.out=m) # Grid sobre o qual queremos estimar a função
y.grid <- numeric(m) # Vetor que receberá os valores estimados em cada ponto
for(i in 1:length(y.grid)) {
  y.grid[i] <- sum(dnorm(x.grid[i], x, h)/n)
}

# P(X>10)
mean(y.grid[x.grid>10])*(max(x.grid) - 10) 

