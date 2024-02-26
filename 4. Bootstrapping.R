# Metodo de Bootstrapping

library(tidyverse)

# Dados de uma Normal N(5000,1000) - média e variância

N = 500
norm.data <- rnorm(N, mean=5000, sd=100)

b.avg <- numeric(N)
b.sd <- numeric(N)

for(i in 1:N) {
  ystar <- sample(norm.data,length(norm.data),replace=T)
  b.avg[i] <- mean(ystar)
  b.sd[i] <- sd(ystar)
    }

b.avg
b.sd

# Estimação da Média - próximo de 5000
mean(b.avg) 

# Estimação do erro padrão da média amostral - # próximo de 4,47
sd(b.avg)



# Geração de uma função 
norm.data <- rnorm(500, mean=5000, sd=100)
boots <- function(data, R){
  b.avg <<- c(); b.sd <<- c()
  for(b in 1:R) {
    ystar <- sample(data,length(data),replace=T)
    b.avg <<- c(b.avg,mean(ystar))
    b.sd <<- c(b.sd,sd(ystar))}
}

boots(norm.data, 1000)


# Dados de uma Bernoulli Ber(0,5)

N = 1000
bern.data <- rbernoulli(N, p=0.5)

b.sum <- numeric(N)
b.var <- numeric(N)

for(i in 1:N) {
  re_amostra <- sample(bern.data,length(bern.data),replace=T)
  b.sum[i] <- sum(re_amostra)
  b.var[i] <- var(re_amostra)
}

b.sum
b.var

# média verdadeira dos dados = p = 0.5
# Variancia verdadeira dos dados = 0.5 * 0,5 = 0,25

# Estimação da Soma - Bin(np,np(1-p)) = Bin(500,250)
mean(b.sum)

# Estimação da variância - Bin(np,np(1-p)) = Bin(500,250)
var(b.sum)



# Exercício 1 

data(swiss)

agricultura <- swiss$Agriculture
N = 1000

Quartil1 <- numeric(N)
Mediana <- numeric(N)
Quartil3 <- numeric(N)

for(i in 1:N) {
  reamostra <- sample(agricultura,length(agricultura),replace=T)
  Quartil1[i] <- quantile(reamostra, probs = 0.25)
  Mediana[i] <- median(reamostra)
  Quartil3[i]<- quantile(reamostra, probs = 0.75)
}

summary(agricultura)

mean(Quartil1) # primeiro quartil
mean(Mediana)  # segundo quartil
mean(Quartil3) # terceiro quartil

# Intervalo de confiança

quantile(Quartil1, probs = 0.025)
quantile(Quartil1, probs = 0.975)

quantile(Quartil3, probs = 0.025)
quantile(Quartil3, probs = 0.975)

quantile(Mediana, probs = 0.025)
quantile(Mediana, probs = 0.975)

var(Quartil1)
var(Quartil3)
var(Mediana)



# Bootstrap para coeficiente de correlação linear
# amostras pareadas

data(women)

altura <- women$height * 2.5
peso <- 0.495 * women$weight

cor(altura,peso)


N = 1000
n = length(altura)
correlacao <- numeric(N)

for(i in 1:N) {
  z <- sample(1:n,n,replace=T) # vetor
  alt <- altura[z]
  pes <- peso[z]
  correlacao[i] <- cor(alt,pes)
}

mean(correlacao)
var(correlacao)
quantile(correlacao, probs = 0.025)
quantile(correlacao, probs = 0.975)
