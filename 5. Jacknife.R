# Metodo de Bootstrapping

library(tidyverse)

# Exemplo 1 - quartis

data(swiss)

agricultura <- swiss$Agriculture
n = length(agricultura)

Quartil1 <- numeric(n)
Mediana <- numeric(n)
Quartil3 <- numeric(n)

for(i in 1:n) {
  reamostra <- agricultura[-i]
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

var(Quartil1) * ((n-1)^2 / n)
var(Quartil3) * ((n-1)^2 / n)
var(Mediana)  * ((n-1)^2 / n)



# Jacknife para coeficientes de regressão

data(rock)

area <- rock$area
peri <- rock$peri

cor(area,peri)
ajuste <- lm(area~peri)

n = length(area)
alfa <- numeric(n)
beta <- numeric(n)


for(i in 1:n) {
  areajack <- area[-i]
  perijack <- peri[-i]
  ajuste <- lm(areajack~perijack)
  alfa[i] <- ajuste$coefficients[1]
  beta[i] <- ajuste$coefficients[2]  
}

mean(alfa)
quantile(alfa, probs = 0.025)
quantile(alfa, probs = 0.975)

mean(beta)
quantile(beta, probs = 0.025)
quantile(beta, probs = 0.975)

for(i in 1:n) {
  ajuste <- lm(area[-i]~peri[-i])
  alfa[i] <- ajuste$coefficients[1]
  beta[i] <- ajuste$coefficients[2]  
}