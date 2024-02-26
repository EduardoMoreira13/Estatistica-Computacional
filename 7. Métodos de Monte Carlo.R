# Métodos de Monte Carlo

# Monte Carlo estimation and standard error ----

# Valor esperado - E|X1 - X2| Xi iid normal padrão

m <- 1000
g <- numeric(m)
for (i in 1:m) {
  x <- rnorm(2)
  g[i] <- abs(x[1] - x[2])
}

est <- mean(g) # média
est

sqrt(sum((g - mean(g))^2)) / m
sqrt(sum((g - mean(g))^2)/ (m-1)) * sqrt(1/m)



# Nível de confiança ----

# tstudent
level <- numeric(1)
n = 100

for(i in 1:1000){
  tty <- rt(n, df = 3)
  # (0, (n-1)S2 / x2)
  ci.t <- c(0, (n-1) * var(tty) / qchisq(0.95, df = n-1))
  if(ci.t[2] < 3) level <- level + 1
  }

level / 1000

# normal padrao
n = 100
level <- numeric(n)
for(j in 1:n){
  for(i in 1:1000){
    tty <- rnorm(n)
    ci.t <- c(0, (n-1) * var(tty) / qchisq(0.95, df = n-1))
    if(ci.t[2] < 1) level[j] <- level[j] + 1
    }
  }

mean(level / 1000)
var(level / 1000)


# outras formas de calcular
n <- 20
level <- numeric(1)
for(i in 1:1000){
  tty <- rnorm(20, 0, 2)
  # (0, (n-1)S2 / x2)
  ci.t <- c(0, (n-1) * var(tty) / qchisq(0.95, df = n-1)) # uso de 1 - alfa
  if(ci.t[2] < 4) level <- level + 1
}

level / 1000

n <- 20
alpha <- 0.05
UCL <- replicate(1000, expr = {
  x <- rnorm(n, mean = 0, sd = 2)
  (n-1) * var(x) / qchisq(alpha, df = n-1) # uso de alfa
  })

sum(UCL>4)
mean(UCL > 4)



# Teste de Hipóteses - Erro do tipo I e Poder do Teste ----

# H0 : μ = 10 com α = 0.05

reject <- numeric(1)

for(i in 1:5000){
  yN <- rnorm(20, 10, sqrt(10))
  Sy <- sqrt(var(yN) / 20)
  muY <- sum(yN) / length(yN)
  tobs <- (muY - 10) / Sy
  if(abs(tobs) > qt(0.975, df = 19)) reject <- 1 + reject}

reject / 5000


# H0 : μ = 10 e  μ = 12

power <- numeric(1)

for(i in 1:10000){
  yN <- rnorm(20, 12, 5)
  Sy <- sqrt(var(yN) / 20)
  muY <- sum(yN) / length(yN)
  tobs <- (muY-10) / Sy
if(abs(tobs) > qt(0.975, df=19)) power <- 1 + power}

power / 10000
