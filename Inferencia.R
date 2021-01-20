## FUN��O NORMAL
# Criando a fun��o com vari�ncia conhecida 1
norm <- function(y, mu) {
  output = ((2*pi)^(-1/2)*exp((-1/2)*(y-mu)^2))
  return(output)
}

# Criando os intervalos de observa��es
y1 <- pnorm(q = 10,mean = 2,sd = 1)                                 #   y<10
y2 <- pnorm(q = 10,mean = 2,sd = 1,lower.tail = F)                  #   y>10
y3 <- pnorm(q = 10,mean = 2,sd = 1) - pnorm(q = 5,mean = 4,sd = 1)  # 5<y<10
y4 <- dnorm(x = 10,mean = 2,sd = 1)                                 #   y=10

# Fun��o de verossimilhan�a y<10
L1 <- function(mu, y1) {
  output = prod(((2*pi)^(-1/2)*exp((-1/2)*(y1-mu)^2)))
  return(output)
}
L1 <- Vectorize(L1, vectorize.args = "mu")

# Fun��o de verossimilhan�a y>10
L2 <- function(mu, y2) {
  output = prod(((2*pi)^(-1/2)*exp((-1/2)*(y2-mu)^2)))
  return(output)
}
L2 <- Vectorize(L2, vectorize.args = "mu")

# Fun��o de verossimilhan�a 5<y<10
L3 <- function(mu, y3) {
  output = prod(((2*pi)^(-1/2)*exp((-1/2)*(y3-mu)^2)))
  return(output)
}
L3 <- Vectorize(L3, vectorize.args = "mu")

# Fun��o de verossimilhan�a y=10
L4 <- function(mu, y4) {
  output = prod(((2*pi)^(-1/2)*exp((-1/2)*(y4-mu)^2)))
  return(output)
}
L4 <- Vectorize(L4, vectorize.args = "mu")

# Gr�fico das verossimilhan�as
par(mfrow = c(2,2))
curve(L1(x, y1 = y1), -4, 4)
abline(v = 0)
curve(L2(x, y2 = y2), -4, 4)
abline(v = 0)
curve(L3(x, y3 = y3), -4, 4)
abline(v = 0)
curve(L4(x, y4 = y4), -4, 4)
abline(v = 0)






## FUNÇÃO POISSON
# Criando a função
poisson <- function(x, lambda) {
  output = ((exp(-lambda)*lambda^x)/factorial(x))
  return(output)
}

x1 <- ppois(q = 10,lambda = 2)                            #  x<10
x2 <- ppois(q = 10,lambda = 2,lower.tail = F)             #  x>10
x3 <- ppois(q = 10,lambda = 2) - ppois(q = 5,lambda = 2)  #5<x<10
x4 <- dpois(x = 10,lambda = 2)                            #  x=10

# função de verossimilhança x<10
P1 <- function(lambda, x1) {
  output = sum(((exp(-lambda)*lambda^x1)/factorial(x1)))
  return(output)
}
P1 <- Vectorize(P1, vectorize.args = "lambda")

# função de verossimilhança x>10
P2 <- function(lambda, x2) {
  output = sum(((exp(-lambda)*lambda^x2)/factorial(x2)))
  return(output)
}
P2 <- Vectorize(P2, vectorize.args = "lambda")

# função de verossimilhança 5<x<10
P3 <- function(lambda, x3) {
  output = sum(((exp(-lambda)*lambda^x3)/factorial(x3)))
  return(output)
}
P3 <- Vectorize(P3, vectorize.args = "lambda")

# função de verossimilhança x=10
P4 <- function(lambda, x4) {
  output = sum(((exp(-lambda)*lambda^x4)/factorial(x4)))
  return(output)
}
P4 <- Vectorize(P4, vectorize.args = "lambda")

# Gráfico das verossimilhanças
par(mfrow = c(2,2))
curve(P1(x, x1=x1), -7, 7)
abline(v = 2)
curve(P2(x, x2=x2), -7, 7)
abline(v = 2)
curve(P3(x, x3=x3), -7, 7)
abline(v = 2)
curve(P4(x, x4=x4), -7, 7)
abline(v = 2)
