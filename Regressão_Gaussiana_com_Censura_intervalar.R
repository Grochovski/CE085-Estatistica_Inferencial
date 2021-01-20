
x <- seq(0, 1, l = 100)
b0 = 10
b1= 2
eta = b0 + b1*x
plot(eta~x)
set.seed(123)
y <-  rnorm(100, mean = eta, sd = 0.5)
plot(y~x)
ymin <- y - 0.8
ymax <- y + 0.8
dados <- data.frame(ymin,ymax)
plot(dados)

ll <- function(theta, dados , x){
  eta = theta[1] + theta[2]*x
  output = -sum(pnorm(ymax, mean = eta, sd = theta[3], log = TRUE)-pnorm(ymin, mean = eta, sd = theta[3], log = TRUE))
  return(output)
}

ll(theta = c(10,2,0.25), dados = dados, x = x)

temp <- optim(par = c(mean(dados),0,sd(dados)), fn = ll, dados= dados, x= x, hessian = TRUE)
temp
# count  numero de vezes que a função foi avaliada

# hessian = segunda derivada

temp$par + qnorm(0.975)*sqrt(diag(solve(temp$hessian)))
temp$par - qnorm(0.975)*sqrt(diag(solve(temp$hessian)))



