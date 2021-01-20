# Verossimilhança

# ------------
set.seed(100) # isso é só pra toda vez que ele rodar o código, ele vai 
# gerar os mesmos valores aleatórios.. Como eu vou dar o exemplo 
# se toda vez ele gerar valores diferentes?! Por isso usamos isso.
# ------------

# vamos gerar uma amostra com tamanho 100, média 6 e variância 1
yi <- rnorm(n = 100, mean = 6, sd = 1)
yi # vai aparecer os números gerados
yi[1] # 5.497808
# Cada um desses 100 números será uma variável aleatória Y1,Y2,Y3...
# Cada número gerado será meu y1,y2,y3.... até y100 no caso 
# Obs.: Y1 = minha variável, y1 = o valor, a realização da variável
# Mas como eu sou desorganizado... eu perdi o valor da média!
# eu só tinha anotado os meus yi e minha variância na minha apostila..
# não tinha folha pra anotar a média.. rs
# Agora eu preciso achar novamente... Então eu tento descobrir quem era 
# minha média.. ou melhor.. meu parâmetro! 
# Por isso eu vou estimar meu parâmetro!

# Em prob. 2 a gente aprende que se minhas variáveis são iid
# (independente e identicamente distribuidas), então 
# eu posso encontrar a conjunta (a mãe de todas elas juntas) apenas 
# multiplicando todas
# A função de verossimilhança nada mais é que a distribuição conjunta

# Função abaixo L(parâmetro)
L <- function(mu){ # mu é a média que eu perdi... 
  # prod = produto
  # função dentro é a da normal
  # yi é cada valorzinho da amostra
  # é um produto de 100 elementos! :O
  prod(1/(sqrt(2*3.14)*sigma)*e^-((yi - mu)^2/(2*sigma^2)))
}

# Se ela depende do meu parâmetro e eu não sei o parâmetro, 
# então é só eu chutar valores, oxê!
# vou chutar valor de 0 a 10, para ser mais exato:10.000 valores entre
# 0 e 10.
valores.mu <- as.matrix(seq(0,10,length = 10000))
# agora aplique esses valores que eu gerei dentro da minha função
vero <- apply(valores.mu,1,L)
# agora terei um gráfico: em x eu tenho os valores do parâmetro (theta)
# e em y eu tenho L(theta)
plot(x = valores.mu, y = vero, type = "l")
 
# --- vendo qual valor exato que deu o maior, ou seja, 
# tentando achar aquele que eu perdi...
# em qual posição tá o maior valor do maior y (ou seja, L(theta)? 
match(max(vero),vero) # posição 6003, nem precisa daquele
# código numérico do Wagner... rs
# Então nessa mesma posição, vou achar o correspondente x, que será o 
# o valor do theta/parâmetro/mu/média que eu tinha perdido...
par <- valores.mu[match(max(vero),vero)]
# marcando esse ponto
abline(v = valores.mu[match(max(vero),vero)], h = max(vero))

# Pronto, achei o valor da média que eu tinha perdido, o R me disse que era
# 6.0026, ele não vai dar um certinho, mas um perto, de preferencia.
# Yayyy!!!! 

# Observações:
# a log da vero serve só pra eu conseguir derivar mais fácil
# a gente usa a derivada na mão porque não tem como fazendo conta com
# 10.000 valores 



