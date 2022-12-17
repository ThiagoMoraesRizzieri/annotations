## Aula 01 no R - Ecologia Quantitativa
# Thiago Moraes Rizzieri 
# 26/04/2022

log(27)

2+2

y <- 1+1
y + 3


y1 <- 5

rm(y)


caixa <- "gato"

caixa_grande <- c("gato1", "gato2", "gato3")
caixa_grande

seq(1,10,1)

z <- rnorm(100,50,2.5)
z

hist(z)

m1 <- cbind(1:5, 6:10)
m1

m1[3,]



z <- rnorm(100,50,2.5)
z

hist(z)

trat <- c("trat","contr")
trat

trat.100 <- rep(trat,each=50)
trat.100

meu.df <- data.frame(z, trat.100)
meu.df

# Escolhemos a coluna com o sifrão
meu.df$trat.100

plot(x=meu.df$trat.100,y=meu.df$z)

boxplot(z~trat.100, data=meu.df)
