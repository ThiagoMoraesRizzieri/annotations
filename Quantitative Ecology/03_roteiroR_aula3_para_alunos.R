# Aula 1 R Eco Quant 2022 

# Tema: simulação dos dados para analise
# Problema: mudança na composição de aves em função da cobertura florestal no Brasil

nomes.spp <- paste0("sp", seq(1:25)) 

nomes.spp

trans.1.100.am <- rpois(n=25, lambda=9)

trans.1.100.am

names(trans.1.100.am) <- nomes.spp

trans.1.100.am

trans.2.100.am <- rpois(n= 25, lambda= trans.1.100.am)

trans.3.100.am <- rpois(n= 25, lambda= trans.2.100.am)

# vamos ver como fica em formato de matriz
rbind(trans.1.100.am, trans.2.100.am, trans.3.100.am)

# ok, construimos um pedaco da tabela
# a logica e a mesma para o resto
# so temos que ajustar a cada caso

trans.1.30.am <- rpois(n=17, lambda=6)

trans.1.30.am

trans.2.30.am <- rpois(n= 17, lambda= trans.1.30.am)

trans.3.30.am <- rpois(n= 17, lambda= trans.2.30.am)
# como geramos menos spp, vamos completar com 0
# 25-17=8
trans.1.30.am <- c(trans.1.30.am, rep(0,8))
trans.1.30.am
trans.2.30.am <- c(trans.2.30.am, rep(0,8))
trans.3.30.am <- c(trans.3.30.am, rep(0,8))

rbind(trans.1.100.am, trans.2.100.am, trans.3.100.am,
  trans.1.30.am, trans.2.30.am, trans.3.30.am)

# vamos construir as amostras para locais com <10%

trans.1.10.am <- rpois(n=10, lambda=3)

trans.1.10.am

trans.2.10.am <- rpois(n= 10, lambda= trans.1.10.am)

trans.3.10.am <- rpois(n= 10, lambda= trans.2.10.am)
# como geramos menos spp, vamos completar com 0
# 25-10=15

trans.1.10.am <- c(trans.1.10.am, rep(0,15))
trans.1.10.am
trans.2.10.am <- c(trans.2.10.am, rep(0,15))
trans.3.10.am <- c(trans.3.10.am, rep(0,15))

rbind(trans.1.100.am, trans.2.100.am, trans.3.100.am,
      trans.1.30.am, trans.2.30.am, trans.3.30.am,
      trans.1.10.am, trans.2.10.am, trans.3.10.am)
#==========================================================
# vou replicar isso em mais duas regioes (Centro-oeste,Sul)
# basta copiar, colar e alterar os nomes.
# se quiser vc pode modificar o n de spp e o lambda:

trans.1.100.ce <- rpois(n=25, lambda=9)
trans.2.100.ce <- rpois(n= 25, lambda= trans.1.100.ce)
trans.3.100.ce <- rpois(n= 25, lambda= trans.2.100.ce)

trans.1.30.ce <- rpois(n=17, lambda=6)
trans.2.30.ce <- rpois(n= 17, lambda= trans.1.30.ce)
trans.3.30.ce <- rpois(n= 17, lambda= trans.2.30.ce)
# como geramos menos spp, vamos completar com 0
# 25-17=8

trans.1.30.ce <- c(trans.1.30.ce, rep(0,8))
trans.2.30.ce <- c(trans.2.30.ce, rep(0,8))
trans.3.30.ce <- c(trans.3.30.ce, rep(0,8))

trans.1.10.ce <- rpois(n=10, lambda=3)
trans.2.10.ce <- rpois(n= 10, lambda= trans.1.10.ce)
trans.3.10.ce <- rpois(n= 10, lambda= trans.2.10.ce)
# como geramos menos spp, vamos completar com 0
# 25-10=15

trans.1.10.ce <- c(trans.1.10.ce, rep(0,15))
trans.2.10.ce <- c(trans.2.10.ce, rep(0,15))
trans.3.10.ce <- c(trans.3.10.ce, rep(0,15))

# veja como esta ficando
rbind(trans.1.100.am, trans.2.100.am, trans.3.100.am,
      trans.1.30.am, trans.2.30.am, trans.3.30.am,
      trans.1.10.am, trans.2.10.am, trans.3.10.am,
      trans.1.100.ce, trans.2.100.ce, trans.3.100.ce,
      trans.1.30.ce, trans.2.30.ce, trans.3.30.ce,
      trans.1.10.ce, trans.2.10.ce, trans.3.10.ce)

# agora no sul
trans.1.100.ma <- rpois(n=25, lambda=9)
trans.2.100.ma <- rpois(n= 25, lambda= trans.1.100.ma)
trans.3.100.ma <- rpois(n= 25, lambda= trans.2.100.ma)

trans.1.30.ma <- rpois(n=17, lambda=6)
trans.2.30.ma <- rpois(n= 17, lambda= trans.1.30.ma)
trans.3.30.ma <- rpois(n= 17, lambda= trans.2.30.ma)
# como geramos menos spp, vamos completar com 0
# 25-17=8

trans.1.30.ma <- c(trans.1.30.ma, rep(0,8))
trans.2.30.ma <- c(trans.2.30.ma, rep(0,8))
trans.3.30.ma <- c(trans.3.30.ma, rep(0,8))

trans.1.10.ma <- rpois(n=10, lambda=3)
trans.2.10.ma <- rpois(n= 10, lambda= trans.1.10.ma)
trans.3.10.ma <- rpois(n= 10, lambda= trans.2.10.ma)
# como geramos menos spp, vamos completar com 0
# 25-10=15

trans.1.10.ma <- c(trans.1.10.ma, rep(0,15))
trans.2.10.ma <- c(trans.2.10.ma, rep(0,15))
trans.3.10.ma <- c(trans.3.10.ma, rep(0,15))

# veja como esta ficando
minha.matriz <-
rbind(trans.1.100.am, trans.2.100.am, trans.3.100.am,
      trans.1.30.am, trans.2.30.am, trans.3.30.am,
      trans.1.10.am, trans.2.10.am, trans.3.10.am,
      trans.1.100.ce, trans.2.100.ce, trans.3.100.ce,
      trans.1.30.ce, trans.2.30.ce, trans.3.30.ce,
      trans.1.10.ce, trans.2.10.ce, trans.3.10.ce,
      trans.1.100.ma, trans.2.100.ma, trans.3.100.ma,
      trans.1.30.ma, trans.2.30.ma, trans.3.30.ma,
      trans.1.10.ma, trans.2.10.ma, trans.3.10.ma)

minha.matriz

dim(minha.matriz)

# vamos criar uns identificadores importantes

regiao <- c("AM", "CE", "MA")


regiao <- rep(c("AM", "CE", "MA"), each=9)

regiao

regiao <- as.factor(regiao)
regiao

cobertura <- c(100, 30, 10)


cobertura <- rep(cobertura, each=3)

cobertura

cobertura <- rep(cobertura, times=3)
cobertura <- as.factor(cobertura)

cobertura

mean(cobertura)

dados.final <- data.frame(regiao, cobertura, minha.matriz)
dados.final

?write.csv
?setwd
write.csv(dados.final, "dados_aves.cesv")
# salve uma copia desse arquivo no seu email ou pen-drive
#========================================


