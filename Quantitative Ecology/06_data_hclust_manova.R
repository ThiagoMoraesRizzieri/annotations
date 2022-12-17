# Aula R Eco Quant 2022 
# Rio Claro, 16/5/2022

# Tema: simulação dos dados para analise
# Problema: mudança na composição de aves em função da 
# cobertura florestal no Brasil

# Vamos simular nossos dados

nomes.spp <- paste0("sp", seq(1:25)) 

nomes.spp

t.1.100.am.A <- rpois(n=25, lambda=9)

t.1.100.am.A

names(t.1.100.am.A) <- nomes.spp

t.1.100.am.A

t.2.100.am.A <- rpois(n= 25, lambda= t.1.100.am.A)

t.3.100.am.A <- rpois(n= 25, lambda= t.2.100.am.A)
#

t.1.100.am.B <- rpois(n=25, lambda=10)

t.2.100.am.B <- rpois(n= 25, lambda= t.1.100.am.B)

t.3.100.am.B <- rpois(n= 25, lambda= t.2.100.am.B)

#
t.1.100.am.C <- rpois(n=25, lambda=8)

t.2.100.am.C <- rpois(n= 25, lambda= t.1.100.am.B)

t.3.100.am.C <- rpois(n= 25, lambda= t.2.100.am.B)
#

# ok, construimos um pedaco da tabela
# a logica e a mesma para o resto
# so temos que ajustar a cada caso

t.1.30.am.A <- rpois(n=17, lambda=6)

t.2.30.am.A <- rpois(n= 17, lambda= t.1.30.am.A)

t.3.30.am.A <- rpois(n= 17, lambda= t.2.30.am.A)

# como geramos menos spp, vamos completar com 0
# 25-17=8
t.1.30.am.A <- c(t.1.30.am.A, rep(0,8))
t.2.30.am.A <- c(t.2.30.am.A, rep(0,8))
t.3.30.am.A <- c(t.3.30.am.A, rep(0,8))
#

t.1.30.am.B <- rpois(n=17, lambda=7)

t.2.30.am.B <- rpois(n= 17, lambda= t.1.30.am.B)

t.3.30.am.B <- rpois(n= 17, lambda= t.2.30.am.B)

# como geramos menos spp, vamos completar com 0
# 25-17=8
t.1.30.am.B <- c(t.1.30.am.B, rep(0,8))
t.2.30.am.B <- c(t.2.30.am.B, rep(0,8))
t.3.30.am.B <- c(t.3.30.am.B, rep(0,8))

#
t.1.30.am.C <- rpois(n=17, lambda=5)

t.2.30.am.C <- rpois(n= 17, lambda= t.1.30.am.C)

t.3.30.am.C <- rpois(n= 17, lambda= t.2.30.am.C)

# como geramos menos spp, vamos completar com 0
# 25-17=8
t.1.30.am.C <- c(t.1.30.am.C, rep(0,8))
t.2.30.am.C <- c(t.2.30.am.C, rep(0,8))
t.3.30.am.C <- c(t.3.30.am.C, rep(0,8))

# Agora com 10% de cobertura ===============

t.1.10.am.A <- rpois(n=10, lambda=3)
t.2.10.am.A <- rpois(n= 10, lambda= t.1.10.am.A)
t.3.10.am.A <- rpois(n= 10, lambda= t.2.10.am.A)

# como geramos menos spp, vamos completar com 0

t.1.10.am.A <- c(t.1.10.am.A, rep(0,15))
t.2.10.am.A <- c(t.2.10.am.A, rep(0,15))
t.3.10.am.A <- c(t.3.10.am.A, rep(0,15))
#

t.1.10.am.B <- rpois(n=10, lambda=4)
t.2.10.am.B <- rpois(n= 10, lambda= t.1.10.am.B)
t.3.10.am.B <- rpois(n= 10, lambda= t.2.10.am.B)

# como geramos menos spp, vamos completar com 0
t.1.10.am.B <- c(t.1.10.am.B, rep(0,15))
t.2.10.am.B <- c(t.2.10.am.B, rep(0,15))
t.3.10.am.B <- c(t.3.10.am.B, rep(0,15))

#
t.1.10.am.C <- rpois(n=10, lambda=5)
t.2.10.am.C <- rpois(n= 10, lambda= t.1.10.am.C)
t.3.10.am.C <- rpois(n= 10, lambda= t.2.10.am.C)

# como geramos menos spp, vamos completar com 0
t.1.10.am.C <- c(t.1.10.am.C, rep(0,15))
t.2.10.am.C <- c(t.2.10.am.C, rep(0,15))
t.3.10.am.C <- c(t.3.10.am.C, rep(0,15))


dado_am_trans = rbind(
  t.1.10.am.A, t.2.10.am.A, t.3.10.am.A, t.1.10.am.B, t.2.10.am.B, t.3.10.am.B,
  t.1.10.am.C, t.2.10.am.C, t.3.10.am.C,
  t.1.30.am.A, t.2.30.am.A, t.3.30.am.A, t.1.30.am.B, t.2.30.am.B, t.3.30.am.B,
  t.1.30.am.C, t.2.30.am.C, t.3.30.am.C,
  t.1.100.am.A, t.2.100.am.A, t.3.100.am.A, t.1.100.am.B, t.2.100.am.B, t.3.100.am.B,
  t.1.100.am.C, t.2.100.am.C, t.3.100.am.C)

library(vegan)
dado_am_trans
#

vegdist(dado_am_trans, "jacc", binary = T)

plot(hclust(d=vegdist(dado_am_trans, "jacc", binary = T), "ave"))

vegdist(dado_am_trans, "bray", binary = F)
plot(hclust(d=vegdist(dado_am_trans, "bray", binary = F), "ave"))

## Com os outros biomas

#==========================================================
# vou replicar isso em mais duas regioes (Cerrado MA)
# basta copiar, colar e alterar os nomes.
# se quiser vc pode modificar o n de spp e o lambda:
# para o dendrograma não ficar muito grande, vou usar apenas 
# um transecto por paisagem

tot.am.100.A = t.1.100.am.A
tot.am.100.B = t.1.100.am.B
tot.am.100.C = t.1.100.am.C

tot.am.30.A = t.1.30.am.A
tot.am.30.B = t.1.30.am.B
tot.am.30.C = t.1.30.am.C

tot.am.10.A = t.1.10.am.A
tot.am.10.B = t.1.10.am.B
tot.am.10.C = t.1.10.am.C

#

tot.ce.100.A = c(rpois(22, lambda = t.1.100.am.A), rep(0,3))
tot.ce.100.B = c(rpois(22, lambda = tot.ce.100.A), rep(0,3))
tot.ce.100.C = c(rpois(22, lambda = tot.ce.100.B), rep(0,3))

tot.ce.30.A = c(rpois(13, lambda = t.1.30.am.A), rep(0,12))
tot.ce.30.B = c(rpois(13, lambda = tot.ce.30.A), rep(0,12))
tot.ce.30.C = c(rpois(13, lambda = tot.ce.30.B), rep(0,12))

tot.ce.10.A = c(rpois(7, lambda = t.1.10.am.A), rep(0,18))
tot.ce.10.B = c(rpois(7, lambda = tot.ce.10.A), rep(0,18))
tot.ce.10.C = c(rpois(7, lambda = tot.ce.10.A), rep(0,18))

#

tot.ma.100.A = c(rpois(24, lambda = t.1.100.am.A), 0)
tot.ma.100.B = c(rpois(24, lambda = tot.ma.100.A), 0)
tot.ma.100.C = c(rpois(24, lambda = tot.ma.100.B), 0)

tot.ma.30.A = c(rpois(15, lambda = t.1.30.am.A), rep(0,10))
tot.ma.30.B = c(rpois(15, lambda = tot.ma.30.A), rep(0,10))
tot.ma.30.C = c(rpois(15, lambda = tot.ma.30.B), rep(0,10))

tot.ma.10.A = c(rpois(8, lambda = t.1.10.am.A), rep(0,17))
tot.ma.10.B = c(rpois(8, lambda = tot.ma.10.A), rep(0,17))
tot.ma.10.C = c(rpois(8, lambda = tot.ma.10.A), rep(0,17))



# veja como esta ficando
minha.matriz <-
rbind(tot.am.100.A, tot.am.100.B, tot.am.100.C,
      tot.am.30.A, tot.am.30.B, tot.am.30.C,
      tot.am.10.A, tot.am.10.B, tot.am.10.C,
      tot.ce.100.A, tot.ce.100.B, tot.ce.100.C,
      tot.ce.30.A, tot.ce.30.B, tot.ce.30.C,
      tot.ce.10.A, tot.ce.10.B, tot.ce.10.C,
      tot.ma.100.A, tot.ma.100.B, tot.ma.100.C,
      tot.ma.30.A, tot.ma.30.B, tot.ma.30.C,
      tot.ma.10.A, tot.ma.10.B, tot.ma.10.C)

minha.matriz

dim(minha.matriz)

plot(hclust(d=vegdist(minha.matriz, "bray", binary = F), "ave"))

##====

# vamos criar uns identificadores importantes

bioma <- c("AM", "CE", "MA")

bioma <- rep(c("AM", "CE", "MA"), each=9)

cobertura <- c(100, 30, 10)

cobertura <- rep(cobertura, each=3)

cobertura <- rep(cobertura, times=3)
cobertura <- as.factor(cobertura)

cobertura

dados.final <- data.frame(bioma, cobertura, minha.matriz)
dados.final

dim(dados.final)

#========================================

plot(betadisper(vegdist(dados.final[,-c(1:2)], "jacc", binary=T), 
                group = dados.final[,1]), main = NULL, sub = "")

plot(betadisper(vegdist(dados.final[,-c(1:2)], "jacc", binary=T), 
                group = dados.final[,2]), main = NULL, sub = "")

adonis(dados.final[,-c(1:2)] ~ bioma*cobertura, data = dados.final,
        strata = bioma, by = "terms", method = "jacc")

#####

plot(betadisper(vegdist(dados.final[,-c(1:2)], "bray"), 
           group = dados.final[,1]), main = NULL, sub = "")

plot(betadisper(vegdist(dados.final[,-c(1:2)], "bray"), 
                group = dados.final[,2]), main = NULL, sub = "")

adonis(dados.final[,-c(1:2)] ~ bioma*cobertura, data = dados.final,
       strata = bioma, by = "terms")


# vamos separar por bioma e por cobertura
teste = paste0(dados.final$bioma,dados.final$cobertura)

plot(betadisper(vegdist(dados.final[,-c(1:2)], "bray"), 
                group = teste), main = NULL, sub = "")

plot(betadisper(vegdist(dados.final[,-c(1:2)], "bray"), 
                group = teste), main = NULL, sub = "",
     label.cex = 0.5)

plot(betadisper(vegdist(dados.final[,-c(1:2)], "bray"), 
                group = teste), main = NULL, sub = "",
     label = F)
