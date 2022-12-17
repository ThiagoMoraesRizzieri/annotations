### Aula Analise de ordenacaoo

# 1.1 - Carregue os pacotes necessarios 
library(FactoMineR)
library(factoextra)
library(vegan)
library(tidyverse)

# 1.2 - Baixe os arquivos e grave na sua pasta
# 1.3 - Careegue os arquivos na sessao R

dados_samu <- read.csv("dados_samu_2.csv", T, row.names = 1)
dados_samu

# explore os dados
names(dados_samu)
str(dados_samu)
rownames(dados_samu)

# Vamos explorar nosso delineamento amostral 
with(dados_samu, table(reserv, periodo))

# Responda:
# O que a tabela abaixo diz sobre nosso delineamento?

# Diz que em todo reservatório, há três amostras em período chuvoso e
# três amostras período de seca

with(dados_samu, table(reserv, periodo, inund))

# Responda:
# Quais sÃ£o os reservatorios que foram inundados com vegetacao?

# Os reservatórios h1, h2 e h3, pois nestes reservatórios, inund = veg.

# Vamos agora mudar de formato largo para longo
# Vou usar o formato mais longo possivel
# Vamos usar a funcao pivot_longer

# Primeiro vamos ver como fica
pivot_longer(dados_samu, 
             cols = bac_meta:n2o, 
             names_to = "variavel", 
             values_to = "valor")

# Agora vamos guradar dentro de um novo objeto
dados_longo <- pivot_longer(dados_samu, cols = bac_meta:n2o, 
                            names_to = "variavel", 
                            values_to = "valor")

# Vamos explorar o que da para fazer no formato longo
# Ex. fica mais facil filtrar parte dos dados
# Vou pegar so os dados de emissao de metano

filter(dados_longo, 
       variavel == "ch4")

# Agora faÃ§a voce, filtre somente os reservatorios com vegetacao inundada


filter(dados_longo, 
       inund == "veg")

# Eu quero filtar somento os dados coletados no periodo chuvoso.
# O que poderia ser mudado na planilha para facilitar esta tarefa?

# Na nossa planilha de dados há três valores que o período pode assumir como
# período chuvoso, o ch1, o ch2 e o ch3. 
# Poderia haver uma coluna que apresentasse com um único termo "Chuva" para os
# três, pois precisaremos colocar cada um no nosso filtro.

filter(dados_longo, 
       periodo == "ch1" | periodo == "ch2" | periodo == "ch3")

###===========================================================
#1.4 - Ordenacao
#1.5 - PCA na matriz ambiental

# Existem diversas funcoes no R que fazem PCA.
# Primeiro vamos fazer com o pacote factoextra
# Para isso vamos usar somente a variaveis numericas.

# Ou seja, vamos ignorar essas:
dados_samu[,c(1:3)]

res.pca <- PCA(dados_samu[,-c(1:3)],  graph = F)

# Vamos ver o que tem dentro do obeto com os resultados
res.pca

# Tem bastante coisa.
# Vamos ver a variacao representada por cada um dos componentes principais
res.pca$eig

# Vamos colocar isso num grafico
# Agora o priemiro grafico
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 60), xlab = "Eixos",
               ylab = "VariaÃ§Ã£o representada (%)", title = "")

# Agora voce. Mostre a contribuicao das variaveis:

res.pca$var$contrib

# Tente descobrir onde esta o resultado olhando aqui:

res.pca


# Agora o grafico com as contribuicoes
# Nao se preocupe com o codigo

fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             xlab = "Eixo 1", ylab = "Eixo 2", title = "")

# Vamos adicionar os reservatorios
# Ou seja, vamos ordenar os reservatorios
# Antes, mostre os escores (ou coordenadas) dos reservatorios

res.pca$ind$coord
# Descubra aqui:
res.pca


# Agora o grafico
fviz_pca_biplot(res.pca, geom = "point", repel = TRUE, xlab = "Eixo 1", 
                ylab = "Eixo 2", title = "", ylim = c(-5,5), xlim = c(-5,5))

# Que reservatorio deve ser aquele ponto mais a direita no eixo 1?

# O h1_ch1_veg, por ter maior contribuição para a primeira componente.

# Vamos confirmar
fviz_pca_biplot(res.pca, repel = TRUE, xlab = "Eixo 1", 
                ylab = "Eixo 2", title = "",
                ylim = c(-5,5), xlim = c(-5,5))

# Inteprete a ordenaÃ§Ã£o.
# Primeiro descreva a ordenaÃ§Ã£o dos locais ao longo do eixo 1 e a associaÃ§Ã£o 

# A componente principal 1 separa os reservatórios em período chuvoso (valores positivos)
# e reservatórios em período de seca (valores negativos)

# com as variÃ¡veis e depois faÃ§a o mesmo com o eixo 2.

# A componente principal 2 separa os reservatórios com vegetação inundada (valores positivos)
# e sem vegetação inundada (valores negativos).

#===============================================================================
# Agora vamos usar a funcao rda() para fazer a pca

?rda

resu.pca <- rda(dados_samu[,-c(1:3)], scale=TRUE)

summary(resu.pca) # Mostra a infor que precisamos; var capturada;
                  # cargas; escores

# vamos olhar algumas informaÃ§Ãµes separadamente

# importÃ¢ncia das variÃ¡veis para os eixos
summary(resu.pca)$species

# Escores ou coordenadas
summary(resu.pca)$sites

# Vamos ver se a contribuicoe sdas variaveis sao as mesmas independente da funcao

plot(x = summary(resu.pca)$species[,1],
     y = res.pca$var$coord[,1])

# POdemos ate medir a correlacao

cor(x = summary(resu.pca)$species[,1],
     y = res.pca$var$coord[,1])

# Ou seja, as funcoes calculam os tamanhos das setas do mesmo jeito, mas
# expressam os resultados de jeitos diferentes

# Faca voce agora.
# Mostre que as duas funcoes, mesmo que expressem os resultados de jeitos 
# diferentes, calculam o escores (ou coordenadas) do mesmo jeito.
# Faca algo parecido que fizemos acima, mas as coordenadas do eixo 1



# Finalmente vamos ver o grafico e comparar com o do outro pacote

biplot(resu.pca, xlim=c(-2, 2))

fviz_pca_biplot(res.pca, repel = TRUE, xlab = "Eixo 1", 
                ylab = "Eixo 2", title = "",
                ylim = c(-5,5), xlim = c(-5,5))

# ==============================================================================
# Agora vamos tentar entender o que a PCA faz com dados com estrutura forte e fraca

# Vamos simular um conjunto de dados com estrutura forte, ou seja, com variaveis
# pouco correlacionadas entre si.

simu_forte <- data.frame(
  a = rnorm (30),
  b = rnorm (30),
  c = rnorm (30)
)

simu_forte

# Vamos explorar a correlacao entre as variaveis originais

plot(x = simu_forte$a,
     y = simu_forte$b,
     cex = 2, # cex aumenta o tamanho dos pontos
     cex.axis = 1.5,
     cex.lab = 1.5)

# Nao parece ter correlacao entre a e b
# Faca o memso para a e c

plot(x = simu_forte$a,
     y = simu_forte$c,
     cex = 2, # cex aumenta o tamanho dos pontos
     cex.axis = 1.5,
     cex.lab = 1.5)

# Vamos fazer a pca
pca.forte <- PCA(simu_forte,  graph = F)

# Vamos ver a variacao representada pelos eixos
pca.forte$eig

# E a contribuicao das variaveis
pca.forte$var$contrib

# E o grafico
fviz_pca_biplot(pca.forte, geom = "point", repel = TRUE, xlab = "Eixo 1", 
                ylab = "Eixo 2", title = "")
# Veja como as variaveis estao apontado para diferentes

#==============================================================================
# Agora um dado com estrutura fraca
# Ou seja, pelo menos duas variaveis redundantes

a2 = rnorm (30)
b2 = a2^2
c2 = b2 * 5

simu_fraca <- data.frame(a2, b2, c2)

simu_fraca

# Vamos explorar a correlacao entre as variaveis originais

plot(x = simu_fraca$a2,
     y = simu_fraca$b2,
     cex = 2, # cex aumenta o tamanho dos pontos
     cex.axis = 1.5,
     cex.lab = 1.5)

plot(x = simu_fraca$a2,
     y = simu_fraca$c2,
     cex = 2, # cex aumenta o tamanho dos pontos
     cex.axis = 1.5,
     cex.lab = 1.5)

plot(x = simu_fraca$b2,
     y = simu_fraca$c2,
     cex = 2, # cex aumenta o tamanho dos pontos
     cex.axis = 1.5,
     cex.lab = 1.5)


# Vamos fazer a pca
pca.fraca <- PCA(simu_fraca,  graph = F)

# Vamos ver a variacao representada pelos eixos
pca.fraca$eig

# E a contribuicao das variaveis
pca.forte$var$contrib

# E o grafico
fviz_pca_biplot(pca.fraca, geom = "point", repel = TRUE, xlab = "Eixo 1", 
                ylab = "Eixo 2", title = "")


