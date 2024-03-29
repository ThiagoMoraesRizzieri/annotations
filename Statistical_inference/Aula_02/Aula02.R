dados<- read.table("dados.txt",
                   na.strings = ".",
                   h=T, 
                   row.names=NULL) # Importacao dos dados para o R

# 
dados$Sexo<-ordered(dados$Sexo, levels =c("F","M", "I"))

# Ao inv�s de chamar dados$sexo, poderemos s� chamar sexo que j� aparece
# com o attach dados
# (por�m requer cuidado)
attach(dados)

Tempo_semanas<-round(dados$Tempo_dias/7,1)
dados<-cbind(dados,Tempo_semanas)
head(dados)

# Tabela de frequ�ncias
addmargins(table(Sexo), FUN = list(Total = sum))

# Calculando as porcentagens por sexo
round(prop.table(table(Sexo))*100,1)

#install.packages("ggplot2")
library(ggplot2)

# Iremos transformar as frequ�ncias dos sexos em um data frame
dados2 <- data.frame(table(Sexo))

# rgb -> vermelho, verde, azul e transpar�ncia da cor
ggplot(dados2, aes(x=Sexo,y=Freq))+
  geom_bar(stat="identity", fill=rgb(0,0,1,0.5))

# Colocando uma cor para cada sexo
# Por�m, colocar o fill j� na parte do ggplot, todo o gr�fico q vier depois
# seguir� aquele esquema de cores. Vc pode criar um aes() dentro do geom_ tb

ggplot(dados2, aes(x=Sexo,y=Freq,fill=Sexo))+
  geom_bar(stat="identity")+
  theme_minimal()


ggplot(dados2, aes(x=Sexo,y=Freq,fill=Sexo))+
  geom_bar(stat="identity")+
  labs(title="Frequ�ncia do sexo das aves observadas",
       x='Sexo',
       y='Frequ�ncia')+
  scale_x_discrete(
    labels=c("F"="F�mea","M"="Macho","I"="Indeterminado")
  )

# Novo banco de dados
dados3 <- data.frame(table(Destino,Sexo))

ggplot(dados3, aes(x=Destino, y=Freq))+
  geom_bar(stat="identity",aes(fill=Sexo))

ggplot(dados3, aes(x=Destino, y=Freq))+
  geom_bar(stat="identity",position="dodge",aes(fill=Sexo))

ggplot(dados3, aes(x=Destino, y=Freq))+
  geom_bar(stat="identity",position="fill",aes(fill=Sexo))

ggplot(dados3, aes(x=Destino, y=Freq))+
  geom_bar(stat="identity",aes(fill=Sexo))+
  facet_grid(~Sexo)

ggplot(dados3, aes(x=Sexo, y=Freq))+
  geom_bar(stat="identity",aes(fill=Destino))+
  facet_grid(~Destino)

# Gr�fico de pizza, indicando pela coordenada polar
ggplot(dados2, aes(x="",y=Freq,fill=Sexo))+
  geom_bar(stat='identity',width=1,color="white")+
  coord_polar("y",start=0)+
  theme_void()


# Histogramas

names(dados)

ggplot(dados,aes(x=Tempo_dias))+
  geom_histogram(binwidth = 1,
                 fill = rgb(0.7,0.1,0.1,1),
                 color="black")

ggplot(dados,aes(x=Tempo_dias))+
  geom_histogram(binwidth = 1,
                 fill = "black")+
  facet_grid(~Destino)

ggplot(dados,aes(x=Tempo_dias))+
  geom_histogram(binwidth = 1,
                 fill = "black")+
  facet_grid(Destino~Sexo)

ggplot(dados,aes(y=Tempo_dias))+
  geom_boxplot()

ggplot(dados,aes(x=Sexo, y=Tempo_dias))+
  geom_boxplot()

# Estat�sticas descritivas

length(Tempo_dias)
mean(Tempo_dias)
median(Tempo_dias)
sort(Tempo_dias)
quantile(Tempo_dias)
sd(Tempo_dias)
var(Tempo_dias)
sd(Tempo_dias)*100/mean(Tempo_dias)


# Estat�stica descritivas por sexo

tapply(Tempo_dias, Sexo, summary)
tapply(Tempo_dias, Sexo, length)


meansexo <- tapply(Tempo_dias, Sexo, mean)
sdsexo <- tapply(Tempo_dias, Sexo, sd)

sdsexo*100/meansexo


