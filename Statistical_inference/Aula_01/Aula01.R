2+2
2/3
2*3
2^3
2**3
1/10000
sqrt(25) # Raiz quadrada 
exp(2)

x<-20
x
x-10
x/2
3*x-10
y<-3*x-10
y
x<-c(20,15,13,34)
x
x[1:3]
1:3
x+1
m<-matrix(c(3,4,2,1,7,3), ncol=2, nrow=3)
m
m[2,1]
m[2,]
m[,2]


matrix(c(3,4,2,1,7,3), ncol=2, byrow=T)

alunos<-c("Ana","Beatriz","Carlos","Daniel")
alunos

dados <- read.table("dados.txt",
                    na.strings = ".",
                    h=T,
                    row.names = NULL)

dados
head(dados)
names(dados)
dados[1,]
dados[1,2]
dados$Sexo
Tempo_semanas<-dados$Tempo_dias/7
Tempo_semanas<-round(dados$Tempo_dias/7,1)
Tempo_semanas
round(2.375,1)
dados<-cbind(dados,Tempo_semanas)

table(dados$Sexo)
dados$Sexo<-ordered(dados$Sexo,
                    levels=c("F","M","I"))
tabela1<-table(dados$Sexo)
tabela1
addmargins(tabela1,FUN=list(Total=sum))

tabela2<-prop.table(tabela1)
round(tabela2*100,1)

tabela3<-table(dados$Sexo, dados$Destino)
tabela3
prop.table(tabela3)*100

round(prop.table(tabela3,margin=1),2)*100
round(prop.table(tabela3,margin=2),2)*100

head(dados)
summary(dados$Tempo_dias)
mean(dados$Tempo_dias)
sd(dados$Tempo_dias)
median(dados$Tempo_dias)
min(dados$Tempo_dias)
max(dados$Tempo_dias)

addmargins(tabela3,FUN=list(Total=sum))
addmargins(tabela3,FUN=list(Total=sum),margin=1)
addmargins(tabela3,FUN=list(Total=sum),margin=2)

