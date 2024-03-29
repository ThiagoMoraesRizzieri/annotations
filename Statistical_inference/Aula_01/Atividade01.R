# 1 - Classificar cada vari�vel

# ID: vari�vel qualitativa nominal
# Origem: vari�vel qualitativa nominal
# N�mero de consultas: vari�vel quantitativa discreta
# Idade: vari�vel quantitativa discreta
# Peso: vari�vel quantitativa cont�nua
# Sexo: vari�vel qualitativa nominal
# Tabagismo: vari�vel qualitativa ordinal
# Atividade f�sica: vari�vel qualitativa nominal
# Frequ�ncia: vari�vel quantitativa discreta

# 2 - Banco de dados numa planilha (feito)

# 3 - Importar ao R

library(readxl)
library(dplyr)
df <- read_excel("Atividade01.xlsx")
View(df)

# 4 - Quantos pacientes por cidade

df %>% 
  group_by(origem) %>% 
  count()

# 5 - M�nimo, m�ximo e m�dia de consultas por paciente

# M�nimo
min(df$n_consultas)

# M�ximo
max(df$n_consultas)

# M�dia
mean(df$n_consultas)

# 6 - M�dia, mediana, desvio padrao e quartis da idade e do peso dos pacientes?

# Estat�sticas para a idade
mean(df$idade)
median(df$idade)
sd(df$idade)
quantile(df$idade)

# Estat�sticas para o peso
mean(df$peso)
median(df$peso)
sd(df$peso)
quantile(df$peso)

# 7 - Porcentagem de pacientes por n�vel de tabagismo e atividade f�sica

# Porcentagem por tabagismo
prop.table(table(df$tabagismo)) * 100

# Porcentagem por atividade f�sica
prop.table(table(df$atividade_fisica)) * 100


# 8 - Tabela de conting�ncia entre sexo e atividade fisica. 

prop.table(table(df$sexo,df$atividade_fisica))
