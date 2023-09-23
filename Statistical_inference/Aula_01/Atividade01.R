# 1 - Classificar cada variável

# ID: variável qualitativa nominal
# Origem: variável qualitativa nominal
# Número de consultas: variável quantitativa discreta
# Idade: variável quantitativa discreta
# Peso: variável quantitativa contínua
# Sexo: variável qualitativa nominal
# Tabagismo: variável qualitativa ordinal
# Atividade física: variável qualitativa nominal
# Frequência: variável quantitativa discreta

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

# 5 - Mínimo, máximo e média de consultas por paciente

# Mínimo
min(df$n_consultas)

# Máximo
max(df$n_consultas)

# Média
mean(df$n_consultas)

# 6 - Média, mediana, desvio padrao e quartis da idade e do peso dos pacientes?

# Estatísticas para a idade
mean(df$idade)
median(df$idade)
sd(df$idade)
quantile(df$idade)

# Estatísticas para o peso
mean(df$peso)
median(df$peso)
sd(df$peso)
quantile(df$peso)

# 7 - Porcentagem de pacientes por nível de tabagismo e atividade física

# Porcentagem por tabagismo
prop.table(table(df$tabagismo)) * 100

# Porcentagem por atividade física
prop.table(table(df$atividade_fisica)) * 100


# 8 - Tabela de contingência entre sexo e atividade fisica. 

prop.table(table(df$sexo,df$atividade_fisica))
