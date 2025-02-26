# Realiznado Exercício 1 de Análise de Dados

setwd("C:/R/AnaliseDeDados/AnaliseDeDados")

library(tidyverse)

# Lendo a base de dados
dados <- read.csv(
    file = "base_de_dados/dirty_cafe_sales.csv"
)

# View(dados)

# Verificando o nome das colunas presentes na base de dados
names(dados)

# Verificando se há IDs duplicados
if(!any(duplicated(dados$Transaction.ID))) {
    print("Não há Transaction.ID duplicados")
} else {
    print("Há Transaction.ID duplicados")
}

# Verificando valores possíveis para uma coluna específica
unique(dados$Item)

unique(dados$Payment.Method)

unique(dados$Location)

# Verificando número de linhas da base de dados original
nrow(dados)

# Filtrando da base somente os valores que não possuem erros
dados_filtro_item <- dados %>% 
    filter(!Item %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Quantity %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Total.Spent %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Price.Per.Unit %in% c("", "ERROR", "UNKNOWN")) %>%
    filter(!Payment.Method %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Location %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Transaction.Date %in% c("", "ERROR", "UNKNOWN"))

# Verificando o número de linhas da base após ser filtrada
nrow(dados_filtro_item)

# view(dados_filtro_item)

# Verificar tempo máximo de data da base de dados
max(dados_filtro_item$Transaction.Date)

# Verificar tempo mínimo de data da base de dados
min(dados_filtro_item$Transaction.Date)


