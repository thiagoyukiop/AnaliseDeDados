# Realiznado Exercício 1 de Análise de Dados

setwd("C:/R/AnaliseDeDados/AnaliseDeDados")

library(tidyverse)
library(Hmisc)
library(gridExtra)
# Lendo a base de dados
dados <- read.csv(
    file = "base_de_dados/dirty_cafe_sales.csv"
)

View(dados)

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
dados_filtrados <- dados %>% 
    filter(!Item %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Quantity %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Total.Spent %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Price.Per.Unit %in% c("", "ERROR", "UNKNOWN")) %>%
    filter(!Payment.Method %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Location %in% c("", "ERROR", "UNKNOWN")) %>% 
    filter(!Transaction.Date %in% c("", "ERROR", "UNKNOWN"))

# Verificando o número de linhas da base após ser filtrada
nrow(dados_filtrados)

# view(dados_filtrados)

# Verificar tempo máximo de data da base de dados
max(dados_filtrados$Transaction.Date)

# Verificar tempo mínimo de data da base de dados
min(dados_filtrados$Transaction.Date)

unique(dados_filtrados$Location)

# view(dados_filtrados)

describe(dados_filtrados)

str(dados_filtrados)

# Ajustar os tipos de cada coluna, para ficarem da forma devida
dados_filtrados$Quantity <- as.integer(dados_filtrados$Quantity)

dados_filtrados$Price.Per.Unit <- as.numeric(dados_filtrados$Price.Per.Unit)

dados_filtrados$Total.Spent <- as.numeric(dados_filtrados$Total.Spent)

dados_filtrados$Transaction.Date <- as.Date(dados_filtrados$Transaction.Date)

ordem_itens <- c(
    "Cookie", "Tea", "Coffee", "Cake", "Juice", "Sandwich", "Smoothie", "Salad"
    )

dados_filtrados <- dados_filtrados %>% 
    mutate(Item = factor(Item, levels = ordem_itens))

p1 <- ggplot(data = dados_filtrados %>%
           distinct(Item, .keep_all = TRUE),
       aes(x = Item, y = Price.Per.Unit)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = Price.Per.Unit,
                  vjust = -0.5)) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, 6)
    ) +
    labs(title = "Gráfico de Barras de Preços Unitário por Item",
         x = "Item",
         y = "Preço por Unidade (US$)") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() 
        )

p2 <- ggplot(data = dados_filtrados %>% 
           group_by(Item) %>% 
           count(),
       aes(x = Item, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = n,
                  vjust = -0.5)) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, 500)
    ) +
    labs(title = "Gráfico de Barras de Vendas por Item",
         x = "Item",
         y = "Número de Vendas") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() 
    )

# grid.arrange(p1, p2, ncol = 1)

p3 <- ggplot(data = dados_filtrados %>% 
                 group_by(Item) %>% 
                 summarise(Total.Price = sum(Price.Per.Unit)),
             aes(x = Item, y = Total.Price)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = Total.Price,
                  vjust = -0.5)) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, 2500)
    ) +
    labs(title = "Gráfico de Barras de Preços Total por Item",
         x = "Item",
         y = "Preço Total (US$)") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() 
    )

grid.arrange(p1, p2, p3, ncol = 1)

dados_filtrados$Mes.Venda <- format(dados_filtrados$Transaction.Date, "%m") 

dados_filtrados$Mes.Venda <- as.integer(dados_filtrados$Mes.Venda)

dados_filtrados$Mes.Venda <- format(dados_filtrados$Transaction.Date, "%B") 

dados_filtrados <- dados_filtrados %>% 
    mutate(Mes.Venda = factor(Mes.Venda, levels = c("janeiro", "fevereiro", 
                                                    "março", "abril", "maio",
                                                    "junho", "julho", "agosto",
                                                    "setembro", "outubro",
                                                    "novembro", "dezembro"
                                                    )))

ggplot(data = dados_filtrados %>% 
           group_by(Mes.Venda) %>% 
           count(),
       aes(x = Mes.Venda, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = n,
                  vjust = -0.5)) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, 320)
    ) +
    labs(title = "Gráfico de Barras de Vendas por Mês",
         x = "Mês",
         y = "Número Total de Vendas") +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() 
    )
