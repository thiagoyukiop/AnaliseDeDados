########################################################################
## Description: Análise de Dados: Código desenvolvido para aula sobre
## limpeza, exploração e imputação de dados perdidos...
##
## Maintainer: UNIVALI / EMCT - NID Análise de Dados
## Author: Rodrigo Sant'Ana
## Created: qua mar  9 08:05:22 2022 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> 01. Instalando e Carregando pacotes / bibliotecas R...

######@> Instalação de novos pacotes / bibliotecas [Instalação de
######@> Pacotes precisa se executada apenas uma vez]...
install.packages(c("dplyr", "tidyr", "lubridate", "readxl", "ggplot2",
                   "patchwork", "sf", "ggcorrplot", "ggmap", "leaflet",
                   "Hmisc", "imputeTS", "stringr", "DataExplorer"),
                 dependencies = TRUE)

if(!require(pacman)) {
    install.packages("pacman", dependencies = TRUE)
    library(pacman)
} else {
    library(pacman)
}

######@> Carregando os pacotes para uso [Carregamento dos pacotes
######@> precisa ser feito sempre que quiser usar o mesmo]...
p_load(Hmisc, dplyr, tidyr, lubridate, stringr, DataExplorer, readxl,
       ggplot2, patchwork, sf, ggcorrplot, ggmap, leaflet, imputeTS)

########################################################################
######@> 2. Importando a base de dados...

######@> Base de dados...
dados <- read_excel("Rio_Itajai_Estuario_ver00.xlsx",
                    sheet = 1)

########################################################################
######@> 3. Visualizando os dados...

######@> Visualizando as primeiras linhas da base...
head(dados) %>% data.frame()

######@> Visualizando toda a base...
View(dados)

######@> Visualizando a estrutura dos dados e como foram importados...
glimpse(dados)

######@> Visualizando espacialmente a distribuição dos pontos
######@> amostrais...

#####@> visualização estática...

####@> passando os limites para imagem...
summary(dados)
br <- c(left = -48.7, bottom = -26.95, right = -48.6, top = -26.85)

register_stadiamaps(key = "7d3050bc-7428-4e79-87a0-f1fa7a52b381")

####@> requisitando a imagem...
mm <- get_stadiamap(br, source = "stadia")

####@> visualizando o mapa...
p00 <- ggmap(mm) +
    geom_point(data = dados, aes(x = Long, y = Lat, fill = Local),
               pch = 21, size = 2) +
    labs(x = "Longitude", y = "Latitude")
p00

#####@> visualização interativa...
background <- addTiles(leaflet())
RioItajai <-
    addMarkers(
        background,
        lat = dados$Lat,
        lng = dados$Long,
        label = dados$Estacao,
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
    )
RioItajai

######@> Visualizando um descritivo estatístico completo dos dados...
describe(dados)

########################################################################
######@> 3. Faxinando os dados...

######@> Corrigindo os problemas na variável "Local"...

#####@> Verificando os casos únicos...
table(dados$Local)

#####@> Corrigindo...
dados$Local <- ifelse(test = dados$Local %in%
                          c("Estuário", "estuario", "estuário"),
                      yes = "Estuário",
                      no = "Adjacência")

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Local)

######@> Corrigindo os problemas na variável "Mês"...

#####@> Verificando os casos únicos...
table(dados$Mês)

#####@> Corrigindo...
dados$Mês <- str_to_title(dados$Mês)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Mês)

######@> Corrigindo os problemas na variável "Draga"...

#####@> Verificando os casos únicos...
table(dados$Draga)

#####@> Corrigindo...
dados$Draga <- str_to_title(dados$Draga)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Draga)

#####@> Corrigindo 02...
dados$Draga <- ifelse(test = dados$Draga == "Nao",
                      yes = "Não",
                      no = dados$Draga)

#####@> Verificando novamente os casos únicos para ver se resolvemos os
#####@> problemas...
table(dados$Draga)

########################################################################
######@> 3. Imputando dados perdidos...

######@> Verificando os dados perdidos...
plot_missing(dados)

######@> Imputando pela média da série de dados...

#####@> Concentração de Carbonato...

####@> visualizando...
dados$Carb

####@> calculando a média...
mean(dados$Carb, na.rm = TRUE)

####@> imputando...
dados$Carb.imp <- na_mean(dados$Carb)

####@> visualizando...
dados$Carb.imp

#####@> Concentração de Matéria Orgânica...

#####@> Concentração de Sedimento Grosso...

#####@> Concentração de Sedimento Fino...

#####@> Concentração de Sedimento Cádmio...

########################################################################
######@> 4. Explorando a base de dados...

######@> Avaliando as variáveis físico-químicas individualmente...

#####@> Temperatura...

####@> média, desvio padrão e quantis da distribuição...
c("Média" = mean(dados$Temp), "Desvio padrão" = sd(dados$Temp),
  quantile(dados$Temp))

####@> Função para histogramas...
criar_histograma <- function(dados, coluna_x, steps, label_x, round_number) {
    # Verifica se a coluna existe nos dados
    if (!coluna_x %in% colnames(dados)) {
        stop("A coluna especificada não existe no data frame.")
    }
    
    # Remove NA da coluna especificada
    dados <- dados[!is.na(dados[[coluna_x]]), ]
    
    # Calcula os limites do eixo x
    min_x <- round(min(dados[[coluna_x]]), digits = round_number)
    max_x <- round(max(dados[[coluna_x]]), digits = round_number)
    
    # Cria o gráfico
    ggplot(data = dados, aes(x = .data[[coluna_x]])) +
        geom_histogram(
            binwidth = 0.5, boundary = 0.5, closed = "right",
            fill = "white", colour = "black"
        ) +
        scale_x_continuous(
            breaks = seq(min_x, max_x, by = steps),
            expand = c(0, 0),
            limits = c(min_x - 1, max_x + 1)
        ) +
        scale_y_continuous(
            expand = c(0, 0),
            limits = c(0, 30)
        ) +
        labs(
            x = label_x,
            y = "Frequência absoluta (n)"
        )
}

####@> histograma...

####@> Temp...
criar_histograma(dados, "Temp", 0.5, "Temperatura (ºC)", 0)
# ggplot(data = dados, aes(x = Temp)) +
#     geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
#                    fill = "white", colour = "black") +
#     scale_x_continuous(breaks = seq(17, 26, 0.5), expand = c(0, 0),
#                        limits = c(16.5, 26.5)) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
#     labs(x = "Temperatura (ºC)", y = "Frequência absoluta (n)")

#####@> pH...
criar_histograma(dados, "pH", 0.5, "pH", 0)
# ggplot(data = dados, aes(x = pH)) +
#     geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
#                    fill = "white", colour = "black") +
#     scale_x_continuous(breaks = seq(7, 9, 0.5), expand = c(0, 0),
#                        limits = c(6.5, 9.5)) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
#     labs(x = "pH", y = "Frequência absoluta (n)")

#####@> Salinidade...
criar_histograma(dados, "Sal", 5, "Salinidade", 0)
# ggplot(data = dados, aes(x = Sal)) +
#     geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
#                    fill = "white", colour = "black") +
#     scale_x_continuous(breaks = seq(0, 35, 5), expand = c(0, 0),
#                        limits = c(-1, 36)) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
#     labs(x = "Salinidade", y = "Frequência absoluta (n)")

#####@> Sedimento fino...
criar_histograma(dados, "Fino", 10, "Sedimento Fino (%)", 0)
# ggplot(data = dados, aes(x = Fino)) +
#     geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
#                    fill = "white", colour = "black") +
#     scale_x_continuous(
#         breaks = seq(
#             round(min(na.omit(dados$Fino))), 
#             round(max(na.omit(dados$Fino))),
#             10
#             ), expand = c(0, 0),
#         limits = c(-1, 101)) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
#     labs(x = "Sedimento fino (%)", y = "Frequência absoluta (n)")

#####@> Sedimento grosso...
criar_histograma(dados, "Grosso", 10, "Sedimento Grosso (%)", 0)
# ggplot(data = dados, aes(x = Grosso)) +
#     geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
#                    fill = "white", colour = "black") +
#     scale_x_continuous(
#         breaks = seq(
#             round(min(na.omit(dados$Grosso))),
#             round(max(na.omit(dados$Grosso))),
#             10
#             ), expand = c(0, 0),
#         limits = c(
#             (round(min(na.omit(dados$Grosso))) - 1), 
#             (round(max(na.omit(dados$Grosso))) + 1))
#         ) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
#     labs(x = "Sedimento grosso (%)", y = "Frequência absoluta (n)")

#####@> Material orgânica...
criar_histograma(dados, "MO", 1, "Material orgânica (%)", 0)
# ggplot(data = dados, aes(x = MO)) +
#     geom_histogram(binwidth =  0.5, boundary = 0.5, closed = "right",
#                    fill = "white", colour = "black") +
#     scale_x_continuous(
#         breaks = seq(
#             round(min(na.omit(dados$MO))),
#             round(max(na.omit(dados$MO))),
#             1
#             ), expand = c(0, 0),
#         limits = c(
#             (round(min(na.omit(dados$MO))) - 1),
#             (round(max(na.omit(dados$MO))) + 1))
#         ) +
#     scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
#     labs(x = "Material orgânica (%)", y = "Frequência absoluta (n)")

#####@> Concentração de carbonatos...
criar_histograma(dados, "Carb", 1, "Concentração de carbonatos (%)", 0)

#####@> Concentração de cádmio (imputado)...
# criar_histograma(dados, "Cd", 0.01, "Concentração de cádmio (mg/kg)",3)
ggplot(data = dados, aes(x = Cd)) +
    geom_histogram(binwidth =  0.02, boundary = 0.1, closed = "right",
                   fill = "white", colour = "black") +
    scale_x_continuous(
        breaks = seq(
            round(min(na.omit(dados$Cd)),2),
            round(max(na.omit(dados$Cd)),2),
            0.05
            ), expand = c(0, 0),
        limits = c(
            (round(min(na.omit(dados$Cd))) - 0.1),
            (round(max(na.omit(dados$Cd))) + 0.5))
        ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
    labs(x = "Concentração de cádmio (mg/kg)", y = "Frequência absoluta (n)")

#####@> Concentração de zinco (imputado)...
# criar_histograma(dados, "Zn", 20, "Concentração de zinco (mg/kg)", 0)
ggplot(data = dados, aes(x = Zn)) +
    geom_histogram(binwidth =  3, boundary = 0.5, closed = "right",
                   fill = "white", colour = "black") +
    scale_x_continuous(
        breaks = seq(
            round(min(na.omit(dados$Zn)),0),
            round(max(na.omit(dados$Zn)),0),
            10
        ), expand = c(0, 0),
        limits = c(
            (round(min(na.omit(dados$Zn))) - 1),
            (round(max(na.omit(dados$Zn))) + 1))
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
    labs(x = "Concentração de zinco (mg/kg)", y = "Frequência absoluta (n)")

#####@> Concentração de niquel (imputado)...
criar_histograma(dados, "Ni", 10, "Concentração de niquel (mg/kg)", 0)

#####@> Concentração de cromo (imputado)...
criar_histograma(dados, "Cr", 10, "Concentração de cromo (mg/kg)", 0)

######@> Avaliando as variáveis físico-químicas em função de outras
######@> variáveis...

#####@> Temperatura...

####@> média, desvio padrão e quantis da distribuição por Local...
dados %>%
    group_by(Local) %>%
    summarise(Media = mean(Temp, na.rm = TRUE),
              Desvio =  sd(Temp, na.rm = TRUE),
              q2.5 = quantile(Temp, 0.025),
              q50 = quantile(Temp, 0.5),
              q97.5 = quantile(Temp, 0.975))

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Temp)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Temperatura (ºC)")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>%
    group_by(Local, Draga) %>%
    summarise(Media = mean(Temp, na.rm = TRUE),
              Desvio =  sd(Temp, na.rm = TRUE),
              q2.5 = quantile(Temp, 0.025),
              q50 = quantile(Temp, 0.5),
              q97.5 = quantile(Temp, 0.975))

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Temp)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Temperatura (ºC)")

#####@> pH...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(pH, na.rm = T),
        Desvio =  sd(pH, na.rm = TRUE),
        q2.5 = quantile(pH, 0.025),
        q50 = quantile(pH, 0.5),
        q97.5 = quantile(pH, 0.975)
        )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = pH)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(5, 10)) +
    labs(x = "Local", y = "pH")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(pH, na.rm = T),
        Desvio =  sd(pH, na.rm = TRUE),
        q2.5 = quantile(pH, 0.025),
        q50 = quantile(pH, 0.5),
        q97.5 = quantile(pH, 0.975)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = pH)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(5, 10)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "pH")

#####@> Salinidade...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(Sal, na.rm = T),
        Desvio =  sd(Sal, na.rm = TRUE),
        q2.5 = quantile(Sal, 0.025),
        q50 = quantile(Sal, 0.5),
        q97.5 = quantile(Sal, 0.975)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Sal)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Sal")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Sal, na.rm = T),
        Desvio =  sd(Sal, na.rm = TRUE),
        q2.5 = quantile(Sal, 0.025),
        q50 = quantile(Sal, 0.5),
        q97.5 = quantile(Sal, 0.975)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Sal)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Sal")

#####@> Sedimento fino...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(Fino, na.rm = T),
        Desvio =  sd(Fino, na.rm = TRUE),
        q2.5 = quantile(Fino, 0.025),
        q50 = quantile(Fino, 0.5),
        q97.5 = quantile(Fino, 0.975)
    )

####@> boxplot por Local...
dados %>% 
    filter(Fino != is.na(Fino)) %>% 
    ggplot(aes(x = Local, y = Fino)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Sedimento Fino")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Fino, na.rm = T),
        Desvio =  sd(Fino, na.rm = TRUE),
        q2.5 = quantile(Fino, 0.025),
        q50 = quantile(Fino, 0.5),
        q97.5 = quantile(Fino, 0.975)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Fino)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Sedimento Fino")


#####@> Sedimento grosso...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    filter(Grosso != is.na(Grosso)) %>%
    group_by(Local) %>% 
    summarise(
        Media = mean(Grosso, na.rm = T),
        Desvio =  sd(Grosso, na.rm = TRUE),
        q2.5 = quantile(Grosso, 0.025),
        q50 = quantile(Grosso, 0.5),
        q97.5 = quantile(Grosso, 0.975)
    )

####@> boxplot por Local...
dados %>% 
    filter(Grosso != is.na(Grosso)) %>% 
    ggplot(aes(x = Local, y = Grosso)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Sedimento Grosso")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    filter(Grosso != is.na(Grosso)) %>%
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Grosso, na.rm = T),
        Desvio =  sd(Grosso, na.rm = TRUE),
        q2.5 = quantile(Grosso, 0.025),
        q50 = quantile(Grosso, 0.5),
        q97.5 = quantile(Grosso, 0.975)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Grosso)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Sedimento Grosso")


#####@> Material orgânica...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    filter(MO != is.na(MO)) %>%
    group_by(Local) %>% 
    summarise(
        Media = mean(MO, na.rm = T),
        Desvio =  sd(MO, na.rm = TRUE),
        q2.5 = quantile(MO, 0.025),
        q50 = quantile(MO, 0.5),
        q97.5 = quantile(MO, 0.975)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = MO)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Material orgânica")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    filter(MO != is.na(MO)) %>%
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(MO, na.rm = T),
        Desvio =  sd(MO, na.rm = TRUE),
        q2.5 = quantile(MO, 0.025),
        q50 = quantile(MO, 0.5),
        q97.5 = quantile(MO, 0.975)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = MO)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Material orgânica")

#####@> Concentração de carbonatos...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(Carb, na.rm = T),
        Desvio =  sd(Carb, na.rm = TRUE),
        q2.5 = quantile(Carb, 0.025, na.rm = T),
        q50 = quantile(Carb, 0.5, na.rm = T),
        q97.5 = quantile(Carb, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Carb)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "Local", y = "Concentração de Carbonato")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Carb, na.rm = T),
        Desvio =  sd(Carb, na.rm = TRUE),
        q2.5 = quantile(Carb, 0.025, na.rm = T),
        q50 = quantile(Carb, 0.5, na.rm = T),
        q97.5 = quantile(Carb, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Carb)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 30)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Concentração de Carbonato")

#####@> Concentração de cádmio (imputado)...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(Cd, na.rm = T),
        Desvio =  sd(Cd, na.rm = TRUE),
        q2.5 = quantile(Cd, 0.025, na.rm = T),
        q50 = quantile(Cd, 0.5, na.rm = T),
        q97.5 = quantile(Cd, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Cd)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 0.5)) +
    labs(x = "Local", y = "Concentração de Cádmio")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Cd, na.rm = T),
        Desvio =  sd(Cd, na.rm = TRUE),
        q2.5 = quantile(Cd, 0.025, na.rm = T),
        q50 = quantile(Cd, 0.5, na.rm = T),
        q97.5 = quantile(Cd, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Cd)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 0.5)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Concentração de Cádmio")

#####@> Concentração de zinco (imputado)...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(Zn, na.rm = T),
        Desvio =  sd(Zn, na.rm = TRUE),
        q2.5 = quantile(Zn, 0.025, na.rm = T),
        q50 = quantile(Zn, 0.5, na.rm = T),
        q97.5 = quantile(Zn, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Zn)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 40)) +
    labs(x = "Local", y = "Concentração de Zinco")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Zn, na.rm = T),
        Desvio =  sd(Zn, na.rm = TRUE),
        q2.5 = quantile(Zn, 0.025, na.rm = T),
        q50 = quantile(Zn, 0.5, na.rm = T),
        q97.5 = quantile(Zn, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Zn)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 40)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Concentração de Zinco")

#####@> Concentração de niquel (imputado)...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(Ni, na.rm = T),
        Desvio =  sd(Ni, na.rm = TRUE),
        q2.5 = quantile(Ni, 0.025, na.rm = T),
        q50 = quantile(Ni, 0.5, na.rm = T),
        q97.5 = quantile(Ni, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Ni)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 60)) +
    labs(x = "Local", y = "Concentração de Niquel")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Ni, na.rm = T),
        Desvio =  sd(Ni, na.rm = TRUE),
        q2.5 = quantile(Ni, 0.025, na.rm = T),
        q50 = quantile(Ni, 0.5, na.rm = T),
        q97.5 = quantile(Ni, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Ni)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 60)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Concentração de Niquel")

#####@> Concentração de cromo (imputado)...
####@> média, desvio padrão e quantis da distribuição por Local...
dados %>% 
    group_by(Local) %>% 
    summarise(
        Media = mean(Cr, na.rm = T),
        Desvio =  sd(Cr, na.rm = TRUE),
        q2.5 = quantile(Cr, 0.025, na.rm = T),
        q50 = quantile(Cr, 0.5, na.rm = T),
        q97.5 = quantile(Cr, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Cr)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "Local", y = "Concentração de Cromo")

####@> média, desvio padrão e quantis da distribuição por Local e Dragagem...
dados %>% 
    group_by(Local, Draga) %>% 
    summarise(
        Media = mean(Cr, na.rm = T),
        Desvio =  sd(Cr, na.rm = TRUE),
        q2.5 = quantile(Cr, 0.025, na.rm = T),
        q50 = quantile(Cr, 0.5, na.rm = T),
        q97.5 = quantile(Cr, 0.975, na.rm = T)
    )

####@> boxplot por Local...
ggplot(data = dados, aes(x = Local, y = Cr)) +
    geom_boxplot(fill = "white", colour = "black") +
    scale_y_continuous(limits = c(0, 100)) +
    facet_wrap(~Draga) +
    labs(x = "Local", y = "Concentração de Cromo")


########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
