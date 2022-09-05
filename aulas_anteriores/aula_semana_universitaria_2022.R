# Aula 1 - R básico ########################################
1 + 2
12 * 5
2 / 2

# funções sqrt() e log()
sqrt(100)
log()
log(100, base = 10)
log(x = 100, base = 10)

log(100, 10)
log(base = 10, x = 1000)

# Como pedir ajuda
?log
help("log")

# Tem diferença entre maiusculo e minusculo
LOG(100)


# Vetores
c(1, 2, 3, 4, 5, 6, 7) %*% c(1, 2, 3, 4, 5, 6, 7)


# Objetos
x <- 2
7 -> x

x * 100

nome_gigantesco_mesmo <- 0
nome com espaco <- 10
9x <- 1000
x9 <- 10000


# classes
# caractere
nome <- "lago paranoá"
novo_nome <- 'lago paranoá aspas simples'

# lógico
2 != 9
c(1, 2, 3, 4, 5, 6, 7) > 4
sum(c(1, 2, 3, 4, 5, 6, 7) > 4)

# inteiro
x_int <- 3L

class(x)
class(x_int)
class(nome)


# Coerção
# Criação de um vetor
x_vetor <- c(1, 2, 3, 4, "nulo")
x_vetor * 10

# coerção para formato numérico, salvando por cima
x_vetor <- as.numeric(c(1, 2, 3, 4, "nulo"))
x_vetor <- as.numeric(x_vetor)
x_vetor


# sequencias
sequencia <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
1:10
seq(10, 1, -2)

sequencia_diferente <- seq(from = 1,
                           to = 10,
                           by = 3)
  
class(sequencia_diferente)

data_hoje <- "2020-08-29"
class(data_hoje)

data_i <- as.Date("2020-08-29")
class(data_hoje)

seq_datas <- seq.Date(from = as.Date("1900-08-29"),
                      to = Sys.Date(),
                      by = "day")

seq_datas

seq_datas <- seq.Date(from = data_i,
                      to = Sys.Date(),
                      by = "day")

# indexing
seq_datas[c(1, 2, 5)]

seq_datas[1]

head(x = seq_datas, n = 2)
tail(seq_datas, 3)

min(seq_datas)
max(seq_datas)


sum(seq_datas >= as.Date("2022-01-01"))
seq_vazao <- rnorm(731, mean = 100, sd = 20)

tabela <- data.frame(Datas = seq_datas,
                     Vazao = seq_vazao)

tabela[731,2]
tabela$Vazao[1]
tabela[1, 2]

tabela[1:3,]
tabela[c(1, 2, 3, 4, 6), 1]

datas_2022 <-
  tabela$Datas >=
  as.Date("2022-01-01")

tabela[datas_2022,]

mean(tabela$Vazao)
min(tabela$Datas)

tabela$Qnorm <-
  (tabela$Vazao - mean(tabela$Vazao)) / sd(tabela$Vazao)

str(tabela)
summary(tabela)


# Funções - como criar?

fun_circ <- function(raio){
  
  area <- pi * raio^2
  return(area)

}


circulo_1 <- fun_per_circ(1)

fun_per_circ("oi")


# ifelse 
x <- 3

ifelse(x == 3,
      "x é igual a 3",
      "x é diferente de 3")


# loops (for e while)
for(i in c(1, 5, 10)){
  x <- fun_per_circ(i)
  print(x)
}


# while
i <- 1

while(i < 100){
  print(i)
}



# Aula 2 - Obtenção de dados ###############################

# diretório
getwd()
setwd("C:/Users/UnB/Desktop/Curso R")

list.files()
dir()


# Puxando um arquivo .txt
dados_ANA <- read.table(file = "vazoes_T_60435000.txt",
                        header = FALSE,
                        skip = 14,
                        sep = ";",
                        dec = ",")


# instalar e carregar pacotes
library(XML)
library(lubridate)

# argumentos pra eu usar
codEstacao <- 60435000
data_i <- "01/01/1800"
data_f <- "30/08/2022"
tipoDados <- 3
nivel_consist <- 1

serie_ANA <- function(codEstacao,
                      data_i = "01/01/1800",
                      data_f = "30/08/2022",
                      tipoDados = 3,
                      nivel_consist = 1){
  url_base <-
    paste0("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/",
           "HidroSerieHistorica?",
           "codEstacao=", codEstacao,
           "&dataInicio=", data_i,
           "&dataFim=", data_f,
           "&tipoDados=", tipoDados,
           "&nivelConsistencia=", nivel_consist)  
  
  url_parse <- XML::xmlParse(url_base, encoding = "UTF-8")
  node_doc <- getNodeSet(url_parse, "//SerieHistorica")
  dados_proxy <- xmlToDataFrame(nodes = node_doc)
  
  dados_proxy$Data <-
    as.Date(substr(dados_proxy$DataHora, 1, 10))
  
  datas_dias <-
    seq.Date(from = min(dados_proxy$Data),
             to = max(dados_proxy$Data) %m+% months(1) - 1,
             by = "day")
  
  tabela_final <-
    data.frame(Cod_estacao = dados_proxy$EstacaoCodigo[1],
               Data = as.character(datas_dias),
               Vazao = as.numeric(NA))
  
  
  for(i in 1:nrow(tabela_final)){
    
    # achar o dia
    dia <- as.numeric(substr(tabela_final$Data[i], 9, 10))
    
    # achar o mês e ano
    mes_ano <- paste0(substr(tabela_final$Data[i], 1, 8), "01")
    mes_ano <- as.Date(mes_ano)
    
    # qual linha que está o dado
    linha_dado <- which(dados_proxy$Data == mes_ano)
    
    # preencher a tabela
    ifelse(length(linha_dado) == 0,
           tabela_final$Vazao[i] <- NA,
           tabela_final$Vazao[i] <-
             as.numeric(dados_proxy[linha_dado, (dia + 15)]))
  }

  return(tabela_final)
  
} # aqui acaba a nossa função

dados_novo <- serie_ANA(codEstacao = 60435000)
  
for(i in c(60435000, 60436000, 60436190, 60443000)){
  
  dados_proxy <- serie_ANA(codEstacao = i)
  
  write.table(x = dados_proxy,
              file = paste0(i, ".txt"),
              sep = "\t",
              dec = ".",
              row.names = FALSE,
              fileEncoding = "UTF-8")
}

for(i in c(60435000, 60436000, 60436190, 60443000)){

  dados_proxy <- serie_ANA(codEstacao = i)
  
  if(i == 60435000) tabela_final <- dados_proxy
  if(i != 60435000) tabela_final <- rbind(tabela_final,
                                          dados_proxy)
  
}

write.table(x = tabela_final,
            file = "todas_estacoes.txt",
            sep = "\t",
            dec = ".",
            row.names = FALSE,
            fileEncoding = "UTF-8")


# Aula 3 - Manipulação de dados ############################
library(tidyverse)

library(lubridate)
library(dplyr)
library(magrittr)

dados_ANA <- 
  read.table(file = "60435000.txt",
             sep = "\t",
             dec = ".",
             header = TRUE,
             fileEncoding = "UTF-8")

dados_ANA$Data <- as.Date(dados_ANA$Data)
class(dados_ANA$Data)

min(dados_ANA$Data)
max(dados_ANA$Data)

max(dados_ANA$Vazao, na.rm = T)
min(dados_ANA$Vazao, na.rm = T)
mean(dados_ANA$Vazao, na.rm = T)

is.na(dados_ANA$Vazao)
sum(is.na(dados_ANA$Vazao))

dados_ANA$Data[is.na(dados_ANA$Vazao)]

max(dados_ANA$Data[is.na(dados_ANA$Vazao)])


# função ano hidrológico

fun_ano_hidro <- function(datas,
                          comeco_ano_hidro = 8){
  
  # garantir formato de Date
  datas <- as.Date(datas)
  
  # se vai pular (deslocar) um ano ou não
  desloc_ano <- ifelse(month(datas) < comeco_ano_hidro,
                       0, 1)
  
  ano_hidro <- year(datas) + desloc_ano
  
  return(ano_hidro)
}


dados_ANA$Ano_hidro <- fun_ano_hidro(datas = dados_ANA$Data)  

c(1, 2, 3, 4, 5) * 10


#  pipe operator
round(mean(exp(diff(log(dados_ANA$Vazao))), na.rm = T), digits = 2)

dados_ANA$Vazao %>%
  log() %>%
  diff() %>%
  exp() %>%
  mean(., na.rm = T) %>%
  round(x = ., digits = 2)

fun_resumo_anual <- function(dados_hidrologicos){
  
  tabela_resumo <-
    dados_hidrologicos %>%
    mutate(Ano_hidro = fun_ano_hidro(datas = Data)) %>%
    group_by(Cod_estacao, Ano_hidro) %>%
    summarise(maxima = max(Vazao, na.rm = T),
              minima = min(Vazao, na.rm = T),
              media = mean(Vazao, na.rm = T),
              NAs = sum(is.na(Vazao)))
  
}

teste <-
  fun_resumo_anual(dados_hidrologicos = dados_ANA)

write.table(x = teste,
            file = "Resumo_60435000.txt",
            sep = "\t",
            dec = ".",
            row.names = FALSE,
            fileEncoding = "UTF-8")

estacoes_cod <-
  c(60435000, 60436000, 60436190, 60443000)

for(i in 1:4){
  dados_proxy <- read.table(file = paste0(estacoes_cod[i],".txt"),
                            sep = "\t",
                            dec = ".",
                            header = T,
                            fileEncoding = "UTF-8")
  
  dados_proxy <- fun_resumo_anual(dados_proxy)
  
  if(i == 1) tabela_final <- dados_proxy
  if(i != 1) tabela_final <- rbind(tabela_final, dados_proxy)
}

tabela_final[!is.finite(tabela_final$maxima), c(3, 4, 5)] <- NA

write.table(x = tabela_final,
            file = "Resumo_estacoes.txt",
            sep = "\t",
            dec = ".",
            row.names = FALSE,
            fileEncoding = "UTF-8")

  
# Aula 4 - Gráficos ####

dados_60435000 <- read.table(file = "60435000.txt",
                             sep = "\t",
                             dec = ".",
                             header = T,
                             fileEncoding = "UTF-8",
                             colClasses = c("factor", "Date", NA))

dados_todas <- read.table(file = "todas_estacoes.txt",
                          sep = "\t",
                          dec = ".",
                          header = T,
                          fileEncoding = "UTF-8",
                          colClasses = c("factor", "Date", NA))

resumo_todas <- read.table(file = "Resumo_estacoes.txt",
                           sep = "\t",
                           dec = ".",
                           header = T,
                           fileEncoding = "UTF-8")

# graficos
dados_60435000$Ano <- year(dados_60435000$Data)
dados_60435000$Mes <- month(dados_60435000$Data)

dados_60435000_v2 <-
  dados_60435000 %>%
  group_by(Ano, Mes) %>%
  summarise(media = mean(Vazao)) %>%
  arrange(desc(Ano))

dados_heatmap <-
  pivot_wider(data = dados_60435000_v2,
              names_from = Mes,
              values_from = media,
              names_sort = TRUE)

dados_heatmap <- as.matrix(dados_heatmap[,-1])

meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
           "Jul", "Ago", "Set", "Out", "Nov", "Dez")

heatmap(dados_heatmap,
        Colv = NA, Rowv = NA, scale = "none",
        labCol = meses,
        labRow = 2021:1978)


# ggplot2() e gráficos base
plot(x = dados_60435000$Data,
     y = dados_60435000$Vazao,
     type = "l",
     col = "blue",
     main = "titulo",
     sub = "subtit",
     xlab = "Data",
     ylab = "Vazao")

grid(nx = NA, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)      # Grid line width


# ggplot2()
library(ggplot2)
library(ggthemes)

theme_set(tema_top)


tema_top <-
  theme_bw() +
  theme(plot.title = element_text(size = 16, face = 2, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 50, vjust = 0.5))

ggplot() +
  geom_line(data = dados_60435000,
            aes(x = Data, y = Vazao)) +
  labs(title = "Estação 60435000",
       x = "Datas", y = "Vazão")

ggplot() +
  geom_line(data = dados_todas,
            aes(x = Data, y = Vazao,
                col = Cod_estacao)) +
  labs(title = "Todas estações",
       x = "Datas", y = "Vazão")
  
resumo_todas$Cod_estacao <-
  as.factor(resumo_todas$Cod_estacao)

ggplot(data = resumo_todas,
       aes(x = Ano_hidro,
           y = media,
           col = Cod_estacao)) +
  geom_ribbon(aes(x = Ano_hidro,
                  fill = Cod_estacao,
                  ymax = maxima,
                  ymin = minima),
              alpha = 0.5) +
  geom_line() +
  geom_point() +
  labs(title = "Todas estações",
       x = "Datas", y = "Vazão") +
  facet_wrap(~ Cod_estacao,
             scales = "free_y",
             nrow = 4)
  





# Aula 5 - Análise de frequência de cheias ####
library(ggplot2)

resumo_todas <- read.table(file = "Resumo_estacoes.txt",
                           sep = "\t",
                           dec = ".",
                           header = T,
                           fileEncoding = "UTF-8")

resumo_60435000 <-
  resumo_todas[resumo_todas$Cod_estacao == 60435000,
               1:3]

# botar em ordem decrescente
resumo_60435000 <-
  resumo_60435000[order(resumo_60435000$maxima,
                        decreasing = TRUE),]

# valor de i
resumo_60435000$index <- 1:nrow(resumo_60435000)

# valor de i/n+1
resumo_60435000$weibull <-
  resumo_60435000$index/(nrow(resumo_60435000) + 1)

# calcular tempo de retorno
resumo_60435000$Tr <- 1/resumo_60435000$weibull


# gráfico ggplot2
ggplot(resumo_60435000,
       aes(x = Tr, y = maxima)) +
  geom_point(col = "blue") +
  scale_x_log10(breaks = c(1:10, seq(20, 100, 10))) +
  theme_bw()


# log das Vazoes
resumo_60435000$Y <- log(resumo_60435000$maxima)

# Cálculo de mu e sigma
mu_Y_hat <- mean(resumo_60435000$Y)
var_Y_hat <- var(resumo_60435000$Y)

# Calcular vários quantis p/ Y e Q
p <- seq(from = 0.01,
         to = 0.99,
         by = 0.01)

Tr <- 1/(1-p)

z_p <- qnorm(p, mean = 0, sd = 1)

Y_p_hat <- mu_Y_hat + z_p*sqrt(var_Y_hat)
Y_p_hat

q_p_hat <- exp(Y_p_hat)
q_p_hat

quantis_p <- data.frame(q_p_hat, Tr)

# gráfico ggplot2 com a linha de q_p_hat
ggplot(resumo_60435000,
       aes(x = Tr, y = maxima)) +
  geom_point(col = "blue") +
  geom_line(data = quantis_p,
            aes(x = Tr, y = q_p_hat),
            col = "red") +
  scale_x_log10(breaks = c(1:10, seq(20, 100, 10))) +
  theme_bw() +
  theme(panel.grid = element_line(linetype = "dashed"),
        panel.grid.minor.x = element_blank())


# incerteza associada a q_p_hat
alpha <- 0.05

var_Yp_hat <-
  var_Y_hat/nrow(resumo_60435000) * (1 + z_p^2/2)

IC_inferior <-
  exp((mu_Y_hat + z_p*sqrt(var_Y_hat)) -
        qnorm(1-alpha/2, mean = 0, sd = 1) * sqrt(var_Yp_hat))

IC_superior <-
  exp((mu_Y_hat + z_p*sqrt(var_Y_hat)) +
        qnorm(1-alpha/2) * sqrt(var_Yp_hat))

quantis_p$IC_inferior <- IC_inferior
quantis_p$IC_superior <- IC_superior


# gráfico ggplot2 com os ICs
ggplot(resumo_60435000,
       aes(x = Tr, y = maxima)) +
  geom_point(col = "blue") +
  geom_line(data = quantis_p,
            aes(x = Tr, y = q_p_hat),
            col = "red") +
  geom_line(data = quantis_p,
            aes(x = Tr, y = IC_inferior),
            col = "red", linetype = "dashed") +
  geom_line(data = quantis_p,
            aes(x = Tr, y = IC_superior),
            col = "red", linetype = "dashed") +
  scale_x_log10(breaks = c(1:10, seq(20, 100, 10))) +
  labs(x = "Tr [ano]", y = expression("Vazão ["~m^{3}~"/s]")) +
  theme_bw() +
  theme(panel.grid = element_line(linetype = "dashed"),
        panel.grid.minor.x = element_blank())




  

