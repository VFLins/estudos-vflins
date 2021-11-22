########################################################################################
# Usando modelos de forecast simples nos dados obtidos
########################################################################################

setwd("C:/Users/Debora/Documents/analise") #Pasta do Google Drive
load("dados.RData")

library(dplyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(forecast)

cores <- c("#000000", "#3363de", "#845aa1", "#b34747", "#c9833c")

# Meus modelos estão usando a variação nominal em vez dos dados brutos

##################
# Decompondo as séries
##################

stl(zoo(dados$vale, dados$datas), s.window ="periodic") %>%
  autoplot() + ggtitle("Decomposição VALE") + xlab("") +
  theme(text =element_text(size =18)) #Apresenta grande fator sazonal

stl(zoo(dados$dolar, dados$datas), s.window ="periodic") %>%
  autoplot() + ggtitle("Decomposição DÓLAR") + xlab("") +
    theme(text =element_text(size =18)) #Sazonal e tendência

### [Coeficientes de amortecimento, suavização e freq. da sazonalidade]

##################
# Suavização Exponencial Simples (SES)
##################

# Modelos com alpha otimizado
ses_vale <- ses(zoo(dados$vale[72:120], dados$datas[72:120]), h =10)
ses_dolar <- ses(zoo(dados$dolar[72:120], dados$datas[72:120]), h =10)

### Intervalos de confiança
list(vale =as.data.frame(ses_vale[5:6]), dolar =as.data.frame(ses_dolar[5:6]))

### Coeficientes AIC
summary(ses_vale)
summary(ses_dolar)

### Valores ajustados
list(vale =ses_vale$fitted, dolar =ses_dolar$fitted)

### Resíduos
list(vale = ses_vale$residuals, dolar = ses_dolar$residuals)

### Gráfico
# vale
autoplot(ses_vale, fcol =cores[3]) + theme_bw() +
  labs(title ="Forecast em SES - VALE", x ="", y ="") +
  theme(text =element_text(size =18))

# dolar
autoplot(ses_dolar, fcol =cores[3]) + theme_bw() + 
  labs(title ="Forecast em SES - DOLAR", x ="", y ="") +
  theme(text =element_text(size =18))

##################
# Suavização Exponencial de Holt
##################
##### Sem armotecimento

holt_vale <- holt(zoo(dados$vale[72:120], dados$datas[72:120]))
holt_dolar <- holt(zoo(dados$dolar[72:120], dados$datas[72:120])) 

### Intervalos de confiança

### Coeficientes AIC
summary(holt_vale)
summary(holt_dolar)

### Valores ajustados
list(vale =holt_vale$fitted, dolar =holt_dolar$fitted)

###residuos
list(vale = holt_vale$residuals, dolar = holt_dolar$residuals)

### Gráfico
autoplot(holt_vale, fcol =cores[4]) + theme_bw() + 
  labs(title ="Forecast Holt - VALE", x ="", y ="") +
  theme(text =element_text(size =18))

autoplot(holt_dolar, fcol =cores[4]) + theme_bw() + 
  labs(title ="Forecast Holt - DOLAR", x ="", y ="") +
  theme(text =element_text(size =18))

########### Com amortecimento 
X <- zoo(dados$vale[72:120], dados$datas[72:120])
holt_vale <- holt(X, initial = c("optimal"), damped = TRUE)

Y <- zoo(dados$dolar[72:120], dados$datas[72:120])
holt_dolar <- holt(Y, initial = c("optimal"), damped = TRUE)
### Intervalos de confiança

### Coeficientes AIC
summary(holt_vale)
summary(holt_dolar)

### Valores ajustados
list(vale =holt_vale$fitted, dolar =ses_dolar$fitted)

###
list(vale =holt_vale$residuals, dolar =ses_dolar$residuals)

### Gráfico
autoplot(holt_vale, fcol =cores[4]) + theme_bw() + 
  labs(title ="Forecast Holt - VALE", x ="", y ="") +
  theme(text =element_text(size =18))

autoplot(holt_dolar, fcol =cores[4]) + theme_bw() + 
  labs(title ="Forecast Holt - DOLAR", x ="", y ="") +
  theme(text =element_text(size =18))

##################
# Suavização Exponencial Holt-Winters
##################
##### Aditivo

window(zoo(dados$vale[72:120], dados$datas[72:120]), start =2016.75) %>%
  hw(., h =10, seasonal ="additive") -> hw_vale

window(zoo(dados$dolar[72:120], dados$datas[72:120]), start =2016.75) %>%
  hw(., h =10, seasonal ="additive") -> hw_dolar

### Intervalos de confiança

### Coeficientes AIC
summary(hw_vale)
summary(hw_dolar)

### Valores ajustados
list(vale =hw_vale$fitted, dolar =hw_dolar$fitted)

### residuals
list(vale =hw_vale$residuals, dolar =hw_dolar$residuals)

### Gráfico
# vale
autoplot(hw_vale, fcol =cores[5]) + theme_bw() + 
  labs(title ="Forecast Holt-Winters - VALE", x ="", y ="") +
  theme(text =element_text(size =18))

# dolar
autoplot(hw_dolar, fcol =cores[5]) + theme_bw() + 
  labs(title ="Forecast Holt-Winters - DOLAR", x ="", y ="") +
  theme(text =element_text(size =18))

##### Multiplicativo

window(zoo(dados$vale[72:120], dados$datas[72:120]), start =2016.75) %>%
  hw(., h =10, seasonal ="multiplicative") -> hw_vale

window(zoo(dados$dolar[72:120], dados$datas[72:120]), start =2016.75) %>%
  hw(., h =10, seasonal ="multiplicative") -> hw_dolar

###Intervalos de confiança e Coeficientes AIC
summary(hw_vale)
summary(hw_dolar)

### Valores ajustados
list(vale =hw_vale$fitted, dolar =hw_dolar$fitted)

### residuals
list(vale =hw_vale$residuals, dolar =hw_dolar$residuals)

### Gráfico
# vale
autoplot(hw_vale, fcol =cores[5]) + theme_bw() + 
  labs(title ="Forecast Holt-Winters - VALE", x ="", y ="") +
  theme(text =element_text(size =18))

# dolar
autoplot(hw_dolar, fcol =cores[5]) + theme_bw() + 
  labs(title ="Forecast Holt-Winters - DOLAR", x ="", y ="") +
  theme(text =element_text(size =18))

#EXPERIMENTAL###########################################################################
# Experimental, não rodar

mod_vale <- diff(dados$vale)/dados$vale[-1]
log_vale <- log(dados$vale)

# comparando dispersão dos dados em var% e log, respectivamente
data.frame(dados[-1, c("datas", "vale")], mod_vale, log_vale =log_vale[-1]) %>%
  melt(., id ="datas") %>%
ggplot(., aes(x =datas, y =value)) + theme_bw() + labs(x ="") + geom_line() +
  facet_wrap(variable~., nrow=3, scales ="free")

# comparando encaixe das previsões SES
ses_modvale <- ses(zoo(mod_vale, dados$datas[-1]), h=24)
ses_logvale <-ses(zoo(log_vale[-1], dados$datas[-1]), h=24)
ses_vale <- ses(zoo(dados$vale[-1], dados$datas[-1]), h =24)
# usando a soma dos quadrados dos resíduos como parâmetro
sum(ses_vale$residuals**2)
sum(ses_modvale$residuals**2)
sum(ses_logvale$residuals**2)

hist(ses_vale$residuals)

#EXPERIMENTAL###########################################################################


#parte 2
########################################################################################
# Usando modelos de forecast simples nos dados obtidos
########################################################################################

setwd("C:/Users/Debora/Documents/analise")
load("dados.RData")
X <- dados$vale
Y <- dados$dolar

library(dplyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(forecast)

################################################################################
###Estimação Suavisacao exponencial simples

#####Vale
previsao1 <- ses(X, h = 12) #alpha otimizado
plot(previsao1, main = "Ações da Vale")

previsao1 <- ses(X, alpha = 0.01, h = 12)
plot(previsao1)

previsao1 <- ses(X, alpha = 0.8, h = 12)
plot(previsao1)

previsao1$fitted
previsao1$model
previsao1$residuals

summary(previsao1)

#####Dolar
previsao2 <- ses(Y, h = 12) #alpha otimizado
plot(previsao2, main = "Dolar")

previsao2 <- ses(Y, alpha = 0.1, h = 12)
plot(previsao2)

previsao2 <- ses(Y, alpha = 0.8, h = 12)
plot(previsao2)

previsao2$fitted
previsao2$model
previsao2$residuals

summary(previsao2)

##################################################################################
###Estimação Suavização Exponencial de Holt

#####Vale 
#sem amortecimento
previsao1 <- holt(X, h = 12) #alpha e beta otimizados
plot(previsao2, main = "Ações da Vale")

previsao1 <- holt(X, alpha = 0.99, beta = 0.09, initial = c("simple"))
previsao1$model
previsao1$fitted
previsao1$residuals
plot(previsao1)

summary(previsao1)

#com amortecimento
previsao2 <- holt(X, h = 12)
plot(previsao2, main = "Ações da Vale")

previsao2 <- holt(Y, damped = TRUE, h = 12)
plot(previsao2, main = "Ações da Vale")

previsao2 <- holt(Y, alpha = 0.8, beta = 0.2, initial = c("optimal"), phi = 0.80,
    damped = TRUE, h = 12)
previsao2 <- holt(Y, initial = c("optimal"), phi = 0.98, damped = TRUE, h = 12)
plot(previsao2, main = "Ações da Vale")

summary(previsao2)

##### Dolar
##
#sem amortecimento

previsao3 <- holt(Y, h = 12) #alpha e beta otimizados
plot(previsao3, main = "Dolar")

previsao3 <- holt(Y, alpha = 0.95, beta = 0.01, initial = c("simple"))

previsao3$model
previsao3$fitted
previsao3$residuals
plot(previsao3)

summary(previsao3)

##
#com amortecimento

previsao4 <- holt(Y, damped = TRUE, h = 12)
plot(previsao4, main = "Dolar")

previsao4 <- holt(Y, alpha = 0.83, beta = 0.2, initial = c("optimal"),
    phi = 0.8, damped = TRUE, h = 12)
previsao <- holt(Y, initial = c("optimal"), phi = 0.98, damped = TRUE, h = 12)
plot(previsao4, main = "Dolar")

summary(previsao4)

####################################################################################
###Estimacao Suavização Exponencial Holt-Winters
##### Vale
x <- ts(X, frequency = 60, start = 1) # Criando um objeto ts contendo o mesmo 
#                                       conjunto de dados dividido em uma 
#                                       frequencia de 60 meses (foi a unica 
#                                       frequencia que identifiquei)
previsao1 <- hw(x, h= 12)
plot(previsao1, main = "Ações da vale")
summary(previsao1)

###
#Aditiva
previsao2 <- hw(x, alpha = 0.9, beta = 0.15, gamma = 0.001, initial = c("optimal"),
    seasonal = c("additive"), phi = 0.80, damped = TRUE, h = 12)
plot(previsao2, main = "Ações da vale")
summary(previsao2)

###
#Multiplicativa
previsao3 <- hw(x, alpha = 0.9, beta = 0.15, gamma = 0.001, initial = c("optimal"),
    seasonal = c("multiplicative"), phi = 0.80, damped = TRUE, h = 12)
plot(previsao3, main = "Ações da vale")
summary(previsao3)

##### Dolar
y <- ts(Y, frequency = 60, start = 1) # Criando um objeto ts contendo o mesmo 
#                                       conjunto de dados dividido em uma 
#                                       frequencia de 60 meses (foi a unica 
#                                       frequencia que identifiquei)
previsao1 <- hw(y, h= 12)
plot(previsao1, main = "Dolar")
summary(previsao1)

###
#Aditiva
previsao2 <- hw(y, alpha = 0.9, beta = 0.15, gamma = 0.001, initial = c("optimal"),
    seasonal = c("additive"), phi = 0.80, damped = TRUE, h = 12)
plot(previsao2, main = "Dolar")
summary(previsao2)

###
#Multiplicativa
previsao3 <- hw(y, alpha = 0.9, beta = 0.15, gamma = 0.001, initial = c("optimal"),
    seasonal = c("multiplicative"), phi = 0.80, damped = TRUE, h = 12)
plot(previsao3, main = "Dolar")
summary(previsao3)













