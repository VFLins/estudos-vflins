#########################################################################################
# Análise de Estacionaridade
#########################################################################################

###Carregamento dos dados###

setwd("C:/Users/Debora/Documents/analise")
load("dados.RData")

library(forecast)
library(tseries)
library(fma)
library(ggplot2)
library(dplyr)

cores <- c("#000000", "#3363de", "#845aa1", "#b34747", "#c9833c", "#7E9438", "#2B8C77")

##############################
# Testes adf nos nossos dados
##############################
# k = lag usado
adf.test(dados$vale, alternative ="stationary", k =2)
# Estatística do teste: -0,19111; p-valor: >0,99

adf.test(dados$dolar, alternative ="stationary", k =2)
# Estatística do teste: -2,363; p-valor: 0,4258

adf.test(dados$cobreUS, alternative ="stationary", k =2)
# Estatística do teste: -1,74; p-valor: 0,6846

adf.test(dados$ferroUS, alternative ="stationary", k =2)
# Estatística do teste: -0,26823; p-valor: >0,99

adf.test(dados$exportBR, alternative ="stationary", k =2)
# Estatística do teste: -3,6971; p-valor: 0,02746

adf.test(dados$importBR, alternative ="stationary", k =2)
# Estatística do teste: -2,2091; p-valor: 0,4897

adf.test(dados$usoPMetal[1:121], alternative ="stationary", k =2)
# Estatística do teste: -3,3465; p-valor: 0,06681
#########################################################################################

##############################
# Testando adf nos dados da atividade 6 letra "b"
##############################
View(EuStockMarkets)
EuStockMarkets[,1] %>% ts() %>% adf.test()
# Estatística: -0,82073; p-value = 0,9598; não estacionario
EuStockMarkets[,1] %>% ts() %>% diff(., differences =1) %>% adf.test()
# Estatística: -9,9997; p-value = 0,01; estacionario
EuStockMarkets[,1] %>% ts() %>% diff(., differences =2) %>% adf.test()
# Estatística: -21,799; p-value = 0,01; estacionário

View(JohnsonJohnson)
adf.test(JohnsonJohnson)
# Estatística: 1,9321; p-value = 0.99, não estacionario
diff(JohnsonJohnson, differences = 1) %>% adf.test()
# Estatística: -3,9886; p-value = 0.01421, estacionario
diff(JohnsonJohnson, differences = 2) %>% adf.test()
# Estatística: -7,7067; p-value = 0.01, estacionario
#########################################################################################

##############################
# Avaliando estacionaridade dos dados da atividade 6 letra "a"
##############################

### Propriedade 1: deve oscilar em torno da média (AR < 0)
EuStockMarkets[,1] %>% ts() %>% arima(., c(1,0,0), method ="ML") %>% summary()
# AR1: 0,9999
EuStockMarkets[,1] %>% ts() %>% diff(., differences =1) %>%
	arima(., c(1,0,0), method ="ML") %>% summary()
# AR1: 0,0008
EuStockMarkets[,1] %>% ts() %>% diff(., differences =2) %>% 
	arima(., c(1,0,0), method ="ML") %>% summary()
# AR1: -0,4931
JohnsonJohnson %>% arima(., c(1,0,0), method ="ML") %>% summary()
# AR1: 0,9512
JohnsonJohnson %>% diff(., differences =1) %>%
	arima(., c(1,0,0), method ="ML") %>% summary()
# AR1: -0,6170
JohnsonJohnson %>% diff(., differences =2) %>% 
	arima(., c(1,0,0), method ="ML") %>% summary()
# AR1: -0,7684

### Propriedade 2: Deve ter variância constante ao longo do tempo (homoscedastico)
### Teste não paramétrico
EuStockMarkets[1:930,1] %>% var #58154,14
EuStockMarkets[931:1860,1] %>% var #1293668

EuStockMarkets[1:930,1] %>% diff(., differences =1) %>% var #313,1298
EuStockMarkets[931:1860,1] %>% diff(., differences =1) %>% var #1794,218

EuStockMarkets[1:930,1] %>% diff(., differences =2) %>% var #610,7263
EuStockMarkets[931:1860,1] %>% diff(., differences =2) %>% var #3601,171

JohnsonJohnson[1:42] %>% var # 0,5082632
JohnsonJohnson[43:84] %>% var # 13,13017

JohnsonJohnson[1:42] %>% diff(., differences =1) %>% var # 0,06869939
JohnsonJohnson[43:84] %>% diff(., differences =1) %>% var # 4,083032

JohnsonJohnson[1:42] %>% diff(., differences =2) %>% var # 0,1720846
JohnsonJohnson[43:84] %>% diff(., differences =2) %>% var # 12,48405

### Propriedade 3: Autocorrelação constante para cada lag
### Teste não paramétrico
a <- EuStockMarkets[1:930,1]
b <- EuStockMarkets[931:1860,1]
lag =1
cov(a[1:length(a)-lag], diff(a, lag =lag)) #-148,8548
cov(b[1:length(b)-lag], diff(b, lag =lag)) #987,9133

a <- EuStockMarkets[1:930,1] %>% diff(., differences =1)
b <- EuStockMarkets[931:1860,1] %>% diff(., differences =1)
lag =5
cov(a[1:c(length(a)-lag)], diff(a, lag =lag)) #-313,9441
cov(b[1:c(length(b)-lag)], diff(b, lag =lag)) #-1830,291

a <- EuStockMarkets[1:930,1] %>% diff(., differences =2)
b <- EuStockMarkets[931:1860,1] %>% diff(., differences =2)
lag =1
cov(a[1:c(length(a)-lag)], diff(a, lag =lag)) #-893,4534
cov(b[1:c(length(b)-lag)], diff(b, lag =lag)) #-5366,135

a <- JohnsonJohnson[1:42]
b <- JohnsonJohnson[43:84]
cov(a[1:c(length(a)-lag)], diff(a, lag =lag)) #0,01322402
cov(b[1:c(length(b)-lag)], diff(b, lag =lag)) #-2,157129

a <- JohnsonJohnson[1:42] %>% diff(., differences =1)
b <- JohnsonJohnson[43:84] %>% diff(., differences =1)
cov(a[1:c(length(a)-lag)], diff(a, lag =lag)) #-0,0821441
cov(b[1:c(length(b)-lag)], diff(b, lag =lag)) #-5,964632

a <- JohnsonJohnson[1:42] %>% diff(., differences =2)
b <- JohnsonJohnson[43:84] %>% diff(., differences =2)
cov(a[1:c(length(a)-lag)], diff(a, lag =lag)) #-0,2739674
cov(b[1:c(length(b)-lag)], diff(b, lag =lag)) #-21,30414
#FIM#####################################################################################
