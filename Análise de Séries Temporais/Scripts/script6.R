#########################################################################################
# Análise de Invertibilidade
#########################################################################################

###Carregamento dos dados###
setwd("C:/Users/Debora/Documents/analise")
load("dados.RData")

library(forecast)
library(tseries)
library(arfima)
library(ggplot2)
library(zoo)
library(dplyr)

cores <- c("#000000", "#3363de", "#845aa1", "#b34747", "#c9833c", "#7E9438", "#2B8C77")

##############################
# Investigando invertibilidade de coeficientes
# MA(2) para todos os dados do projeto
##############################

### Exemplos:
# Testando invertibilidade de vale MA(4)
arima(dados$vale, order =c(0,0,4))$coef[-5] %>%
	IdentInvertQ(theta =.) #Não invertível
# Testando invertibilidade de vale AR(4)
arima(dados$vale, order =c(4,0,0), method ="ML")$coef[-5] %>%
	IdentInvertQ(phi =.) #Invertível
	
### Testes para modelos sem diferenciação
#vale
arima(dados$vale, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Não Invertível
#dolar
arima(dados$dolar, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Não Invertível
#ferroUS
arima(dados$ferroUS, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Não Invertível
#cobreUS
arima(dados$cobreUS, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Não Invertível
#exportBR
arima(dados$exportBR, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
#importBR
arima(dados$importBR, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
#usoPMetal
arima(dados$usoPMetal, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Não Invertível

### Testes para modelos usando primeiras diferenças
# vale
arima(diff(dados$vale), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
# dolar
arima(diff(dados$dolar), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
# ferroUS
arima(diff(dados$ferroUS), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
# cobreUS
arima(diff(dados$cobreUS), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
# exportBR
arima(diff(dados$exportBR), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
# importBR
arima(diff(dados$importBR), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível
#usoPMetal
arima(diff(dados$usoPMetal), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invertível

# A invertibilidade das raízes está relacionada com a estacionaridade dos dados.
# Todas as séries invertíveis passaram no teste de estacionaridade, mas não o contrário*
# *: Verificado apenas para os dados sem diferenciação.

##############################
# Visualizações das simulações
##############################

# sem diferenciação
vale.0d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(dados$vale, order =c(0,0,2))$coef[-3]))
dolar.0d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(dados$dolar, order =c(0,0,2))$coef[-3]))
ferroUS.0d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(dados$ferroUS, order =c(0,0,2))$coef[-3]))
cobreUS.0d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(dados$cobreUS, order =c(0,0,2))$coef[-3]))
exportBR.0d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(dados$exportBR, order =c(0,0,2))$coef[-3]))
importBR.0d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(dados$importBR, order =c(0,0,2))$coef[-3]))
usoPMetal.0d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(dados$usoPMetal, order =c(0,0,2))$coef[-3]))
	
data.frame(datas =dados$datas, 'Vale' =vale.0d, 'Dólar' =dolar.0d, ferroUS =ferroUS.0d,
	cobreUS =cobreUS.0d, exportBR =exportBR.0d, importBR =importBR.0d,
	usoPMetal =usoPMetal.0d) %>% read.zoo %>%
	autoplot(.) + ggtitle("Simulações para dados sem diferenciação") + xlab("") +
		theme_bw() + theme(text =element_text(size =16))

# com primeira diferenciação
vale.1d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(diff(dados$vale), order =c(0,0,2))$coef[-3]))
dolar.1d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(diff(dados$dolar), order =c(0,0,2))$coef[-3]))
ferroUS.1d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(diff(dados$ferroUS), order =c(0,0,2))$coef[-3]))
cobreUS.1d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(diff(dados$cobreUS), order =c(0,0,2))$coef[-3]))
exportBR.1d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(diff(dados$exportBR), order =c(0,0,2))$coef[-3]))
importBR.1d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(diff(dados$importBR), order =c(0,0,2))$coef[-3]))
usoPMetal.1d <- arima.sim(n =122, list(order =c(0,0,2),
	ma =  arima(diff(dados$usoPMetal), order =c(0,0,2))$coef[-3]))

data.frame(datas =dados$datas, 'Vale' =vale.1d, 'Dólar' =dolar.1d, ferroUS =ferroUS.1d,
	cobreUS =cobreUS.1d, exportBR =exportBR.1d, importBR =importBR.1d,
	usoPMetal =usoPMetal.1d) %>% read.zoo %>%
	autoplot(.) + ggtitle("Simulações para dados com primeira diferenciação") +
		xlab("") + theme_bw() + theme(text =element_text(size =16))

#FIM#####################################################################################
