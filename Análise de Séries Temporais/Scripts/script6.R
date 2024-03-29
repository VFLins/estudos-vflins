#########################################################################################
# An�lise de Invertibilidade
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
	IdentInvertQ(theta =.) #N�o invert�vel
# Testando invertibilidade de vale AR(4)
arima(dados$vale, order =c(4,0,0), method ="ML")$coef[-5] %>%
	IdentInvertQ(phi =.) #Invert�vel
	
### Testes para modelos sem diferencia��o
#vale
arima(dados$vale, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #N�o Invert�vel
#dolar
arima(dados$dolar, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #N�o Invert�vel
#ferroUS
arima(dados$ferroUS, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #N�o Invert�vel
#cobreUS
arima(dados$cobreUS, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #N�o Invert�vel
#exportBR
arima(dados$exportBR, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
#importBR
arima(dados$importBR, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
#usoPMetal
arima(dados$usoPMetal, order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #N�o Invert�vel

### Testes para modelos usando primeiras diferen�as
# vale
arima(diff(dados$vale), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
# dolar
arima(diff(dados$dolar), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
# ferroUS
arima(diff(dados$ferroUS), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
# cobreUS
arima(diff(dados$cobreUS), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
# exportBR
arima(diff(dados$exportBR), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
# importBR
arima(diff(dados$importBR), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel
#usoPMetal
arima(diff(dados$usoPMetal), order =c(0,0,2))$coef[-3] %>%
	IdentInvertQ(theta =.) #Invert�vel

# A invertibilidade das ra�zes est� relacionada com a estacionaridade dos dados.
# Todas as s�ries invert�veis passaram no teste de estacionaridade, mas n�o o contr�rio*
# *: Verificado apenas para os dados sem diferencia��o.

##############################
# Visualiza��es das simula��es
##############################

# sem diferencia��o
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
	
data.frame(datas =dados$datas, 'Vale' =vale.0d, 'D�lar' =dolar.0d, ferroUS =ferroUS.0d,
	cobreUS =cobreUS.0d, exportBR =exportBR.0d, importBR =importBR.0d,
	usoPMetal =usoPMetal.0d) %>% read.zoo %>%
	autoplot(.) + ggtitle("Simula��es para dados sem diferencia��o") + xlab("") +
		theme_bw() + theme(text =element_text(size =16))

# com primeira diferencia��o
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

data.frame(datas =dados$datas, 'Vale' =vale.1d, 'D�lar' =dolar.1d, ferroUS =ferroUS.1d,
	cobreUS =cobreUS.1d, exportBR =exportBR.1d, importBR =importBR.1d,
	usoPMetal =usoPMetal.1d) %>% read.zoo %>%
	autoplot(.) + ggtitle("Simula��es para dados com primeira diferencia��o") +
		xlab("") + theme_bw() + theme(text =element_text(size =16))

#FIM#####################################################################################
