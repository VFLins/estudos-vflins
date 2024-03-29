#########################################################################################
# Elegendo e executando um modelo de previs�o auto regressivo
#########################################################################################

###Carregamento dos dados###

setwd("C:/Users/Debora/Documents/analise")
load("dados.RData")

library(forecast)
library(urca)
library(ggplot2)
library(reshape2)
library(zoo)
library(dplyr)

cores <- c("#000000", "#3363de", "#845aa1", "#b34747", "#c9833c", "#7E9438", "#2B8C77")

####################
# Adaptando dados aos modelos simples
####################

stl(zoo(diff(dados$exportBR, 12), dados$datas), s.window ="periodic") %>% autoplot() 

##### Ser�o utilizados dados em car�ter estacion�rio

### Modificando dados de exporta��o
# dados crus
dados$exportBR %>% as.ts() %>%
	autoplot(., facets =TRUE) + theme_bw() + theme(text =element_text(size =18))
	
# diferenciamento de 1ro grau em diferenciamento sazonal
dados$exportBR %>% diff(, 12) %>% diff() %>% as.ts() %>%
	autoplot(., facets =TRUE) + theme_bw() + theme(text =element_text(size =18))

# diferenciamento de 1ro grau
dados$exportBR %>% diff() %>% as.ts() %>%
	autoplot(., facets =TRUE) + theme_bw() + theme(text =element_text(size =18))
	
# diferenciamento de 2do grau
dados$exportBR %>% diff() %>% diff() %>% as.ts() %>%
	autoplot(., facets =TRUE) + theme_bw() + theme(text =element_text(size =18))

### Testando a estacionaridade dos dados
dados$exportBR %>% ur.kpss() %>% summary()
#Valor cr�tico: 0,5195 com lag 4. Aprovado com 52% de signific�ncia
dados$exportBR %>% diff(, 12) %>% diff() %>% ur.kpss() %>% summary()
# Valor cr�tico: 0,0889 com lag 4. Aprovado com 9% de signific�ncia
dados$exportBR %>% diff() %>% ur.kpss() %>% summary()
# Valor cr�tico: 0,0282 com lag 4. Aprovado com 3% de signific�ncia
dados$exportBR %>% diff() %>% diff() %>% ur.kpss() %>% summary()
# Valor cr�tico: 0,0420 com lag 4. Aprovado com 5% de signific�ncia
# Ser�o usados dados das exporta��es com diferenciamento de 1ro grau
#########################################################################################

####################
# Testando modelos
####################

### Correlogramas (ACF, PCF) ###
dados$exportBR %>% diff() %>% acf()
dados$exportBR %>% diff() %>% pacf()

a_vale <- acf(dados$vale, lag.max =16, main ="Fun��o de Autocorrela��o Vale")
pa_vale <- pacf(dados$vale, lag.max = 8, main = "Fun��o de Autocorrela��o Parcial Vale")
a_vale$acf
pa_vale$acf

### Estima��o Par�metro Autoregressivo e Simula��es ###
# testando modelos ARIMA, com dados de teste [1:97] e de avalia��o [98:121]
# Usando valores coerentes com o PACF

test <- diff(dados$exportBR)[1:97]
eval <- diff(dados$exportBR)[98:121]

diff(dados$exportBR) %>% auto.arima() # order =c(1, 0, 1) min. AICc

test %>% Arima(., c(4, 0, 4)) %>% forecast(., h =10) -> arimat_exBR
diff(dados$exportBR) %>% Arima(., c(4, 0, 4)) %>% forecast(., h =10) -> arima_exBR
summary(arima_exBR) #4,0,4 min. AIC, res�duos e erro de previs�o

autoplot(arima_exBR) + theme_bw() + theme(text =element_text(size =16))

# testando modelo de M�dia
test %>% as.ts() %>% meanf(., h =24) -> mean_exBR
summary(mean_exBR)
autoplot(mean_exBR) + theme_bw() + theme(text =element_text(size =16))

# AR(1): AIC=1769.72   AICc=1769.98   BIC=1777.44
# AR(2): AIC=1769.23   AICc=1769.66   BIC=1779.52
# AR(3): AIC=1769.32   AICc=1769.98   BIC=1782.2
#*AR(4): AIC=1767.57   AICc=1768.51   BIC=1783.02
# ARIMA(1,0,1): AIC=1765.63   AICc=1766.07   BIC=1775.93
#*ARIMA(4,0,4): AIC=1763.04   AICc=1765.6   BIC=1788.79

##### AR(4) e ARIMA(4,0,4) s�o os melhores modelos que minimizam AIC e AICc
#########################################################################################

ar_vale <- arima(dados$vale, c(1,0,0), method = "ML")

ar_vale <- arima(dados$vale, c(2,0,0), method = "ML")
	#n quer rodar, especifiquei para [4:122] e rodou
	#mas n entendi o pq. vc consegue resolver?

ar_vale <- arima(dados$vale, c(3,0,0), method = "ML")

ar_vale %>% forecast(h =10) %>% plot() # visualiza��o do modelo

ar_vale
ar_vale$var.coef
ar_vale$coef
ar_vale$sigma2
ar_vale$residual

#########################################################################################
####################
# Simula��es
####################

sim1 <- arima.sim(n =122, list(order =c(1,0,1),
	ar = 0,5202,
	ma =  -0,8850))
sim2 <- arima.sim(n =122, list(order = c(4,0,4),
	ar =c(-0.1865, -0.3323, -0.1371, 0.4128),
	ma = c(-0.1970, 0.1702, 0.1433, -0.8319)))
sim3 <- arima.sim(n =122, list(order = c(4,0,4),
	ar =c(-0.12, -0.28,  -0.08, -0.37),
	ma = c(-1.4574,  -0.0075,  1.1498,  -0.486)))
sim4 <- arima.sim(n =122, list(order = c(4,0,4),
	ar =c(-0.07, -0.23,  -0.03, -0.32),
	ma = c(-1.4574,  -0.0075,  1.1498,  -0.486)))
sim5 <- arima.sim(n =122, list(order = c(4,0,4),
	ar =c(-0.2, 0.18,  -0.1, -0.27),
	ma = c(-1.4574,  -0.0075,  1.1498,  -0.486)))

sim <- data.frame(datas =dados$datas,
  Original =c(0, diff(dados$exportBR)),
	Sim1 =sim1,
	Sim2 =sim2,
	Sim3 =sim3,
	Sim4 =sim4,
	Sim5 =sim5)
simlong <- melt(sim, id="datas")
	
ggplot(simlong, aes(x =datas, y =value,  group =variable, color =variable)) +
	geom_line(size =1) + scale_color_manual(values =cores) +theme_bw() + 
	theme(text =element_text(size =16), legend.position = "none") +
	facet_wrap(~variable, nrow =7, scales ="free") + labs(x ="", y ="")

acf(sim1)
acf(sim2)
acf(sim3)
pacf(sim1)
pacf(sim2)
pacf(sim3)

#########################################################################################
##### Anota��es // T�cnicas de tratamento de dados para ajustar aos modelos
### DIFERENCIAMENTO: modifica a varia��o dos dados (y_1 - y_0) ao inv�s dos dados em si,
### pode ser usado qualquer m�todo de previs�o estacion�ria (sem tendencia ou saz.)
# dados estacion�rios se parecem com:
plot(rnorm(100), type ="l")
# diferenciamento nas exporta��es do brasil
plot(diff(dados$exportBR), type ="l")

### DIFERENCIAMENTO DE SEGUNDO GRAU: mesma id�ia do anterior, mas medindo a diferen�a das
### diferen�as
# diferenciamento de segundo grau nas exporta��es
plot(diff(diff(dados$exportBR)), type ="l")

### DIFERENCIAMENTO SAZONAL: compara o per�odo atual com o mesmo da temporada passada
# diferenciamento sazonal nas exporta��es, m�s do ano atual vs mesmo m�s do ano passado
plot(diff(dados$exportBR, 12), type ="l")

#########################################################################################
##### Anota��es // Modelos autoregressivos b�sicos
### random walk, rwf(): prev� que a pr�xima observa��o � igual a um ru�do branco
# exemplo
diff(dados$exportBR, 12) %>% as.ts() %>% rwf() %>% plot()

### mean, meanf(): prev� que a proxima observa��o ser� a m�dia das observa��es anteriores
# exemplo
dados$exportBR %>% as.ts() %>% meanf() %>% plot()

### auto regressivo, arima(order =c(p,0,0)): funciona como uma regress�o, mas todas as
### vari�veis indepen dentes s�o a vari�vel dependente com ordens diferentes (at� p) de
### diferenciamento
# exemplo
dados$exportBR %>% as.ts() %>% Arima(., c(10,0,0)) %>% forecast(h =10) %>% plot()

### m�dia m�vel, arima(order =c(0,0,q)): tamb�m funciona como uma regress�o, mas as
### vari�veis independentes s�o os res�duos para cada lag (at� q) da vari�vel dependente
# exemplo
dados$exportBR %>% as.ts() %>% Arima(., c(0,0,10)) %>% forecast(h =10) %>% plot()

### ARIMA, arima(p, r, q): integra��o entre m�dia m�vel e auto regressivo, inclui
### intera��es entre AR e MA (at� r), modelo b�sico exige dados estacion�rios
# exemplo
diff(dados$exportBR) %>% as.ts() %>% Arima(., c(10,2,10)) %>% forecast(h =10) %>% plot()

#########################################################################################
