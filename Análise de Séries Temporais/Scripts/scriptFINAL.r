##########################################################################################
# CALCULOS PARA O PROJETO FINAL
###################################
setwd("") #Inserir diretorio aqui
load("dados.RData")

library(dplyr)
library(reshape2)
library(zoo)
library(ggplot2)
library(forecast)
library(lmtest)
library(sandwich)
library(tseries)

cores <- c("#000000", "#3C85EE", "#85EE3C", "#EE3C85")

theme_bgm <- theme_minimal() + theme(text =element_text(size =18), 
	legend.position ="bottom", plot.title = element_text(hjust = 0.5))
	
ts.dados <- read.zoo(dados)

# MODELO LINEAR
#########################
# Estimando com dados de treino
lm0 <- lm(lead(log(vale)) ~0 + log(dolar) + log(exportBR) + log(ferroUS) +
	log(usoPMetal), dados[1:98,])
lm1 <- lm(lead(log(vale)) ~0 + log(dolar) + log(exportBR) + log(ferroUS) +
	log(usoPMetal) + log(vale), dados[1:98,])
lm2 <- lm(log(vale) ~ 0 + lag(log(vale)), dados[1:98,])

#teste BP para homocedasticidade
bptest(lm0)
bptest(lm1)
bptest(lm2)

#significancia robusta
coeftest(lm0, vcov=vcovHC(lm0, "HC"))

# AUTO REGRESSIVO
#########################
# Últimas 24 obs. deixadas para avaliar a previsão
# Mais simples

arima110 <- ts.dados[1:98, "vale"] %>% arima(., order =c(1,1,0))
arima111 <- ts.dados[1:98, "vale"] %>% arima(., order =c(1,1,1))
# Usando PACF lag 6
arima610 <- ts.dados[1:98, "vale"] %>% arima(., order =c(6,1,0))
arima611 <- ts.dados[1:98, "vale"] %>% arima(., order =c(6,1,1))

## Correlação dos erros (Ljung-Box)
# h0: não correlação
# Lag escolhido é o mais distante de 0, observado no PACF
Box.test(arima110$residuals, lag =6, type ="Ljung-Box") 
Box.test(arima111$residuals, lag =6, type ="Ljung-Box")
Box.test(arima610$residuals, lag =6, type ="Ljung-Box")
Box.test(arima611$residuals, lag =20, type ="Ljung-Box") # todos os testes aprovaram

## Tabela de Coeficientes
col <- c("ARIMA (1,1,0)", "ARIMA (1,1,1)", "ARIMA (6,1,0)", "ARIMA (6,1,1)")
coefc <- matrix(c(
	c(arima110$coef, rep(0,6)), 
	c(arima111$coef[1], rep(0,5), arima111$coef[2]),
	c(arima610$coef, rep(0,1)),
	arima611$coef), ncol =4)
colnames(coefc) <- col
rownames(coefc) <- names(arima611$coef)

## Tabela das estatísticas AIC e BIC
lin <- c("AIC", "BIC")
statc <- matrix(c(AIC(arima110, arima111, arima610, arima611)[,2],
	BIC(arima110, arima111, arima610, arima611)[,2]), ncol =4, byrow =T)
colnames(statc) <- col
rownames(statc) <- lin

# COMPARANDO TODOS OS MODELOS PELOS ERROS DE PREVISÃO
#########################
## Avaliando erro de previsão dos modelos lineares
# Método MAPE, fórmula: mean(|(100*(yo-ye))/yo|)
yo <- log(dados$vale[99:122])
ye0 <- predict(lm0, dados[99:122,])
ye1 <- predict(lm1, dados[99:122,])
ye2 <- predict(lm2, dados[99:122,])

#lm0, MAPE = 18,4437
mean(abs((100*(yo-ye0))/yo))
#lm1, MAPE = 1,910892
mean(abs((100*(yo-ye1))/yo))
#lm2, MAPE = 0,01662422
mean(abs((100*(yo-ye2))/yo), na.rm=T)

## Avaliando previsão de modelos Arima
#arima 110, MAPE =11,7945
accuracy(forecast(arima110, 24), window(ts.dados$vale, start=dados$data[99]))
#arima 111, MAPE =11,78193
accuracy(forecast(arima111, 24), window(ts.dados$vale, start=dados$data[99]))
#arima 610, MAPE =11,63822
accuracy(forecast(arima610, 24), window(ts.dados$vale, start=dados$data[99]))
#arima 610, MAPE =13,94996
accuracy(forecast(arima611, 24), window(ts.dados$vale, start=dados$data[99]))

# Visualização dos dados sem diferenciação
ggplot(dadoslong, aes(group =variable, x =datas, y= value))+ geom_line(size =1.2) + 
facet_wrap(.~variable, ncol =2, scales= "free_y") + theme_minimal() + labs(x ="", y ="") +
theme(text =element_text(size =18))

# VISUALIZAÇÕES PARA MODELO LINEAR
#########################
# Ajustes
tibble(
	datas =dados$datas[-1],
	Vale =log(dados$vale)[-1],
	'Modelo 1' =lm0$fitted,
	'Modelo 2' =lm1$fitted,
	'Modelo 3' =lm2$fitted) %>% read.zoo %>%
autoplot(., facets =NULL) + geom_line(size =1.5) + theme_minimal() + 
theme(text =element_text(size =18), legend.position ="bottom") +
labs(x ="", y= "", color ="") + scale_color_manual(values =c("darkgrey",cores[-1])) 

# Erros padronizados
data.frame(
	y =c(rstandard(lm0), rstandard(lm1), rstandard(lm2)),
	x =rep(seq(1, 121), 3),
	names =c(rep("Modelo Linear 1", 121), rep("Modelo Linear 2", 121),
		rep("Modelo Linear 3", 121))) %>%
ggplot(., aes(y =y, x =x, group =names, color =names)) + geom_line(size =0.6) +
geom_point(size =3) + geom_hline(yintercept = 0, size =1) + theme_bgm + 
labs(x ="Índice", y ="Erros Padronizados", color ="") + 
scale_color_manual(values =cores[-1]) +
facet_wrap(.~names, scales ="free_x", ncol =3)

# Avaliando previsão
data.frame(datas =dados$datas, vale =log(dados$vale)) %>% read.zoo %>%
autoplot() + theme_bgm + geom_line(color ="darkgrey", size =1.2) + xlab("") + ylab("") +
autolayer(as.ts(zoo(predict(lm0, dados[99:122,]), dados$datas[99:122])),
	series ="Modelo 1", size =1.5) +
autolayer(as.ts(zoo(predict(lm1, dados[99:122,]), dados$datas[99:122])),
	series ="Modelo 2", size =1.5) +
autolayer(as.ts(zoo(predict(lm2, dados[99:122,]), dados$datas[99:122])), 
	series ="Modelo 3",size =1.5) +
scale_color_manual(values = cores[c(2,3,4,1)])

# Distribuição dos resíduos
data.frame(
	x =c(lm0$residuals, lm1$residuals, lm2$residuals),
	names =c(rep("Modelo Linear 1", 121), rep("Modelo Linear 2", 121),
		rep("Modelo Linear 3", 121))) %>%
ggplot(., aes(group =names, x =x, y =..density..)) +
geom_histogram(aes(x =x), bins =12, fill ="darkgrey", color ="black", size =1) +
geom_density(aes(x =x, color =names), size =1.5) + 
geom_vline(xintercept =0, size =2, linetype ="dashed") +
scale_color_manual(values = cores[-1]) +
facet_wrap(.~names, ncol =3) + labs(x ="", y ="Densidade") + theme_bgm

# VISUALIZAÇÕES PARA AUTO REGRESSIVO
#########################
# Erros padronizados
data.frame(
	y =c(arima110$residuals,arima111$residuals,arima610$residuals,arima611$residuals),
	x =rep(seq(1, 122), 4),
	names =c(rep(col[1], 122), rep(col[2], 122), rep(col[3], 122), rep(col[4], 122))) %>%
ggplot(., aes(y =y, x =x, group =names, color =names)) + geom_line(size =0.6) +
geom_point(size =3) + geom_hline(yintercept = 0, size =1) + theme_bgm + 
labs(x ="Índice", y ="Resíduos", color ="") +
facet_wrap(.~names, scales ="free_x", ncol =2) +
scale_color_manual(values =cores)

## Gráficos de ajuste dos modelos
#Ajuste dos modelos com MA
tibble(
	datas =dados$datas,
	Vale =c(NA, diff(dados$vale)),
	'Arima (1,1,1)' =c(NA, diff(dados$vale)) + arima111$residuals,
	'Arima (6,1,1)' =c(NA, diff(dados$vale)) + arima611$residuals,
	'Arima Log (0,2,1)' = ) %>% read.zoo %>%
autoplot(., facets =NULL) + geom_line(size =1.5) + theme_bgm + 
labs(x ="", y= "", color ="") + scale_color_manual(values =c("darkgrey", cores[c(2,4)]))

#Ajuste do modelo log com MA
tibble(
	datas =dados$datas,
	Vale =log(dados$vale),
	'Arima Log (0,2,1)' = log(dados$vale) +arimalog021$residuals) %>% read.zoo %>%
autoplot(., facets =NULL) + geom_line(size =1.5) + theme_bgm + 
labs(x ="", y= "", color ="") + scale_color_manual(values =c("darkgrey", cores[c(2,4)]))

#Ajuste dos modelos sem MA
tibble(
	datas =dados$datas,
	Vale =c(NA, diff(dados$vale)),
	'Arima (1,1,0)' =c(NA, diff(dados$vale)) + arima110$residuals,
	'Arima (6,1,0)' =c(NA, diff(dados$vale)) + arima610$residuals) %>% read.zoo %>%
autoplot(., facets =NULL) + geom_line(size =1.5) + theme_bgm + 
labs(x ="", y= "", color ="") + scale_color_manual(values =c("darkgrey", cores[c(1,3)]))

## Gráficos de previsão
dados[, c("datas", "vale")] %>% read.zoo %>%
autoplot() + geom_line(size =1.2, color ="darkgrey") + theme_bgm + xlab("") + ylab("")+
autolayer(forecast(arima110, h =24), series ="Arima (1,1,0)", size =1.2, PI =F) +
autolayer(forecast(arima111, h =24), series ="Arima (1,1,1)", size =1.2, PI =F) +
autolayer(forecast(arima610, h =24), series ="Arima (6,1,0)", size =1.2, PI =F) +
autolayer(forecast(arima611, h =24), series ="Arima (6,1,1)", size =1.2, PI =F) +
scale_color_manual(values =c("brown", cores[-1]))

## gráfico de raízes unitárias
#Modelo 110
autoplot(arima110) + theme_bgm + geom_point(size =5, color =cores[1]) + ggtitle(col[1])
#Modelo 111
autoplot(arima111) + theme_bgm + geom_point(size =6, color =cores[2]) + ggtitle(col[2])
#Modelo 610
autoplot(arima610) + theme_bgm + geom_point(size =6, color =cores[3]) + ggtitle(col[3])
#Modelo 611
autoplot(arima611) + theme_bgm + geom_point(size =6, color =cores[4]) + ggtitle(col[4])

#FIM######################################################################################
