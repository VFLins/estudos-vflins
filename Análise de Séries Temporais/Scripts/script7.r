##########################################################################################
# Identificação de Modelos ARMA
##########################################################################################
setwd("")
load("dados.RData")

library(zoo)
library(tseries)
library(dplyr)
library(arfima)
library(ggplot2)
library(reshape2)

cores <- c("#000000", "#3363de", "#845aa1","#b33d8d", "#b34747", "#c9833c", 
	"#9c9248", "#7E9438", "#2D6E14")
####################
# Análises de autocorrelação
####################

### Visualizações
# Dados (gráfico 2)
plot.dados <-
melt(dados[-122,], id ="datas") %>% 
ggplot(., aes(y= value, x =datas, group =variable, color =variable)) +
	geom_line(size =1) +
	theme_bw() +
	facet_wrap(.~variable, scales ="free_y", ncol =2) +
	scale_color_manual(values =cores[-1]) +
	theme(text =element_text(size =13),
		legend.position="none",
		strip.background =element_blank(),
		strip.text.x =element_text(size =15, face ="bold")) +
	labs(x ="", y ="", color ="Variável")
	
plot.dados # para exibir os dados

# ACF (gráfico 3), executar as duas linhas para ver
par(mfrow =c(4,2))
for(i in 2:9) {dados[-122,i] %>% acf(main =names(dados)[i])}

# PACF (gráfico 4), executar as duas linhas para ver
par(mfrow =c(4,2))
for(i in 2:9) {dados[-122,i] %>% pacf(main =names(dados)[i])}

### Simulações (gráfico 5)
ferroUS.sim <- arima.sim(n =122, list(order =c(1,0,0),
	ar =  arima(dados$ferroUS, order =c(1,0,0))$coef[-2]))
var.dolar.sim <- arima.sim(n =122, list(order =c(0,0,1),
	ma =  arima(dados$var.dolar, order =c(0,0,1))$coef[-2]))
importBR.sim <- arima.sim(n =122, list(order =c(3,0,0),
	ar =  arima(dados$importBR, order =c(3,0,0))$coef[-4]))

plot.sim <-
data.frame(datas =dados$datas,
	ferroUS =dados$ferroUS,
	Sim_ferroUS =ferroUS.sim,
	var.dolar =dados$var.dolar,
	Sim_var.dolar =var.dolar.sim,
	importBR =dados$importBR,
	Sim_importBR =importBR.sim
	) %>% melt(., id ="datas") %>%
ggplot(., aes(x =datas, y =value, group =variable, color =variable)) +
	geom_line(size =1) +
	theme_bw() +
	facet_wrap(.~variable, scales ="free_y", ncol =2) +
	scale_color_manual(values =cores[c(2,2,6,6,8,8)]) +
	theme(text =element_text(size =13),
		legend.position="none",
		strip.background =element_blank(),
		strip.text.x =element_text(size =15, face ="bold")) +
	labs(x ="", y ="", color ="Variável")
	
plot.sim # para exibir as simulações

#FIM######################################################################################
