########################################################################################
### Capturando dados do servidor do IPEA e salvos do Investing.com
########################################################################################

setwd("C:/Users/vflin/Documents/ECONOMIA/An�lise de S�ries Temporais")

library(zoo)
library(dplyr)
library(ggplot2)
library(GGally)
library(ipeadatar)
library(quantmod)
library(reshape2)


#### Periodicidade mensal para todas as vari�veis 
#### Dados de nov-2010 at� dez-2020

#### Vari�veis sugeridas:

# Futuros de Min�rio de Ferro Negociado nos EUA
# Come�ou a ser negociado em 14-out-2010, dados a partir de nov-2010
ferro.us <- read.csv("FerroFutUS(PIOc1) M1 112010-2020.csv")
ferro.us <- ferro.us[rev(2:nrow(ferro.us)), "fechamento"]

# Futuros de Min�rio de Cobre Negociado nos EUA
cobre.us <- read.csv("CobreFutUS(HG) M1 112010-2020.csv")
cobre.us <- cobre.us[rev(1:nrow(cobre.us)), "fechamento"]

# Pre�os das a��es da VALE na bolsa brasileira
vale <- read.csv("VALE3 M1 112010-2020.csv")
vale <- vale[rev(1:nrow(vale)), "fechamento"]

# Dados da cota��o do d�lar frente ao real
# Respectivamente, pre�o no �ltimo dia do m�s e varia��o intra-mensal
usdbrl <- read.csv("USD_BRL M1 112010-2020.csv")
usdbrl <- usdbrl[rev(1:nrow(usdbrl)), c(names(usdbrl[c(1, 2, 6)]))]

# Datas a utilizar nas s�ries temporais
datas <- as.yearmon(
		c(
			2010+(10/12),
			2010+(11/12),
			2011 + seq(0, nrow(usdbrl)-3)/12
		)
	)

# Dados brasileiros da SECEX: volume de exporta��o e importa��o, respectivamente
xvbr <- ipeadata("SECEX12_XVTOT12")
xvbr <- xvbr[as.yearmon(xvbr$date) %in% datas, "value"]
names(xvbr) <- "exportBR"
mvbr <- ipeadata("SECEX12_MVTOT12")
mvbr <- mvbr[as.yearmon(mvbr$date) %in% datas, "value"]
names(mvbr) <- "importBR"

# Dados brasileiros da FIEMG: Utiliza��o da capacidade extrativa mineral
# at� o momento (1-fev-2021) n�o h� observa��o para dez-2020
# Adicionando um NA na �ltima observa��o
cutind <- ipeadata("FIEMG12_CUTIND12")
cutind <- cutind[as.yearmon(cutind$date) %in% datas, "value"]
cutind <- rbind(cutind, NA)
names(cutind) <- "usoPMetal"


##################
# Agregando dados
##################
dados <- tibble(
	datas =datas,
	ferroUS =gsub(",", ".", ferro.us) %>% as.numeric(.),
	cobreUS =gsub(",", ".", cobre.us) %>% as.numeric(.),
	vale =gsub(",", ".", vale) %>% as.numeric(.),
	dolar =usdbrl$fechamento,
	var.dolar =gsub("%", "", usdbrl$var) %>% as.numeric(.),
	xvbr,
	mvbr,
	cutind
)

dadoslong <- melt(dados, id="datas", variable_name ="variavel")

# Estat�sticas descritivas r�pidas
summary(dados[, -1])

##################
# Gr�ficos
##################

# Scatterplot
plot(dados[-1], pch =16, col ="#999999", cex =0.5)

# Correlogram
ggcorr(dados[-122,], method = c("everything", "pearson")) + 
	ggtitle("Correlograma")+
	theme(title =element_text(size =18))
# Boxplot
ggplot(dadoslong) +
	geom_boxplot(aes(x =variable, y =value)) +
	facet_wrap(variable~., scales ="free", ncol =4) +
	theme_bw() +
	labs(x ="", y ="", title ="Diagrama de Caixa") +
	theme(axis.title.x=element_blank(),
		axis.text.x=element_blank(),
		axis.ticks.x=element_blank(),
		text =element_text(size =18))

# Varia��o do d�lar vs a��o da vale 2015
ggplot(dados[51:62,], aes(x =datas)) +
  geom_line(aes(y =diff(vale)/vale, color ="Vale"), size =1.2) +
  geom_point(aes(y =diff(vale)/vale), size =4, color ="#954ac7") +
  geom_line(aes(y =diff(dolar)/dolar, color ="D�lar"), size =1.2) +
  geom_point(aes(y =diff(dolar)/dolar), size =4, color ="#3363de") +
  theme_bw() + scale_color_manual(values =c("#3363de", "#954ac7")) +
  labs(x ="", y ="Varia��o (%)") +
  theme(legend.position ="bottom", legend.title =element_blank(),
        text =element_text(size =18))

# Valores absolutos: d�lar vs a��o da vale 2015
ggplot(dados[51:62,], aes(x =vale, y =dolar)) +
  geom_point(size =4) + geom_smooth(method ="lm", se =FALSE, color ="#3363de") +
  theme_bw() + 
  labs(x ="Pre�o das a��es da Vale", y ="Pre�o do d�lar em Reais",
       subtitle ="Intercepto = 5,64\nInclina��o = -0,14") +
  theme(legend.position ="bottom", legend.title =element_blank(),
        text =element_text(size =18),
        plot.subtitle =element_text(hjust =0.5, size =12))

########################################################################################
# Atividade 2
########################################################################################

##############
# Gr�ficos
##############
# histograma vale
ggplot(dados, aes(y =..density..))+
  geom_histogram(aes(x =vale), bins =17, fill ="white", color ="#3363de", size =1.5) +
  geom_density(aes(x =vale), size =2, color ="#954ac7") +
  theme_bw() + labs(x ="Vale", y ="") + 
  theme(text =element_text(size =18))

# Histograma dolar
ggplot(dados, aes(y =..density..))+
  geom_histogram(aes(x =dolar), bins =17, fill ="white", color ="#3363de", size =1.5) +
  geom_density(aes(x =dolar), size =2, color ="#954ac7") +
  theme_bw() + labs(x ="D�lar", y ="") + 
  theme(text =element_text(size =18))

# Scatterplot log(dolar)~log(vale)
ggplot(dadoslong[245:488, ], aes())

ano2015 <- dados$datas %in% yearmon(2015 + seq(0, 11)/12)
ggplot(cbind(dados, ano2015), aes(x =log(vale), y =log(dolar), color =ano2015)) +
  geom_point(size =2) + theme_bw() +
  geom_smooth(method ="lm", se =F, size =2) +
  labs(color="Ano 2015") +
  theme(text =element_text(size =18), legend.position ="bottom")
  

########################################################################################


