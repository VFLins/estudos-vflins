########################################################################################
### Capturando dados do servidor do IPEA e salvos do Investing.com
########################################################################################

setwd("C:/Users/vflin/Documents/ECONOMIA/Análise de Séries Temporais")

library(zoo)
library(dplyr)
library(ggplot2)
library(GGally)
library(ipeadatar)
library(quantmod)
library(reshape2)


#### Periodicidade mensal para todas as variáveis 
#### Dados de nov-2010 até dez-2020

#### Variáveis sugeridas:

# Futuros de Minério de Ferro Negociado nos EUA
# Começou a ser negociado em 14-out-2010, dados a partir de nov-2010
ferro.us <- read.csv("FerroFutUS(PIOc1) M1 112010-2020.csv")
ferro.us <- ferro.us[rev(2:nrow(ferro.us)), "fechamento"]

# Futuros de Minério de Cobre Negociado nos EUA
cobre.us <- read.csv("CobreFutUS(HG) M1 112010-2020.csv")
cobre.us <- cobre.us[rev(1:nrow(cobre.us)), "fechamento"]

# Preços das ações da VALE na bolsa brasileira
vale <- read.csv("VALE3 M1 112010-2020.csv")
vale <- vale[rev(1:nrow(vale)), "fechamento"]

# Dados da cotação do dólar frente ao real
# Respectivamente, preço no último dia do mês e variação intra-mensal
usdbrl <- read.csv("USD_BRL M1 112010-2020.csv")
usdbrl <- usdbrl[rev(1:nrow(usdbrl)), c(names(usdbrl[c(1, 2, 6)]))]

# Datas a utilizar nas séries temporais
datas <- as.yearmon(
		c(
			2010+(10/12),
			2010+(11/12),
			2011 + seq(0, nrow(usdbrl)-3)/12
		)
	)

# Dados brasileiros da SECEX: volume de exportação e importação, respectivamente
xvbr <- ipeadata("SECEX12_XVTOT12")
xvbr <- xvbr[as.yearmon(xvbr$date) %in% datas, "value"]
names(xvbr) <- "exportBR"
mvbr <- ipeadata("SECEX12_MVTOT12")
mvbr <- mvbr[as.yearmon(mvbr$date) %in% datas, "value"]
names(mvbr) <- "importBR"

# Dados brasileiros da FIEMG: Utilização da capacidade extrativa mineral
# até o momento (1-fev-2021) não há observação para dez-2020
# Adicionando um NA na última observação
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

# Estatísticas descritivas rápidas
summary(dados[, -1])

##################
# Gráficos
##################

# Scatterplot
plot(dados[-1], pch =16, col ="#999999", cex =0.5)

# Correlogram
ggcorr(dados, method = c("everything", "pearson")) + 
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

# Variação do dólar vs ação da vale 2015
ggplot(dados[51:62,], aes(x =datas)) +
  geom_line(aes(y =diff(vale)/vale, color ="Vale"), size =1.2) +
  geom_point(aes(y =diff(vale)/vale), size =4, color ="#954ac7") +
  geom_line(aes(y =diff(dolar)/dolar, color ="Dólar"), size =1.2) +
  geom_point(aes(y =diff(dolar)/dolar), size =4, color ="#3363de") +
  theme_bw() + scale_color_manual(values =c("#3363de", "#954ac7")) +
  labs(x ="", y ="Variação (%)") +
  theme(legend.position ="bottom", legend.title =element_blank(),
        text =element_text(size =18))

# Valores absolutos: dólar vs ação da vale 2015
ggplot(dados[51:62,], aes(x =vale, y =dolar)) +
  geom_point(size =4) + geom_smooth(method ="lm", se =FALSE, color ="#3363de") +
  theme_bw() + 
  labs(x ="Preço das ações da Vale", y ="Preço do dólar em Reais",
       subtitle ="Intercepto = 5,64\nInclinação = -0,14") +
  theme(legend.position ="bottom", legend.title =element_blank(),
        text =element_text(size =18),
        plot.subtitle =element_text(hjust =0.5, size =12))

########################################################################################
#### Rascunho (defasado, não executar)
########################################################################################

#### Dados temporais com a data de fechamento
data <- as.yearmon(2001 + seq(0, nrow(vale)-1)/12)

dados <- data.frame(data,
	vale =gsub(",", ".", vale$ultimo) %>%
		as.numeric(.),
	cobre =gsub(",", ".", cobre.us$ultimo) %>%
		as.numeric(.))

n <- nrow(dados)
dadoslong <- data.frame(data =c(data, data),
	var =c(dados$vale, dados$cobre),
	id =c(rep("Ações da Vale", n), rep("Cobre Futuro EUA", n)))

#### Estatísticas descritivas
summary(dados)[, -1]

#### Plot pq sim
## Scatter plot
ggplot(dados, aes(x= vale, y =cobre)) + geom_point()

## Line plot
ggplot(dadoslong, aes(x =data, y =var, color =id)) +
	geom_line() + theme_bw() +
	facet_wrap(id ~ ., scales ="free") +
	labs(title ="Ações VALE3 vs futuros do Minério de Cobre nos EUA", y ="Preço",
		x="") +
	theme(legend.position ="bottom", text =element_text(size =14))


########################################################################################
#########################script com tabelas prontas

setwd("C:/Users/Debora/Documents/analise/dados")

library(readxl)
library(tseries)
library(ggplot2)

#carregando os dados
vale_cambio <- read_excel("vale_cambio_11.2010-2019.xls")
vale_cambio_2015 <- read_excel("vale_cambio_2015.xlsx")

##primeiras hipoteses, 11/2010:2019
#H0: ?? = 0
#H1: não H0

#primeira regressao linear
re_vale_cambio <- lm(log(vale_cambio$ultimo) ~ log(vale_cambio$CBO) - 1)
summary(re_vale_cambio)

#segundas hipoteses, 2015
#H0: ?? = 0
#H1: não H0

#segunda regressao linear
re_vale_cambio_2015 <- lm(log(vale_cambio_2015$ultimo) ~ log(vale_cambio_2015$CBO) - 1)
summary(re_vale_cambio_2015)

#testando normalidade 11/2010:2019
jarque.bera.test(vale_cambio$CBO)
jarque.bera.test(vale_cambio$ultimo)

#testando normalidade 2015
jarque.bera.test(vale_cambio_2015$CBO)
jarque.bera.test(vale_cambio_2015$ultimo)

#variancia 
#11/2010:2019
var(vale_cambio$ultimo)
var(vale_cambio$CBO)
var.test(vale_cambio$CBO, vale_cambio$ultimo)

#2015
var(vale_cambio_2015$ultimo)
var(vale_cambio_2015$CBO)
var.test(vale_cambio_2015$CBO, vale_cambio_2015$ultimo)

#histogramas 11/2010:2019
h <- hist(vale_cambio$ultimo, col="blue", density=20, breaks = 10, xlab = "câmbio", main = "Histograma")
xfit <- seq(min(vale_cambio$ultimo), max(vale_cambio$ultimo), length(100))
yfit <- dnorm(xfit, mean=mean(vale_cambio$ultimo), sd = sd(vale_cambio$ultimo))
yfit <- yfit*diff(h$mids[1:2]*length(vale_cambio$ultimo))
lines(xfit, yfit, col="red", lwd = 2)

h2 <- hist(vale_cambio$CBO, col="blue", density=20, breaks = 5)
xfit <- seq(min(vale_cambio$CBO), max(vale_cambio$ultimo), length(100))
yfit <- dnorm(xfit, mean=mean(vale_cambio$CBO), sd = sd(vale_cambio$CBO))
yfit <- yfit*diff(h$mids[1:2]*length(vale_cambio$CBO))
lines(xfit, yfit, col="red", lwd = 2)

#histogramas 2015
h3 <- hist(vale_cambio_2015$ultimo, col="blue", density=20, breaks = 10, xlab = "câmbio", main = "Histograma")
xfit <- seq(min(vale_cambio_2015$ultimo), max(vale_cambio_2015$ultimo), length(100))
yfit <- dnorm(xfit, mean=mean(vale_cambio_2015$ultimo), sd = sd(vale_cambio_2015$ultimo))
yfit <- yfit*diff(h$mids[1:2]*length(vale_cambio_2015$ultimo))
lines(xfit, yfit, col="red", lwd = 2)

