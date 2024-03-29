########################################################################################
### Capturando dados do servidor do IPEA e salvos do Investing.com
########################################################################################

setwd("~dados/input") # mudar para caminho verdadeiro para funcionar

library(zoo)
library(dplyr)
library(ipeadatar)
library(quantmod)
library(reshape2)

#### Periodicidade mensal para todas as vari�veis 
#### Dados de nov-2010 at� dez-2020
datas <- c(2010 + seq(10, 131)/12) %>% as.yearmon

#### Vari�veis sugeridas:
# Futuros de Min�rio de Ferro Negociado nos EUA
# Come�ou a ser negociado em 14-out-2010, dados a partir de nov-2010
nomes <- c("Date", "Price", "Open", "High", "Low", "Vol", "Var")

ferro.us <- read.csv("FerroFutUS(TIOc1) M1 112010-2020.csv", col.names =nomes)
ferro.us <- ferro.us[rev(1:nrow(ferro.us)), c("Date", "Price")]

# Futuros de Min�rio de Cobre Negociado nos EUA
cobre.us <- read.csv("CobreFutUS(HG) M1 112010-2020.csv", col.names =nomes)
cobre.us <- cobre.us[rev(1:nrow(cobre.us)), c("Date", "Price")]

# Pre�os das a��es da VALE na bolsa brasileira
vale <- read.csv("VALE3 M1 112010-2020.csv", col.names =nomes)
vale <- vale[rev(1:nrow(vale)), c("Date", "Price")]

# Dados da cota��o do d�lar frente ao real
# Respectivamente, pre�o no �ltimo dia do m�s e varia��o intra-mensal
usdbrl <- read.csv("USD_BRL M1 112010-2020.csv", col.names =nomes[-6])
usdbrl <- usdbrl[rev(1:nrow(usdbrl)), c("Date", "Price", "Var")]

# Dados brasileiros da SECEX: volume de exporta��o e importa��o, respectivamente
xvbr <- ipeadata("SECEX12_XVTOT12") %>% subset(., as.yearmon(date) %in% datas)
mvbr <- ipeadata("SECEX12_MVTOT12") %>% subset(., as.yearmon(date) %in% datas)

# Dados brasileiros da FIEMG: Utiliza��o da capacidade extrativa mineral
# at� o momento (1-fev-2021) n�o h� observa��o para dez-2020
cutind <- ipeadata("FIEMG12_CUTIND12") %>% subset(., as.yearmon(date) %in% datas)
# Adicionando um NA na �ltima observa��o

##################
# Agregando dados
##################
dados <- data.frame(
	datas =datas,
	ferroUS =ferro.us$Price %>% as.numeric,
	cobreUS =cobre.us$Price %>% as.numeric,
	vale =vale$Price %>% as.numeric,
	dolar =gsub(",", ".", usdbrl$Price) %>% as.numeric,
	var.dolar =gsub("%", "", usdbrl$Var) %>% gsub(",", ".", .) %>% as.numeric,
	exportBR =xvbr$value,
	importBR =mvbr$value,
	usoPMetal =cutind$value
)

dadoslong <- melt(dados, id="datas", variable_name ="variavel")

# Estat�sticas descritivas r�pidas
summary(dados[, -1])

# Limpando �rea de trabalho
rm(list =ls()[!(ls() %in% c("dados", "dadoslong"))])
ls() #para ver se funcionou, deve aparecer: "dados" "dadoslong"

# Exportando dados como .csv

write.csv(dados, row.names =FALSE,
	"~dados/output/dados.csv")# mudar para caminho verdadeiro para funcionar
write.csv(dadoslong, row.names =FALSE,
	"~dados/output/dadoslong.csv")# mudar para caminho verdadeiro para funcionar

#FIM####################################################################################
