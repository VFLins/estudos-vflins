######################################################################################################
# Study on Brazilian product and unenployment rate
######################################################################################################

library(ggplot2)
library(sidrar)
library(zoo)
library(dplyr)

### Gathering data

# Unenployment rate
desemp <- get_sidra(api ="/t/6397/n1/all/v/4099/p/all/c58/95253/d/v4099%201")[, 
	c("Trimestre (Código)", "Valor")]
names(desemp) <- c("trimestre", "valor")

# Product by 1000 brl
pib <- get_sidra(api ="/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%204")[, 
	c("Trimestre (Código)", "Valor")]
names(pib) <- c("trimestre", "valor")

# Subsetting by dates to make lengths match
pib <- subset(pib, trimestre %in% desemp$trimestre)

# Date vector
trimestre <- yearqtr(2012 + seq(1:length(desemp$valor))/4)

# Binding data
dados <- data.frame(trimestre, desemp =desemp$valor, pib =pib$valor)
n <- dim(dados)[1]
dadoslong <- data.frame(trimestre =c(trimestre, trimestre), 
	val =c(desemp$valor, pib$valor), id =c(rep("Desemprego (%)", n), rep("PIB 1995 = 100", n)))

### Model

reg0 <- lm(desemp ~ pib, dados)

### Graphic

plot0 <- ggplot(dadoslong, aes(trimestre, val, group =id, color =id, size ='0.8')) +
	geom_line() + facet_grid(id~., scales ="free") + theme_bw() + scale_color_hue(h = c(75, 110)) +
	theme(legend.position ="none", text =element_text(size =15)) + xlab("") + ylab("") + 
	labs(title ="Taxa de Desemprego vs PIB Brasileiro", caption ="Fonte: SIDRA")

plot1 <- ggplot(dados) +
	geom_point(aes(desemp, pib), shape =21, size =2, stroke =3) + 
	geom_smooth(aes(desemp, pib), method ="lm", se =FALSE, size =2) + 
	xlab("Desemprego") + ylab("PIB") + theme_bw() +
	theme(legend.position ="none", text =element_text(size =15), plot.subtitle =
		element_text(size =12, hjust = 0.5)) +
	labs(title ="Regressão simples: Desemprego ~ PIB" , 
		subtitle ="Intercepto = 68,6831   Coeficiente = -0,3473")

plot0
plot1
##### END
