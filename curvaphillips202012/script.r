########################################################################################
# Brief study of Phillip's curve on Brazillian quarterly data
########################################################################################

### Data selection
library(sidrar)

#Unenployment rate for each quarter
desemp <- get_sidra(api ="/t/6397/n1/all/v/4099/p/all/c58/95253/d/v4099%201")[,c(8,13)]

#Acumulated price index for each quarter
ipcaq <- get_sidra(api =
	"/t/1737/n1/all/v/2263/p/last%20107/d/v2263%202")[seq(3, dim(desemp)[1]*3,
	3 ), c(8,11)]

dados <- as.data.frame(cbind(ipcaq, desemp$Valor))
names(dados) <- c("Trimestre", "IPCA", "Desemprego")

### Linear Model
library(dplyr)
reg <- lm(IPCA ~ Desemprego, dados)

### Time series
library(zoo)
time <- yearqtr(2012 + seq(1:length(desemp$Valor))/4)

### Graphs
library(ggplot2)

ts_plot <- ggplot(dados) +
	geom_line(aes(time, IPCA, color= "IPCA", size ="0.6")) + 
	geom_line(aes(time , Desemprego, color= "Desemprego", size ="0.6")) +
	labs(title ="IPCA vs Desemprego (trimestral)", caption ="Fonte: SIDRA") + 
	theme_classic() + xlab("") + ylab("Valor %") + scale_color_hue(h = c(30, 110)) +
	theme(legend.title =element_blank(), text =element_text(size =15), 
		legend.position ='bottom') +
	guides(size ="none", color=guide_legend(override.aes =list(size =3)))
ts_plot

model_plot <- ggplot(dados) +
	geom_point(aes(Desemprego, IPCA), shape =21, size =2, stroke =3) + 
	geom_smooth(aes(Desemprego, IPCA), method ="lm", se =FALSE, size =2) + 
	theme_classic() + 
	labs(title ="Encaixe do modelo simples", 
	     subtitle = "Intercepto = 2,78   Coeficiente = -0,14") +
	xlab("Desemprego (%)") + ylab("IPCA (%)") + 
	theme(text =element_text(size =15), 
	      plot.subtitle =element_text(size =12, hjust = 0.5)) 
model_plot

### END ###
