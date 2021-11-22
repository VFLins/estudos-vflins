### Resolução da atividade de regressão linear
########################################################################################

library(dplyr)
library(ggplot2)
library(readxl)

setwd("C:/Users/vflin/Documents/ECONOMIA/Análise de Séries Temporais/Tabelas")

# DADOS DA REGRESSÃO SIMPLES
########################################
dados0 <- read_excel("Dados.xlsx", 1, "A2:B60") %>% setNames(., c("Midterm", "Final"))
lm0 <- lm(Final~Midterm, dados0)
vt0 <- qt(c(.025, .975), df=56)

## Diagrama de dispersão
####################
ggplot(dados0, aes(y =Final, x =Midterm)) +
geom_point(size =4) + geom_smooth(method ="lm", size =2,
	aes(color ="Final = (1,014)Midterm -1,557")) +
labs(x ="Midterm", y ="Final", color ="",
	caption ="Midterm IC: ±0,3125\n Intercepto IC: ±24.0389") + theme_bw() +
theme(text =element_text(size =26), plot.title = element_text(hjust = 0.5),
	legend.position="bottom", legend.text =element_text(face ="italic")) 

# DADOS DA REGRESSÃO MÚLTIPLA
########################################
dados <- read_excel("Dados.xlsx", 2, "A2:G34")
dados$Freeway <- as.factor(dados$Freeway)
lm1 <- lm(Assessed ~ Floor + Age + Offices + Entrances + Freeway, dados) #modelo
vt1 <- qt(c(.025, .975), df=26) # Adquirindo valores tabelados para signif. 5% bicaudal

#formula: estimador +/- Ep*Ic
mtxic <- matrix(c(summary(lm1)$coefficients[,1] + summary(lm1)$coefficients[,2]*vt1[1],
	summary(lm1)$coefficients[,1] + summary(lm1)$coefficients[,2]*vt1[2]),
	ncol =2) # Matriz de intervalos de confiança
colnames(mtxic) <- c("inf", "sup")
rownames(mtxic) <- lm1$coefficients %>% names

mtxsr <- matrix(rstandard(lm1), nrow =8, byrow =TRUE)
colnames(mtxsr) <- c("","","","")
rownames(mtxsr) <- c("1", "5", "9", "13", "17", "21", "25", "29")

## Visualização do modelo apenas intercepto
####################
ggplot(dados, aes(x= seq(1, 32), y =Assessed)) + 
geom_hline(yintercept =1235.938, color ="red", size =2) + 
geom_hline(yintercept =1235.938+(2.039513*455.6721), linetype ="dashed", size =1.5) +
geom_hline(yintercept =1235.938+(-2.039513*455.6721), linetype ="dashed", size =1.5) +
geom_point(size =4) + labs(x ="") + theme_bw() +
theme(text =element_text(size =26), plot.title = element_text(hjust = 0.5)) 

## Histograma dos resíduos
####################
ggplot(data.frame(res =rstandard(lm1)), aes(y =..density..)) +
geom_histogram(aes(x =res), bins =12, fill ="grey", color ="black", size =1) +
geom_density(aes(x =res), size =2, color ="blue") + 
geom_vline(xintercept =mean(lm1$residuals), size =2, linetype ="dashed") +
labs(x ="", y ="Densidade") + theme_bw() +
theme(text =element_text(size =26), plot.title = element_text(hjust = 0.5)) 

## Resíduos vs valores ajustados
####################
ggplot(data.frame(res =lm1$residuals, fit =lm1$fitted), aes(y =res, x =fit)) +
geom_point(size =4) + geom_hline(yintercept =0, size =1) +
labs(x ="Ajustados", y ="Resíduos") + theme_bw() +
theme(text =element_text(size =26), plot.title = element_text(hjust = 0.5)) 


########################################################################################
