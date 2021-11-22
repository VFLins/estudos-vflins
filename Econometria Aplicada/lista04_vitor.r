##########    VITOR FERREIRA LINS    ##########
setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada/Datasets")

###	Quest�o 1
load("wage2.RData")

#	Letra A
reg01 <- lm(log(wage) ~ educ + exper + tenure + married + black + south + 
	urban, data)
perg01 <- "Sim, pois � aprovada pelo teste t com signific�ncia menor que 0,1%"

res1a <- list(summary(reg01)[4], perg01)
names(res1a) <- c("REGRESS�O", "RESPOSTA")
res1a

#	Letra B
reg02 <- lm(log(wage) ~ educ + exper + tenure + married + black + south +
	 urban + I(exper^2) + I(tenure^2), data)
perg02 <- anova(reg02)[5]
perg03 <- "Ambas as vari�veis adicionadas possuem resultados fracos no teste f"

res1b <- list(REGRESS�O =reg02, TESTE =perg02, RESPOSTA =perg03)
res1b

#	Letra C
reg03 <- lm(log(wage) ~ educ + exper + tenure + married + black + south + 
	urban + educ:black, data)
perg04 <- summary(reg03)[4]
perg05 <- anova(reg03)[5]
perg06 <- "N�o obt�m bons resultados no teste t nem f"

res1c <- list(REGRESS�O =reg03, TESTES =c(perg04, perg05), RESPOSTA =perg06)
res1c

#	Letra D
reg04 <- lm(log(wage) ~ educ + exper + tenure + married + black +
	south + urban + educ:black, data)
summary(reg04)[4]
perg07 = 5.382609+0.19008+0.039611
perg08 = 5.382609+0.19008

res1d <- list(perg07-perg08,c(perg07, perg08))
names(res1d) <- c("DIFF. NOS INTERCEPTOS",
	"INTERCEPTO married+black // married")
res1d

###	Quest�o 2
rm(list =c("data", "desc", "self"))
load("catholic.RData")

#	Letra A
divide <- function(a, b){a/b}
res2a <- list(divide(sum(data$cathhs, na.rm=TRUE), dim(data)[1])*100,
	mean(data$math12))
names(res2a) <- c("FREQ. NO ENSINO M�DIO (%)", "M�DIA DE math12")
res2a

#	Letra B
reg05 <- lm(math12 ~ cathhs, data)
summary(reg05)[4]
hist(data$math12, breaks ="FD")

res2b <- list(RESPOSTA =c(
	"A m�dia � bastante representativa dos dados em math12,",
	"portanto uma regress�o simples vai ter bastante efic�cia...",
	"para explicar o comportamento de math12",
	"(intercepto com valor parecido com a m�dia)"))
res2b

#	Letra C
reg06 <- lm(math12 ~ cathhs + lfaminc + motheduc + fatheduc, data)
summary(reg06)

res2c <- list(OBSERVA��ES =nobs(reg06), RESPOSTA =c(
	"Seu valor cai para a metade,",
	"Mas sua signific�ncia estat�stica permanece relevante"))
res2c

#	Letra D
res2d <- "As duas regress�es utilizam todas as observa��es"
res2d

#	Letra E
reg07 <- lm(math12 ~ cathhs*lfaminc + cathhs*motheduc +
	cathhs*fatheduc, data)

res2e <- list(AN�LISE =summary(reg07)[4], RESPOSTA =c(
	"Todas as intera��es s�o rejeitadas � signific�ncia de 5%",
	"s�o conjuntamente n�o significativos"))
res2e

#	Letra F
res2f <- "Diminui. Porque possui amostra proporcionalmente pequena"
res2f

##### ////////////////////////////// #####
rm(list =ls())

