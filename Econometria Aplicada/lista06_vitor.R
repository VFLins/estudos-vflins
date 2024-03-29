setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada/Datasets")
##########    VITOR FERREIRA LINS    ##########

##### QUEST�O 1
# ANULADA #

##### QUEST�O 2
load("Wage2.RData")

### Letra A
reg01 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + IQ, data)
reg02 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + KWW, data)

c(coef(reg01)[9], coef(reg02)[9])
mean(data$KWW) < mean(data$IQ)

res2a <- list(Resposta =c(
	"O impacto com KWW � maior que com IQ",
	"Pois os valores amostrais de KWW s�o menores"))
res2a

### Letra B
# Interpreta��o de "juntas como proxy" amb�gua

reg03 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + IQ*KWW, data) # INTERPRETA��O 1
reg04 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + I(data$IQ*data$KWW), data) # INTERPRETA��O 2

res2b <- list(
	Interpreta��o1 ="Valor de beta cai para ambas as vari�veis e intera��o",
	Interpreta��o2 ="Estimador se torna muito mais significante, com beta muito inferior")
res2b

### Letra C
# Respondendo apenas para interpreta��o 1
summary(lm(log(wage) ~ IQ*KWW, data))
# Teste individual
a <- rbind(summary(reg01)[[4]], summary(reg02)[[4]], summary(reg03)[[4]])
tabela01 <- matrix(c(a[c(9, 27, 18, 28), 3], "NA", a[29,3]), nrow =2, byrow =FALSE)
colnames(tabela01) <- c("Valor-t IQ", "Valor-t KWW", "Valor-t IQ:KWW")
rownames(tabela01) <- c("isolado", "conjunto")

# Teste conjunto
reg05 <- lm(log(wage) ~ IQ*KWW, data)
tabela02 <- lmtest::resettest(reg05, data =data)

res2c <- list(Testes_Individuais =tabela01[2,], Teste_Conjunto =tabela02[4], Resposta =c(
	"No teste individual apresenta valores-t baixos, exceto na intera��o.",
	"Mas no teste conjunto, o resultado � pior"))
res2c

###### QUEST�O 3
rm(list =c("a", "data", "desc", "self"))
load("jtrain.RData")

### Letra A
reg06 <- lm(log(scrap) ~ grant, data)

res3a <- list(Resposta =c(
	"grant pode estar relacionado � Experi�ncia e faturamento da empresa"))
res3a

### Letra B
reg07 <- lm(log(scrap) ~ grant, data =data, subset =c(d88==1))

res3b <- list(Coeficientes =coef(reg07), Resposta =c(
	"Segundo o modelo, se o governo oferece subs�dio, o n�vel de refugo aumenta.",
	"Embora a vari�vel seja pouco signifiacnte"))
res3b

### Letra C
reg08 <- lm(log(scrap) ~ grant + lscrap_1, data =data, subset =c(d88==1))

res3c <- list(Coeficientes =coef(reg08), Resposta =c(
	"O par�metro de grant se tornou negativo. Com os erros constantes no tempo fora de u,",
	"o valor de grant passa a ser mais significante, mas n�o ao n�vel de 5%"))
res3c

###### QUEST�O 4
rm(list =c("data", "desc", "self"))
load("rdchem.RData")

### Letra A
reg09 <- lm(rdintens ~ sales + I(sales^2) + profmarg, data)
reg10 <- lm(rdintens ~ sales + I(sales^2) + profmarg, data =data, subset =c(sales <39709))

res4a <- list(summary(reg09)[4], summary(reg10)[4], c(
	"No geral foi poss�vel perceber uma redu��o na signific�ncia da maioria dos estimadores,",
	"Al�m de que o valor estimado em sales^2 aumentou bastante.",
	"Aparentemente a vari�ncia elevada dos dados com outlier afetava o resultado dos testes"))
names(res4a) <- c("Regress�o com outlier", "Regress�o sem outlier", "Resposta")
res4a

### Letra B
reg11 <- quantreg::rq(rdintens ~ sales + I(sales^2) + profmarg, data =data)
reg12 <- quantreg::rq(rdintens ~ sales + I(sales^2) + profmarg, data =data,
	subset =c(sales <39709))

a <- c(reg11[1] ,reg12[1])
names(a) <- c("Com outlier", "Sem outlier")
res4b <- list(Coeficientes =a, Resposta =c(
	"Ao remover o outlier, inverteu-se a concavidade do sales^2, al�m do efeito de sales",
	"� vis�vel tamb�m que profmarg diminui ligeiramente"))
res4b

### Letra C
# Para o modelo proposto, MDA parece ser mais sens�vel para outliers, pois seus estimadores
# variam mais com a presen�a dos mesmos

##### ////////////////////////////// #####
rm(list =ls())
