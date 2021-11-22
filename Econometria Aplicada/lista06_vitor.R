setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada/Datasets")
##########    VITOR FERREIRA LINS    ##########

##### QUESTÃO 1
# ANULADA #

##### QUESTÃO 2
load("Wage2.RData")

### Letra A
reg01 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + IQ, data)
reg02 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + KWW, data)

c(coef(reg01)[9], coef(reg02)[9])
mean(data$KWW) < mean(data$IQ)

res2a <- list(Resposta =c(
	"O impacto com KWW é maior que com IQ",
	"Pois os valores amostrais de KWW são menores"))
res2a

### Letra B
# Interpretação de "juntas como proxy" ambígua

reg03 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + IQ*KWW, data) # INTERPRETAÇÃO 1
reg04 <- lm(log(wage) ~ educ + exper + tenure + married + south + 
	urban + black + I(data$IQ*data$KWW), data) # INTERPRETAÇÃO 2

res2b <- list(
	Interpretação1 ="Valor de beta cai para ambas as variáveis e interação",
	Interpretação2 ="Estimador se torna muito mais significante, com beta muito inferior")
res2b

### Letra C
# Respondendo apenas para interpretação 1
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
	"No teste individual apresenta valores-t baixos, exceto na interação.",
	"Mas no teste conjunto, o resultado é pior"))
res2c

###### QUESTÃO 3
rm(list =c("a", "data", "desc", "self"))
load("jtrain.RData")

### Letra A
reg06 <- lm(log(scrap) ~ grant, data)

res3a <- list(Resposta =c(
	"grant pode estar relacionado à Experiência e faturamento da empresa"))
res3a

### Letra B
reg07 <- lm(log(scrap) ~ grant, data =data, subset =c(d88==1))

res3b <- list(Coeficientes =coef(reg07), Resposta =c(
	"Segundo o modelo, se o governo oferece subsídio, o nível de refugo aumenta.",
	"Embora a variável seja pouco signifiacnte"))
res3b

### Letra C
reg08 <- lm(log(scrap) ~ grant + lscrap_1, data =data, subset =c(d88==1))

res3c <- list(Coeficientes =coef(reg08), Resposta =c(
	"O parâmetro de grant se tornou negativo. Com os erros constantes no tempo fora de u,",
	"o valor de grant passa a ser mais significante, mas não ao nível de 5%"))
res3c

###### QUESTÃO 4
rm(list =c("data", "desc", "self"))
load("rdchem.RData")

### Letra A
reg09 <- lm(rdintens ~ sales + I(sales^2) + profmarg, data)
reg10 <- lm(rdintens ~ sales + I(sales^2) + profmarg, data =data, subset =c(sales <39709))

res4a <- list(summary(reg09)[4], summary(reg10)[4], c(
	"No geral foi possível perceber uma redução na significância da maioria dos estimadores,",
	"Além de que o valor estimado em sales^2 aumentou bastante.",
	"Aparentemente a variância elevada dos dados com outlier afetava o resultado dos testes"))
names(res4a) <- c("Regressão com outlier", "Regressão sem outlier", "Resposta")
res4a

### Letra B
reg11 <- quantreg::rq(rdintens ~ sales + I(sales^2) + profmarg, data =data)
reg12 <- quantreg::rq(rdintens ~ sales + I(sales^2) + profmarg, data =data,
	subset =c(sales <39709))

a <- c(reg11[1] ,reg12[1])
names(a) <- c("Com outlier", "Sem outlier")
res4b <- list(Coeficientes =a, Resposta =c(
	"Ao remover o outlier, inverteu-se a concavidade do sales^2, além do efeito de sales",
	"é visível também que profmarg diminui ligeiramente"))
res4b

### Letra C
# Para o modelo proposto, MDA parece ser mais sensível para outliers, pois seus estimadores
# variam mais com a presença dos mesmos

##### ////////////////////////////// #####
rm(list =ls())
