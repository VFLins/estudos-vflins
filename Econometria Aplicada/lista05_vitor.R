setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada/Datasets")
##########    VITOR FERREIRA LINS    ##########

##### QUESTÃO 1
load("sleep75.RData")

### Letra A
# Var(u|x1, x2, . . . , xn) = s_sqr + male

### Letra B
reg01 <- lm(sleep ~ totwrk +  educ + age + I(age^2) + yngkid + male, data)
reg02 <- lm(resid(reg01)^2 ~ male, data)
summary(reg02)
# Prob. P-valor da estatística F: 0,2909; Hipótese fraca de heterocedasticidade

reg03 <- lm(resid(reg01)^2 ~ totwrk +  educ + age + I(age^2) + yngkid,
	data, subset =data$male ==0)
reg04 <- lm(resid(reg01)^2 ~ totwrk +  educ + age + I(age^2) + yngkid,
	data, subset =data$male ==1)
summary(reg03)
summary(reg04)
# O modelo se torna um pouco mais homoscedástico com male=1

### Letra C
# 

##### QUESTÃO 2
rm(list =c("data", "self", "desc"))
load("hprice1.RData")

### Letra A
reg05 <- lm(price ~ lotsize + sqrft + bdrms, data)
tab01 <- car::hccm(reg05, type ="hc0")
lmtest::coeftest(reg05, vcov =tab01)
# Todos os coeficientes possuem h_0 difícil de rejeitar à signif. de 10%
# Diferentemente do intercepto

### Letra B
reg06 <- lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms, data)
tab02 <- car::hccm(reg06, type ="hc0")
lmtest::coeftest(reg06, vcov =tab02)

### Letra C
# A hipótese de homocedasticidade melhora para todos os parâmetros
# Porém bdrms passa a ter hipótese fraca de heterocedasticidade

##### ////////////////////////////// #####
rm(list =ls())
