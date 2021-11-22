##### VITOR FERREIRA LINS #####
setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada/Datasets")

### Questão 1
load("ceosal2.RData")

# Letra A
reg01 <- lm(salary ~ sales + mktval, data)
reg01
perg01 <- "salary = 716.5762 + sales * 0,0165 + mktval * 0,0253 + û"

res1A <- c(perg01)
names(res1A) <- "Resposta"
res1A

# Letra B
reg02 <- lm(salary ~ sales + mktval + profits, data)
summary(reg02)
perg02 <- "Não é viável usar log(profits), pois existem valores negativos"
perg03 <- "Não são representativos, pois possuem níveis de significância muito baixos"

res1B <- c(perg02, perg03)
names(res1B) <- c("Resposta 1", "Resposta 2")
res1B

# Letra C
reg03 <- lm(log(salary) ~ sales + mktval + profits + ceoten, data)
reg03
perg04 <- (coef(reg03)[5])*100

res1C <- c(perg04)
names(res1C) <- "Variação no salário por ano como ceo (%)"
res1C

# Letra D
perg05 <- cor(data$lmktval, data$profits)*100
perg06 <- "Sim, as estatísticas vão ter maior variância (multicolinearidade)"

res1D <- c(perg05, perg06)
names(res1D) <- c("Correlação (%)", "Explicação")
res1D

### Questão 2
rm(list =c("data", "desc", "self"))
load("meap93.RData")

# Letra A
reg04 <- lm(math10 ~ log(expend) + lnchprg, data)
cov(data$lexpen, data$lnchprg)
perg07 <- list(coef(reg04), dim(data)[1], summary(reg04)[8:9])
names(perg07) <- c("Coeficientes", "Tamanho da amostra", "R-quadrado")
perg08 <- c("Não para lnchpr, esperaria eficiência do programa na nota do teste_", 
    "a covariância entre lnchprg e expen explica parte deste desencaixe")

res2A <- list(perg07, perg08)
names(res2A) <- c("Regressão", "Explicação")
res2A

# Letra B
perg09 <- "Por estar usando Log(expend), o intercepto mostra um caso e diminuição de gasto"
perg10 <- "Não, a menos que se deseje estimar casos extremos"

res2B <- c(perg09, perg10)
names(res2B) <- c("Explicação Intercepto", "Explicação coeficientes")
res2B

# Letra C
reg05 <- lm(math10 ~ log(expend), data)

res2C <- list(c(coef(reg04)[2], coef(reg05)[2], use.names =FALSE),
    c(coef(reg05)[2] > coef(reg04)[2], use.names =FALSE))
names(res2C) <- c("Coef. Regressão multipla / simples", "É maior?")
res2C

### Questão 3
rm(list =c("desc", "data", "self"))
load("htv.RData")

# Letra A
perg11 <- var(data$educ)
perg12 <- (dim(subset(data, educ ==15))[1] / dim(data)[1])*100
perg13 <- mean(data$educ) > mean(data$fatheduc)

res3A <- list(perg11, perg12, perg13)
names(res3A) <-  c("Var. de educ", "Homens que estudaram até o colegial (%)", 
    "Educação média dos filhos maior que a dos pais?")
res3A

# Letra B
reg06 <- lm(educ ~ motheduc + fatheduc, data)
perg14 <- summary(reg06)[8:9]
perg15 <- "O valor do R-quadrado apresenta o nível de representatividade das variáveis"

res3B <- list(perg14, perg15)
names(res3B) <- c("R-quadrado ajustado(%)", "Explicação")
res3B
 
# Letra C
reg07 <- lm(educ ~ motheduc + fatheduc + abil, data)
perg16 <- summary(reg07)[8:9]
perg17 <- "Sim, pois o R-quadrado aumentou"

res3C <- list(perg16, perg17)
names(res3C) <- c("R-quadrado", "Explicação")
res3C

# Letra D
reg08 <- lm(educ ~ motheduc + fatheduc + abil + I(abil^2), data)
reg08[1]
# D(educ)/D(abil): educ = 0,0414624 + 2 * 0,05059901 * abil
# Min D(educ)/D(abil): abil = aprox. -0,409715526

res3D <- "Para encontrar o menor valor de educ estimado, abil = -0,409715526, e as demais variáveis = 0"
names(res3D) < -"Resposta"
res3D

# Letra E
res3E <- c("A média dos resíduos deve ser igual a zero,_", 
    "se todos os dados estivessem acima,_",
    "a média seria maior que zero.")
names(res3E) <- c("Resposta", "", "")
res3E

# Letra F

##### ////////////////////////////// #####
rm(list =ls())
