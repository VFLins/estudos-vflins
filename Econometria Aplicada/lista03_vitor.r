##### VITOR FERREIRA LINS #####
setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada/Datasets")

### Questão 1
load("vote1.RData")

# Letra A
# Mede a proporção em que as mudanças percentuais nos _
# gastos do Candidato A, afeta a quantidade de votos do mesmo

# Letra B
# x1 aumenta y em (bhat*100)%, os valores de x
lm00 <- lm(voteA ~ lexpendA + lexpendB,data)
lm01 <- lm(voteA ~ lexpendA, data)
lm02 <- lm(voteA ~ lexpendB, data)
rsq00 = as.numeric(summary(lm00)[9])
rsq01 = as.numeric(summary(lm01)[9])
rsq02 = as.numeric(summary(lm02)[9])

rsq00 > mean(rsq02, rsq01)
# O modelo explica muito melhor quando se adiciona as duas variáveis

# Letra C
reg01 <- lm(voteA ~ lexpendA + lexpendB + prtystrA, data)
perg02 <- anova(reg01)[4:5]

res1c <- list(reg01, perg02)
names(res1c) <- c("REGRESSÃO", "RESPOSTA")
res1c

# As estatísticas de relevância das variáveis isoladas são boas
# Não é possível usar este resultado para a hipótese pedida

# Letra D
perg03 <- summary(lm00)[4]
perg04 = "As hipóteses nulas dos testes t são altamente rejeitáveis"

res1D =list(lm00, perg03, perg04) 
names(res1D) <- c("REGRESSÃO", "TESTE", "EXPLICAÇÃO")
res1D

### Questão 2
rm(list =c("data", "desc", "self"))
load("wage1.RData")

# Letra A
reg02 <- lm(wage ~ educ + exper + I(exper^2), data)
reg02

# Letra B
perg05 <- summary(reg02)[4]
perg06 <- "Sim, pois seu teste t é dificilmente rejeitado"

res2B <- list(perg05, perg06)
names(res2B) <- c("TESTE", "RESPOSTA")
res2B

# Letra C
perg07 <- 100*(0.268287-2*0.004612*5)*5
perg08 <- 100*(0.268287-2*0.004612*20)*20

res2C <- list(perg07, perg08)
res2C(names) <- c("RETORNO 5 ANOS", "RETORNO 20 ANOS")
res2C

# Letra D
#min wage: 0.268287 - 2*0.004612exper = 0
#min wage: exper = -29,08575455

# Letra E
curve01 <- function(x){ 
    -3.964890 + mean(data$educ)*0.595343 + 0.268287*x -0.004612*I(x^2)
}
curve(curve01)

### Questão 3
reg03 <- lm(log(wage) ~ educ + educ*exper, data)
reg03

# Letra A
# Ao diferenciar para educ: Dwage/wage=(bhat1 + bhat2*exper)*Deduc
# beta chapeu = bhat

# Letra B
# teste t, h_0: bhat2=0

# Letra C
perg07 <- summary(reg03)[[4]][11]
perg08 <- "O resultado do teste t encontrado é inferior ao tabelado para signif. = 1%"

res3C <- list(perg07, perg08)
names(res3C) <- c("TESTE T", "RESPOSTA")
res3C

##### ////////////////////////////// #####
rm(list =ls())
