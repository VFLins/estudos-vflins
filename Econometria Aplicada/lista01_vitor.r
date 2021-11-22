##### Aluno: Vitor Ferreira Lins #####
setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada")

# Rodar o script todo de uma vez deve mostrar as respostas de maneira organizada
# Se o caminho não coincidir, pode ser necessário definir os diretórios manualmente

### Questão 1
load("Datasets/bwght.RData")

# Letra A
# a quantidade de mulheres não consta nos dados em "bwght"
# usarei a quantidade de bebês do sexo feminino no lugar
perg01 <- dim(subset(data, male ==0))[1]
perg02 <- dim(subset(data, cigs >0))[1]

resposta1A <- c(perg01, perg02)
names(resposta1A) <- c("Bebês do sexo feminino", "Mães fumantes")
resposta1A

# Letra B
perg03 <- mean(data$cigs)
perg04 <- "Não é uma boa medida, pois a maioria das mulheres não fuma"

resposta1B <- c(perg03, perg04)
names(resposta1B) <- c("Média de cigarros consumidos", "Explicação da média")
resposta1B

# Letra C
perg05 <- mean(subset(data, cigs >0)[,c("cigs")])
perg06 <- "Esta média mostra como os outliers influenciam na média da população"

resposta1C <- c(perg05, perg06)
names(resposta1C) <- c("Média considerando apenas fumantes", "Explicação da média")
resposta1C

# Letra D
perg07 <- mean(subset(data, fatheduc >=0)[,c("fatheduc")])
perg08 <- "Pois existem valores 'NA'"

resposta1D <- c(perg07, perg08)
names(resposta1D) <- c("Média de educação do pai", "Explicação da média")
resposta1D

# Letra E
perg09 <- mean(data$faminc)
perg10 <- sd(data$faminc)

resposta1E <- c(perg09, perg10)
names(resposta1E) <- c("Renda média familiar", "Desvio padrão da renda")
resposta1E

### Questão 2
rm(list =c("data", "self", "desc"))
load("Datasets/jtrain2.RData")

# Letra A
perg11 <- sum(data$train)

resposta2A <- c(perg11)
names(resposta2A) <- "Quantidade de pessoas que receberam treinamento"
resposta2A

# Letra B
perg12 <- mean(subset(data, train ==1)$re78)
perg13 <- mean(subset(data, train ==0)$re78)

resposta2B <- c(perg12, perg13)
names(resposta2B) <- c("Média para pessoas com capacitação", "(...) sem capacitação")
resposta2B

# Letra C
perg14 <- sum(subset(data, train ==1)$unem78) / dim(data)[1]
perg15 <- sum(subset(data, train ==0)$unem78) / dim(data)[1]
perg16 <- "Ter treinamento capacitório parece ser importante para manter o emprego"

resposta2C <- c(perg14, perg15, perg16)
names(resposta2C) <- c("Desemprego entre capacitados", "(...) não capacitados", "Comentário")
resposta2C

# Letra D
perg17 <- "Sim, um teste de hipótese para obter a certeza estatística da afirmação"

resposta2D <- c(perg17)
names(resposta2D) <- "Explicação"
resposta2D

### Questão 3
rm(list =c("data", "self", "desc"))
load("Datasets/ceosal2.RData")

# Letra A
perg18 <- mean(data$salary)
perg19 <- mean(data$comten)
perg20 <- mean(data$ceoten)

resposta3A <- c(perg18, perg19, perg20)
names(resposta3A) <- c("Média salarial", "Permanência média na empresa", "Permanência média como CEO")
resposta3A

# Letra B
perg21 <- sum(data$ceoten ==0)
perg22 <- max(data$ceoten)

resposta3B <- c(perg21, perg22)
names(resposta3B) <- c("CEOs novatos", "Permanênca do CEO mais experiente")
resposta3B 

# Letra C
perg23 <- lm(log(salary) ~ ceoten, data)
perg24 <- c("Salário inicial médio", "Acréscimo salarial percentual por ano como CEO")

resposta3C <- rbind(perg23$coefficients, perg24)
resposta3C

### Questão 4
rm(list =c("data", "self", "desc"))
load("Datasets/sleep75.RData")

# Letra A
summary(lm(sleep ~ totwrk, data))
lm01 <- lm(sleep ~ totwrk, data)
ndata01 <- dim(data)[1]
rsqr01 <- "0,1033 / 0,102"
perg25 <- list(coefficients(lm01), ndata01, rsqr01)
names(perg25) <- c("Coeficientes", "Observações", "R-Squared: Mult / Adj")
perg26 <- "A pessoa que não trabalha teria seu tempo de sono aprox. ao valor do intercepto"

resposta4A <- list(perg25, perg26)
names(resposta4A) <- c("Resposta", "Explicação")
resposta4A

# Letra B
perg27 <- c("Mudará no dobro do valor do coeficiente de totwrk, que é de", coefficients(lm01)[2]*2)
perg28 <- "Não"

resposta4B <- matrix(c(perg27, perg28), nrow =1)
rownames(resposta4B) <- "Resposta"
colnames(resposta4B) <- c("Variação em totwrk", "", "O efeito é grande?")
resposta4B

### Questão 5
rm(list =c("data", "desc", "self"))
load("Datasets/wage2.RData")

# Letra A
perg29 <- c(mean(data$wage), mean(data$IQ))
names(perg29) <- c("Salário médio", "QI médio")
perg30 <- c(sd(data$IQ))
names(perg30) <- "Desvio Padrão do QI"

resposta5A <- c(perg29, perg30)
resposta5A

# Letra B
lm02 <- lm(wage ~ IQ, data)
perg31 <- coefficients(lm02)[2]*15
summary(lm02)
perg32 <- "Não, pois o R-quadrado é muito baixo"

resposta5B <- c(perg31, perg32)
names(resposta5B) <- c("Variação no salário para cada 15 pontos de QI", "Explicação")
resposta5B

# Letra C
lm03 <- lm(log(wage) ~ IQ, data)
resposta5C <- c("15 pontos de QI aumenta o salário em(%)", coefficients(lm03)[2]*15*100)
names(resposta5C) <- c("", "Resposta")
resposta5C
