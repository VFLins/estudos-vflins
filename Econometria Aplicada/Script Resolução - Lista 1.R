#lista de exercicio 1


############ Quest�o 1 ######################

#carregando a base de dados
load("bwght.RData")
attach(data)

# item a)

#exibindo a listagem da estrutura, onde pode ser visto o n�mero de observa��es. 
#1388 obs.
str(data)

#Criando um subgrupo s� de m�es que fumaram na gravidez
fumgrav <- subset(data, cigs!=0)
nrow(fumgrav) #Encontrando o numero de linhas (e portanto de observa��es que satisfazem a condi��o)

# A abordagem usando subset � interessante quando vc quiser manter todas as vari�veis que satisfazem aquele crit�rio. Por exemplo,
# no comando acima, todas as informa��es das m�es fumantes ser�o mantidas no novo dataframe

# Um outra op��o � utilizar a referencia��o a elementos de um vetor, focando apenas no vetor que armazena as informa��es de cigs
# Como feito pelo colega John.

sum(cigs>0)

# Outro comando interessante � o str, que exibe a listagem da estrutura, onde pode ser visto o n�mero de observa��es. 
#212 obs. (Numero bem pequeno de fumantes em rela��o ao total)
str(fumgrav)

# item b) 

#media de cigarros fumados. Aqui consideramos todas as mulheres. At� n�o fumantes
mean(cigs)

# item c)

#media de cigarros fumados na gravidez. Aqui consideramos apenas mulheres que fumaram na gravidez. 
mean(fumgrav$cigs)

# Ou sem usar o subset, usando apenas referencia��o ao vetor
mean(cigs[cigs>0])

#Nota-se que como muitas mulheres da amostra n�o fumam, o m�dia do conjunto total � relativamente baixa. A mullher t�pica � 
# uma n�o fumante. Quando se exclui as n�o fumantes temos um n�mero mais representativo do n�mero de cicarros fumados pelas 
# mulheres fumantes.

# item d) 

#media da educa��o dos pais descnsiderando os valores "NA" (n�o dispon�vel)
mean(data$fatheduc, na.rm = "TRUE")


#Criando um subgrupo s� com pais que registraram escolaridade. Explicando porque nem todas as obseva��es foram consideradas na m�dia
# algumas observa��es n�o tem valor para esta variavel. assim, s�o desconsiderados na m�dia
pais <- subset(data, fatheduc>0)
nrow(pais)

#exibindo a listagem da estrutura, onde pode ser visto o n�mero de observa��es
str(pais)

# e) 

#Calculando Renda m�dia e desvio padr�o em d�lares
mean(faminc)
sd(faminc)


detach(data)
rm(list = ls())

################ Quest�o 2:######################
#carregando a base de dados
load("jtrain2.RData")
attach(data)

##### item a) 

# Criando subgrupos de trabalhadores treinados e n�o treinados
treinados <- subset(data, train == 1)
numero_treinados <- nrow(treinados)
ntreinados <- subset(data, train == 0)
numero_nao_treinados <- nrow(ntreinados)

#Aqui novamente � poss�vel fazer utilizando apenas a referencia a vetores (ver quest�o anterior)

##### item b)

#Calculando as medias de ganhos no ano de 78 para os dois subgrupos (treinados e n�o treinados)
mean(treinados$re78)
mean(ntreinados$re78)

##### item c)

#Criando subgrupos para separar empregados e desempregrados dos trabahadores treinados e n�o treinados
treinados_emp <- subset(treinados, unem78 == 0)
treinados_desemp <- subset(treinados, unem78 == 1)

ntreinado_emp <- subset(ntreinados, unem78 == 0)
ntreinados_desemp <- subset(ntreinados, unem78 == 1)


#propor��o de treinados desempregados em rela��o a todos os treinados
(proporcao1 <- nrow(treinados_desemp)/nrow(treinados))

#propor��o de n�o treinados desempregos em rela��o a todos os n�o treinados
(proporcao2 <- nrow(ntreinados_desemp)/nrow(ntreinados))

#diferen�a entre as propor��es
(diff <- proporcao1 - proporcao2) 

#portanto, existem mais n�o treinados desempregados, do que o contr�rio.

##### item d)

#funcion�rios treinados receberem em m�dias maiores sal�rios que os funcion�rios n�o treinados e a propor��o de funcion�rios treinados que est�o desempregados (24,3%) � inferior 
#a dos funcion�rios n�o treinados que est�o deseempregados (35,3%), o programa aparenta ter sido efetivo. Mas para responder essa
#pergunta de forma mais convincente ser� necess�rio utilizar modelos de regress�o que aprenderemos nas proximas aulas.

detach(data)
rm(list = ls())
################ Quest�o 3:######################

#carregando a base de dados
load("ceosal2.RData")
attach(data)

##### item a)
#encontrando o salario e permanencia medios
mean(salary)
mean(ceoten)


##### item b)
#Criando subgrupo dos ceo's em primeiro ano do cargo
primeiro_ano <- subset(data, ceoten == 0)
nrow(primeiro_ano)

# permanencia m�xima
max(ceoten)


##### item c)
#estimando a regress�o
(reg <- lm(lsalary ~ ceoten ))

#com tudo mais constante, o aumento de um ano do funcionario como ceo eleva seu sal�rio em aproximadamente 0,9724%


detach(data)
rm(list = ls())
################ Quest�o4 ######################

#carregando a base de dados
load("sleep75.RData")
attach(data)

##### item a)
#estimando a regress�o
(reg <- lm(sleep ~  totwrk))

#registrando resultados da reg + n de obs + r2
summary(reg)
coeficientes <- coef(reg)
numobs <- nobs(reg) 
r2 <- cor(data$sleep, data$totwrk)^2 # terceira maneira de calcular R2 � calcular o quadrado do coeficiente de correla��o entre y e x)

#o intercepto indica que se a pessoa n�o trabalhar nenhum minuto na semana, a quantidade de minutos dormindos estimados por
#semana � de 3.586.


##### item b)
delta.trabalho <- 120 # duas horas a mais de trabalho, ou seja, 120 minutos

(impacto <- reg$coefficients[2]*delta.trabalho)
#supondo que totwrk aumentar 2 hrs semanais, sleep deve cair em 18 min por semana, valor relativamente pequeno.


detach(data)
rm(list = ls())

################ Quest�o 5 ######################

#carregando a base de dados
load("wage2.RData")
attach(data)

#### item a)
#encontrando o salario e iq medios e o desvio padr�o amostral de IQ
mean(data$wage)
mean(data$IQ)
sd(data$IQ, na.rm = TRUE)

#tamb�m � poss�vel usar o comando summary, como sugerido na aula s�ncrona dessa semana.

#### item b)

#estimando a regress�o
(reg <- lm(wage ~  IQ))

#com tudo mais constante, um aumento de um ponto na IQ vai resultar em uma eleva��o de 8,3 dolares no salario.
#com tudo mais constante, um aumento de 15 pontos na IQ vai resultar em uma eleva��o de 124,5 dolares no salario
delta.qi <- 15
(impacto <- reg$coefficients[2]*delta.qi)

#encontrando o R2 da regress�o
r2 <- cor(wage, IQ)^2
#ele indica que apenas 9,5% da varia��o de wage est� relacionada com o IQ

# as duas outras maneiras de encontrar o r2 s�o r2 = SQE/SQT = 1 - SQR/SQT

(r2 <- var(reg$fitted.values)/var(wage))
(r2 <- 1 - var(reg$residuals)/var(wage))

#### item c)

#estimando a regress�o com log do wage
(reglog <- lm(lwage) ~  IQ)

#com tudo mais constante, um aumento de um ponto na IQ vai resultar em uma eleva��o de 0,88% nos salarios
#com tudo mais constante, um aumento de 15 pontos na IQ vai resultar em uma eleva��o de 13,21% nos salarios
delta.qi <- 15
impacto <- reglog$coefficients[2]*delta.qi
