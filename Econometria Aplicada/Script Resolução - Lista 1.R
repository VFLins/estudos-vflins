#lista de exercicio 1


############ Questão 1 ######################

#carregando a base de dados
load("bwght.RData")
attach(data)

# item a)

#exibindo a listagem da estrutura, onde pode ser visto o número de observações. 
#1388 obs.
str(data)

#Criando um subgrupo só de mães que fumaram na gravidez
fumgrav <- subset(data, cigs!=0)
nrow(fumgrav) #Encontrando o numero de linhas (e portanto de observações que satisfazem a condição)

# A abordagem usando subset é interessante quando vc quiser manter todas as variáveis que satisfazem aquele critério. Por exemplo,
# no comando acima, todas as informações das mães fumantes serão mantidas no novo dataframe

# Um outra opção é utilizar a referenciação a elementos de um vetor, focando apenas no vetor que armazena as informações de cigs
# Como feito pelo colega John.

sum(cigs>0)

# Outro comando interessante é o str, que exibe a listagem da estrutura, onde pode ser visto o número de observações. 
#212 obs. (Numero bem pequeno de fumantes em relação ao total)
str(fumgrav)

# item b) 

#media de cigarros fumados. Aqui consideramos todas as mulheres. Até não fumantes
mean(cigs)

# item c)

#media de cigarros fumados na gravidez. Aqui consideramos apenas mulheres que fumaram na gravidez. 
mean(fumgrav$cigs)

# Ou sem usar o subset, usando apenas referenciação ao vetor
mean(cigs[cigs>0])

#Nota-se que como muitas mulheres da amostra não fumam, o média do conjunto total é relativamente baixa. A mullher típica é 
# uma não fumante. Quando se exclui as não fumantes temos um número mais representativo do número de cicarros fumados pelas 
# mulheres fumantes.

# item d) 

#media da educação dos pais descnsiderando os valores "NA" (não disponível)
mean(data$fatheduc, na.rm = "TRUE")


#Criando um subgrupo só com pais que registraram escolaridade. Explicando porque nem todas as obsevações foram consideradas na média
# algumas observações não tem valor para esta variavel. assim, são desconsiderados na média
pais <- subset(data, fatheduc>0)
nrow(pais)

#exibindo a listagem da estrutura, onde pode ser visto o número de observações
str(pais)

# e) 

#Calculando Renda média e desvio padrão em dólares
mean(faminc)
sd(faminc)


detach(data)
rm(list = ls())

################ Questão 2:######################
#carregando a base de dados
load("jtrain2.RData")
attach(data)

##### item a) 

# Criando subgrupos de trabalhadores treinados e não treinados
treinados <- subset(data, train == 1)
numero_treinados <- nrow(treinados)
ntreinados <- subset(data, train == 0)
numero_nao_treinados <- nrow(ntreinados)

#Aqui novamente é possível fazer utilizando apenas a referencia a vetores (ver questão anterior)

##### item b)

#Calculando as medias de ganhos no ano de 78 para os dois subgrupos (treinados e não treinados)
mean(treinados$re78)
mean(ntreinados$re78)

##### item c)

#Criando subgrupos para separar empregados e desempregrados dos trabahadores treinados e não treinados
treinados_emp <- subset(treinados, unem78 == 0)
treinados_desemp <- subset(treinados, unem78 == 1)

ntreinado_emp <- subset(ntreinados, unem78 == 0)
ntreinados_desemp <- subset(ntreinados, unem78 == 1)


#proporção de treinados desempregados em relação a todos os treinados
(proporcao1 <- nrow(treinados_desemp)/nrow(treinados))

#proporção de não treinados desempregos em relação a todos os não treinados
(proporcao2 <- nrow(ntreinados_desemp)/nrow(ntreinados))

#diferença entre as proporções
(diff <- proporcao1 - proporcao2) 

#portanto, existem mais não treinados desempregados, do que o contrário.

##### item d)

#funcionários treinados receberem em médias maiores salários que os funcionários não treinados e a proporção de funcionários treinados que estão desempregados (24,3%) é inferior 
#a dos funcionários não treinados que estão deseempregados (35,3%), o programa aparenta ter sido efetivo. Mas para responder essa
#pergunta de forma mais convincente será necessário utilizar modelos de regressão que aprenderemos nas proximas aulas.

detach(data)
rm(list = ls())
################ Questão 3:######################

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

# permanencia máxima
max(ceoten)


##### item c)
#estimando a regressão
(reg <- lm(lsalary ~ ceoten ))

#com tudo mais constante, o aumento de um ano do funcionario como ceo eleva seu salário em aproximadamente 0,9724%


detach(data)
rm(list = ls())
################ Questão4 ######################

#carregando a base de dados
load("sleep75.RData")
attach(data)

##### item a)
#estimando a regressão
(reg <- lm(sleep ~  totwrk))

#registrando resultados da reg + n de obs + r2
summary(reg)
coeficientes <- coef(reg)
numobs <- nobs(reg) 
r2 <- cor(data$sleep, data$totwrk)^2 # terceira maneira de calcular R2 é calcular o quadrado do coeficiente de correlação entre y e x)

#o intercepto indica que se a pessoa não trabalhar nenhum minuto na semana, a quantidade de minutos dormindos estimados por
#semana é de 3.586.


##### item b)
delta.trabalho <- 120 # duas horas a mais de trabalho, ou seja, 120 minutos

(impacto <- reg$coefficients[2]*delta.trabalho)
#supondo que totwrk aumentar 2 hrs semanais, sleep deve cair em 18 min por semana, valor relativamente pequeno.


detach(data)
rm(list = ls())

################ Questão 5 ######################

#carregando a base de dados
load("wage2.RData")
attach(data)

#### item a)
#encontrando o salario e iq medios e o desvio padrão amostral de IQ
mean(data$wage)
mean(data$IQ)
sd(data$IQ, na.rm = TRUE)

#também é possível usar o comando summary, como sugerido na aula síncrona dessa semana.

#### item b)

#estimando a regressão
(reg <- lm(wage ~  IQ))

#com tudo mais constante, um aumento de um ponto na IQ vai resultar em uma elevação de 8,3 dolares no salario.
#com tudo mais constante, um aumento de 15 pontos na IQ vai resultar em uma elevação de 124,5 dolares no salario
delta.qi <- 15
(impacto <- reg$coefficients[2]*delta.qi)

#encontrando o R2 da regressão
r2 <- cor(wage, IQ)^2
#ele indica que apenas 9,5% da variação de wage está relacionada com o IQ

# as duas outras maneiras de encontrar o r2 são r2 = SQE/SQT = 1 - SQR/SQT

(r2 <- var(reg$fitted.values)/var(wage))
(r2 <- 1 - var(reg$residuals)/var(wage))

#### item c)

#estimando a regressão com log do wage
(reglog <- lm(lwage) ~  IQ)

#com tudo mais constante, um aumento de um ponto na IQ vai resultar em uma elevação de 0,88% nos salarios
#com tudo mais constante, um aumento de 15 pontos na IQ vai resultar em uma elevação de 13,21% nos salarios
delta.qi <- 15
impacto <- reglog$coefficients[2]*delta.qi
