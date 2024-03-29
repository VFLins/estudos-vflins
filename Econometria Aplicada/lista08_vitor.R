setwd("C:/Users/vflin/Documents/ECONOMIA/Econometria Aplicada/Datasets")
##########    VITOR FERREIRA LINS    ##########

##### QUEST�O 1
load("kielmc.RData")

# Letra A
# delta_0 deve ser positivo, pois se um apartamento mantem dist�ncias crescentes � in_
# cineradores, seu pre�o deve aumentar.
# Um beta_1 positivo significaria que aumentos percentuais na dist�ncia ao incinerador
# t�m efeitos percentuais no pre�o, igual o valor de beta_1.

# Letra B
reg01 <- lm(log(price) ~ y81*log(dist), data)
reg01$coefficients
# Os estimadores apresentam os sinais indicados no item anterior.
# Al�m da tend�ncia de valoriza��o ao longo do tempo (efeito de y81), existe o efeito
# de log(dist) discutido acima. Certamente este �timo afeta mais o valor do par�metro,
# pois como observado, y81 possui par�metro negativo.

# Letra C
reg02 <- lm(log(price) ~ y81*log(dist) + age + I(age^2) + rooms + baths + log(intst) +
	log(land) + log(area) , data)
summary(reg02)$coefficients
# O valor e a signific�ncia estat�stica de log(dist) diminuiram. Pode se interpretar
# que a dist�ncia ao incinerador n�o � t�o relevante quanto as outras vari�veis adi_
# cionadas.

# Letra D
# Aumentar a especifica��o do modelo reduziu os valores estimados de log(dist) e
# consequentemente, tornou a hip�tese h_o dos testes t mais diff�ceis de rejeitar.

##### QUEST�O 2
rm("data", "desc", "self")
load ("rental.RData")

# Letra A
reg03 <- lm(lrent ~ y90 + lpop + lavginc + log(pctstu), data)
reg03$coefficients
# Em 1990 o n�vel de renda geral � maior.

# Letra B
lmtest::bptest(reg03)
# N�o, pois a hip�tese de heteroscedasticidade � fraca.

# Letra C e D
mtx <- plm::pdata.frame(data, index =64)
pdlrent <- diff(mtx$lrent)
pdy90 <- diff(mtx$y90)
pdlpop <- diff(mtx$lpop)
pdlavginc <- diff(mtx$lavginc)
pdlpcststu <- diff(log(mtx$pctstu))

pddata <- data.frame(pdlrent, pdy90, pdlpop, pdlavginc, pdlpcststu)
reg04 <- lm(pdlrent ~ + pdlpop + pdlavginc + pdlpcststu, pddata)
#reg04 <- plm::plm(lrent ~ y90 + lpop + lavginc + log(pctstu), data =mtx, 
#	model ="within")
lmtest::bptest(reg04)
# O novo modelo teve sua hip�tese de heteroscedasticidade enfraquecida.

list(POOLING =reg03$coefficients, PAINEL =reg04$coefficients)
# O effeito observado � maior no modelo de painel.

sdmt <- car::hccm(reg04, type="hc4")
lmtest::coeftest(reg04, vcov=sdmt)
# Neste caso, os n�veis de signific�ncia foram pouco afetados, portanto, n�o altera as
# conclus�es feitas anteriormente sobre signific�ncia e efeito dos estimadores.

##### ////////////////////////////// #####
rm(list =ls())
