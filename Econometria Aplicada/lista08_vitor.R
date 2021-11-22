setwd("C:/Users/vflin/Documents/ECONOMIA/Econometria Aplicada/Datasets")
##########    VITOR FERREIRA LINS    ##########

##### QUESTÃO 1
load("kielmc.RData")

# Letra A
# delta_0 deve ser positivo, pois se um apartamento mantem distâncias crescentes à in_
# cineradores, seu preço deve aumentar.
# Um beta_1 positivo significaria que aumentos percentuais na distância ao incinerador
# têm efeitos percentuais no preço, igual o valor de beta_1.

# Letra B
reg01 <- lm(log(price) ~ y81*log(dist), data)
reg01$coefficients
# Os estimadores apresentam os sinais indicados no item anterior.
# Além da tendência de valorização ao longo do tempo (efeito de y81), existe o efeito
# de log(dist) discutido acima. Certamente este útimo afeta mais o valor do parâmetro,
# pois como observado, y81 possui parâmetro negativo.

# Letra C
reg02 <- lm(log(price) ~ y81*log(dist) + age + I(age^2) + rooms + baths + log(intst) +
	log(land) + log(area) , data)
summary(reg02)$coefficients
# O valor e a significância estatística de log(dist) diminuiram. Pode se interpretar
# que a distância ao incinerador não é tão relevante quanto as outras variáveis adi_
# cionadas.

# Letra D
# Aumentar a especificação do modelo reduziu os valores estimados de log(dist) e
# consequentemente, tornou a hipótese h_o dos testes t mais diffíceis de rejeitar.

##### QUESTÃO 2
rm("data", "desc", "self")
load ("rental.RData")

# Letra A
reg03 <- lm(lrent ~ y90 + lpop + lavginc + log(pctstu), data)
reg03$coefficients
# Em 1990 o nível de renda geral é maior.

# Letra B
lmtest::bptest(reg03)
# Não, pois a hipótese de heteroscedasticidade é fraca.

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
# O novo modelo teve sua hipótese de heteroscedasticidade enfraquecida.

list(POOLING =reg03$coefficients, PAINEL =reg04$coefficients)
# O effeito observado é maior no modelo de painel.

sdmt <- car::hccm(reg04, type="hc4")
lmtest::coeftest(reg04, vcov=sdmt)
# Neste caso, os níveis de significância foram pouco afetados, portanto, não altera as
# conclusões feitas anteriormente sobre significância e efeito dos estimadores.

##### ////////////////////////////// #####
rm(list =ls())
