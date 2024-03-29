setwd("C:/Users/vflin/Documents/ECONOMIA/EconometriaAplicada/Datasets")
##########    VITOR FERREIRA LINS    ##########

##### QUEST�O 1
load("pntsprd.RData")

### Letra A
# Ao medir a chance de vit�ria de um time vencer pelo spread, teremos y=beta_0 _
# quando n�o houver spread, ou seja, quando n�o houver time favorito, a chance _
# de vit�ria dever� ser igual para ambos os times.

### Letra B
reg01 <- lm(favwin ~ spread, data =data)
(reg01$coefficients[1] - 0.5) / (sd(fitted(reg01)) /  dim(data)[1] -1)
# Dado o valor do teste t, a hip�tese h_0: intercepto =0.5 � fraca e n�o pode _
# ser rejeitada.

### Letra C
summary(reg01)$coefficients
# Segundo o teste t observado, spread � estat�sticamente relevante.
0.57695 + 10*0.01937
# A chance � de 77,065%.

### Letra D
reg02 <- glm(favwin ~ spread, data, family =binomial(link =probit))
# O efeito parcial do intercepto n�o deve distanciar o valor estimado de 0.5
summary(reg02)$coefficients
# A hip�tese nula h_0: intercept =0, � muito fraca e n�o pode ser rejeitada

### Letra E
-0.01059189 + 0.09246286*10
# A chance de vit�ria � de 91,40367%, s�o 14,33867% de diferen�a.

### Letra F
reg03 <- glm(favwin ~ spread + favhome + fav25 + und25, data, 
	family =binomial(link =probit))
lmtest::lrtest(reg02, reg03)
# O teste retorna h_0 n�o rejeit�vel a nenhum n�vel de signific�ncia usual.

##### QUEST�O 2
rm(list =c("data", "desc", "self"))
load("loanapp.RData")

### Letra A
# O valor seria positivo, pessoas brancas seriam aceitas mais frequentemente.

### Letra B
reg04 <- lm(approve ~ white, data)
summary(reg04)$coefficients
# O valor obtido na regress�o � significativo, e, na pr�tica, pessoas brancas _
# possuem 20,06% mais chance de serem aceitos, o que seria um efeito gande.

### Letra C
reg05 <- lm(approve ~ white + hrat + obrat + loanprc + unem + male + married +
	dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data)
list(white =summary(reg05)$coefficients[2, ])
# Seu valor e sua estat�stica t caem, mas o valor continua relevante, e a _
# estat�stica continua significante

### Letra D
reg06 <- glm(approve ~ white, data, family =binomial(link =probit))
summary(reg06)$coefficients
# O valor do par�metro de white � maior e continua significante.

### Letra E
reg07 <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married +
	dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data, 
	family =binomial(link =probit))
list(white =summary(reg07)$coefficients[2, 1:4])
# Acontece o mesmo efeito observado anteriormente, valor e estat�sticas caem, _
# mas ainda existe evid�ncia de discrimina��o, com estat�stica ainda relevante.

### Letra F
reg08 <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married +
	dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr, data, 
	family =binomial(link =logit))
list(logit.white =summary(reg08)$coefficients[2, ],
	probit.white =summary(reg07)$coefficients[2, ])
# A estat�stica mant�m relev�ncia similar, mas o efeito � superior no logit.

### Letra G
# Exibindo efeitos parciais m�dios
list(LOGIT.EfParcMedio =mfx::logitmfx(reg08, atmean =FALSE, data)$mfxest,
	PROBIT.EfParcMEdio =mfx::logitmfx(reg07, atmean =FALSE, data)$mfxest)

# Exibindo efeitos parciais na m�dia
list(LOGIT.EfParcNaMedia =mfx::logitmfx(reg08, atmean =TRUE, data)$mfxest,
	PROBIT.EfParcNaMedia =mfx::logitmfx(reg07, atmean =TRUE, data)$mfxest)

##### ////////////////////////////// #####
rm(list =ls())
