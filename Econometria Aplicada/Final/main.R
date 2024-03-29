#//////////////////////////////////////////////////////////////////////////////#
# setwd("pasta com a planilha dos dados")

# Dados abaixo selecionados para munic�pios de Alagoas
# A PARTIR DAQUI � NECESS�RIO O PACOTE "readxl"
dados <- readxl::read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet =2)
dicio <- readxl::read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet =1)

# Vari�veis selecionadas na tabela, para usar na regress�o
dados_reg <- subset(dados[ , c(1, 5, 8, 17, 76, 77, 79, 92, 234)], ANO ==2010)
names(dados_reg) <- c("ANO", "NM_MUNICIP", "MORT1", "T_ANALF15M", "GINI",
	"PIND", "PMPOB", "RDPC", "IDHM" )

# Para visualizar o dicion�rio
View(dicio)

### Modelos de Regress�o
# Para pobreza
regpob <- lm(log(PMPOB) ~  log(GINI) + log(RDPC), dados_reg)

# Para pobreza extrema
regexpob <- lm(log(PIND) ~  log(GINI) + log(RDPC), dados_reg)

### Testes, come�ando pelo final
# A PARTIR DAQUI, S�O NECESS�RIOS OS PACOTES "lmtest" e "sandwich"

# Estimadores robustos no modelo heteroscedastico
regpobhc <- lmtest::coeftest(regpob,
	vcov = sandwich::vcovHC(regpob, type ="HC4")

regpobhcmtx <- matrix(c(regpobhc[1:12]), nrow =3)
rownames(regpobhcmtx) <- c("(Intercept)", "log(GINI)", "log(RDPC)")
colnames(regpobhcmtx) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

list(Pobreza =regpobhcmtx, Extrema_Pobreza =summary(regexpob)$coefficients)


# Tabela Heteroscedasticidade (BP)
bpmtx <-data.frame(c(lmtest :: bptest(regpob)[1],lmtest :: bptest(regpob)[4]), 
	c(lmtest :: bptest(regexpob)[1], lmtest :: bptest(regexpob)[4]),
	nrow =2)

# Visualiza��o dos res�duos: Densidade
plot(density(regpob$residuals),
	main ="Gr�fico 1: Distribui��o dos erros no Modelo Pobreza",
	xlab ="", ylab ="", lwd=4, axes =FALSE) + 
		axis(1, at =seq(-0.2:0.15, by =0.05)) +
		abline(v =mean(regpob$residuals), lwd=2, lty=8)
plot(density(regexpob$residuals),
	main ="Gr�fico 2: Distribui��o dos erros no Modelo Pobreza Extrema",
	xlab ="", ylab ="", lwd=4, axes =FALSE)+
		axis(1, at= seq(-0.5:0.4, by =0.1)) +
		abline(v =mean(regexpob$residuals), lwd=2, lty=8)

#//////////////////////////////////////////////////////////////////////////////#
