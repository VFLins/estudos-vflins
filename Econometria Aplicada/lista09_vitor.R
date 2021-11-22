setwd("C:/Users/vflin/Documents/ECONOMIA/Econometria Aplicada/Final")
##########    VITOR FERREIRA LINS    ##########

# "Mapas.zip" acompanha os dados usados, bem como as imagens geradas

# Dados foram selecionados para municípios de Alagoas
# A partir daqui, é necessário o pacote "readxl"
dados <- readxl::read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet =2)

dados_reg <- subset(dados[ , c(1, 5, 8, 17, 76, 77, 79, 92, 234)],
	ANO ==2010)
names(dados_reg) <- c("ANO", "NM_MUNICIP", "MORT1", "T_ANALF15M", "GINI",
	"PIND", "PMPOB", "RDPC", "IDHM" )

############################
# Trabalhando com os dados:
############################

# A partir daqui, são necessários os pacotes "dplyr" e "sf"
mapa <- sf::st_read("Mapas/27MUE250GC_SIR.shp")
mapaedados <- dplyr::inner_join(mapa, dados_reg)

############################
# Criando mapas:
############################

########################################################################
##### Método "tmap"
# A partir daqui, é necessário o pacote "tmap"
library(tmap)
graph01 <- tm_shape(mapaedados, name ="IDH por municÃ­pio de Alagoas") +
	tm_polygons("IDHM", id ="NM_MUNICIP", palette ="Blues")
graph01

########################################################################
##### Método "ggplot2"
# A partir daqui, é necessário o pacote "ggplot2"
library(ggplot2)
graph02 <- ggplot(mapaedados) +
	geom_sf(aes(fill =IDHM)) +
	scale_fill_gradient(low ="White", high ="#008080") +
	ggtitle("IDH por município de Alagoas em 2010") +
	theme(panel.grid =element_line(color ="transparent"), 
		panel.background =element_blank(),
		axis.text =element_blank(),
		axis.ticks =element_blank()
		)
graph02

##### ////////////////////////////// #####
rm(list =ls())
