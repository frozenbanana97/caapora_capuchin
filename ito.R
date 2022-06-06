##### 
##### Ajustando banco de dados para a análise de estratos
##### Confirmando a pasta de trabalho,lendo dados e corrigindo as variáveis:
##### 

pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
			 "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
			 "cowplot", "ggpubr", "GGally", "readxl", "ggplot2", "gridExtra", "dplyr", "circular", "factoextra", "mice", "leaflet", "leaflet.extras")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
	instalador <- pacotes[!pacotes %in% installed.packages()]
	for(i in 1:length(instalador)) {
		install.packages(instalador, dependencies = T)
		break()}
	sapply(pacotes, require, character = T) 
} else {
	sapply(pacotes, require, character = T) #script pra automatizar se pacotes n instalados: instalar e rodar
}


getwd()
Dados <- read.csv("gps_data.csv", sep = ";")

Dados$Mês <- factor(Dados$Mês, levels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
Dados$Ano <- factor(Dados$Ano)
Dados$Estr_1 <- factor(Dados$Estr_1, levels = c("Árvore", "Solo"))
Dados$Estr_2 <- factor(Dados$Estr_2, levels = c("Superior", "Médio", "Inferior", "Solo"))

Dados$Clas_1 <- factor(Dados$Clas_1, c("MA", "FA", "SA", "Juv", "FF", "FG"))
Dados["Clas_1"][Dados["Clas_1"] == "FG"] <- "FA"
Dados["Clas_1"][Dados["Clas_1"] == "FF"] <- "FA"
Dados$Clas_1 <- factor(Dados$Clas_1, levels = c("MA", "FA", "SA", "Juv"))

###
### Criando variáveis clusters
###

load("Dados_clusters.RData")

Dados$cluster_FAI <- as.numeric(Dados$Mês)
Dados$cluster_FAI2 <- as.numeric(Dados$Mês)
Dados$cluster_Invertebrados <- as.numeric(Dados$Mês)

Dados$cluster_Tmin <- as.numeric(Dados$Mês)
Dados$cluster_Tmed <- as.numeric(Dados$Mês)
Dados$cluster_Tmax <- as.numeric(Dados$Mês)
Dados$cluster_Prec <- as.numeric(Dados$Mês)

Dados$Mês <- as.numeric(Dados$Mês)

# Atribuindo temporadas às variáveis clusters definidas:

Dados$cluster_FAI <- ifelse (Dados$cluster_FAI > 1 & Dados$cluster_FAI < 4, "Alta", "Baixa")
Dados$cluster_FAI2 <- ifelse (Dados$cluster_FAI2 > 1 & Dados$cluster_FAI2 < 5, "Alta", "Baixa")
Dados$cluster_Invertebrados <- ifelse (Dados$cluster_Invertebrados > 1 & Dados$cluster_Invertebrados < 8, "Alta", "Baixa")

Dados$cluster_Prec <- ifelse (Dados$cluster_Prec > 2 & Dados$cluster_Prec < 8, "Alta", "Baixa")
Dados$cluster_Tmin <- ifelse (Dados$cluster_Tmin > 4 & Dados$cluster_Tmin < 10, "Baixa", "Alta")
Dados$cluster_Tmed <- ifelse (Dados$cluster_Tmed > 5 & Dados$cluster_Tmed < 10, "Baixa", "Alta")
Dados$cluster_Tmax <- ifelse (Dados$cluster_Tmax > 5 & Dados$cluster_Tmax < 10, "Baixa", "Alta")


#####
##### Carregando estágios de regeneração
#####

DadosBackup <- Dados # Não rodar mais
Dados <- DadosBackup

# Removendo linhas baseado nos NA's da coluna Longitude.

Dados <- Dados[!is.na(Dados$Longitude),] 

reg <- readOGR("Regeneration stages/Regeneration_stages.shp")
reg <- spTransform(reg, CRS= "+proj=utm +zone=25 +south ellps=WGS84")

Dados$X<- project(cbind(Dados$Longitude, Dados$Latitude),
				  "+proj=utm +zone=25 +south ellps=WGS84")[,1]
Dados$Y<- project(cbind(Dados$Longitude, Dados$Latitude),
				  "+proj=utm +zone=25 +south ellps=WGS84")[,2]

Dados<- SpatialPointsDataFrame(Dados[c("X","Y")], Dados[, c(1:19)],
							   proj= CRS("+proj=utm +zone=25 +south ellps=WGS84"))

# Adicionando coluna relativa ao estágio de regeneração:

Dados@data$Reg_Stage <- extract(rasterize(reg, raster(extent(reg), res= 10), "Stage"), Dados, factors= T)

reg@data$Bor_Int <- factor(reg@data$Bor_Int)

Dados$Bor_Int <- extract(rasterize(reg, raster(extent(reg), res= 10), "Bor_Int"), Dados, factors= T)
View(Dados@data)

plot(reg)
# Obs. Note que alguns pontos estão realmente fora do fragmento...

#####
##### Análise de proximidade à borda
#####

patch <- readOGR("Forest Patch/Forest_patch.kml", "Poligono de teste")
patch<- spTransform(patch, CRS= "+proj=utm +zone=25 +south ellps=WGS84")

for(i in 1:nrow(Dados)){
	Dados$Dist2Edge[i]<- gDistance(Dados[i,], as(patch, "SpatialLinesDataFrame"))
}; rm(i)
plot(patch)
# Removendo dados extremamente fora do fragmento

Dados <- Dados[row.names(Dados)!= 538L, ]
Dados <- Dados[row.names(Dados)!= 539L, ]
Dados <- Dados[row.names(Dados)!= 540L, ]
Dados <- Dados[row.names(Dados)!= 541L, ]
Dados <- Dados[row.names(Dados)!= 542L, ]
Dados <- Dados[row.names(Dados)!= 543L, ]
Dados <- Dados[row.names(Dados)!= 544L, ]
Dados <- Dados[row.names(Dados)!= 545L, ]
Dados <- Dados[row.names(Dados)!= 546L, ]
Dados <- Dados[row.names(Dados)!= 547L, ]
Dados <- Dados[row.names(Dados)!= 548L, ]
Dados <- Dados[row.names(Dados)!= 549L, ]
Dados <- Dados[row.names(Dados)!= 550L, ]
Dados <- Dados[row.names(Dados)!= 551L, ]
Dados <- Dados[row.names(Dados)!= 552L, ]
Dados <- Dados[row.names(Dados)!= 553L, ]
Dados <- Dados[row.names(Dados)!= 554L, ]
Dados <- Dados[row.names(Dados)!= 555L, ]
Dados <- Dados[row.names(Dados)!= 556L, ]
Dados <- Dados[row.names(Dados)!= 557L, ]
Dados <- Dados[row.names(Dados)!= 558L, ]
Dados <- Dados[row.names(Dados)!= 559L, ]
Dados <- Dados[row.names(Dados)!= 560L, ]
Dados <- Dados[row.names(Dados)!= 561L, ]
Dados <- Dados[row.names(Dados)!= 562L, ]
Dados <- Dados[row.names(Dados)!= 563L, ]
Dados <- Dados[row.names(Dados)!= 564L, ]
Dados <- Dados[row.names(Dados)!= 565L, ]
Dados <- Dados[row.names(Dados)!= 566L, ]
Dados <- Dados[row.names(Dados)!= 567L, ]
Dados <- Dados[row.names(Dados)!= 568L, ]
Dados <- Dados[row.names(Dados)!= 569L, ]
Dados <- Dados[row.names(Dados)!= 570L, ]
Dados <- Dados[row.names(Dados)!= 571L, ]
Dados <- Dados[row.names(Dados)!= 572L, ]
Dados <- Dados[row.names(Dados)!= 573L, ]
Dados <- Dados[row.names(Dados)!= 574L, ]
Dados <- Dados[row.names(Dados)!= 575L, ]
Dados <- Dados[row.names(Dados)!= 576L, ]
Dados <- Dados[row.names(Dados)!= 577L, ]
Dados <- Dados[row.names(Dados)!= 578L, ]
Dados <- Dados[row.names(Dados)!= 579L, ]
Dados <- Dados[row.names(Dados)!= 580L, ]
Dados <- Dados[row.names(Dados)!= 581L, ]
Dados <- Dados[row.names(Dados)!= 582L, ]
Dados <- Dados[row.names(Dados)!= 583L, ]
Dados <- Dados[row.names(Dados)!= 584L, ]
Dados <- Dados[row.names(Dados)!= 585L, ]
Dados <- Dados[row.names(Dados)!= 586L, ]
Dados <- Dados[row.names(Dados)!= 587L, ]
Dados <- Dados[row.names(Dados)!= 588L, ]
Dados <- Dados[row.names(Dados)!= 589L, ]
Dados <- Dados[row.names(Dados)!= 590L, ]
Dados <- Dados[row.names(Dados)!= 591L, ]
Dados <- Dados[row.names(Dados)!= 592L, ]
Dados <- Dados[row.names(Dados)!= 593L, ]
Dados <- Dados[row.names(Dados)!= 594L, ]
Dados <- Dados[row.names(Dados)!= 595L, ]
Dados <- Dados[row.names(Dados)!= 596L, ]
Dados <- Dados[row.names(Dados)!= 597L, ]
Dados <- Dados[row.names(Dados)!= 598L, ]
Dados <- Dados[row.names(Dados)!= 599L, ]
Dados <- Dados[row.names(Dados)!= 600L, ]
Dados <- Dados[row.names(Dados)!= 601L, ]
Dados <- Dados[row.names(Dados)!= 602L, ]
Dados <- Dados[row.names(Dados)!= 603L, ]
Dados <- Dados[row.names(Dados)!= 604L, ]
Dados <- Dados[row.names(Dados)!= 605L, ]
Dados <- Dados[row.names(Dados)!= 606L, ]
Dados <- Dados[row.names(Dados)!= 607L, ]

Dados <- Dados[row.names(Dados)!= 1L, ]
Dados <- Dados[row.names(Dados)!= 2L, ]
Dados <- Dados[row.names(Dados)!= 3L, ]
Dados <- Dados[row.names(Dados)!= 4L, ]
Dados <- Dados[row.names(Dados)!= 5L, ]
Dados <- Dados[row.names(Dados)!= 6L, ]
Dados <- Dados[row.names(Dados)!= 7L, ]
Dados <- Dados[row.names(Dados)!= 8L, ]
Dados <- Dados[row.names(Dados)!= 9L, ]
Dados <- Dados[row.names(Dados)!= 10L, ]
Dados <- Dados[row.names(Dados)!= 11L, ]
Dados <- Dados[row.names(Dados)!= 12L, ]
Dados <- Dados[row.names(Dados)!= 13L, ]
Dados <- Dados[row.names(Dados)!= 14L, ]
Dados <- Dados[row.names(Dados)!= 15L, ]
Dados <- Dados[row.names(Dados)!= 16L, ]
Dados <- Dados[row.names(Dados)!= 17L, ]
Dados <- Dados[row.names(Dados)!= 18L, ]
Dados <- Dados[row.names(Dados)!= 19L, ]
Dados <- Dados[row.names(Dados)!= 20L, ]
Dados <- Dados[row.names(Dados)!= 21L, ]
Dados <- Dados[row.names(Dados)!= 22L, ]
Dados <- Dados[row.names(Dados)!= 23L, ]
Dados <- Dados[row.names(Dados)!= 24L, ]
Dados <- Dados[row.names(Dados)!= 25L, ]
Dados <- Dados[row.names(Dados)!= 26L, ]
Dados <- Dados[row.names(Dados)!= 27L, ]
Dados <- Dados[row.names(Dados)!= 28L, ]
Dados <- Dados[row.names(Dados)!= 29L, ]
Dados <- Dados[row.names(Dados)!= 30L, ]

Dados <- Dados[row.names(Dados)!= 2671L, ]
Dados <- Dados[row.names(Dados)!= 2672L, ]
Dados <- Dados[row.names(Dados)!= 2673L, ]
Dados <- Dados[row.names(Dados)!= 2674L, ]
Dados <- Dados[row.names(Dados)!= 2675L, ]
Dados <- Dados[row.names(Dados)!= 2676L, ]
Dados <- Dados[row.names(Dados)!= 2677L, ]
Dados <- Dados[row.names(Dados)!= 2678L, ]
Dados <- Dados[row.names(Dados)!= 2679L, ]
Dados <- Dados[row.names(Dados)!= 2680L, ]
Dados <- Dados[row.names(Dados)!= 2681L, ]
Dados <- Dados[row.names(Dados)!= 2682L, ]
Dados <- Dados[row.names(Dados)!= 2683L, ]
Dados <- Dados[row.names(Dados)!= 2684L, ]
Dados <- Dados[row.names(Dados)!= 2685L, ]
Dados <- Dados[row.names(Dados)!= 2686L, ]
Dados <- Dados[row.names(Dados)!= 2687L, ]
Dados <- Dados[row.names(Dados)!= 2688L, ]
Dados <- Dados[row.names(Dados)!= 2689L, ]
Dados <- Dados[row.names(Dados)!= 2690L, ]
Dados <- Dados[row.names(Dados)!= 2691L, ]
Dados <- Dados[row.names(Dados)!= 2692L, ]
Dados <- Dados[row.names(Dados)!= 2693L, ]
Dados <- Dados[row.names(Dados)!= 2694L, ]
Dados <- Dados[row.names(Dados)!= 2695L, ]
Dados <- Dados[row.names(Dados)!= 2696L, ]
Dados <- Dados[row.names(Dados)!= 2697L, ]
Dados <- Dados[row.names(Dados)!= 2698L, ]
Dados <- Dados[row.names(Dados)!= 2699L, ]
Dados <- Dados[row.names(Dados)!= 2700L, ]
Dados <- Dados[row.names(Dados)!= 2701L, ]
Dados <- Dados[row.names(Dados)!= 2702L, ]
Dados <- Dados[row.names(Dados)!= 2703L, ]
Dados <- Dados[row.names(Dados)!= 2704L, ]
Dados <- Dados[row.names(Dados)!= 2705L, ]
Dados <- Dados[row.names(Dados)!= 2706L, ]
Dados <- Dados[row.names(Dados)!= 2707L, ]
Dados <- Dados[row.names(Dados)!= 2708L, ]
Dados <- Dados[row.names(Dados)!= 2709L, ]
Dados <- Dados[row.names(Dados)!= 2710L, ]
Dados <- Dados[row.names(Dados)!= 2711L, ]
Dados <- Dados[row.names(Dados)!= 2712L, ]
Dados <- Dados[row.names(Dados)!= 2713L, ]
Dados <- Dados[row.names(Dados)!= 2714L, ]
Dados <- Dados[row.names(Dados)!= 2715L, ]
Dados <- Dados[row.names(Dados)!= 2716L, ]
Dados <- Dados[row.names(Dados)!= 2717L, ]
Dados <- Dados[row.names(Dados)!= 2718L, ]
Dados <- Dados[row.names(Dados)!= 2719L, ]

Dados <- Dados[row.names(Dados)!= 639L, ]
Dados <- Dados[row.names(Dados)!= 640L, ]
Dados <- Dados[row.names(Dados)!= 641L, ]
Dados <- Dados[row.names(Dados)!= 642L, ]
Dados <- Dados[row.names(Dados)!= 643L, ]
Dados <- Dados[row.names(Dados)!= 644L, ]
Dados <- Dados[row.names(Dados)!= 645L, ]
Dados <- Dados[row.names(Dados)!= 646L, ]
Dados <- Dados[row.names(Dados)!= 647L, ]
Dados <- Dados[row.names(Dados)!= 648L, ]
Dados <- Dados[row.names(Dados)!= 649L, ]
Dados <- Dados[row.names(Dados)!= 650L, ]
Dados <- Dados[row.names(Dados)!= 651L, ]
Dados <- Dados[row.names(Dados)!= 652L, ]
Dados <- Dados[row.names(Dados)!= 653L, ]
Dados <- Dados[row.names(Dados)!= 654L, ]
Dados <- Dados[row.names(Dados)!= 655L, ]
Dados <- Dados[row.names(Dados)!= 656L, ]
Dados <- Dados[row.names(Dados)!= 657L, ]
Dados <- Dados[row.names(Dados)!= 658L, ]
Dados <- Dados[row.names(Dados)!= 659L, ]
Dados <- Dados[row.names(Dados)!= 660L, ]
Dados <- Dados[row.names(Dados)!= 661L, ]
Dados <- Dados[row.names(Dados)!= 662L, ]
Dados <- Dados[row.names(Dados)!= 663L, ]
Dados <- Dados[row.names(Dados)!= 664L, ]
Dados <- Dados[row.names(Dados)!= 665L, ]
Dados <- Dados[row.names(Dados)!= 666L, ]

Dados <- Dados[row.names(Dados)!= 667L, ]
Dados <- Dados[row.names(Dados)!= 668L, ]
Dados <- Dados[row.names(Dados)!= 669L, ]
Dados <- Dados[row.names(Dados)!= 670L, ]
Dados <- Dados[row.names(Dados)!= 671L, ]
Dados <- Dados[row.names(Dados)!= 672L, ]
Dados <- Dados[row.names(Dados)!= 673L, ]
Dados <- Dados[row.names(Dados)!= 674L, ]
Dados <- Dados[row.names(Dados)!= 675L, ]
Dados <- Dados[row.names(Dados)!= 676L, ]
Dados <- Dados[row.names(Dados)!= 677L, ]
Dados <- Dados[row.names(Dados)!= 678L, ]
Dados <- Dados[row.names(Dados)!= 679L, ]
Dados <- Dados[row.names(Dados)!= 680L, ]
Dados <- Dados[row.names(Dados)!= 681L, ]
Dados <- Dados[row.names(Dados)!= 682L, ]
Dados <- Dados[row.names(Dados)!= 683L, ]
Dados <- Dados[row.names(Dados)!= 684L, ]
Dados <- Dados[row.names(Dados)!= 685L, ]
Dados <- Dados[row.names(Dados)!= 686L, ]
Dados <- Dados[row.names(Dados)!= 687L, ]
Dados <- Dados[row.names(Dados)!= 688L, ]
Dados <- Dados[row.names(Dados)!= 689L, ]

rm(patch)
rm(reg)

# Salvando banco de dados, para não precisar rodar novamente até aqui
save("Dados", file = "Dados_estratos.RData")

