#RODEI ATE 162

rm(list= ls())

library(raster)
library(rgdal)
library(rgeos)


## Import, correct and project spatial data
# GPS data
gpsNA<- read.csv("GPS_data ito.csv", sep = ";")
str(gpsNA)

#fazendo a data e hora se juntarem
gpsNA$Date <- paste(gpsNA$Date, gpsNA$ltime)
gpsNA$ltime <- paste(gpsNA$Date)
View(gpsNA)

#colocando em utm
gpsNA$DateTime<- as.POSIXct(gpsNA$Date, "%d/%m/%Y %H:%M", tz= "Brazil/East") #14-16 transforma na nossa area 
gpsNA$X<- project(cbind(gpsNA$Longitude, gpsNA$Latitude),
                "+proj=utm +zone=25 +south ellps=WGS84")[,1]
gpsNA$Y<- project(cbind(gpsNA$Longitude, gpsNA$Latitude),
                "+proj=utm +zone=25 +south ellps=WGS84")[,2]
gpsNA<- droplevels(gpsNA[order(gpsNA$DateTime), -c(1, 2, 5, 6, 7, 8, 9)]) #tira algumas colunas pra ficar que nem da 22

gps <- na.omit(gpsNA) #remove linhas com NA

gps<- SpatialPointsDataFrame(gps[c("X","Y")], gps[, c(3:5, 2, 1)],
                             proj= CRS("+proj=utm +zone=25 +south ellps=WGS84"))
View(gps@data)


library(leaflet)
library(leaflet.extras) #25 e 26 pacotes para trabalhar com mapa
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= gps@data$Longitude, lat=gps@data$Latitude, radius=1, col="#a6cee3")%>% #raio de 1 cor azul
  
  addScaleBar(position="bottomleft") #mostrar os dados do gps plotados




## Now let's split the data into months/year
View(gps@data)
library(lubridate)
Location_points_Months <- split(gps, f = paste(month(ymd_hms(gps@data$DateTime)), sep="-" ))
Location_points_Year <- split(gps, f = paste(year(ymd_hms(gps@data$DateTime)), sep="-" ))

View(Location_points_Months$`3`@data)

# ok, thats worked to split my data into months/year
# Now, let's try to get home range's and core area's values by these months/years
# Let's try go create a new column with these obtained values

library(dplyr)
library(adehabitatHR)
library(rgeos)


# h paremeter = 70
# kernel method = epa
# heatmap parameters: max = 10, blur = 25, radius = 20
# polygons' parameters:  filopacity = .25, weight = 3, color = white
# points parameters: radius = 1, color = #a6cee3

########################################################################################
# First, expand the grid to avoid problems like "grid too small"
View(Location_points_Year$`2021`@data)

LP_FAI_ALTA <- c(Location_points_Months$`2`, Location_points_Months$`3`) #n precisa rodar



x <- seq(-500, 1000, by=1) 
y <- seq(-500, 1000, by=1)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

##########################################
kudLP_Mar <- kernelUD(Location_points_Months$`3`[,], h=70,  kern = c("epa")) # o h=70 é largura de banda
hrLP_Mar <- getverticeshr(kudLP_Mar, percent =50) #, percent =50 adiciona pra ver o core range 50
summary(Location_points_Months)
Mes <- (c("Mar"))
N <- (21)
kde.areasLP_Mar <- kernel.area(kudLP_Mar, percent=c(50, 95))
kde.areasLP_Mar
HR50 <- (3.594936)
HR95 <- (13.481011) #a partir daqui é um grande loop


sdfLP_Mar <- spTransform(hrLP_Mar, CRS('+init=epsg:4326'))

leaflet(sdfLP_Mar) %>%   
	addProviderTiles(providers$Esri.WorldImagery)%>%
	addCircleMarkers(lng= Location_points_Months$`3`$Longitude, lat=Location_points_Months$`3`$Latitude, radius=1, col="#a6cee3")%>%
	addHeatmap(lng= Location_points_Months$`3`$Longitude, lat=Location_points_Months$`3`$Latitude, max=10,blur=25, radius=20)%>%
	addScaleBar(position="bottomleft")%>%
	addPolygons(weight = 3, fillOpacity = .25, col = "white") #clicar em exportar salvar como img e deixar 500x600


kudLP_2021 <- kernelUD(Location_points_Year$`2021`[,], h=70,  kern = c("epa"))
hrLP_2021 <- getverticeshr(kudLP_2021)
summary(Location_points_Year)
Ano <- c(2021)
N <- c(295)
#antes tava assim
#Ano <- c(2014, 2016)
#N <- c(3684, 194)
kde.areasLP_2021 <- kernel.area(kudLP_2021, percent=c(50, 95))
kde.areasLP_2021
HR50 <- c(12.36208)
HR95 <- c(51.21431)

#a partir daqui n tenho dados

kudLP_2017 <- kernelUD(Location_points_Year$`2017`[,], h=70,  kern = c("epa"))
hrLP_2017 <- getverticeshr(kudLP_2017)
summary(Location_points_Year)
Ano <- c(2014, 2016, 2017)
N <- c(3684, 194, 535)
kde.areasLP_2017 <- kernel.area(kudLP_2017, percent=c(50, 95))
kde.areasLP_2017
HR50 <- c(22.4, 11.9, 12.7)
HR95 <- c(61.1, 53.8, 43.5)


kudLP_2019 <- kernelUD(Location_points_Year$`2019`, h=70,  kern = c("epa"), extent = 0.98)
hrLP_2019 <- getverticeshr(kudLP_2019)
summary(Location_points_Year)
Ano <- c(2014, 2016, 2017, 2019)
N <- c(3684, 194, 535, 840)
kde.areasLP_2019 <- kernel.area(kudLP_2019, percent=c(50, 95))
kde.areasLP_2019
HR50 <- c(22.4, 11.9, 12.7, 13.4)
HR95 <- c(61.1, 53.8, 43.5, 61.5)


kudLP_2020 <- kernelUD(Location_points_Year$`2020`[,], h=70,  kern = c("epa"))
hrLP_2020 <- getverticeshr(kudLP_2020)
summary(Location_points_Year)
Ano <- c(2014, 2016, 2017, 2019, 2020)
N <- c(3684, 194, 535, 840, 244)
kde.areasLP_2020 <- kernel.area(kudLP_2020, percent=c(50, 95))
kde.areasLP_2020
HR50 <- c(22.4, 11.9, 12.7, 13.4, 9.78)
HR95 <- c(61.1, 53.8, 43.5, 61.5, 41.58)

Data_HR_Anual <- data.frame(Ano, N, HR95, HR50)
save(Data_HR_Anual, file = "Data_HR_Anual.RData")

library(kableExtra)
Data_HR_Anual %>%
  kable(caption  = "Tabela 1 - Estimativas de Areas de vida (95%) e Area Central (50%) anuais") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = T, 
                position = "center",
                font_size = 16)
##########################################


#RODEI ATÉ AQUI
# 2014-2 (2014/Febrary)

kudLP_2021.3 <- kernelUD(Location_points_Year$`2021-3`[,], h=70,  kern = c("epa"))
hrLP_2021.3 <- getverticeshr(kudLP_2021.3)
#tava assim
#kudLP_2014.2 <- kernelUD(Location_points_Year$`2014-2`[,], h=70,  kern = c("epa"))

# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points_Year)
Date <- (c("2014_2"))
N <- (174)

N_days <- nlevels(factor(format(Location_points$`2014-2`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2014.2<- kernel.area(kudLP_2014.2, percent=c(50, 95))
plot(hrLP_2014.2)
kde.areasLP_2014.2

HR50 <- (12.45879)
HR95 <- (38.50898)

LP_2014.2.mcp <- mcp(Location_points$`2014-2`, percent = 95)
plot(LP_2014.2.mcp)
LP_2014.2.mcp

MCP95 <- (106.898)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.2 <- spTransform(hrLP_2014.2, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.2) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-2`@data$Longitude, lat=Location_points$`2014-2`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-2`@data$Longitude, lat=Location_points$`2014-2`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.2.mcp<- spTransform(LP_2014.2.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.2.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-2`@data$Longitude, lat=Location_points$`2014-2`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-2`@data$Longitude, lat=Location_points$`2014-2`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")


##################################################################################
########################################################################################
# 2014-3 (2014/March)

kudLP_2014.3 <- kernelUD(Location_points$`2014-3`[,], h=70,  kern = c("epa"))
hrLP_2014.3 <- getverticeshr(kudLP_2014.3)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-3`)
Date <- (c("2014_2", "2014_3"))
N <- c(174, 345)
N_days <- nlevels(factor(format(Location_points$`2014-3`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.3<- kernel.area(kudLP_2014.3, percent=c(50, 95))
plot(hrLP_2014.3)
kde.areasLP_2014.3

HR50 <- c(12.45879, 13.93387)
HR95 <- c(38.50898, 65.38199)

LP_2014.3.mcp <- mcp(Location_points$`2014-3`, percent = 95)
plot(LP_2014.3.mcp)
LP_2014.3.mcp

MCP95 <- c(106.898, 85.23622)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.3 <- spTransform(hrLP_2014.3, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.3) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-3`@data$Longitude, lat=Location_points$`2014-3`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-3`@data$Longitude, lat=Location_points$`2014-3`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.3.mcp<- spTransform(LP_2014.3.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.3.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-3`@data$Longitude, lat=Location_points$`2014-3`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-3`@data$Longitude, lat=Location_points$`2014-3`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

##################################################################################
##################################################################################

# 2014-4 (2014/April)

kudLP_2014.4 <- kernelUD(Location_points$`2014-4`[,], h=70,  kern = c("epa"))
hrLP_2014.4 <- getverticeshr(kudLP_2014.4)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-4`)
Date <- c("2014_2", "2014_3", "2014_4")
N <- c(174, 345, 345)
N_days <- nlevels(factor(format(Location_points$`2014-4`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.4<- kernel.area(kudLP_2014.4, percent=c(50, 95))
plot(hrLP_2014.4)
kde.areasLP_2014.4

HR50 <- c(12.45879, 13.93387, 9.376534)
HR95 <- c(38.50898, 65.38199, 45.177847)

LP_2014.4.mcp <- mcp(Location_points$`2014-4`, percent = 95)
plot(LP_2014.4.mcp)
LP_2014.4.mcp

MCP95 <- c(106.898, 85.23622, 72.39717)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.4 <- spTransform(hrLP_2014.4, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.4) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-4`@data$Longitude, lat=Location_points$`2014-4`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-4`@data$Longitude, lat=Location_points$`2014-4`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.4.mcp<- spTransform(LP_2014.4.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.4.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-4`@data$Longitude, lat=Location_points$`2014-4`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-4`@data$Longitude, lat=Location_points$`2014-4`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

##################################################################################
##################################################################################

# 2014-5 (2014/May)

kudLP_2014.5 <- kernelUD(Location_points$`2014-5`[,], h=70,  kern = c("epa"))
hrLP_2014.5 <- getverticeshr(kudLP_2014.5)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-5`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5")
N <- c(174, 345, 345, 354)
N_days <- nlevels(factor(format(Location_points$`2014-5`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.5<- kernel.area(kudLP_2014.5, percent=c(50, 95))
plot(hrLP_2014.5)
kde.areasLP_2014.5

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531)

LP_2014.5.mcp <- mcp(Location_points$`2014-5`, percent = 95)
plot(LP_2014.5.mcp)
LP_2014.5.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.5 <- spTransform(hrLP_2014.5, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.5) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-5`@data$Longitude, lat=Location_points$`2014-5`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-5`@data$Longitude, lat=Location_points$`2014-5`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.5.mcp<- spTransform(LP_2014.5.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.5.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-5`@data$Longitude, lat=Location_points$`2014-5`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-5`@data$Longitude, lat=Location_points$`2014-5`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

##################################################################################
##################################################################################

# 2014-6 (2014/June)

kudLP_2014.6 <- kernelUD(Location_points$`2014-6`[,], h=70,  kern = c("epa"))
hrLP_2014.6 <- getverticeshr(kudLP_2014.6)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-6`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6")
N <- c(174, 345, 345, 354, 406)
N_days <- nlevels(factor(format(Location_points$`2014-6`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.6<- kernel.area(kudLP_2014.6, percent=c(50, 95))
plot(hrLP_2014.6)
kde.areasLP_2014.6

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504)

LP_2014.6.mcp <- mcp(Location_points$`2014-6`, percent = 95)
plot(LP_2014.6.mcp)
LP_2014.6.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.6 <- spTransform(hrLP_2014.6, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.6) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-6`@data$Longitude, lat=Location_points$`2014-6`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-6`@data$Longitude, lat=Location_points$`2014-6`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.6.mcp<- spTransform(LP_2014.6.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.6.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-6`@data$Longitude, lat=Location_points$`2014-6`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-6`@data$Longitude, lat=Location_points$`2014-6`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

##################################################################################
##################################################################################

# 2014-7 (2014/July)

kudLP_2014.7 <- kernelUD(Location_points$`2014-7`[,], h=70,  kern = c("epa"))
hrLP_2014.7 <- getverticeshr(kudLP_2014.7)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-7`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7")
N <- c(174, 345, 345, 354, 406, 15)
N_days <- nlevels(factor(format(Location_points$`2014-7`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.7<- kernel.area(kudLP_2014.7, percent=c(50, 95))
plot(hrLP_2014.7)
kde.areasLP_2014.7

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021)

LP_2014.7.mcp <- mcp(Location_points$`2014-7`, percent = 95)
plot(LP_2014.7.mcp)
LP_2014.7.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.7 <- spTransform(hrLP_2014.7, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.7) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-7`@data$Longitude, lat=Location_points$`2014-7`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-7`@data$Longitude, lat=Location_points$`2014-7`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.7.mcp<- spTransform(LP_2014.7.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.7.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-7`@data$Longitude, lat=Location_points$`2014-7`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-7`@data$Longitude, lat=Location_points$`2014-7`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#####################################################################################
#####################################################################################

# 2014-8 (2014/August)

kudLP_2014.8 <- kernelUD(Location_points$`2014-8`[,], h=70,  kern = c("epa"))
hrLP_2014.8 <- getverticeshr(kudLP_2014.8)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-8`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8")
N <- c(174, 345, 345, 354, 406, 15, 364)
N_days <- nlevels(factor(format(Location_points$`2014-8`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.8<- kernel.area(kudLP_2014.8, percent=c(50, 95))
plot(hrLP_2014.8)
kde.areasLP_2014.8

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105)

LP_2014.8.mcp <- mcp(Location_points$`2014-8`, percent = 95)
plot(LP_2014.8.mcp)
LP_2014.8.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.8 <- spTransform(hrLP_2014.8, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.8) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-8`@data$Longitude, lat=Location_points$`2014-8`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-8`@data$Longitude, lat=Location_points$`2014-8`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.8.mcp<- spTransform(LP_2014.8.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.8.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-8`@data$Longitude, lat=Location_points$`2014-8`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-8`@data$Longitude, lat=Location_points$`2014-8`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#####################################################################################
#####################################################################################

# 2014-9 (2014/September)

kudLP_2014.9 <- kernelUD(Location_points$`2014-9`[,], h=70,  kern = c("epa"))
hrLP_2014.9 <- getverticeshr(kudLP_2014.9)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-9`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9")
N <- c(174, 345, 345, 354, 406, 15, 364, 427)
N_days <- nlevels(factor(format(Location_points$`2014-9`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.9<- kernel.area(kudLP_2014.9, percent=c(50, 95))
plot(hrLP_2014.9)
kde.areasLP_2014.9

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113)

LP_2014.9.mcp <- mcp(Location_points$`2014-9`, percent = 95)
plot(LP_2014.9.mcp)
LP_2014.9.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.9 <- spTransform(hrLP_2014.9, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.9) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-9`@data$Longitude, lat=Location_points$`2014-9`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-9`@data$Longitude, lat=Location_points$`2014-9`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.9.mcp<- spTransform(LP_2014.9.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.9.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-9`@data$Longitude, lat=Location_points$`2014-9`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-9`@data$Longitude, lat=Location_points$`2014-9`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#####################################################################################
#####################################################################################

# 2014-10 (2014/October)

kudLP_2014.10 <- kernelUD(Location_points$`2014-10`[,], h=70,  kern = c("epa"))
hrLP_2014.10 <- getverticeshr(kudLP_2014.10)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-10`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427)
N_days <- nlevels(factor(format(Location_points$`2014-10`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.10<- kernel.area(kudLP_2014.10, percent=c(50, 95))
plot(hrLP_2014.10)
kde.areasLP_2014.10

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618)

LP_2014.10.mcp <- mcp(Location_points$`2014-10`, percent = 95)
plot(LP_2014.10.mcp)
LP_2014.10.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.10 <- spTransform(hrLP_2014.10, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.10) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-10`@data$Longitude, lat=Location_points$`2014-10`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-10`@data$Longitude, lat=Location_points$`2014-10`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.10.mcp<- spTransform(LP_2014.10.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.10.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-10`@data$Longitude, lat=Location_points$`2014-10`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-10`@data$Longitude, lat=Location_points$`2014-10`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#####################################################################################
#####################################################################################

# 2014-11 (2014/November)

kudLP_2014.11 <- kernelUD(Location_points$`2014-11`[,], h=70,  kern = c("epa"))
hrLP_2014.11 <- getverticeshr(kudLP_2014.11)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-11`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441)
N_days <- nlevels(factor(format(Location_points$`2014-11`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.11<- kernel.area(kudLP_2014.11, percent=c(50, 95))
plot(hrLP_2014.11)
kde.areasLP_2014.11

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069)

LP_2014.11.mcp <- mcp(Location_points$`2014-11`, percent = 95)
plot(LP_2014.11.mcp)
LP_2014.11.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.11 <- spTransform(hrLP_2014.11, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.11) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-11`@data$Longitude, lat=Location_points$`2014-11`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-11`@data$Longitude, lat=Location_points$`2014-11`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.11.mcp<- spTransform(LP_2014.11.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.11.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-11`@data$Longitude, lat=Location_points$`2014-11`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-11`@data$Longitude, lat=Location_points$`2014-11`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#####################################################################################
#####################################################################################

# 2016-7 (2014/December)

kudLP_2014.12 <- kernelUD(Location_points$`2014-12`[,], h=70,  kern = c("epa"))
hrLP_2014.12 <- getverticeshr(kudLP_2014.12)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2014-12`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386)
N_days <- nlevels(factor(format(Location_points$`2014-12`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2014.12<- kernel.area(kudLP_2014.12, percent=c(50, 95))
plot(hrLP_2014.12)
kde.areasLP_2014.12

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636)

LP_2014.12.mcp <- mcp(Location_points$`2014-12`, percent = 95)
plot(LP_2014.12.mcp)
LP_2014.12.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2014.12 <- spTransform(hrLP_2014.12, CRS('+init=epsg:4326'))

leaflet(sdfLP_2014.12) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-12`@data$Longitude, lat=Location_points$`2014-12`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-12`@data$Longitude, lat=Location_points$`2014-12`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2014.12.mcp<- spTransform(LP_2014.12.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2014.12.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2014-12`@data$Longitude, lat=Location_points$`2014-12`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2014-12`@data$Longitude, lat=Location_points$`2014-12`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################



# 2016-7 (2016/July)

kudLP_2016.7 <- kernelUD(Location_points$`2016-7`[,], h=70,  kern = c("epa"))
hrLP_2016.7 <- getverticeshr(kudLP_2016.7)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2016-7`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88)
N_days <- nlevels(factor(format(Location_points$`2016-7`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2016.7<- kernel.area(kudLP_2016.7, percent=c(50, 95))
plot(hrLP_2016.7)
kde.areasLP_2016.7

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238)

LP_2016.7.mcp <- mcp(Location_points$`2016-7`, percent = 95)
plot(LP_2016.7.mcp)
LP_2016.7.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2016.7 <- spTransform(hrLP_2016.7, CRS('+init=epsg:4326'))

leaflet(sdfLP_2016.7) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-7`@data$Longitude, lat=Location_points$`2016-7`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-7`@data$Longitude, lat=Location_points$`2016-7`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2016.7.mcp<- spTransform(LP_2016.7.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2016.7.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-7`@data$Longitude, lat=Location_points$`2016-7`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-7`@data$Longitude, lat=Location_points$`2016-7`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################



# 2016-8 (2016/August)

kudLP_2016.8 <- kernelUD(Location_points$`2016-8`[,], h=70,  kern = c("epa"))
hrLP_2016.8 <- getverticeshr(kudLP_2016.8)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2016-8`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72)
N_days <- nlevels(factor(format(Location_points$`2016-8`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2016.8<- kernel.area(kudLP_2016.8, percent=c(50, 95))
plot(hrLP_2016.8)
kde.areasLP_2016.8

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867)

LP_2016.8.mcp <- mcp(Location_points$`2016-8`, percent = 95)
plot(LP_2016.8.mcp)
LP_2016.8.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2016.8 <- spTransform(hrLP_2016.8, CRS('+init=epsg:4326'))

leaflet(sdfLP_2016.8) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-8`@data$Longitude, lat=Location_points$`2016-8`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-8`@data$Longitude, lat=Location_points$`2016-8`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2016.8.mcp<- spTransform(LP_2016.8.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2016.8.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-8`@data$Longitude, lat=Location_points$`2016-8`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-8`@data$Longitude, lat=Location_points$`2016-8`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2016-9 (2016/September)

kudLP_2016.9 <- kernelUD(Location_points$`2016-9`[,], h=70,  kern = c("epa"))
hrLP_2016.9 <- getverticeshr(kudLP_2016.9)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2016-9`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19)
N_days <- nlevels(factor(format(Location_points$`2016-9`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2016.9<- kernel.area(kudLP_2016.9, percent=c(50, 95))
plot(hrLP_2016.9)
kde.areasLP_2016.9

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0)

LP_2016.9.mcp <- mcp(Location_points$`2016-9`, percent = 95)
plot(LP_2016.9.mcp)
LP_2016.9.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2016.9 <- spTransform(hrLP_2016.9, CRS('+init=epsg:4326'))

leaflet(sdfLP_2016.9) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-9`@data$Longitude, lat=Location_points$`2016-9`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-9`@data$Longitude, lat=Location_points$`2016-9`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2016.9.mcp<- spTransform(LP_2016.9.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2016.9.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-9`@data$Longitude, lat=Location_points$`2016-9`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-9`@data$Longitude, lat=Location_points$`2016-9`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#####################################################################################

# 2016-10 (2016/October)


kudLP_2016.10 <- kernelUD(Location_points$`2016-10`[,], h=70,  kern = c("epa"))
hrLP_2016.10 <- getverticeshr(kudLP_2016.10)

summary(Location_points$`2016-10`)

# There is just one location points. It's necessary at least 5 points to analyze.

# Ok. Let's get these values!
# Fisrt by KDE method

#summary(Location_points$`2016-10`)
#Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9")
#N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19)


#kde.areasLP_2016.10<- kernel.area(kudLP_2016.10, percent=c(50, 95))
#plot(hrLP_2016.10)
#kde.areasLP_2016.10

#HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322)
#HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0)

#LP_2016.10.mcp <- mcp(Location_points$`2016-10`, percent = 95)
#plot(LP_2016.10.mcp)
#LP_2016.10.mcp

#MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583)

# Add columns to HR50, HR95 and MCP 95 values.

#Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

#sdfLP_2016.10 <- spTransform(hrLP_2016.10, CRS('+init=epsg:4326'))

#leaflet(sdfLP_2016.10) %>%   
#  addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2016-10`@data$Longitude, lat=Location_points$`2016-10`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2016-10`@data$Longitude, lat=Location_points$`2016-10`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

#LP_2016.10.mcp<- spTransform(LP_2016.10.mcp, CRS('+init=epsg:4326'))

#leaflet(LP_2016.10.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2016-10`@data$Longitude, lat=Location_points$`2016-10`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2016-10`@data$Longitude, lat=Location_points$`2016-10`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2016-12 (2016/December)

kudLP_2016.12 <- kernelUD(Location_points$`2016-12`[,], h=70,  kern = c("epa"))
hrLP_2016.12 <- getverticeshr(kudLP_2016.12)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2016-12`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14)
N_days <- nlevels(factor(format(Location_points$`2016-12`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2016.12<- kernel.area(kudLP_2016.12, percent=c(50, 95))
plot(hrLP_2016.12)
kde.areasLP_2016.12

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215)

LP_2016.12.mcp <- mcp(Location_points$`2016-12`, percent = 95)
plot(LP_2016.12.mcp)
LP_2016.12.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2016.12 <- spTransform(hrLP_2016.12, CRS('+init=epsg:4326'))

leaflet(sdfLP_2016.12) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-12`@data$Longitude, lat=Location_points$`2016-12`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-12`@data$Longitude, lat=Location_points$`2016-12`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2016.12.mcp<- spTransform(LP_2016.12.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2016.12.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2016-12`@data$Longitude, lat=Location_points$`2016-12`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2016-12`@data$Longitude, lat=Location_points$`2016-12`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2017-01 (2017/January)

kudLP_2017.1 <- kernelUD(Location_points$`2017-1`[,], h=70,  kern = c("epa"))
hrLP_2017.1 <- getverticeshr(kudLP_2017.1)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2017-1`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6)
N_days <- nlevels(factor(format(Location_points$`2017-1`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2017.1<- kernel.area(kudLP_2017.1, percent=c(50, 95))
plot(hrLP_2017.1)
kde.areasLP_2017.1

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831)

LP_2017.1.mcp <- mcp(Location_points$`2017-1`, percent = 95)
plot(LP_2017.1.mcp)
LP_2017.1.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2017.1 <- spTransform(hrLP_2017.1, CRS('+init=epsg:4326'))

leaflet(sdfLP_2017.1) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-1`@data$Longitude, lat=Location_points$`2017-1`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-1`@data$Longitude, lat=Location_points$`2017-1`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2017.1.mcp<- spTransform(LP_2017.1.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2017.1.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-1`@data$Longitude, lat=Location_points$`2017-1`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-1`@data$Longitude, lat=Location_points$`2017-1`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2017-2 (2017/February)

kudLP_2017.2 <- kernelUD(Location_points$`2017-2`[,], h=70,  kern = c("epa"))
hrLP_2017.2 <- getverticeshr(kudLP_2017.2)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2017-2`)
View(Location_points$`2017-2`@data)

#There is only one location point. 

#Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1")
#N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6)


#kde.areasLP_2017.2<- kernel.area(kudLP_2017.2, percent=c(50, 95))
#plot(hrLP_2017.2)
#kde.areasLP_2017.2

#HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277)
#HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831)

#LP_2017.2.mcp <- mcp(Location_points$`2017-2`, percent = 95)
#plot(LP_2017.2.mcp)
#LP_2017.2.mcp

#MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982)

# Add columns to HR50, HR95 and MCP 95 values.

#Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

#sdfLP_2017.2 <- spTransform(hrLP_2017.2, CRS('+init=epsg:4326'))

#leaflet(sdfLP_2017.2) %>%   
#  addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2017-2`@data$Longitude, lat=Location_points$`2017-2`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2017-2`@data$Longitude, lat=Location_points$`2017-2`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

#LP_2017.2.mcp<- spTransform(LP_2017.2.mcp, CRS('+init=epsg:4326'))

#leaflet(LP_2017.2.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2017-2`@data$Longitude, lat=Location_points$`2017-2`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2017-2`@data$Longitude, lat=Location_points$`2017-2`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col="white")


#############################################################################


# 2017-04 (2017/April)

kudLP_2017.4 <- kernelUD(Location_points$`2017-4`[,], h=70,  kern = c("epa"))
hrLP_2017.4 <- getverticeshr(kudLP_2017.4)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2017-4`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147)
N_days <- nlevels(factor(format(Location_points$`2017-4`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2017.4<- kernel.area(kudLP_2017.4, percent=c(50, 95))
plot(hrLP_2017.4)
kde.areasLP_2017.4

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323)

LP_2017.4.mcp <- mcp(Location_points$`2017-4`, percent = 95)
plot(LP_2017.4.mcp)
LP_2017.4.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2017.4 <- spTransform(hrLP_2017.4, CRS('+init=epsg:4326'))

leaflet(sdfLP_2017.4) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-4`@data$Longitude, lat=Location_points$`2017-4`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-4`@data$Longitude, lat=Location_points$`2017-4`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2017.4.mcp<- spTransform(LP_2017.4.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2017.4.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-4`@data$Longitude, lat=Location_points$`2017-4`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-4`@data$Longitude, lat=Location_points$`2017-4`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2017-05 (2017/May)

kudLP_2017.5 <- kernelUD(Location_points$`2017-5`[,], h=70,  kern = c("epa"))
hrLP_2017.5 <- getverticeshr(kudLP_2017.5)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2017-5`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295)
N_days <- nlevels(factor(format(Location_points$`2017-5`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2017.5<- kernel.area(kudLP_2017.5, percent=c(50, 95))
plot(hrLP_2017.5)
kde.areasLP_2017.5

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114)

LP_2017.5.mcp <- mcp(Location_points$`2017-5`, percent = 95)
plot(LP_2017.5.mcp)
LP_2017.5.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2017.5 <- spTransform(hrLP_2017.5, CRS('+init=epsg:4326'))

leaflet(sdfLP_2017.5) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-5`@data$Longitude, lat=Location_points$`2017-5`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-5`@data$Longitude, lat=Location_points$`2017-5`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2017.5.mcp<- spTransform(LP_2017.5.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2017.5.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-5`@data$Longitude, lat=Location_points$`2017-5`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-5`@data$Longitude, lat=Location_points$`2017-5`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2017-06 (2017/June)

kudLP_2017.6 <- kernelUD(Location_points$`2017-6`[,], h=70,  kern = c("epa"))
hrLP_2017.6 <- getverticeshr(kudLP_2017.6)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2017-6`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80)
N_days <- nlevels(factor(format(Location_points$`2017-6`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2017.6<- kernel.area(kudLP_2017.6, percent=c(50, 95))
plot(hrLP_2017.6)
kde.areasLP_2017.6

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277)

LP_2017.6.mcp <- mcp(Location_points$`2017-6`, percent = 95)
plot(LP_2017.6.mcp)
LP_2017.6.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2017.6 <- spTransform(hrLP_2017.6, CRS('+init=epsg:4326'))

leaflet(sdfLP_2017.6) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-6`@data$Longitude, lat=Location_points$`2017-6`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-6`@data$Longitude, lat=Location_points$`2017-6`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2017.6.mcp<- spTransform(LP_2017.6.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2017.6.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2017-6`@data$Longitude, lat=Location_points$`2017-6`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2017-6`@data$Longitude, lat=Location_points$`2017-6`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2017-8 (2017/August)

kudLP_2017.8 <- kernelUD(Location_points$`2017-8`[,], h=70,  kern = c("epa"))
hrLP_2017.8 <- getverticeshr(kudLP_2017.8)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2017-8`)
# There are only 4 location points.

#Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6")
#N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80)


#kde.areasLP_2017.8<- kernel.area(kudLP_2017.8, percent=c(50, 95))
#plot(hrLP_2017.8)
#kde.areasLP_2017.8

#HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499)
#HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277)

#LP_2017.8.mcp <- mcp(Location_points$`2017-8`, percent = 95)
#plot(LP_2017.8.mcp)
#LP_2017.8.mcp

#MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192)

# Add columns to HR50, HR95 and MCP 95 values.

#Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

#sdfLP_2017.8 <- spTransform(hrLP_2017.8, CRS('+init=epsg:4326'))

#leaflet(sdfLP_2017.8) %>%   
#  addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2017-8`@data$Longitude, lat=Location_points$`2017-8`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2017-8`@data$Longitude, lat=Location_points$`2017-8`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

#LP_2017.8.mcp<- spTransform(LP_2017.8.mcp, CRS('+init=epsg:4326'))

#leaflet(LP_2017.8.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2017-8`@data$Longitude, lat=Location_points$`2017-8`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2017-8`@data$Longitude, lat=Location_points$`2017-8`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2017-9 (2017/September)

kudLP_2017.9 <- kernelUD(Location_points$`2017-9`[,], h=70,  kern = c("epa"))
hrLP_2017.9 <- getverticeshr(kudLP_2017.9)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2017-9`)
# There are only 2 location points.

#Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6")
#N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80)


#kde.areasLP_2017.9<- kernel.area(kudLP_2017.9, percent=c(50, 95))
#plot(hrLP_2017.9)
#kde.areasLP_2017.9

#HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499)
#HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277)

#LP_2017.9.mcp <- mcp(Location_points$`2017-9`, percent = 95)
#plot(LP_2017.9.mcp)
#LP_2017.9.mcp

#MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192)

# Add columns to HR50, HR95 and MCP 95 values.

#Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

#sdfLP_2017.9 <- spTransform(hrLP_2017.9, CRS('+init=epsg:4326'))

#leaflet(sdfLP_2017.9) %>%   
#  addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2017-9`@data$Longitude, lat=Location_points$`2017-9`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2017-9`@data$Longitude, lat=Location_points$`2017-9`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

#LP_2017.9.mcp<- spTransform(LP_2017.9.mcp, CRS('+init=epsg:4326'))

#leaflet(LP_2017.9.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
#  addCircleMarkers(lng= Location_points$`2017-9`@data$Longitude, lat=Location_points$`2017-9`@data$Latitude, radius=1, col="#a6cee3")%>%
#  addHeatmap(lng= Location_points$`2017-9`@data$Longitude, lat=Location_points$`2017-9`@data$Latitude, max=10,blur=25, radius=20)%>%
#  addScaleBar(position="bottomleft")%>%
#  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2019-2 (2019/February)


kudLP_2019.2 <- kernelUD(Location_points$`2019-2`[,], h=70,  kern = c("epa"))
hrLP_2019.2 <- getverticeshr(kudLP_2019.2)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-2`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49)
N_days <- nlevels(factor(format(Location_points$`2019-2`@data$DateTime, format = "%d")))
N_days


kde.areasLP_2019.2<- kernel.area(kudLP_2019.2, percent=c(50, 95))
plot(hrLP_2019.2)
kde.areasLP_2019.2

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920)

LP_2019.2.mcp <- mcp(Location_points$`2019-2`, percent = 95)
plot(LP_2019.2.mcp)
LP_2019.2.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.2 <- spTransform(hrLP_2019.2, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.2) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-2`@data$Longitude, lat=Location_points$`2019-2`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-2`@data$Longitude, lat=Location_points$`2019-2`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.2.mcp<- spTransform(LP_2019.2.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.2.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-2`@data$Longitude, lat=Location_points$`2019-2`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-2`@data$Longitude, lat=Location_points$`2019-2`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#############################################################################


# 2019-3 (2019/March)


kudLP_2019.3 <- kernelUD(Location_points$`2019-3`[,], h=70,  kern = c("epa"))
hrLP_2019.3 <- getverticeshr(kudLP_2019.3)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-3`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17)
N_days <- nlevels(factor(format(Location_points$`2019-3`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.3<- kernel.area(kudLP_2019.3, percent=c(50, 95))
plot(hrLP_2019.3)
kde.areasLP_2019.3

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348)

LP_2019.3.mcp <- mcp(Location_points$`2019-3`, percent = 95)
plot(LP_2019.3.mcp)
LP_2019.3.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.3 <- spTransform(hrLP_2019.3, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.3) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-3`@data$Longitude, lat=Location_points$`2019-3`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-3`@data$Longitude, lat=Location_points$`2019-3`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.3.mcp<- spTransform(LP_2019.3.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.3.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-3`@data$Longitude, lat=Location_points$`2019-3`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-3`@data$Longitude, lat=Location_points$`2019-3`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")


#############################################################################


# 2019-4 (2019/Abril)


kudLP_2019.4 <- kernelUD(Location_points$`2019-4`[,], h=70,  kern = c("epa"))
hrLP_2019.4 <- getverticeshr(kudLP_2019.4)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-4`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3", "2019_4")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17, 11)
N_days <- nlevels(factor(format(Location_points$`2019-4`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.4<- kernel.area(kudLP_2019.4, percent=c(50, 95))
plot(hrLP_2019.4)
kde.areasLP_2019.4

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923, 0.5688718)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348, 2.5340654)

LP_2019.4.mcp <- mcp(Location_points$`2019-4`, percent = 95)
plot(LP_2019.4.mcp)
LP_2019.4.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447, 0.08231155)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.4 <- spTransform(hrLP_2019.4, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.4) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-4`@data$Longitude, lat=Location_points$`2019-4`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-4`@data$Longitude, lat=Location_points$`2019-4`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.4.mcp<- spTransform(LP_2019.4.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.4.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-4`@data$Longitude, lat=Location_points$`2019-4`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-4`@data$Longitude, lat=Location_points$`2019-4`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")


#############################################################################


# 2019-5 (2019/May)


kudLP_2019.5 <- kernelUD(Location_points$`2019-5`[,], h=70,  kern = c("epa"))
hrLP_2019.5 <- getverticeshr(kudLP_2019.5)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-5`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3", "2019_4", "2019_5")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17, 11, 39)
N_days <- nlevels(factor(format(Location_points$`2019-5`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.5<- kernel.area(kudLP_2019.5, percent=c(50, 95))
plot(hrLP_2019.5)
kde.areasLP_2019.5

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923, 0.5688718, 1.458284)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348, 2.5340654, 5.833135)

LP_2019.5.mcp <- mcp(Location_points$`2019-5`, percent = 95)
plot(LP_2019.5.mcp)
LP_2019.5.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447, 0.08231155, 14.98502)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.5 <- spTransform(hrLP_2019.5, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.5) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-5`@data$Longitude, lat=Location_points$`2019-5`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-5`@data$Longitude, lat=Location_points$`2019-5`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.5.mcp<- spTransform(LP_2019.5.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.5.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-5`@data$Longitude, lat=Location_points$`2019-5`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-5`@data$Longitude, lat=Location_points$`2019-5`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#######################################################
# 2019-6 (2019/June)


kudLP_2019.6 <- kernelUD(Location_points$`2019-6`[,], h=70,  kern = c("epa"))
hrLP_2019.6 <- getverticeshr(kudLP_2019.6)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-6`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3", "2019_4", "2019_5", "2019_6")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17, 11, 39, 55)
N_days <- nlevels(factor(format(Location_points$`2019-6`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.6<- kernel.area(kudLP_2019.6, percent=c(50, 95))
plot(hrLP_2019.6)
kde.areasLP_2019.6

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923, 0.5688718, 1.458284, 1.434803)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348, 2.5340654, 5.833135, 6.635964)

LP_2019.6.mcp <- mcp(Location_points$`2019-6`, percent = 95)
plot(LP_2019.6.mcp)
LP_2019.6.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447, 0.08231155, 14.98502, 3.978203)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.6 <- spTransform(hrLP_2019.6, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.6) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-6`@data$Longitude, lat=Location_points$`2019-6`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-6`@data$Longitude, lat=Location_points$`2019-6`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.6.mcp<- spTransform(LP_2019.6.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.6.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-6`@data$Longitude, lat=Location_points$`2019-6`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-6`@data$Longitude, lat=Location_points$`2019-6`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")


#######################################################
# 2019-7 (2019/July)


kudLP_2019.7 <- kernelUD(Location_points$`2019-7`[,], h=70,  kern = c("epa"))
hrLP_2019.7 <- getverticeshr(kudLP_2019.7)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-7`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3", "2019_4", "2019_5", "2019_6", "2019_7")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17, 11, 39, 55, 117)
N_days <- nlevels(factor(format(Location_points$`2019-7`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.7<- kernel.area(kudLP_2019.7, percent=c(50, 95))
plot(hrLP_2019.7)
kde.areasLP_2019.7

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923, 0.5688718, 1.458284, 1.434803, 1.618376)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348, 2.5340654, 5.833135, 6.635964, 6.280266)

LP_2019.7.mcp <- mcp(Location_points$`2019-7`, percent = 95)
plot(LP_2019.7.mcp)
LP_2019.7.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447, 0.08231155, 14.98502, 3.978203, 3.661758)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.7 <- spTransform(hrLP_2019.7, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.7) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-7`@data$Longitude, lat=Location_points$`2019-7`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-7`@data$Longitude, lat=Location_points$`2019-7`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.7.mcp<- spTransform(LP_2019.7.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.7.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-7`@data$Longitude, lat=Location_points$`2019-7`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-7`@data$Longitude, lat=Location_points$`2019-7`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")


#######################################################
# 2019-10 (2019/October)


kudLP_2019.10 <- kernelUD(Location_points$`2019-10`[,], h=70,  kern = c("epa"))
hrLP_2019.10 <- getverticeshr(kudLP_2019.10)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-10`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3", "2019_4", "2019_5", "2019_6", "2019_7", "2019_10")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17, 11, 39, 55, 117, 173)
N_days <- nlevels(factor(format(Location_points$`2019-10`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.10<- kernel.area(kudLP_2019.10, percent=c(50, 95))
plot(hrLP_2019.10)
kde.areasLP_2019.10

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923, 0.5688718, 1.458284, 1.434803, 1.618376, 6.868904)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348, 2.5340654, 5.833135, 6.635964, 6.280266, 35.718298)

LP_2019.10.mcp <- mcp(Location_points$`2019-10`, percent = 95)
plot(LP_2019.10.mcp)
LP_2019.10.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447, 0.08231155, 14.98502, 3.978203, 3.661758, 40.399)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.10 <- spTransform(hrLP_2019.10, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.10) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-10`@data$Longitude, lat=Location_points$`2019-10`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-10`@data$Longitude, lat=Location_points$`2019-10`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.10.mcp<- spTransform(LP_2019.10.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.10.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-10`@data$Longitude, lat=Location_points$`2019-10`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-10`@data$Longitude, lat=Location_points$`2019-10`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")


#######################################################
# 2019-11 (2019/November)


kudLP_2019.11 <- kernelUD(Location_points$`2019-11`[,], h=70,  kern = c("epa"))
hrLP_2019.11 <- getverticeshr(kudLP_2019.11)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-11`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3", "2019_4", "2019_5", "2019_6", "2019_7", "2019_10", "2019_11")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17, 11, 39, 55, 117, 173, 131)
N_days <- nlevels(factor(format(Location_points$`2019-11`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.11<- kernel.area(kudLP_2019.11, percent=c(50, 95))
plot(hrLP_2019.11)
kde.areasLP_2019.11

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923, 0.5688718, 1.458284, 1.434803, 1.618376, 6.868904, 4.909598)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348, 2.5340654, 5.833135, 6.635964, 6.280266, 35.718298, 22.093193)

LP_2019.11.mcp <- mcp(Location_points$`2019-11`, percent = 95)
plot(LP_2019.11.mcp)
LP_2019.11.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447, 0.08231155, 14.98502, 3.978203, 3.661758, 40.399, 23.23657)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.11 <- spTransform(hrLP_2019.11, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.11) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-11`@data$Longitude, lat=Location_points$`2019-11`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-11`@data$Longitude, lat=Location_points$`2019-11`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.11.mcp<- spTransform(LP_2019.11.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.11.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-11`@data$Longitude, lat=Location_points$`2019-11`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-11`@data$Longitude, lat=Location_points$`2019-11`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")


#######################################################
# 2019-12 (2019/December)


kudLP_2019.12 <- kernelUD(Location_points$`2019-12`[,], h=70,  kern = c("epa"))
hrLP_2019.12 <- getverticeshr(kudLP_2019.12)


# Ok. Let's get these values!
# Fisrt by KDE method

summary(Location_points$`2019-12`)
Date <- c("2014_2", "2014_3", "2014_4", "2014_5","2014_6", "2014_7", "2014_8", "2014_9", "2014_10", "2014_11", "2014_12", "2016_7", "2016_8", "2016_9", "2016_12", "2017_1", "2017_4", "2017_5", "2017_6", "2019_2", "2019_3", "2019_4", "2019_5", "2019_6", "2019_7", "2019_10", "2019_11", "2019_12")
N <- c(174, 345, 345, 354, 406, 15, 364, 427, 427, 441, 386, 88, 72, 19, 14, 6, 147, 295, 80, 49, 17, 11, 39, 55, 117, 173, 131, 248)
N_days <- nlevels(factor(format(Location_points$`2019-12`@data$DateTime, format = "%d")))
N_days

kde.areasLP_2019.12<- kernel.area(kudLP_2019.12, percent=c(50, 95))
plot(hrLP_2019.12)
kde.areasLP_2019.12

HR50 <- c(12.45879, 13.93387, 9.376534, 15.53772, 8.719376, 1.053720, 9.706709, 9.283798, 8.324124, 10.61753, 8.504159, 9.292026, 4.299881, 3.640322, 4.84486, 1.911277, 9.301247, 9.072529, 4.014499, 1.163622, 1.410923, 0.5688718, 1.458284, 1.434803, 1.618376, 6.868904, 4.909598, 5.76675)
HR95 <- c(38.50898, 65.38199, 45.177847, 51.62531, 34.877504, 3.688021, 39.215105, 37.754113, 41.620618, 40.11069, 34.016636, 34.513238, 20.270867, 0, 12.11215, 5.733831, 35.433323, 35.335114, 13.999277, 6.399920, 6.926348, 2.5340654, 5.833135, 6.635964, 6.280266, 35.718298, 22.093193, 26.91150)

LP_2019.12.mcp <- mcp(Location_points$`2019-12`, percent = 95)
plot(LP_2019.12.mcp)
LP_2019.12.mcp

MCP95 <- c(106.898, 85.23622, 72.39717, 66.08823, 45.00072, 3.866569, 45.7897, 74.96289, 48.67728, 66.34441, 55.01007,55.78752, 45.21537, 27.11583, 40.204, 1.523982, 48.89251, 40.03124, 14.74192, 2.325366, 11.43447, 0.08231155, 14.98502, 3.978203, 3.661758, 40.399, 23.23657, 38.03661)

# Add columns to HR50, HR95 and MCP 95 values.

Area_values <- data.frame(Date, N, HR50, HR95, MCP95) 

# Plotting KDE graph

sdfLP_2019.12 <- spTransform(hrLP_2019.12, CRS('+init=epsg:4326'))

leaflet(sdfLP_2019.12) %>%   
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-12`@data$Longitude, lat=Location_points$`2019-12`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-12`@data$Longitude, lat=Location_points$`2019-12`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col = "white")

# Plotting MCP graph

LP_2019.12.mcp<- spTransform(LP_2019.12.mcp, CRS('+init=epsg:4326'))

leaflet(LP_2019.12.mcp) %>%   addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng= Location_points$`2019-12`@data$Longitude, lat=Location_points$`2019-12`@data$Latitude, radius=1, col="#a6cee3")%>%
  addHeatmap(lng= Location_points$`2019-12`@data$Longitude, lat=Location_points$`2019-12`@data$Latitude, max=10,blur=25, radius=20)%>%
  addScaleBar(position="bottomleft")%>%
  addPolygons(weight = 3, fillOpacity = .25, col="white")

#######################################################
# Finished! Now you have all the HR and CA values.

#Let's export them to a CSV file

write.csv(Area_values,"C:/Users/idssc/Desktop/Manuscript 1 - Data/Data/Area values/Area_values.csv", row.names = FALSE)
