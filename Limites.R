# Límites astronómicos y geográficos de Colombia
Gr<-c(4,12,66,79)
Mn<-c(12,26,50,02)
Sg<-c(30,46,54,33)

# Sexagesimal a decimal
Coord <- Gr +(Mn/60)+(Sg/3600) 
lat<-c(Coord[1],Coord[2],Coord[1],Coord[2])
lng<-c(Coord[3],Coord[4],Coord[4],Coord[3])
# Crar tabla
lims<-data.frame(lat,lng)
lims$lng<- lims$lng * -1
lims$lat[lims$lat==lims$lat[1]]<-lims$lat[1] * -1
lims$Label<-paste("lngitud geográfica",round(lims$lng,2),"Latitud geográfica",round(lims$lat,2))

#Puntos extremos de colombia 
Lati<-c(15.88417,-4.226944,12.53,1.23)
Long<-c(-81.37056,-69.94722,-81.73556,-66.85028)
Punto<-c("Bajo Nuevo","Río Amazonas,Leticia","San Andres Islas","Piedra del Cocuy")
Ext<-data.frame(Lati,Long,Punto)

#Puntos extremos continentales
Lati<-c(12.45833,-4.226944,1.650556,1.23)
Long<-c(-71.66444,-69.94722,-79.00806,-66.85028)
Punto<-c("Punta Gallinas","Río Aamzonas","Cabo Manglares","Piedra del Cocuy")
Ext_Cont<-data.frame(Lati,Long,Punto)
names(Ext_Cont)<-c("lat","lng","Nombre")

# Etiqueta de mapa
Ext_Cont$Label<-paste("Longitud:",Ext_Cont$lng,"Latitud:",Ext_Cont$lat,"Lugar:",Ext_Cont$Nombre)
# Representar em mapa
library(leaflet)
Ext_Cont %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = Ext_Cont$Label)%>%
  addRectangles(
    lng1=Ext$Long[3], lat1=Ext$Lati[1],
    lng2=Ext$Long[4], lat2=Ext$Lati[2],
    fillColor = "transparent"
  )



