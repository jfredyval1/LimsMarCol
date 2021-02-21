# Función para la conversión de coordenadas sexagesimales a decimales

sex2dec<- function(Gr,Mn,Sg,Hemis) { # Función para convertir coordenadas
  Hemis<-toupper(Hemis)
  coor<-Gr + (Mn/60) + (Sg/3600)
  if (Hemis=="S") {
    coor<-coor * -1
    }
  if (Hemis=="W") {
    coor <-coor * -1
  }
  print(coor)
}

#Puntos extremos de Colombia 
Gr<-c(15,81,4,69,12,81,1,66,12,71,1,79) # Grados 
Mn<-c(53,22,13,56,31,44,13,51,27,39,39,00) # Minutos
Sg<-c(03,14,37,50,51,08,48,01,30,52,02,29) # Segundos
Hemis<-c("n","w","s","w","n","w","n","w","n","w","n","w") # Hemisferio
Nombre<-c("Bajo Nuevo","Bajo Nuevo","R. Amazonas","R. Amazonas", # Nombre del lugar
          "San Andres","San Andres","Piedra del Cocuy","Piedra del Cocuy",
          "Punta gallinas","Punta gallinas","Cabo Manglares","Cabo Manglares")

# sex2dec a todos los datos
Cors<-vector("double",length = length(Gr)) # Crear vector vacio

for (i in 1:length(Gr)) { # Aplicar función y compilar resultados en vector "Cors"
    Cors[i]<-sex2dec(Gr[i],Mn[i],Sg[i],Hemis[i])
    }

# Crear tabla de coordenadas
ExtCords<-data.frame(Nombre,Cors,Hemis)
ExtCords$TipCord[ExtCords$Hemis=="w"]<-"Longitud" # Crear columna de diferenciación
ExtCords$TipCord[ExtCords$Hemis!="w"]<-"Latitud"

ExtCords<-cbind(ExtCords[ExtCords$TipCord=="Longitud",], # Fila por punto a ubicar
                ExtCords[ExtCords$TipCord!="Longitud",])
# Renombrar columnas
names(ExtCords)[c(4,8,2,6)]<-c("Longitud","Latitud","lng","lat")

# Crear label para mapa dinámico
ExtCords$Label<-paste("Lugar:",ExtCords$Nombre,ExtCords$Longitud,round(ExtCords$lng,2),
                      ExtCords$Latitud,round(ExtCords$lat,2))


# Representar en mapa
library(leaflet)
ExtCords %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = ExtCords$Label)%>%
  addRectangles(
    lng1=max(ExtCords$lng), lat1=max(ExtCords$lat),
    lng2=min(ExtCords$lng), lat2=min(ExtCords$lat),
    fillColor = "transparent"
  )

######################
# Límites marítimos #
######################

# Se agrupan a continuación las coordenadas que señalas los límites marítimos de
# Sur Occidente a Nor Oriente

#Puntos extremos de Colombia 
Gr<-c(3,84,3,84,5,84,5,79,6,79,6,79,6,78,6,78,6,78,
      7,77,8,77,9,77,9,77,10,77,11,77,12,77,12,77,
      12,78,12,79,11,80,11,80,11,81,10,82,14,82,14,79,15,79,
      15,80,15,79,16,79,16,79,16,79,16,79,16,79,16,78,15,78,
      15,78,14,78,14,78,14,77,14,74,15,73,15,71,15,69) # Grados 

Mn<-c(03,46,32,19,00,19,00,52,00,14,16,03,28,47,44,18,44,18,
      12,53,41,21,09,13,27,03,28,15,27,34,00,43,19,49,
      30,00,30,00,50,00,00,00,00,15,49,00,59,00,59,56,30,56,
      46,03,58,56,04,50,04,29,10,29,10,16,04,16,04,25,36,25,
      36,38,29,38,15,19,05,40,44,30,02,27,00,40,18,29) # Minutos

Sg<-c(00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
      39.3,20.9,07.3,50.9,00,00,00,00,00,00,00,00,00,00,00,00,
      00,00,00,00,00,00,00,00,00,00,00,00,08,00,08,00,10,00,
      00,55,40,40,15,32,15,20,10,20,10,40,15,40,15,50,00,50,
      00,00,37,00,00,30,00,00,10,50,00,20,40,30,00,30) # Segundos
Hemis<-c("n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w",
         "n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w",
         "n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w",
         "n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w",
         "n","w","n","w","n","w","n","w","n","w","n","w","n","w","n","w") # Hemisferio
# Conversión de coordenadas
Cors<-vector("double",length = length(Gr)) # Crear vector vacio

for (i in 1:length(Gr)) { # Aplicar función y compilar resultados en vector "Cors"
  Cors[i]<-sex2dec(Gr[i],Mn[i],Sg[i],Hemis[i])
}

# Unificación en tabla
# Crear tabla de coordenadas
MarCoords<-data.frame(Cors,Hemis)
MarCoords$TipCord[MarCoords$Hemis=="w"]<-"Longitud" # Crear columna de diferenciación
MarCoords$TipCord[MarCoords$Hemis!="w"]<-"Latitud"

MarCoords<-cbind(MarCoords[MarCoords$TipCord=="Longitud",], # Fila por punto a ubicar
                 MarCoords[MarCoords$TipCord!="Longitud",])
# Renombrar columnas
names(MarCoords)[c(3,6,1,4)]<-c("Longitud","Latitud","lng","lat")

# Representar en mapa
library(leaflet)
MarCoords %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(popup = paste("Longitud:",round(MarCoords$lng,2),"Latitud:",round(MarCoords$lat,2)))
  