##Obtiene la base de datos
"Cargando Datos"

incidentes <- read.csv("C:\\Proyectos\\FY16\\Piloto Advance Analytics\\Base de datos\\IncidentesvialesdelDFeneroaagosto2014-incidentesViales2014.csv", header=FALSE, na.strings=c("NA", "NULL"),stringsAsFactors = F)
#incidentes <- read.csv("http://www.datosabiertos.df.gob.mx/sigdata/resources/datasets/IncidentesvialesdelDFeneroaagosto2014-incidentesViales2014.csv", header=FALSE, na.strings=c("NA", "NULL"),stringsAsFactors = F)
"Datos Listos"
##View(incidentes)
##Crea los titulos de las columnas
titulosIn <- c("Tipo","Lugar", "Alternativa", "Alternativa.2", "Fecha.Hora", "Lat", "Long" ,"C8","C9","C10","C11","C12","C13")
colnames(incidentes) <- titulosIn
#Crear un data frame con los incidentes tipo ACV
ACV <-incidentes[incidentes$Tipo=="ACV - Accidente Vehicular",]

## Libre de errores
ACV<- ACV[ACV$C8=="",]
##ACV <- subset(ACV, ACV$Lat!="\"\"")
"Se ha creado el data frame de incidentes viales libre de errores"

#Errores Encontrados
#ACVCTosolve <- ACV[ACV$C8!="",]

#nTotObs <- nrow(ACV)
#nerroresObs <- nrow(ACVCTosolve)
#if(nerroresObs<nTotObs*.01){ 
#	"El número total de datos sucios es menor al 1%,"
#}else{
#	c("El número total de datos sucios es mayor al 1% el experimento prodría no ser exacto debido a esto el % de datos sucios es" , (nerroresObs/nTotObs)*100, " %")
#}
##plot(ACV$Lat,ACV$Long, xlab="Lat", ylab="long" )
ACV$Lat <- gsub("\"",'',ACV$Lat)
library(stringr)
ACV$Lat <- str_trim(ACV$Lat)

#Limpiar Longitud
ACV$Long <- gsub("\"",'',ACV$Long)
ACV$Long <- str_trim(ACV$Long)
#Limpiar Fecha
ACV$Fecha.Hora <- gsub("\"",'',ACV$Fecha.Hora)
ACV$Fecha.Hora <- str_trim(ACV$Fecha.Hora)

#limpiar lot y longitud que no ssean numeros
good <- complete.cases(as.numeric(ACV$Lat), as.numeric(ACV$Long))
ACV <- ACV[good,]

#crear la cuadricula de zonas del DF
source("PoCFunctions.R")
dfZona <- createGrid(19.266608,-99.266069,19.569618,-98.933722,10)
"Calculando las zonas de Accidentes"
zonasName <- getZones(dfZona, ACV$Lat,ACV$Long)
factZonas <- factor(zonasName)
ACV$C8<-factZonas
baresDF <- read.csv(file="INEGI_DENUE_20102015.csv")
"Calculando zona de Bares"
zonasBares <- getZones(dfZona, baresDF$Latitud,baresDF$Longitud)
factZonasBares <- factor(zonasBares)
baresDF["zona"]<- factZonasBares
dfACV<-sapply(split(ACV,factZonas),nrow)
dfBares <- sapply(split(baresDF,factZonasBares),nrow)
dataACV<- data.frame(dfACV)
de <- merge(dataACV, dfBares, by=0, all=TRUE)
de[is.na(de)] <- 0 
colnam <- c("Zona", "Accidentes Viales", "Total de Bares")
colnames(de) <- colnam
dfTodo <- data.frame(zona=as.numeric(de$Zona), Accidentes=de$`Accidentes Viales`,bares=de$`Total de Bares`)
"Relación entre las métricas"
pairs(dfTodo)
"Coeficiente de coorelación"
cor(dfTodo)
#dfPuntoI <-dfZona[dfZona$area==44,]
#createKML(dfPuntoI$area,c(dfPuntoI$lat1,dfPuntoI$lon1,dfPuntoI$lat2,dfPuntoI$lon2))

regresion <- lm( Accidentes ~bares, data=dfTodo)
plot(dfTodo$bares,dfTodo$Accidentes,ylab="Accidentes", xlab = "Bares")
abline(regresion)

#Predicción
nuevosBares <- data.frame(bares = seq(1, 10))
"Predicciones"
predict(regresion, nuevosBares)

#dfPuntoI <-dfZona[dfZona$area==54,]
#createKML(dfPuntoI$area,c(dfPuntoI$lat1,dfPuntoI$lon1,dfPuntoI$lat2,dfPuntoI$lon2))

 hist(as.numeric(ACV$C8), col="green", xlab = "Zonas Ciudad de México", ylab = "#Accidentes Viales", main= "Total de Accidentes DF")
 hist(as.numeric(baresDF$zona), col="blue", xlab = "Zonas", ylab = "# Bares", main = "Bares Ciduad de México")
 