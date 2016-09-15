##funciones para el calculo de localizacion
coordinateIsInsidePolygon <- function( latitude, longitude, lat_array, long_array){
PI <- 3.14159265
i <- 1
angle <-0
point1_lat <- numeric()
point1_long <- numeric()
point2_lat <- numeric()
point2_long <- numeric()
n <- length(lat_array)
f <-T
for (i in 1:n) {
  point1_lat <- lat_array[i] - latitude
  point1_long <- long_array[i] - longitude
  
  j <-(i%%n)+1
  #print(c("i=",i," n=",n, " j=", j))
  
  point2_lat <- lat_array[j] - latitude
  point2_long <- long_array[j] - longitude
  
  #print(c("angulo:", angle))
  angle <- angle + Angle2D(point1_lat,point1_long,point2_lat,point2_long)
  #print(c("angulo:", angle))
}

if (abs(angle) < PI){
  f<- F
}
else{
  f<- T
}
f
}

Angle2D <-function(y1, x1, y2, x2){
dtheta <- 0
theta1 <- 0
theta2 <- 0
PI <- 3.14159265
NPI <- -3.14159265
TWOPI <- 2*PI

#print(c("Valores=",y1, x1, y2, x2))

theta1 <- atan2(y1,x1)
theta2 <- atan2(y2,x2)
dtheta <- theta2 - theta1

#print(c("Los datos", dtheta, PI, TWOPI))
while (dtheta > PI){
  dtheta <-  dtheta - TWOPI
  #print(c("entra al primer While",dtheta, TWOPI))
}

while (dtheta < NPI){
 dtheta <-dtheta + TWOPI
 #print(c("entra al segundo While", dtheta, TWOPI))
}
dtheta
}

##Funciones para la creación de cuadricula
createGrid <-function(latitud1, longitude1, latitud2, longitud2, nCells=100){
 
  factorLat <- abs((latitud1 - latitud2) / nCells)
  factorLon <- abs((longitude1 - longitud2) / nCells)

  arrayLat <- numeric(length = nCells+1)
  arrayLon <- numeric(length = nCells+1)
  
  finalArrLength =(nCells)^2
  area <- numeric(length = (finalArrLength))
  pArrayLat1 <- numeric(length = (finalArrLength))
  pArrayLon1 <- numeric(length = (finalArrLength))
  pArrayLat2 <- numeric(length = (finalArrLength))
  pArrayLon2 <- numeric(length = (finalArrLength))
  
  areaIt <-1
  arrayLat[1] <- latitud1
  arrayLon[1] <- longitude1

  for(i in 2:(nCells+1)){
    arrayLat[i] <- arrayLat[i-1] + factorLat
    arrayLon[i] <- arrayLon[i-1] + factorLon
  }

  for(j in 1:nCells){
    for(k in 1:nCells){
      pArrayLat1 [areaIt]<- arrayLat[j]
      pArrayLon1 [areaIt]<- arrayLon [k]
      pArrayLat2 [areaIt]<- arrayLat[j+1]
      pArrayLon2 [areaIt]<- arrayLon[k+1]
      area[areaIt] <- areaIt
      areaIt <-areaIt +1
    }
  }
  dfZonas <- data.frame(area=area, lat1=pArrayLat1, lon1=pArrayLon1, lat2=pArrayLat2, 
                        lon2=pArrayLon2,stringsAsFactors = F)
  dfZonas
}

getGPSArea<-function(dfZone, dfLat, dfLong){
   len <-  nrow(dfZone)
   aZone <- 0
  for(i in 1:len){
    latArr <- c(dfZone[i,2],dfZone[i,4],dfZone[i,4],dfZone[i,2])
    lonArr <- c(dfZone[i,3],dfZone[i,3],dfZone[i,5],dfZone[i,5])
    if(coordinateIsInsidePolygon(dfLat,dfLong,latArr,lonArr)){
      aZone<-dfZone[i,1]
      break
    }
  }
  aZone
}

getZones <- function(dfZone, latArr, longArr){
  n <-length(latArr)
  zonas <-numeric()
  for(i in 1:n){
    zonas[i]<-getGPSArea(dfZone,as.numeric(latArr[i]),as.numeric(longArr[i]))
    print(paste(i,zonas[i],sep = ","))
  }
  zonas
}

#Funcion para crear un archivo XML para mostrar una zona en google earth

createKML <- function(name, pointArr){
  line1<-paste(pointArr[2],",",pointArr[1],sep = '')
  line2<-paste(pointArr[2],",",pointArr[3],sep = '')
  line3<-paste(pointArr[4],",",pointArr[3],sep = '')
  line4<-paste(pointArr[4],",",pointArr[1],sep = '')
  library(XML)
  root <- newXMLNode("kml")
  Placemark <- newXMLNode("Placemark", parent = root)
  newXMLNode("name", name, parent = Placemark)
  Polygon <- newXMLNode("Polygon", parent = Placemark)
  outerBoundaryIs <- newXMLNode("outerBoundaryIs", parent = Polygon)
  LinearRing <- newXMLNode("LinearRing", parent = outerBoundaryIs)
  coordinates <- newXMLNode("coordinates",  parent = LinearRing)
  newXMLTextNode("\n", parent = coordinates)
  newXMLTextNode(line1, parent = coordinates)
  newXMLTextNode("\n", parent = coordinates)
  newXMLTextNode(line2, parent = coordinates)
  newXMLTextNode("\n", parent = coordinates)
  newXMLTextNode(line3, parent = coordinates)
  newXMLTextNode("\n", parent = coordinates)
  newXMLTextNode(line4, parent = coordinates)
  newXMLTextNode("\n", parent = coordinates)
  saveXML(Placemark,file=paste(name,".kml",sep = ''))
  #Placemark
}