title: "Primary School"
author: "Ho Man Tang"
date: "2020/1/8"
output: html_document

#Library a few packages that will be used
```{r}
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(tmap)
library(sf)
library(geojsonio)
library(tmaptools)
library(data.table)
library(raster)
library(fpc)
library(plyr)
library(ggplot2)
```
#Plot all good primary schools 
```{r}
GPrimary = fread("Good Primary School Coordinations.csv")
GPrimary_dat = as.data.table(GPrimary)
coordinates(GPrimary_dat) = c("LONGITUDE","LATITUDE")
crs.geo1 = CRS("+proj=longlat")  
proj4string(GPrimary_dat) = crs.geo1 
plot(GPrimary_dat, pch = 20, col = "steelblue")
```
#Sets up an EPSG string to help set the projection of good primary school locations
```{r}
BNG = "+init=epsg:2326"
WGS = "+init=epsg:4326"
GPrimaryBNG <- spTransform(GPrimary_dat, BNG)
```
#Plot all primary schools 
```{r}
Primary = fread("Primary School Coordinations.csv")
Primary_dat = as.data.table(Primary)
coordinates(Primary_dat) = c("LONGITUDE","LATITUDE")
crs.geo1 = CRS("+proj=longlat")  
proj4string(Primary_dat) = crs.geo1 
plot(Primary_dat, pch = 20, col = "steelblue")
```
#Sets up an EPSG string to help set the projection of primary school locations
```{r}
BNG = "+init=epsg:2326"
WGS = "+init=epsg:4326"
PrimaryBNG <- spTransform(Primary_dat, BNG)
```
#Projects the administrative boundary of Hong Kong
```{r}
ADM <- st_read("Hong Kong Land Area Boundaries/HKG_adm0.shp")
ADMSP <- as(ADM, 'Spatial')
BNG = "+init=epsg:2326"
WGS = "+init=epsg:4326"
ADMSPBNG <- spTransform(ADMSP,BNG)
```
#Data cleaning for good primary school
```{r}
GPrimaryBNG <- remove.duplicates(GPrimaryBNG)
PrimaryBNG <- remove.duplicates(PrimaryBNG)
```
#Creates study area
```{r}
GPrimarySub <- GPrimaryBNG[ADMSPBNG,]
PrimarySub <- PrimaryBNG[ADMSPBNG,]
```
#Sets a window as the administrative boundary
```{r}
window <- as.owin(ADMSPBNG)
plot(window)
```
#Creates a PPP object
```{r}
GPrimarySub.ppp <- ppp(x=GPrimarySub@coords[,1],y=GPrimarySub@coords[,2],window=window)
PrimarySub.ppp <- ppp(x=PrimarySub@coords[,1],y=PrimarySub@coords[,2],window=window)
```
#Ripley's K
```{r}
GK <- Kest(GPrimarySub.ppp, correction="border", rmax = 3000)
K <- Kest(PrimarySub.ppp, correction="border", rmax = 3000)
plot(K,
xlab="Distance (M)", main ="K",col='blue',ylim = range(0,10^8*1.3))
legend(-50,10^8*1.28, c("All Primary Schools", "Good Primary Schools", "Kpois(r)"), fill=c("blue","black","red"),cex = 1.1)
plot(GK,
xlab="Distance (M)", main ="Good Primary Schools",add=TRUE)



```
#DBSCAN
```{r}
GPrimarySub <- data.frame(GPrimarySub@coords[,1:2])
db <- fpc::dbscan(GPrimarySub, eps = 1600, MinPts = 5)
plot(db, GPrimarySub, main = "DBSCAN Output", frame = F, xlab = "X", ylab = "Y" )
```
#Extract the coordinates of the cluster locations
```{r}
GPrimarySub$cluster <- db$cluster
GPrimarySub$isseed <- db$isseed
View(PrimarySub)
```

















