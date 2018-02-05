##############################################
# install and load package
install.packages("rgdal") # package for spatial data manipulation
library(rgdal)
install.packages("tidyverse")
library(tidyverse)
install.packages("raster")
library(raster)
install.packages("rasterVis")
library(rasterVis)
install.packages("rgeos")
library(rgeos)
install.packages("maptools")
library(maptools)
install.packages("mapdata")
library(mapdata)

# load data
df <- read.csv("data_corrected.csv")
str(df)
View(df)

# spatial environment
coordinates <- SpatialPoints(coords = df[,c("X_Long", "Y_Lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))
spointsdf <- SpatialPointsDataFrame(coordinates,df)


# plot it
plot(spointsdf, pch = 20, col = "steelblue")

# checking map by country
getData("ISO3")%>%
  as.data.frame%>%
  filter(NAME=="Indonesia")
IDN <- getData('GADM', country='IDN', level=1)
plot(IDN)
IDN %>% gSimplify(0.01) %>% plot() #gSimplify to simplify polygon

# checking at West Java (Jabar) Province
IDN@data
map_jabar <- subset(IDN,VARNAME_1=="Jabar") %>% gSimplify(0.01) 
plot(map_jabar)

# plot map using ggplot2
 
p <- ggplot() + geom_polygon(data = map_jabar, aes(x=long, y = lat, fill="#FF9999", group=group)) + 
  coord_fixed(1.3) +
  geom_point(data = df, aes(x=X_Long, y = Y_Lat))  


















