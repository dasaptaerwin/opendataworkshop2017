# Add latitude and longitude to the Palaeochannel data Yasmin
# bounding lat and longs
long <- c(149.743,149.746)
lat <- c(-29.272,-29.274)

# libraries
library(sp)
library(rgdal)
library(tidyverse)

# set working dir
setwd("C:/Users/rver4657/Google Drive/ITB_Usyd_Project2017/workshop_opendata/OriginalDataFolder")

# conversion to spatial data
xy <- data.frame(ID = 1:2, X = long, Y = lat)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
# transform to eastings and northings
res <- spTransform(xy, CRS("+proj=utm +zone=55 ellps=WGS84"))
test <- as(res, "SpatialPoints")
str(test)

# calculate the hypothenusa 
z <- sqrt((test@coords[1,1]-test@coords[2,1])^2 + 
            (test@coords[1,2]-test@coords[2,2])^2)
z
# calculate sin and cos
sin_theta <- (test@coords[1,1]-test@coords[2,1])/z
cos_theta <- (test@coords[1,2]-test@coords[2,2])/z

# calculate eastings and northings for drill locations
sites <- c(0,50,120,160,240,370)
newX <- vector("numeric", length=length(sites))
newY <- vector("numeric", length=length(sites))

for (i in 1:length(sites)) {
  newX[i] <- test@coords[1,1] - sites[i] * sin_theta
  newY[i] <- test@coords[1,2] - sites[i] * cos_theta
}

# check
plot(res)
points(newX,newY)

# calculate eastings and northings for all locations
sites_all <- seq(0,370,by=10)
newX_all <- vector("numeric", length=length(sites_all))
newY_all <- vector("numeric", length=length(sites_all))

for (i in 1:length(sites_all)) {
  newX_all[i] <- test@coords[1,1] - sites_all[i] * sin_theta
  newY_all[i] <- test@coords[1,2] - sites_all[i] * cos_theta
}

# check
plot(res)
points(newX,newY)
points(newX_all,newY_all, pch=2)

# Now transform back to Lat and Long
newxy <- data.frame(ID = 1:length(sites), X = newX, Y = newY)
coordinates(newxy) <- c("X", "Y")
proj4string(newxy) <- CRS("+proj=utm +zone=55 ellps=WGS84")  ## for example
# transform back
new_res <- spTransform(newxy, CRS("+proj=longlat +datum=WGS84"))
new_res

# Now transform back to Lat and Long
newxy_all <- data.frame(ID = 1:length(sites_all), 
                        X = newX_all, Y = newY_all)
coordinates(newxy_all) <- c("X", "Y")
proj4string(newxy_all) <- CRS("+proj=utm +zone=55 ellps=WGS84") 
# transform back
new_res_all <- spTransform(newxy_all, CRS("+proj=longlat +datum=WGS84"))
new_res_all


# read in the soil particle, chloride and moisture data
soil <- read_csv("Willem/soilparticles.csv")
moisture <- read_csv("Willem/moisture.csv")
chloride <- read_csv("Willem/chloride.csv")

sites <- c(1,60,130,170,250,380)
# Do soil first
soil_test <- list()

# Add latitude and longitude
for (i in 1:length(sites)) {
  soil_test[[i]] <- soil %>%
    filter(Distance == sites[i]) %>%
    mutate(Long = new_res@coords[i,1],
           Lat = new_res@coords[i,2])
}

soil_new <- do.call(rbind,soil_test)
# rearrange columns 
soil_new <- soil_new[, c(10,11,1:9)]

# repeat for moisture
mois_test <- list()

for (i in 1:length(sites)) {
  mois_test[[i]] <- moisture %>%
    filter(Distance == sites[i]) %>%
    mutate(Long = new_res@coords[i,1],
           Lat = new_res@coords[i,2])
}

moisture_new <- do.call(rbind,mois_test)
moisture_new <- moisture_new[, c(5,6,1:4)]

# repeat for chloride
cl_test <- list()

for (i in 1:length(sites)) {
  cl_test[[i]] <- chloride %>%
    filter(Distance == sites[i]) %>%
    mutate(Long = new_res@coords[i,1],
           Lat = new_res@coords[i,2])
}

chloride_new <- do.call(rbind,cl_test)
chloride_new <- chloride_new[, c(9,10,1:8)]

# now write all to disk
write_csv(soil_new,"Willem/SoilParticles.csv")
write_csv(moisture_new,"Willem/Moisture.csv")
write_csv(chloride_new,"Willem/Chloride.csv")

# now do the same for the EM34 data
EM34 <- read_csv("Willem/EM34.csv")

EM34_new <- EM34 %>%
    mutate(Long = new_res_all@coords[,1],
           Lat = new_res_all@coords[,2])

# rearrange columns 
EM34_new <- EM34_new[, c(23,24,1:22)]

write_csv(EM34_new,"Willem/EM34.csv")

# EM38 is different, every 1 m only for first 150 m 
EM38 <- read_csv("Willem/EM38.csv")

# calculate eastings and northings for all locations
sites_EM38 <- EM38$Distance
newX_EM38 <- vector("numeric", length=length(sites_EM38))
newY_EM38 <- vector("numeric", length=length(sites_EM38))

for (i in 1:length(sites_EM38)) {
  newX_EM38[i] <- test@coords[1,1] - sites_EM38[i] * sin_theta
  newY_EM38[i] <- test@coords[1,2] - sites_EM38[i] * cos_theta
}


# Now transform back to Lat and Long
newxy_EM38 <- data.frame(ID = 1:length(sites_EM38), 
                        X = newX_EM38, Y = newY_EM38)
coordinates(newxy_EM38) <- c("X", "Y")
proj4string(newxy_EM38) <- CRS("+proj=utm +zone=55 ellps=WGS84") 
# transform back
new_res_EM38 <- spTransform(newxy_EM38, CRS("+proj=longlat +datum=WGS84"))
new_res_EM38

EM38_new <- EM38 %>%
  mutate(Long = new_res_EM38@coords[,1],
         Lat = new_res_EM38@coords[,2])

# rearrange columns 
EM38_new <- EM38_new[, c(10,11,1:9)]

write_csv(EM38_new,"Willem/EM38.csv")
