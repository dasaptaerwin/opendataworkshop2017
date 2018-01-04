# Code for image manipulation and plotting exercise
# Open data workshop
# Dasapta Erwin Irawan, Willem Vervoort
# 5-9 Feb 2018

###### EBImage ######
# Ref: http://www.bioconductor.org/packages/3.7/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library("EBImage")
img = readImage("2.jpg")
display(img, method="browser")
display(img, method="raster")
text(x = 20, y = 20, label = "Parrots", adj = c(0,1), col = "orange", cex = 2)

###### imager #######
# Ref: https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html
# Good for photo manipulation
install.packages("imager")
library(imager)
plot(boats)
library(tidyverse)
bdf <- as.data.frame(boats)
head(bdf,3)
bdf <- mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf,aes(value,col=channel))+
  geom_histogram(bins=30)+
  facet_wrap(~ channel)


####### exifr #######
# Ref: https://www.r-bloggers.com/extracting-exif-data-from-photos-using-r/
# I think this is good for photos plotting exercise
install.packages(c("exifr", "leaflet"))
# you have to install Perl on windows, as this is not included
# It is included on Apple
# install from:http://strawberryperl.com/
# then point exifr to perl dir
options(exifr.perlpath='c:/strawberry')
library(exifr)
library(leaflet)
library(tidyverse)
files <- list.files(pattern = "*.jpg")
dat <- read_exif(files)
plot(dat$GPSLongitude, dat$GPSLatitude)
leaflet(dat) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(~ GPSLongitude, ~ GPSLatitude)  
