library(raster)
library(spatialEco)
library(maptools)

setwd("C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Final_EI_Manuscript/GIS_for_R")
herder <- shapefile("LEK_polygon2.shp")
seasonal <- shapefile("Seasonal_Pasture.shp")

#####Get Restrend data
#NDVI
ndvineg <- raster("RESTREND_neg.tif")
ndvipos <- raster("RESTREND_pos.tif")
ndvipot <- raster("RESTREND_pot.tif")


#Get NDVI data
csmk_ndvi_neg <- raster("NDVI_neg.tif")
csmk_ndvi_pos <- raster("NDVI_pos.tif")
csmk_pot <- raster("NDVI_pot.tif")


sum1 <- function(x) {
          sum(x)
        }

nneg <- zonal.stats(herder, ndvineg, sum1)
npos <- zonal.stats(herder, ndvipos, sum1)
npot <- zonal.stats(herder, ndvipot, sum1)
#csmk
cnneg <- zonal.stats(herder, csmk_ndvi_neg, sum1)
cnpos <- zonal.stats(herder, csmk_ndvi_pos, sum1)
csmkpot <- zonal.stats(herder, csmk_pot, sum1)
name = herder$Name

ss <- data.frame(name = name, ndvineg = nneg, ndvipos = npos, ndvipot = npot, sk_ndvi_neg = cnneg,
                 sk_ndvi_pos = cnpos, sk_pot = csmkpot, lab = herder$PstrClass)

write.csv(ss, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Final_EI_Manuscript/Herder_ZonalStats.csv")
#You have to manually change NA to 0. 
######Seasonal Pasture Use


####Seasonal pastures with single pastures


nneg2 <- zonal.stats(seasonal, ndvineg, sum1)
npos2 <- zonal.stats(seasonal, ndvipos, sum1)
npot2 <- zonal.stats(seasonal, ndvipot, sum1)

cnneg2 <- zonal.stats(seasonal, csmk_ndvi_neg, sum1)
cnpos2 <- zonal.stats(seasonal, csmk_ndvi_pos, sum1)
csmkpot2 <- zonal.stats(seasonal, csmk_pot, sum1)
name = seasonal$Pstr_Class

ss2 <- data.frame(name = name, ndvineg = nneg2, ndvipos = npos2,ndvipot = npot2, sk_ndvi_neg = cnneg2,
                  sk_ndvi_pos = cnpos2, csmkpot = csmkpot2)

write.csv(ss2, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Final_EI_Manuscript/Seasonal_Pasture_Stats.csv")