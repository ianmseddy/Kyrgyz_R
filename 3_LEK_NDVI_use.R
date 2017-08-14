library(raster)
library(spatialEco)
library(maptools)
#####To switch to 0.1 thresholds for significance, just take out the 05 at the end of all the tifs. 


setwd("C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/Chapter2/GIS/")
herder <- shapefile("orig_herdermap.shp")
seasonal <- shapefile("Final_pasture_WGS.shp")

#####Get EMODIS data

#NDVI
ndvineg <- raster("G:/MODIS/Chapter 2/April18/ndvi_trend_neg05.tif")
ndvipos <- raster("G:/MODIS/Chapter 2/April18/ndvi_trend_pos05.tif")
ndvipot <- raster("G:/MODIS/Chapter 2/April18/ndvipot.tif")

#CSMK
csmk_ndvi_neg <- raster("G:/MODIS/Chapter 2/April18/csmk_ndvi_neg05.tif")
csmk_ndvi_pos <- raster("G:/MODIS/Chapter 2/April18/csmk_ndvi_pos05.tif")
csmk_pot <- raster("G:/MODIS/Chapter 2/April18/csmk_pot.tif")


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
                 sk_ndvi_pos = cnpos, sk_pot = csmkpot)

write.csv(ss, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/herderPolygons_05_july22.csv")

####Seasonal pastures with single pastures

nneg2 <- zonal.stats(seasonal, ndvineg, sum1)
npos2 <- zonal.stats(seasonal, ndvipos, sum1)
npot2 <- zonal.stats(seasonal, ndvipot, sum1)

cnneg2 <- zonal.stats(seasonal, csmk_ndvi_neg, sum1)
cnpos2 <- zonal.stats(seasonal, csmk_ndvi_pos, sum1)
csmkpot2 <- zonal.stats(seasonal, csmk_pot, sum1)
name = seasonal$Pstr_Class

ss2 <- data.frame(name = name, ndvineg = nneg2, ndvipos = npos2, ndvipot = npot2, sk_ndvi_neg = cnneg2,
                  sk_ndvi_pos = cnpos2, csmkpot = csmkpot2)

write.csv(ss2, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/SeasonalPolygons_05_july22.csv")