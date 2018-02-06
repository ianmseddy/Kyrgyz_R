library(raster)
library(spatialEco)
library(maptools)
#####To switch to 0.1 thresholds for significance, just take out the 05 at the end of all the tifs. 


setwd("C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/Chapter2/GIS/")
herder <- shapefile("orig_herdermap.shp")
seasonal <- shapefile("Final_pasture_WGS.shp")

#####Get EMODIS data
emod_ndvineg <- raster("G:/MODIS/Chapter 2/April18/emod_trend_neg05.tif")
emod_ndvipos <- raster("G:/MODIS/Chapter 2/April18/emod_trend_pos05.tif")
emodpot <- raster("G:/MODIS/Chapter 2/April18/emodpot.tif")

#NDVI
ndvineg <- raster("G:/MODIS/Chapter 2/April18/ndvi_trend_neg05.tif")
ndvipos <- raster("G:/MODIS/Chapter 2/April18/ndvi_trend_pos05.tif")
ndvipot <- raster("G:/MODIS/Chapter 2/April18/ndvipot.tif")

#EVI
evineg <- raster("G:/MODIS/Chapter 2/April18/evi_trend_neg05.tif")
evipos <- raster("G:/MODIS/Chapter 2/April18/evi_trend_pos05.tif")
evipot <- raster("G:/MODIS/Chapter 2/April18/evipot.tif")

#CSMK
csmk_evi_neg <- raster("G:/MODIS/Chapter 2/April18/csmk_evi_neg05.tif")
csmk_evi_pos <- raster("G:/MODIS/Chapter 2/April18/csmk_evi_pos05.tif")
csmk_ndvi_neg <- raster("G:/MODIS/Chapter 2/April18/csmk_ndvi_neg05.tif")
csmk_ndvi_pos <- raster("G:/MODIS/Chapter 2/April18/csmk_ndvi_pos05.tif")
csmk_pot <- raster("G:/MODIS/Chapter 2/April18/csmk_pot.tif")


sum1 <- function(x) {
          sum(x)
        }

emneg <- zonal.stats(herder, emod_ndvineg, sum1)
empos <- zonal.stats(herder, emod_ndvipos, sum1)
empot <- zonal.stats(herder, emodpot, sum1)
nneg <- zonal.stats(herder, ndvineg, sum1)
npos <- zonal.stats(herder, ndvipos, sum1)
npot <- zonal.stats(herder, ndvipot, sum1)
evneg <- zonal.stats(herder, evineg,sum1)
evpos <- zonal.stats(herder, evipos, sum1)
evpot <- zonal.stats(herder, evipot, sum1)
#csmk
cnneg <- zonal.stats(herder, csmk_ndvi_neg, sum1)
cnpos <- zonal.stats(herder, csmk_ndvi_pos, sum1)
ceneg <- zonal.stats(herder, csmk_evi_neg, sum1)
cepos <- zonal.stats(herder, csmk_evi_pos, sum1)
csmkpot <- zonal.stats(herder, csmk_pot, sum1)
name = herder$Name

ss <- data.frame(name = name, emodneg = emneg, emodpos = empos, empot = empot, ndvineg = nneg, ndvipos = npos,
                 ndvipot = npot, evineg = evneg, evipos = evpos, evipot = evpot, sk_ndvi_neg = cnneg,
                 sk_ndvi_pos = cnpos, sk_evi_neg = ceneg, sk_evi_pos = cepos, sk_pot = csmkpot)

write.csv(ss, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/herderPolygons_1_05.csv")

######Seasonal Pasture Use

#emneg1 <- zonal.stats(seasonal1, emod_ndvineg, sum1)
#empos1 <- zonal.stats(seasonal1, emod_ndvipos, sum1)
#empot1 <- zonal.stats(seasonal1, emodpot, sum1)
#nneg1 <- zonal.stats(seasonal1, ndvineg, sum1)
#npos1 <- zonal.stats(seasonal1, ndvipos, sum1)
#npot1 <- zonal.stats(seasonal1, ndvipot, sum1)
#evneg1 <- zonal.stats(seasonal1, evineg,sum1)
#evpos1 <- zonal.stats(seasonal1, evipos, sum1)
#evpot1 <- zonal.stats(seasonal1, evipot, sum1)

#cnneg1 <- zonal.stats(seasonal1, csmk_ndvi_neg, sum1)
#cnpos1 <- zonal.stats(seasonal1, csmk_ndvi_pos, sum1)
#ceneg1 <- zonal.stats(seasonal1, csmk_evi_neg, sum1)
#cepos1 <- zonal.stats(seasonal1, csmk_evi_pos, sum1)
#csmkpot1 <- zonal.stats(seasonal1, csmk_pot, sum1)
#name = seasonal1$Pstr_Class
#
#ss1 <- data.frame(name = name, emodneg = emneg1, emodpos = empos1, empot = empot1, ndvineg = nneg1, ndvipos = npos1,
#                 ndvipot = npot1, evineg = evneg1, evipos = evpos1, evipot = evpot1, sk_ndvi_neg = cnneg1,
#                 sk_ndvi_pos = cnpos1, sk_evi_neg = ceneg1, sk_evi_pos = cepos1, csmkpot = csmkpot1)
#
#write.csv(ss1, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/SeasonalPolygons_1_05.csv")


####Seasonal pastures with single pastures


emneg2 <- zonal.stats(seasonal, emod_ndvineg, sum1)
empos2 <- zonal.stats(seasonal, emod_ndvipos, sum1)
empot2 <- zonal.stats(seasonal, emodpot, sum1)
nneg2 <- zonal.stats(seasonal, ndvineg, sum1)
npos2 <- zonal.stats(seasonal, ndvipos, sum1)
npot2 <- zonal.stats(seasonal, ndvipot, sum1)
evneg2 <- zonal.stats(seasonal, evineg,sum)
evpos2 <- zonal.stats(seasonal, evipos, sum1)
evpot2 <- zonal.stats(seasonal, evipot, sum1)

cnneg2 <- zonal.stats(seasonal, csmk_ndvi_neg, sum1)
cnpos2 <- zonal.stats(seasonal, csmk_ndvi_pos, sum1)
ceneg2 <- zonal.stats(seasonal, csmk_evi_neg, sum1)
cepos2 <- zonal.stats(seasonal, csmk_evi_pos, sum1)
csmkpot2 <- zonal.stats(seasonal, csmk_pot, sum1)
name = seasonal$Pstr_Class

ss2 <- data.frame(name = name, emodneg = emneg2, emodpos = empos2, empot = empot2, ndvineg = nneg2, ndvipos = npos2,
                  ndvipot = npot2, evineg = evneg2, evipos = evpos2, evipot = evpot2, sk_ndvi_neg = cnneg2,
                  sk_ndvi_pos = cnpos2, sk_evi_neg = ceneg2, sk_evi_pos = cepos2, csmkpot = csmkpot2)

write.csv(ss2, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/SeasonalPolygons_indiv_05.csv")