#For finding the significant trends in MODIS pixels that meet all OLS assumptions
library(raster)
library(spatialEco)

setwd("G:/MODIS/Chapter 2/")

ndviR2 <- raster("NDVI/mrNDVI_tot_r2.tif")
ndvisw <- raster("NDVI/mrNDVI_sw.tif")
ndvibg <- raster("NDVI/mrNDVI_bg.tif")
ndvibp <- raster("NDVI/mrNDVI_bp.tif")
ndvi_sg <- raster("Mar14_Final/RESTREND_NDVI/mrNDVI_restrend_mkSg.tif")
ndvi_pval <- raster("Mar14_Final/RESTREND_NDVI/mrNDVI_restrend_mkpval.tif")
cloud <- raster("MOD13_Cloudmask/Less5pt_cloudy.tif")

#Make table to use reclassify tool then reclassify rasters based on masks
m <- c(0, 0.05, NA, 0.05,1,1)
rclmat <- matrix(m, ncol=3, byrow = TRUE)

#make R2 table
m2 <- c(0,0.35,NA, 0.35,1,1)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)

#Make Kendall's sign masks for positive and no trend
m3 <- c(-1,0,NA,0,1,1)
m4 <- c(-1,0,1,0,1,NA)

rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
rclmat4 <- matrix(m4, ncol=3, byrow=TRUE)

#For potential
ndvisw <- reclassify(ndvisw, rclmat)
ndvibg <- reclassify(ndvibg, rclmat)
ndvibp <- reclassify(ndvibp, rclmat)
ndvir2 <- reclassify(ndviR2, rclmat2)

#Make new Extent
newextent <- raster::alignExtent(ndvisw, ndvi_pval)

ndvisw2 <- setExtent(ndvisw, newextent, keepres = FALSE)
ndvipval2 <- setExtent(ndvi_pval, ext = newextent, keepres = FALSE)
ndvibg2 <- setExtent(ndvibg, newextent, keepres = FALSE)
ndvibp2 <- setExtent(ndvibp, newextent, keepres = FALSE)
cloud2 <- setExtent(cloud, newextent, keepres = FALSE)
ndvir22 <- setExtent(ndvir2, newextent, keepres = FALSE)

####Mask files#####
ndvipot <- mask(ndvipval2, ndvisw2)
ndvipot <- mask(ndvipot, ndvibg2)
ndvipot <- mask(ndvipot, ndvibp2)
ndvipot <- mask(ndvipot, ndvir22)
ndvipot <- mask(ndvipot, cloud2)

#####RESTREND NDVI pos and neg trends####
m5 <- c(0,0.05,1,0.05,1,NA)
rclmat5 <- matrix(m5, ncol=3, byrow=TRUE)
ndvi_sig <- reclassify(ndvipot,rclmat5)
#negative values
neg6 <- c(-84,0,1,0,84,NA)
rclmatneg <- matrix(neg6, ncol=3,byrow = TRUE)
neg <- reclassify(ndvi_sg, rclmatneg)
#positive values
pos6 <- c(-84,0,NA,0,84,1)
rclmatpos <- matrix(pos6, ncol=3,byrow = TRUE)
pos <- reclassify(ndvi_sg, rclmatpos)

#Mask
pos <- setExtent(pos, newextent, keepres = FALSE)
neg <- setExtent(neg, newextent, keepres = FALSE)

NDVI_trendPos <- mask(ndvi_sig,pos)
NDVI_trendNeg <- mask(ndvi_sig,neg)
writeRaster(NDVI_trendPos, filename = "April18/ndvi_trend_pos05.tif", overwrite = TRUE)
writeRaster(NDVI_trendNeg, filename = "April18/ndvi_trend_neg05.tif", overwrite = TRUE)


m1 <- c(0,1,1)
m11 <- matrix(m1, ncol=3,byrow=TRUE)
ndvipot <- reclassify(ndvipot, m11)
writeRaster(ndvipot, filename = "April18/ndvipot.tif", overwrite = TRUE)


######RESTREND EVI#####

eviR2 <- raster("evi/mrevi_tot_r2.tif")
evisw <- raster("evi/mrevi_sw.tif")
evibg <- raster("evi/mrevi_bg.tif")
evibp <- raster("evi/mrevi_bp.tif")
evi_sg <- raster("Mar14_Final/RESTREND_evi/mrevi_restrend_mkSg.tif")
evi_pval <- raster("Mar14_Final/RESTREND_evi/mrevi_restrend_mkpval.tif")
cloud <- raster("MOD13_Cloudmask/Less5pt_cloudy.tif")

#Make table to use reclassify tool then reclassify rasters based on masks
m <- c(0, 0.05, NA, 0.05,1,1)
rclmat <- matrix(m, ncol=3, byrow = TRUE)

#make R2 table
m2 <- c(0,0.35,NA, 0.35,1,1)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)

#Make Kendall's sign masks for positive and no trend
m3 <- c(-1,0,NA,0,1,1)
m4 <- c(-1,0,1,0,1,NA)

rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
rclmat4 <- matrix(m4, ncol=3, byrow=TRUE)

#For potential
evisw <- reclassify(evisw, rclmat)
evibg <- reclassify(evibg, rclmat)
evibp <- reclassify(evibp, rclmat)
evir2 <- reclassify(eviR2, rclmat2)

#Make new Extent
newextent <- raster::alignExtent(evisw, evi_pval)

evisw2 <- setExtent(evisw, newextent, keepres = FALSE)
evipval2 <- setExtent(evi_pval, ext = newextent, keepres = FALSE)
evibg2 <- setExtent(evibg, newextent, keepres = FALSE)
evibp2 <- setExtent(evibp, newextent, keepres = FALSE)
cloud2 <- setExtent(cloud, newextent, keepres = FALSE)
evir22 <- setExtent(evir2, newextent, keepres = FALSE)

####Mask files
evipot <- mask(evipval2, evisw2)
evipot <- mask(evipot, evibg2)
evipot <- mask(evipot, evibp2)
evipot <- mask(evipot, evir22)
evipot <- mask(evipot, cloud2)

#####evi pos and neg trends##
m5 <- c(0,0.05,1,0.05,1,NA)
rclmat5 <- matrix(m5, ncol=3, byrow=TRUE)
evi_sig <- reclassify(evipot,rclmat5)
#negative values
neg6 <- c(-84,0,1,0,84,NA)
rclmatneg <- matrix(neg6, ncol=3,byrow = TRUE)
neg <- reclassify(evi_sg, rclmatneg)
#positive values
pos6 <- c(-84,0,NA,0,84,1)
rclmatpos <- matrix(pos6, ncol=3,byrow = TRUE)
pos <- reclassify(evi_sg, rclmatpos)

#Mask
pos <- setExtent(pos, newextent, keepres = FALSE)
neg <- setExtent(neg, newextent, keepres = FALSE)

evi_trendPos <- mask(evi_sig,pos)
evi_trendNeg <- mask(evi_sig,neg)
writeRaster(evi_trendPos, filename = "April18/evi_trend_pos05.tif", overwrite = TRUE)
writeRaster(evi_trendNeg, filename = "April18/evi_trend_neg05.tif", overwrite = TRUE)
##
#
#This has to go last or you end up overwriting the EVI_significanec file you use for potential. 

m1 <- c(0,1,1)
m11 <- matrix(m1, ncol=3,byrow=TRUE)
evipot <- reclassify(evipot, m11)
writeRaster(evipot, filename = "April18/evipot.tif", overwrite = TRUE)



#
####EMODIS NDVI#####
#

emodR2 <- raster("eMODIS/R_Output_MR/mrNDVI_tot_r2.tif")
emodsw <- raster("eMODIS/R_Output_MR/mrNDVI_sw.tif")
emodbg <- raster("eMODIS/R_Output_MR/mrNDVI_bg.tif")
emodbp <- raster("eMODIS/R_Output_MR/mrNDVI_bp.tif")
emod_sg <- raster("eMODIS/R_Output_MR/mrNDVI_restrend_Sg.tif")
emod_pval <- raster("eMODIS/R_Output_MR/mrNDVI_restrend_mkp.tif")
cloud <- raster("eMODIS/FailMasks/Naryn_ValidArea.tif")

#Make table to use reclassify tool then reclassify rasters based on masks
m <- c(0, 0.05, NA, 0.05,1,1)
rclmat <- matrix(m, ncol=3, byrow = TRUE)

#make R2 table
m2 <- c(0,0.35,NA, 0.35,1,1)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)

#Make Kendall's sign masks for positive and no trend
m3 <- c(-1,0,NA,0,1,1)
m4 <- c(-1,0,1,0,1,NA)

rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
rclmat4 <- matrix(m4, ncol=3, byrow=TRUE)

#For potential
emodsw <- reclassify(emodsw, rclmat)
emodbg <- reclassify(emodbg, rclmat)
emodbp <- reclassify(emodbp, rclmat)
emodr2 <- reclassify(emodR2, rclmat2)

#Make new Extent
newextent <- raster::alignExtent(emodsw, emod_pval)

emodsw2 <- setExtent(emodsw, newextent, keepres = FALSE)
emodpval2 <- setExtent(emod_pval, ext = newextent, keepres = FALSE)
emodbg2 <- setExtent(emodbg, newextent, keepres = FALSE)
emodbp2 <- setExtent(emodbp, newextent, keepres = FALSE)
cloud2 <- setExtent(cloud, newextent, keepres = FALSE)
emodr22 <- setExtent(emodr2, newextent, keepres = FALSE)

####Mask files#####
emodpot <- mask(emodpval2, emodsw2)
emodpot <- mask(emodpot, emodbg2)
emodpot <- mask(emodpot, emodbp2)
emodpot <- mask(emodpot, emodr22)
emodpot <- mask(emodpot, cloud2)


#####emod pos and neg trends####
m5 <- c(0,0.05,1,0.05,1,NA)
rclmat5 <- matrix(m5, ncol=3, byrow=TRUE)
emod_sig <- reclassify(emodpot,rclmat5)
#negative values
neg6 <- c(-84,0,1,0,84,NA)
rclmatneg <- matrix(neg6, ncol=3,byrow = TRUE)
neg <- reclassify(emod_sg, rclmatneg)
#positive values
pos6 <- c(-84,0,NA,0,84,1)
rclmatpos <- matrix(pos6, ncol=3,byrow = TRUE)
pos <- reclassify(emod_sg, rclmatpos)

#Mask
pos <- setExtent(pos, newextent, keepres = FALSE)
neg <- setExtent(neg, newextent, keepres = FALSE)

emod_trendPos <- mask(emod_sig,pos)
emod_trendNeg <- mask(emod_sig,neg)
writeRaster(emod_trendPos, filename = "April18/emod_trend_pos05.tif", overwrite = TRUE)
writeRaster(emod_trendNeg, filename = "April18/emod_trend_neg05.tif", overwrite = TRUE)

m1 <- c(0,1,1)
m11 <- matrix(m1, ncol=3,byrow=TRUE)
emodpot <- reclassify(emodpot, m11)
writeRaster(emodpot, filename = "April18/emodpot.tif", overwrite = TRUE)

#######Seasonal Kendall on EVI and NDVi######

csmk_evi <- raster("G:/MODIS/Chapter 2/Mar14_Final/CSMK/csmk_EVI_pval.tif")
csmk_evi_Sg <- raster("G:/MODIS/Chapter 2/Mar14_Final/CSMK/csmk_EVI_stot.tif")
m5 <- c(-6335,-1,NA,0,6335,1)
m6 <- c(-6335,0,1,0,6335,NA)
num1 <- matrix(m5, ncol=3, byrow=TRUE)
num2 <- matrix(m6, ncol=3, byrow=TRUE)
pos_Sg <- reclassify(csmk_evi_Sg, num1)
neg_Sg <- reclassify(csmk_evi_Sg, num2)
m7 <- c(0,0.05,1,0.05,1,NA)
num3 <- matrix(m7, ncol=3, byrow=TRUE)
sig <- reclassify(csmk_evi,num3)
pos_evi <- mask(sig,pos_Sg)
test <- c(-1,1,1)
pos_evi <- reclassify(pos_evi, test)
neg_evi <- mask(sig, neg_Sg)

#Mask for clouds
cloud <- raster("MOD13_Cloudmask/Less5pt_cloudy.tif")
newextent <- raster::alignExtent(cloud, csmk_evi_Sg)
cloud2 <- setExtent(cloud, newextent, keepres = FALSE)
neg_evi<- setExtent(neg_evi, newextent, keepres= FALSE)
pos_evi<- setExtent(pos_evi, newextent, keepres= FALSE)
neg_evi <- mask(neg_evi, cloud2)
pos_evi <- mask(pos_evi, cloud2)

writeRaster(neg_evi, filename = "G:/MODIS/Chapter 2/April18/csmk_evi_neg05.tif", overwrite = TRUE)
writeRaster(pos_evi, filename = "G:/MODIS/Chapter 2/April18/csmk_evi_pos05.tif", overwrite = TRUE)
#
#
####NDVI
csmk_ndvi <- raster("G:/MODIS/Chapter 2/Mar14_Final/CSMK/csmk_ndvi_pval.tif")
csmk_ndvi_Sg <- raster("G:/MODIS/Chapter 2/Mar14_Final/CSMK/csmk_ndvi_stot.tif")
m5 <- c(-5670,0,NA,0,6704,1)
m6 <- c(-5670,0,1,0,6704,NA)
num1 <- matrix(m5, ncol=3, byrow=TRUE)
num2 <- matrix(m6, ncol=3, byrow=TRUE)
pos_Sg <- reclassify(csmk_ndvi_Sg, num1)
neg_Sg <- reclassify(csmk_ndvi_Sg, num2)
m7 <- c(0,0.05,1,0.05,1,NA)
num3 <- matrix(m7, ncol=3, byrow=TRUE)
sig <- reclassify(csmk_ndvi,num3)
pos_ndvi <- mask(sig, pos_Sg)
#can't figure out why this is happening but need to be done to make some 0 values into 1s
test <- c(-1,1,1)
pos_ndvi <- reclassify(pos_ndvi, test)
neg_ndvi <- mask(sig, neg_Sg)

#Cloud mask
cloud <- raster("MOD13_Cloudmask/Less5pt_cloudy.tif")
newextent <- raster::alignExtent(cloud, csmk_ndvi_Sg)
cloud2 <- setExtent(cloud, newextent, keepres = FALSE)
neg_ndvi<- setExtent(neg_ndvi, newextent, keepres= FALSE)
pos_ndvi<- setExtent(pos_ndvi, newextent, keepres= FALSE)
neg_ndvi <- mask(neg_ndvi, cloud2)
pos_ndvi <- mask(pos_ndvi, cloud2)


writeRaster(neg_ndvi, filename = "G:/MODIS/Chapter 2/April18/csmk_ndvi_neg05.tif", overwrite = TRUE)
writeRaster(pos_ndvi, filename = "G:/MODIS/Chapter 2/April18/csmk_ndvi_pos05.tif", overwrite = TRUE)

bob <- c(-1,7,1)
bob2 <- matrix(bob, ncol = 3, byrow = TRUE)
csmk_pot <- reclassify(cloud2, bob2)
writeRaster(csmk_pot, filename = "G:/MODIS/Chapter 2/April18/csmk_pot.tif", overwrite = TRUE)
