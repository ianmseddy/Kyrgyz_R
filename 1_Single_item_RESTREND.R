#For testing linear regression on annual sum of MODIS images .

#####Load raster stack####
library(lmtest)
library(raster)
library(rgdal)
library(trend)
setwd("G:/MODIS/Chapter 2/GrowSeasonSum/")
WG.monthly <- read.csv("C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/WG_Climate_Monthly_Final.csv")
rain <- WG.monthly$MonthlyPrecip.mm

#Test GDD dataset
NCDC.GDD <- read.csv("C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/Naryn_NCDC_GDD.csv")
gdd <- NCDC.GDD$AprilAugust

####test out some different precipitation datasets. 
gr <- subset.data.frame(WG.monthly, WG.monthly$Month < 7 & WG.monthly$Month >3, select = 1:3)
gr1 <- gr$MonthlyPrecip.mm
gr2 <- list(as.factor(gr$Year))
gr_lm <- aggregate(gr1, FUN = sum, by = gr2)
lnApJune <- log(gr_lm$x)

####test association between pp and AGDD#######
spearmanrho <- cor.test(gr_lm$x, gdd, method = "spearman")
pearsonr <- cor.test(gr_lm$x, gdd, method = "pearson")

#Load MODIS Annual NDVI Sums
rlist <- list.files(pattern = '\\.tif$')
for(i in rlist) {assign(unlist(strsplit(i, "[.]"))[1], raster(i))}
rstack <- raster::stack(rlist)

ymax = raster::ncol(rstack)
xmax = raster::nrow(rstack)

dxmax = xFromCol(rstack,ymax)
dxmin = xFromCol(rstack,1)
dymin <- yFromRow(rstack,xmax)
dymax <- yFromRow(rstack, 1)


outputarray <- array(dim = c(xmax,ymax,1))
outputarray1 <- array(dim = c(xmax,ymax,1))
outputarray2 <- array(dim = c(xmax,ymax,1))
outputarray3 <- array(dim = c(xmax,ymax,1))
rmatrix <- as.array(rstack)

####OLS Assumptions####
#note you used a for loop here, borrowed from BFAST code. 
#Likely not as efficient as sapply but you'll see progress bar. Doesn't work for actual coefficients though

#testing NDVI ~ time for 
#homoscedasticity (Breusch-Pagan), normality (Shapiro-Wilkes), and autocorrelation (Breusch-Godfrey)
#create time variable


#time = c(1:16)
#for (x in 1:xmax) {
#  progress=x/xmax*100
#  print(progress)
#  
#  for(y in 1:ymax) {
 #   vect <- rmatrix[x,y,]
#    count <- sum(is.na(vect))
#    if (count>0) next
#    #Shapiro fails if all x values are identical. So slightly change the first
#    m = ts(vect, start = 2000, end = 2015)
#    temp = mk.test(m)
#    temp1 <- temp$Sg
#    temp2 <- temp$pvalg
#    temp3 <- temp$taug
#    outputarray[x,y,1] <-temp1
   # outputarray1[x,y,1] <- temp2
  #  outputarray2[x,y,1] <- temp3
 #   }
#}




for (x in 1:xmax) {
  progress=x/xmax*100
  print(progress)
  for(y in 1:ymax) {
    vect <- rmatrix[x,y,]
    count <- sum(is.na(vect))
    if (count>0) next
    #Shapiro fails if all x values are identical. So slightly change the first
    m = lm(vect~lnallpp + gdd)
    temp = summary(m)
    temp1 <- temp$coefficients[8]
    temp3 <- temp$r.squared
    temp4 <- temp$
    outputarray[x,y,1] <-temp1
    outputarray2[x,y,1] <- temp3
    outputarray3[x,y,1] <- temp4  
    }
}

img1=raster(outputarray[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img1, filename = "D:/MODIS/Chapter 2/Feb29_reanal/pp_OnlyPval.tif", overwrite = TRUE)

img3=raster(outputarray2[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img3, filename = "D:/MODIS/Chapter 2/Feb29_reanal/pponly_rsqr.tif", overwrite = TRUE)

##
