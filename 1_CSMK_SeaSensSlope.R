#For testing linear regression on annual sum of MODIS images .

#####Load raster stack####
library(lmtest)
library(raster)
library(rgdal)
library(trend)
setwd("E:/MODIS/Chapter 2/ImputeData/Impute_NDVI/")

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

rmatrix <- as.array(rstack)
library(trend)

for (x in 1:xmax) {
  progress=x/xmax*100
  print(progress)
  
  for(y in 1:ymax) {
    vect <- rmatrix[x,y,]
    count <- sum(is.na(vect))
    if (count>0) next
    #Shapiro fails if all x values are identical. So slightly change the first
    tsvect <- ts(vect, start = 2000,end = 2015, frequency = 9)
    tsvect[1] <- tsvect[1] + 0.001
    m = sea.sens.slope(tsvect)
    temp1 <- m$b.sen
    temp2 <- m$intercept
    outputarray[x,y,1] <-temp1
    outputarray1[x,y,1] <-temp2
    }
}
Sys.time()

img1=raster(outputarray[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img1, filename = "E:/MODIS/NDVI_sss_slope.tif", overwrite = TRUE)

img2 = raster(outputarray1[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img2, filename = "E:/MODIS/NDVI_sss_intercept.tif", overwrite = TRUE)
##

