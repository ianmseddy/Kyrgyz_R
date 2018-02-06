#For testing linear regression on annual sum of MODIS images .

#####Load raster stack####
library(lmtest)
library(raster)
library(rgdal)
library(trend)
setwd("E:/MODIS/Chapter 2/RESTREND/NDVI/Res/")


rlist <- list.files(pattern = '\\.tif$')
for(i in rlist) {assign(unlist(strsplit(i, "[.]"))[1], raster(i))}
rstack <- raster::stack(rlist)

ymax = raster::ncol(rstack)
xmax = raster::nrow(rstack)

dxmax = xFromCol(rstack,ymax)
dxmin = xFromCol(rstack,1)
dymin <- yFromRow(rstack,xmax)
dymax <- yFromRow(rstack, 1)


#Convert raster stack to 3 dimensional array
rmatrix <- as.array(rstack)
outputarray1 <- array(dim = c(xmax, ymax,1))

#Perform mk test on residuals
for (x in 1:xmax) {
  progress=x/xmax*100
  print(progress)
  
  for(y in 1:ymax) {
    vect <- rmatrix[x,y,]
    count <- sum(is.na(vect))
    if (count>0) next
    #Create TS with pixel values
    m = ts(vect, start = 2000, end = 2015, frequency = 1)
    temp1 = sens.slope(m)
    mkp = temp1$b.sen
    outputarray1[x,y,] <-mkp
  }
}

img1=raster(outputarray1[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")

writeRaster(img1, filename = "E:/MODIS/Restrend_slope.tif", overwrite = TRUE)