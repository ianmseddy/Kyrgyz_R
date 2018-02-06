#For testing linear regression on annual sum of MODIS images .

#####Load raster stack####
library(lmtest)
library(raster)
library(rgdal)
library(relaimpo)
setwd("E:/MODIS/Chapter 2/RESTREND/GrowSeasonSum_NDVI/")

#Load precipitation data to regress against NDVI
WG.monthly <- read.csv("C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/Chapter2/Ultimate_precipitation.csv")
rain <- WG.monthly$Total

#Load Growing Degree Day data
NCDC.GDD <- read.csv("C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV/Naryn_NCDC_GDD.csv")
gdd <- NCDC.GDD$Rnd_Base0

###test a few variations of climate data. 
lnallpp <- log(rain)
plot(lnallpp)

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

#Create blank arrays of all variables you will need (e.g. coefficients, r2, normality tests)
bgarray1 <- array(dim = c(xmax,ymax,1)) #breusch-godfrey test of autocorrelation
bparray2 <- array(dim = c(xmax, ymax,1)) # breusch-pagan test of heteroskedasticity
swarray3 <- array(dim = c(xmax, ymax,1)) #shapiro-wilkes of normality
ppparray4 <- array(dim = c(xmax, ymax,1))
ppparray5 <- array(dim = c(xmax, ymax,1))
gddarray6 <- array(dim = c(xmax, ymax,1))
gddarray7 <- array(dim= c(xmax, ymax,1))
totr2array8 <- array(dim = c(xmax, ymax,1))
resarray9 <- array(dim = c(xmax, ymax,16)) #create array with dimensions = n of dataset (16)
rmatrix <- as.array(rstack) #create multidimensional array of MODIS images from raster stack


#######Begin Modeling####
####OLS Assumptions####
#note you used a for loop here, borrowed from BFAST code. 
#Likely not as efficient as sapply but you'll see progress bar. Doesn't work for actual coefficients though

#testing NDVI ~ time for 
#homoscedasticity (Breusch-Pagan), normality (Shapiro-Wilkes), and autocorrelation (Breusch-Godfrey)
#create time variable


for (x in 1:xmax) {
  progress=x/xmax*100
  print(progress)
  
  for(y in 1:ymax) {
    vect <- rmatrix[x,y,]
    count <- sum(is.na(vect))
    if (count>0) next
    #Shapiro fails if all x values are identical. So slightly change the first
    vect[1] = vect[1] + 0.0001
    m = lm(vect~lnallpp + gdd)
    #Breusch-Godfrey and Breusch-Pagan
    temp2 = bptest(m)
    temp3 = bgtest(m, order = 2)
    bg = temp3$p.value 
    bp = temp2$p.value
    bparray2[x,y,] <-bp
    bgarray1[x,y,] <-bg
    
    #Get r2 and p values
    tempsum <- summary(m)
    ppparray4[x,y,] <- m$coefficients[2]
    ppparray5[x,y,] <- tempsum$coefficients[11]
    gddarray6[x,y,] <- m$coefficients[3]
    gddarray7[x,y,] <- tempsum$coefficients[12]
    totr2array8[x,y,] <- tempsum$r.squared
    
    #test for normality of residuals with shapiro-wilkes
    res = residuals(m)
    temp1 = shapiro.test(res)
    Shap= temp1$p.value
    swarray3[x,y,] <-Shap
    
    #output the residuals
    resarray9[x,y,] <- res
    
    }
}

img2=raster(bparray2[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img2, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_bp.tif", overwrite = TRUE)

img1=raster(bgarray1[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img1, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_bg.tif", overwrite = TRUE)


img8=raster(totr2array8[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img8, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_tot_r22.tif", overwrite = TRUE)

img3=raster(swarray3[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img3, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_sw.tif", overwrite = TRUE)

img4=raster(ppparray4[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img4, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_ppcoeff.tif", overwrite = TRUE)

img5=raster(ppparray5[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img5, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_ppp_Pval2.tif", overwrite = TRUE)

img6=raster(gddarray6[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img6, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_gddcoeff.tif", overwrite = TRUE)

img7=raster(gddarray7[,,1], xmn=dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
writeRaster(img7, filename = "E:/MODIS/Chapter 2/Test/AllPP/mrNDVI_gdd_Pval2.tif", overwrite = TRUE)


#####For getting residual 
#Write the residuals
img9 = raster(resarray9[,,1], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img10 = raster(resarray9[,,2], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")
img11 = raster(resarray9[,,3], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img12 = raster(resarray9[,,4], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")
img13 = raster(resarray9[,,5], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img14 = raster(resarray9[,,6], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")
img15 = raster(resarray9[,,7], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img16 = raster(resarray9[,,8], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")
img17 = raster(resarray9[,,9], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img18 = raster(resarray9[,,10], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")
img19= raster(resarray9[,,11], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img20 = raster(resarray9[,,12], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")
img21 = raster(resarray9[,,13], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img22 = raster(resarray9[,,14], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")
img23= raster(resarray9[,,15], xmn = dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat +datum=WGS84")
img24 = raster(resarray9[,,16], xmn= dxmin, xmx=dxmax, ymn=dymin, ymx=dymax, crs="+proj=longlat + datum=WGS84")

#Save them
writeRaster(img9, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res01.tif", overwrite = TRUE)
writeRaster(img10, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res02.tif", overwrite = TRUE)
writeRaster(img11, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res03.tif", overwrite = TRUE)
writeRaster(img12, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res04.tif", overwrite = TRUE)
writeRaster(img13, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res05.tif", overwrite = TRUE)
writeRaster(img14, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res06.tif", overwrite = TRUE)
writeRaster(img15, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res07.tif", overwrite = TRUE)
writeRaster(img16, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res08.tif", overwrite = TRUE)
writeRaster(img17, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res09.tif", overwrite = TRUE)
writeRaster(img18, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res10.tif", overwrite = TRUE)
writeRaster(img19, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res11.tif", overwrite = TRUE)
writeRaster(img20, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res12.tif", overwrite = TRUE)
writeRaster(img21, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res13.tif", overwrite = TRUE)
writeRaster(img22, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res14.tif", overwrite = TRUE)
writeRaster(img23, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res15.tif", overwrite = TRUE)
writeRaster(img24, filename = "E:/MODIS/Chapter 2/Test/AllPP/Res/test_res16.tif", overwrite = TRUE)
