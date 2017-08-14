##this is for analysing the trends in herder identified areas of pasture 
#degradation and seasonal pasture class
library(ggplot2)
library(gridExtra)

setwd('C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Code/CSV')
herder <- read.csv(file = "herderPolygons_1_05.csv", stringsAsFactors = FALSE)
seasonal <- read.csv(file = "SeasonalPolygons_1_05.csv", stringsAsFactors = FALSE)
seasonal1 <- read.csv(file = "SeasonalPolygons_indiv_05.csv", stringsAsFactors = FALSE)

#####You have to do this to make the 0 trend values be read in as 0 instead of NA. 
seasonal1$ndvineg[is.na(seasonal1$ndvineg)] <- 0
seasonal1$ndvipos[is.na(seasonal1$ndvipos)] <- 0
seasonal1$evineg[is.na(seasonal1$evineg)] <- 0
seasonal1$evipos[is.na(seasonal1$evipos)] <- 0
seasonal1$emodneg[is.na(seasonal1$emodneg)] <- 0
seasonal1$emodpos[is.na(seasonal1$emodpos)] <- 0
seasonal1$sk_ndvi_neg[is.na(seasonal1$sk_ndvi_neg)] <- 0
seasonal1$sk_evi_neg[is.na(seasonal1$sk_evi_neg)] <- 0
seasonal1$sk_ndvi_pos[is.na(seasonal1$sk_ndvi_pos)] <- 0
seasonal1$sk_evi_pos[is.na(seasonal1$sk_evi_pos)] <- 0


res_ndvipropn <- herder$ndvineg/herder$ndvipot
res_ndvipropp <- herder$ndvipos/herder$ndvipot
res_evipropn <- herder$evineg/herder$evipot
res_evipropp <- herder$evipos/herder$evipot
res_emodpropn <- herder$emodneg/herder$empot
res_emodpropp <- herder$emodpos/herder$empot
csk_ndvipropn <- herder$sk_ndvi_neg/herder$sk_pot
csk_ndvipropp <- herder$sk_ndvi_pos/herder$sk_pot
csk_evipropn <- herder$sk_evi_neg/herder$sk_pot
csk_evipropp <- herder$sk_evi_pos/herder$sk_pot

hp <- data.frame(pasture = herder$name, rndvi_n = res_ndvipropn, rndvi_p = res_ndvipropp, revi_n = res_evipropn,
                 revi_p = res_evipropp, remod_n = res_emodpropn, remod_p = res_evipropp,
                 ndvi_n = csk_ndvipropn, ndvi_p = csk_ndvipropp, evi_n = csk_evipropn, 
                 evi_p = csk_evipropp)

barplot(height = hp$rndvi_n, names.arg = hp$pasture, ylim = c(0,1), ylab = "Prop. of polygon with negative trend in climate-NDVI")
barplot(height = hp$ndvi_n, names.arg = hp$pasture, ylim = c(0,1), ylab = "Prop. of polygon with negative trend in NDVI")
barplot(height = hp$remod_n, names.arg = hp$pasture, ylim = c(0,1), ylab = "Prop. of polygon with negative trend in climate/eMODIS")

####seasonal with grouped pasture class####
res_ndvipropn <- seasonal$ndvineg/seasonal$ndvipot
res_ndvipropp <- seasonal$ndvipos/seasonal$ndvipot
res_evipropn <- seasonal$evineg/seasonal$evipot
res_evipropp <- seasonal$evipos/seasonal$evipot
res_emodpropn <- seasonal$emodneg/seasonal$empot
res_emodpropp <- seasonal$emodpos/seasonal$empot
csk_ndvipropn <- seasonal$sk_ndvi_neg/seasonal$csmkpot
csk_ndvipropp <- seasonal$sk_ndvi_pos/seasonal$csmkpot
csk_evipropn <- seasonal$sk_evi_neg/seasonal$csmkpot
csk_evipropp <- seasonal$sk_evi_pos/seasonal$csmkpot

sp <- data.frame(pasture = seasonal$name, rndvi_n = res_ndvipropn, rndvi_p = res_ndvipropp, revi_n = res_evipropn,
                 revi_p = res_evipropp, remod_n = res_emodpropn, remod_p = res_evipropp,
                 ndvi_n = csk_ndvipropn, ndvi_p = csk_ndvipropp, evi_n = csk_evipropn, 
                 evi_p = csk_evipropp)


####seasonal with individual polygons####
res_ndvipropn <- seasonal1$ndvineg/seasonal1$ndvipot
res_ndvipropp <- seasonal1$ndvipos/seasonal1$ndvipot
res_evipropn <- seasonal1$evineg/seasonal1$evipot
res_evipropp <- seasonal1$evipos/seasonal1$evipot
res_emodpropn <- seasonal1$emodneg/seasonal1$empot
res_emodpropp <- seasonal1$emodpos/seasonal1$empot
csk_ndvipropn <- seasonal1$sk_ndvi_neg/seasonal1$csmkpot
csk_ndvipropp <- seasonal1$sk_ndvi_pos/seasonal1$csmkpot
csk_evipropn <- seasonal1$sk_evi_neg/seasonal1$csmkpot
csk_evipropp <- seasonal1$sk_evi_pos/seasonal1$csmkpot


sp1 <- data.frame(pasture = seasonal1$name, rndvi_n = res_ndvipropn, rndvi_p = res_ndvipropp, revi_n = res_evipropn,
                 revi_p = res_evipropp, remod_n = res_emodpropn, remod_p = res_evipropp,
                 ndvi_n = csk_ndvipropn, ndvi_p = csk_ndvipropp, evi_n = csk_evipropn, 
                 evi_p = csk_evipropp, revi_num = seasonal1$evipot, rndvi_num = seasonal1$ndvipot,
                 vi_num = seasonal1$csmkpot, remod_num = seasonal1$empot)



boxplot(sp1$rndvi_n ~ sp1$pasture, ylab = "Prop. of RESTREND pixels with negative trends in NDVI")
boxplot(sp1$revi_n ~ sp1$pasture, ylab = "Prop. of RESTREND pixels with negative trends in EVI")
boxplot(sp1$rndvi_p ~ sp1$pasture, ylab = "Prop. of RESTREND pixels with positive trends in NDVI")
boxplot(sp1$revi_p ~ sp1$pasture, ylab = "Prop. of RESTREND pixels with positive trends in EVI")
boxplot(sp1$remod_n ~ sp1$pasture, ylab = "Prop. of RESTREND pixels with negative trends in eMODIS NDVI")
boxplot(sp1$remod_p ~ sp1$pasture, ylab = "Prop. of RESTREND pixels with positive trends in eMODIS NDVI")

boxplot(sp1$ndvi_n ~ sp1$pasture, id.method = c("identify"), ylab = "Prop. of landscape with negative trends in NDVI")
boxplot(sp1$evi_n ~ sp1$pasture, ylab = "Prop. of landscape with negative trends in EVI")
boxplot(sp1$ndvi_p ~ sp1$pasture, ylab = "Prop. of landscape with positive trends in NDVI")
boxplot(sp1$evi_p ~ sp1$pasture, ylab = "Prop. of landscape with positive trends in EVI")
boxplot(sp1$remod_n ~ sp1$pasture, ylab = "Prop. of landscape with negative trends in emodis")
boxplot(sp1$remod_p ~ sp1$pasture, ylab = "Prop. of landscape with positive trends in eMODIS")
boxplot(sp1$revi_num~sp1$pasture, ylab = "Restrend pixels")

#####N size#####

sp1$lab <- sp1$pasture
sp1$lab <- as.character(sp1$lab)
sp1$lab[sp1$lab == "Spring.Fall"] <- "Spring Fall"
sp1$lab[sp1$lab == "Ag"] <- "Agriculture"
herddat <- subset(sp1, sp1$pasture == "Summer-Degraded" | sp1$pasture == "Winter-Degraded" | sp1$pasture == "Spring.Fall-Degraded")


sp1$lab[sp1$lab == "Spring.Fall-Degraded"] <- "Spring Fall"
sp1$lab[sp1$lab == "Winter-Degraded"] <- "Winter"
sp1$lab[sp1$lab == "Summer-Degraded"] <- "Summer"
herddat$lab[herddat$lab == "Spring.Fall-Degraded"] <- "Spring Fall"
herddat$lab[herddat$lab == "Winter-Degraded"] <- "Winter"
herddat$lab[herddat$lab == "Summer-Degraded"] <- "Summer"


#####Exclude polygons with no restrend pixels####

sp1$rndvi_n[sp1$rndvi_num < 10] <- NA
sp1$rndvi_p[sp1$rndvi_num < 10] <- NA
sp1$rndvi_num[sp1$rndvi_num < 10] <- NA



#######RESTREND boxplots
RN <- ggplot(sp1, aes(lab,rndvi_n)) + 
  geom_boxplot() + 
  theme_bw() +
  ylab("Prop. negative trends") +
  xlab("Pasture class") +
  geom_point(data = herddat, aes(lab, rndvi_n), size = 3, colour = "red", shape = 4) + 
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.y = element_text(size = 11))

RP <- ggplot(sp1, aes(lab,rndvi_p)) + 
  geom_boxplot() + 
  geom_point(data = herddat, aes(lab, rndvi_p), size = 3, colour = "red", shape = 4) + 
  ylab("Prop. positive trends") + 
  xlab("") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) + 
  theme(axis.title.y = element_text(size = 11))

grid.arrange(RP, RN, ncol = 1)

##RESTREND boxplots without herder areas####
RN <- ggplot(sp1, aes(lab,rndvi_n)) + 
  geom_boxplot() + 
  theme_bw() +
  ylab("Prop. negative trends") +
  xlab("Pasture class") +
  scale_y_continuous(limits = c(0,1))+
  theme(axis.title.y = element_text(size = 11))

RP <- ggplot(sp1, aes(lab,rndvi_p)) + 
  geom_boxplot() + 
  ylab("Prop. positive trends") + 
  xlab("") +
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) + 
  theme(axis.title.y = element_text(size = 11))

grid.arrange(RP, RN, ncol = 1)
###########NDVI boxplots########
###with herder areas
NN <- ggplot(sp1, aes(lab,ndvi_n)) + 
  geom_boxplot() + 
  theme_bw() + 
  geom_point(data = herddat, aes(lab, ndvi_n), size = 3, colour = "red", shape = 4) + 
  ylab("Prop. negative trends") + 
  xlab("Pasture class") +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.title.y = element_text(size = 11))

NP <- ggplot(sp1, aes(lab,ndvi_p)) + 
  geom_boxplot() + 
  theme_bw() + 
  geom_point(data = herddat, aes(lab,ndvi_p), size = 3, colour = "red", shape = 4) + 
  ylab("Prop. positive trends") + 
  xlab("") +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.title.y = element_text(size = 11))

grid.arrange(NP,NN, ncol = 1)

######NDVI boxplotswithout herder areas######
NN <- ggplot(sp1, aes(lab,ndvi_n)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Prop. negative trends") + 
  xlab("Pasture class") +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.title.y = element_text(size = 11))

NP <- ggplot(sp1, aes(lab,ndvi_p)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Prop. positive trends") + 
  xlab("") +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.title.y = element_text(size = 11))

grid.arrange(NP,NN, ncol = 1)

### Number of RESTREND pixels in each class####
sp1$sample <- sp1$rndvi_num/sp1$vi_num
herddat$sample <- herddat$rndvi_num/herddat$vi_num


Sample <- ggplot(sp1, aes(lab, sp1$sample)) + 
  geom_boxplot() + 
  theme_bw() + 
  geom_point(data = herddat, aes(lab,sample), size = 3, colour = "red", shape = 4) +
  ylab("number of NDVI-residual pixels") + 
  xlab("Pasture Class")
Sample

PolygonSample <- ggplot(sp1, aes(lab, sp1$sample)) + 
  geom_boxplot() + 
  theme_bw() + 
  ylab("Prop. of NDVI-residual pixels") + 
  xlab("Pasture Class")
  theme(axis.title.y = element_text(size = 11))
PolygonSample


mean(sp1$rndvi_n[sp1$pasture == "Winter"], na.rm = TRUE)
median(sp1$ndvi_n[sp1$pasture == "Winter"], na.rm = TRUE)

write.csv(sp1, file = "C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Herderstats.csv")

####Tukey's range test####
fit <- aov(rndvi_n ~ lab, data = sp1)
summary(fit, test = "Pillai")

TukeyHSD(fit)

