##this is for analysing the trends in herder identified areas of pasture 
#degradation and seasonal pasture class
library(ggplot2)
library(gridExtra)

setwd('C:/Users/ianeddy.stu/Dropbox/Thesis/Kyrgyzstan/Final_EI_Manuscript')
herder <- read.csv(file = "Herder_ZonalStats.csv", stringsAsFactors = FALSE)
seasonal1 <- read.csv(file = "Seasonal_Pasture_Stats.csv", stringsAsFactors = FALSE)
herder$lab[herder$lab == "Spring.Fall"] <- "Spring Fall"


#####You have to do this to make the 0 trend values be read in as 0 instead of NA. 
seasonal1$ndvineg[is.na(seasonal1$ndvineg)] <- 0
seasonal1$ndvipos[is.na(seasonal1$ndvipos)] <- 0
seasonal1$sk_ndvi_neg[is.na(seasonal1$sk_ndvi_neg)] <- 0
seasonal1$sk_ndvi_pos[is.na(seasonal1$sk_ndvi_pos)] <- 0

herder$ndvineg[is.na(herder$ndvineg)] <- 0
herder$ndvipos[is.na(herder$ndvipos)] <- 0
herder$sk_ndvi_neg[is.na(herder$sk_ndvi_neg)] <- 0
herder$sk_ndvi_pos[is.na(herder$sk_ndvi_pos)] <- 0


res_ndvipropn <- herder$ndvineg/herder$ndvipot
res_ndvipropp <- herder$ndvipos/herder$ndvipot
csk_ndvipropn <- herder$sk_ndvi_neg/herder$sk_pot
csk_ndvipropp <- herder$sk_ndvi_pos/herder$sk_pot

hp <- data.frame(pasture = herder$name, rndvi_n = res_ndvipropn, rndvi_p = res_ndvipropp,
                 ndvi_n = csk_ndvipropn, ndvi_p = csk_ndvipropp, lab = herder$lab)


barplot(height = hp$rndvi_n, names.arg = hp$pasture, ylim = c(0,1), ylab = "Prop. of polygon with negative trend in climate-NDVI")
barplot(height = hp$ndvi_n, names.arg = hp$pasture, ylim = c(0,1), ylab = "Prop. of polygon with negative trend in NDVI")

####seasonal with grouped pasture class####

####seasonal with individual polygons####
res_ndvipropn1 <- seasonal1$ndvineg/seasonal1$ndvipot
res_ndvipropp1 <- seasonal1$ndvipos/seasonal1$ndvipot
csk_ndvipropn1 <- seasonal1$sk_ndvi_neg/seasonal1$csmkpot
csk_ndvipropp1 <- seasonal1$sk_ndvi_pos/seasonal1$csmkpot


sp1 <- data.frame(pasture = seasonal1$name, rndvi_n = res_ndvipropn1, rndvi_p = res_ndvipropp1,
                 ndvi_n = csk_ndvipropn1, ndvi_p = csk_ndvipropp1, rndvi_num = seasonal1$ndvipot,
                 vi_num = seasonal1$csmkpot)

#####Fix labels, because I had some dumb idea about degraded vs  not that I evidently forgot#####
#Have to convert the factor to text
sp1$lab <- sp1$pasture
sp1$lab <- as.character(sp1$lab)
sp1$lab[sp1$lab == "Spring.Fall"] <- "Spring Fall"
sp1$lab[sp1$lab == "Ag"] <- "Agriculture"

#####Exclude polygons with no restrend pixels####


sp1$rndvi_num[sp1$rndvi_num < 10] <- NA



#######RESTREND boxplots
RN <- ggplot(sp1, aes(lab,rndvi_n)) + 
  geom_boxplot() + 
  theme_bw() +
  ylab("Prop. negative trends") +
  xlab("") +
  geom_point(data = hp, aes(lab, rndvi_n), size = 3, colour = "red", shape = 4) + 
  scale_y_continuous(limits = c(0,.8))+
  theme(axis.title.y = element_text(size = 11))

RP <- ggplot(sp1, aes(lab,rndvi_p)) + 
  geom_boxplot() + 
  geom_point(data = hp, aes(lab, rndvi_p), size = 3, colour = "red", shape = 4) + 
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
  geom_point(data = hp, aes(lab, ndvi_n), size = 3, colour = "red", shape = 4) + 
  ylab("Prop. negative trends") + 
  xlab("Pasture class") +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.title.y = element_text(size = 11))

NP <- ggplot(sp1, aes(lab,ndvi_p)) + 
  geom_boxplot() + 
  theme_bw() + 
  geom_point(data = hp, aes(lab,ndvi_p), size = 3, colour = "red", shape = 4) + 
  ylab("Prop. positive trends") + 
  xlab("") +
  scale_y_continuous(limits = c(0,1)) +
  theme(axis.title.y = element_text(size = 11))

grid.arrange(NP,NN, ncol = 1)


### Number of RESTREND pixels in each class####
sp1$sample <- sp1$rndvi_num/sp1$vi_num
herder$sample <- herder$ndvipot/herder$sk_pot


Sample <- ggplot(sp1, aes(lab, sp1$sample)) + 
  geom_boxplot() + 
  theme_bw() + 
  geom_point(data = herder, aes(lab,sample), size = 3, colour = "red", shape = 4) +
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


