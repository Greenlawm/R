#Script for Species Distribution Models for Cusk
#M.E. Greenlaw November 2013
# Import landings, longline, trap survey and RV survey data
# Aggregate data by grid squares
# Import Environmental variables. Clip, aggregate, resmaple and stack variables
# Creating training and testing data for the species distributiom models
# Run three species distribution models GLM (P/A), Maxent (Presence only), and Random Forest (P/A)
##########################################################

# Only extrapolate with predictors that are expected to be directly correlates of species distribution

##########################################################

install.packages(c('raster', 'rgdal', 'dismo', 'rJava', 'RColorBrewer', 'randomForest', 'maptools'))
install.packages(c('AUCRF'))
install.packages(c('pROC'))
install.packages(c('extrafont'))
#loads the dismo library
library(dismo)
library(raster)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(randomForest)
library(ggplot2)
library(plyr)
library(gam)
library(mgcv)
library(sm)
library(plyr)
library(pROC)
library(extrafont)

#set working directory
setwd("C:/Michelle/Michelle/R")
pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)

#set Coastline
coastline <- readOGR(".", "GOMCoastline_WGS84")
coastline<-spTransform(coastline,CRS("+proj=utm +zone=20 ellps=WGS84"))
grid <- 3000
# Import Longline Data
#################################################################################
#Import Cusk longline CSV File
cusk.lo = read.csv("C:/Michelle/Michelle/R/HSlonglineallsetsforMichelle.csv", sep=",")

# Project longline 
cusklo.utm <- project(as.matrix(cusk.lo[c("LONGITUDE", "LATITUDE")]), "+proj=utm +zone=20 ellps=WGS84")
cusklo.utm <- cbind(cusklo.utm, cusk.lo)
names(cusklo.utm)[1] <- "LONUTM"
names(cusklo.utm)[2] <- "LATUTM"

cusklo.utm <- cusklo.utm[,c('LONUTM','LATUTM','CATCH')]
names(cusklo.utm) <-c("LONUTM", "LATUTM", "CATCH")


#Convert raster to points data frame 
#Test to make sure the points data frame and shapefile are working correctly
#write.table(cusklo.e,file="C:/Michelle/Michelle/R/Results/Testing/cusklo.csv",sep=",",row.names=F)
#writeOGR(cusklo.s, ".", "cusklo", driver="ESRI Shapefile", overwrite=TRUE)

# Import and aggregate Landings
#Import Cusk landings CSV File
###################################################################
cusk.la = read.csv("C:/Michelle/Michelle/R/2002 to 2010 by gear 51.txt", sep=";", row.names=NULL, header=T, fill=T)
cusk.la2 = read.csv("C:/Michelle/Michelle/R/2011 to 2013 by gear 51.txt", sep=";", row.names=NULL, header=T, fill=T)

cusk.la = rbind(cusk.la, cusk.la2)

#Subset File
na<- cusk.la == "null"
is.na(cusk.la)[na] <- TRUE
cusk.la <- subset(cusk.la, !is.na(ENT_LATITUDE) & !is.na(EFFORT_AMOUNT))
# should I remove the effort with less than 10 hooks......
cusk.la$LATDD<-with(cusk.la, ((as.numeric(substr(ENT_LATITUDE,1,2)))+((as.numeric(substr(ENT_LATITUDE,3,4))) + (as.numeric(substr(ENT_LATITUDE,5,6))/100))/60))
cusk.la$LONDD<-with(cusk.la, ((as.numeric(substr(ENT_LONGITUDE,1,2)))+((as.numeric(substr(ENT_LONGITUDE,3,4))) + (as.numeric(substr(ENT_LONGITUDE,5,6))/100))/60))
cusk.la$LONDD <- -cusk.la$LONDD
cusk.la <- subset(cusk.la, LONDD!= 0 & LATDD!= 0  )
cusk.la <- subset(cusk.la, as.numeric(EFFORT_AMOUNT) > 500)
cusk.la <- transform (data.matrix(cusk.la))
cusk.la$cuskcpue <- cusk.la$CSK/as.numeric(cusk.la$EFFORT_AMOUNT) * 1000
cusk.q <- as.numeric(cusk.la$EFFORT_AMOUNT)
plot(density(cusk.q), ylab="Kernel Density of Effort (# of Hooks)", main="")
quantile(cusk.q, c(0.1,0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
quantile(cusk.la$cuskcpue, c(0.1,0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
cusk.test <- subset(cusk.la, cuskcpue > 2500)
cusk.la <- subset(cusk.la, cuskcpue < 2500)
plot(density(cusk.la$cuskcpue))
cusk.la <- data.frame(cusk.la)

# Project landings
cuskla.utm <- project(as.matrix(cusk.la[c("LONDD", "LATDD")]), "+proj=utm +zone=20 ellps=WGS84")
cuskla.utm <- cbind(cuskla.utm, cusk.la)
names(cuskla.utm)[1] <- "LONUTM"
names(cuskla.utm)[2] <- "LATUTM"
cuskla.utm <- cuskla.utm[,c('LONUTM','LATUTM','cuskcpue')]
names(cuskla.utm) <-c("LONUTM", "LATUTM", "CATCH")

#Write Raster
#writeRaster(rstack2[[1]], 'C:/Michelle/Michelle/R/cuskras.asc', NAflag=-9999, overwrite=TRUE)
#Test to make sure the points data frame and shapefile are working correctly
#Write Table
#write.table(cuskla.e,file="C:/Michelle/Michelle/R/Results/Testing/cuskla.csv",sep=",",row.names=F)
#Write Shapefile
#writeOGR(cuskla.s1, ".", "cuskla", driver="ESRI Shapefile", overwrite=T)

#Import Cusk RV  csv File
#################################################
cusk.rv = read.csv("C:/Michelle/Michelle/R/RVSurvey.csv", sep=",", row.names=NULL)

# Project RV Survey Data
cuskrv.utm <- project(as.matrix(cusk.rv[c("longitude", "latitude")]), "+proj=utm +zone=20 ellps=WGS84")
cuskrv.utm <- cbind(cuskrv.utm, cusk.rv)
names(cuskrv.utm)[1] <- "LONUTM"
names(cuskrv.utm)[2] <- "LATUTM"
cuskrv.utm <- cuskrv.utm[,c('LONUTM','LATUTM','cusk')]
names(cuskrv.utm) <-c("LONUTM", "LATUTM", "CATCH")

#Test to make sure the points data frame and shapefile are working correctly
#Write Table
#write.table(cuskrv.e,file="C:/Michelle/Michelle/R/Results/Testing/cuskrv.csv",sep=",",row.names=F)
#Write Shapefile
#writeOGR(cuskrv.s, ".", "cuskrv", driver="ESRI Shapefile", overwrite=T)

#Import Trap Data
#Import Trap CSV File
#################################################
file <- paste("C:/Michelle/Michelle/R/CuskPA_16092013.csv", sep=" ")

#read file
cusk.t <- read.table(file, header=TRUE, sep=',', row.names = NULL)
cusk.t <- subset(cusk.t, (lSlat > 4 & lSlong > 4))
cusk.t <- subset(cusk.t, SlongDD!= 0 & SlatDD!= 0)

cuskt.utm <- project(as.matrix(cusk.t[c("SlongDD", "SlatDD")]), "+proj=utm +zone=20 ellps=WGS84")
cuskt.utm <- cbind(cuskt.utm, cusk.t)
names(cuskt.utm)[1] <- "LONUTM"
names(cuskt.utm)[2] <- "LATUTM"
cuskt.utm <- do.call(data.frame, lapply(cuskt.utm, function(x) replace(x, is.infinite(x), NA)))
cuskt.utm <- subset(cuskt.utm, !is.na(LONUTM))
cuskt.utm <- cuskt.utm[,c('LONUTM','LATUTM','CuskCatch')]
names(cuskt.utm) <-c("LONUTM", "LATUTM", "CATCH")

#Test to make sure the points data frame and shapefile are working correctly
#Write Table
#write.table(cuskt.e,file="C:/Michelle/Michelle/R/Results/Testing/cuskt.csv",sep=",",row.names=F)
#Write Shapefile
#writeOGR(cuskt.s, ".", "cuskt", driver="ESRI Shapefile", overwrite=T)

#Bind all the data together PA
#################h##################################################
cusk <- rbind(cuskt.utm, cuskrv.utm)
cusk <- rbind(cusk, cusklo.utm)
cusk <- rbind(cusk, cuskla.utm)
cusk$pa <- as.numeric(cusk$CATCH > 0)

# Bind the longline, landings and RV Survey 0 catches
rv0 <- cuskrv.utm[cuskrv.utm[, "CATCH"] ==0,]
cusk.c <- rbind(cusklo.utm, cuskla.utm)
cusk.c <- rbind(cusk.c, rv0)
#cusk.c <- cuskla.e
#cusk.c <- cusklo.e

#cords <- cbind(x=cusk.c["lonutm"], Y=cusk.c["latutm"])
#sPDF <- SpatialPointsDataFrame(cords, data=cusk.c)
#xmn <- 100000
#ymn <- 4500000
#xmx <- 1000000
#ymx <- 5300000
#ext <-extent(xmn, xmx, ymn, ymx)
#ncols <- length(xmx:xmn)/grid
#nrows <- (length(ymx:ymn) - 1)/grid
#blankraster <- raster(nrows=nrows, ncols=ncols, xmn=xmn, xmx=xmx, ymn=ymn, ymx = ymx)
#blankraster[] <- 1:ncell(blankraster)
#lalorv <- rasterize(x=sPDF, y=blankraster, field="mean", fun=mean)
#plot(lalorv)
#Test to make sure the points data frame and shapefile are working correctly
#writeRaster(lalorv, filename='C:/Michelle/Michelle/R/lalo0rv.tif', datatype='GTiff', overwrite=TRUE)

# Check data
setwd("C:/Michelle/Michelle/R/Results/Input")
coordinates(cusk.c)=c("lonutm", "latutm") #Set Coordinates for Shapefile
proj4string(cusk.c) <-CRS("+proj=utm +zone=20 ellps=WGS84") # Set Projection for Shapefile
# cusk_CRS <- CRS("+proj=utm +zone=20 ellps=WGS84")
writeOGR(cusk.c, ".", "cuskc", driver="ESRI Shapefile", overwrite=TRUE)

#Create Histograms of Catch to understand the difference between catchability and effort
##################################################################################
#Create histogram of cusk catch
#par(mfrow = c(1,2))
#cusklo.cpue <- cusklo.e[cusklo.e[,4] > 0,]
#with(cusklo.cpue, hist(log(mean), freq=FALSE, ylim=c(0, 0.35), xlim=c(0, 8), main=" ", xlab = "log(Sentinal CPUE)"))
#cuskla.cpue <- cuskla.e[cuskla.e[,4] > 0,]
#with(cuskla.cpue, hist(log(mean), freq=FALSE, col="lightgreen", ylim=c(0, 0.35), xlim=c(0, 8), main="", xlab="log(Landings CPUE)"))
#curve(dnorm(x, mean=mean(is.numeric(cusklo.cpue)), sd=sd(is.numeric(cusklo.cpue))), add=TRUE, lwd=2, col='darkblue')

#Create histogram of cusk catch
#par(mfrow = c(1,2))
#cusklocpue <- cusklo1[cusklo1[,19] > 0,]
#with(cusklocpue, hist(cpue, freq=FALSE, xlim=c(0,1000), ylim=c(0, 0.0005), main=" ", xlab = "Landings CPUE", breaks=30))
#cusklacpue <- cuskla1[cuskla1[,20] > 0,]
#with(cusklacpue, hist(CATCH, freq=FALSE, col="lightgreen", xlim=c(0, 1000), ylim =c(0, 0.0005), main="", xlab="Sentinal CPUE", breaks=, 30))
#curve(dnorm(x, mean=mean(is.numeric(cusklocpue)), sd=sd(is.numeric(cusklocpue))), add=TRUE, lwd=2, col='darkblue')

#plot points
#par(mfrow = c(1,1))
#plot(coastline, xlim=c(-68, -57), ylim=c(41, 46), axes=TRUE, border='dark grey')
#presence<- as.factor(cuskall[,4])
#points(cuskall$longitude, cuskall$latitude, col=presence, pch = ifelse(presence==1, 19, 4), cex = ifelse(presence==1, 0.5, 0.2))
#presence<- as.factor(cusklas[,4])
#points(cusklas$longitude, cusklas$latitude, col=presence, pch = ifelse(presence==1, 19, 4), cex = ifelse(presence==1, 0.5, 0.2))
#presence<- as.factor(cusklos[,4])
#points(cusklos$longitude, cusklos$latitude, col=presence, pch = ifelse(presence==1, 19, 4), cex = ifelse(presence==1, 0.5, 0.2))

#Import Rasters
# Loop through and import rasters
# Loop through and import rasters
########################################################################################
#setwd("C:/Michelle/Michelle/R/Variables")
#hab = list.files(getwd(), pattern!="ovr$", full.names=FALSE)  # raster file paths  
#hab.names <- c(unlist(lapply(strsplit(hab, "[.]"), FUN=function(x) {x[1]})))
#  process each raster in HAB
#for(j in 1:length(hab) ){
  #assign(hab.names[j], raster(hab[j], crs= "+proj=longlat +datum=WGS84"))}
#files<- list(hab.names)
#Import Rasters
# Import Rasters
###################################################################################
#set working directory
setwd("C:/Michelle/Michelle/R")

#import Rasters
compl <- raster("C:/Michelle/Michelle/R/Variables/compl")
slopel <- raster("C:/Michelle/Michelle/R/Variables/slopel")
botcurr <- raster("C:/Michelle/Michelle/R/Variables/botcurr")
salvar <- raster("C:/Michelle/Michelle/R/Variables/salvar")
sal <- raster("C:/Michelle/Michelle/R/Variables/sal")
tvar <- raster("C:/Michelle/Michelle/R/Variables/tvar")
t <- raster("C:/Michelle/Michelle/R/Variables/t")
rms<- raster("C:/Michelle/Michelle/R/Variables/rms")
strat <- raster("C:/Michelle/Michelle/R/Variables/strat")
twin <- raster("C:/Michelle/Michelle/R/Variables/twin")
tsum <- raster("C:/Michelle/Michelle/R/Variables/tsum")
tspr <- raster("C:/Michelle/Michelle/R/Variables/tspr")
tfall <- raster("C:/Michelle/Michelle/R/Variables/tfall")
depth <- raster("C:/Michelle/Michelle/R/Variables/depth")
pp2010 <- raster("C:/Michelle/Michelle/R/Variables/pp2010")
pp06_10 <- raster("C:/Michelle/Michelle/R/Variables/pp06_10")
sst01_10 <- raster("C:/Michelle/Michelle/R/Variables/sst01_10")
chlmo03_12 <- raster("C:/Michelle/Michelle/R/Variables/chlmo03_12")
tsm06_10 <- raster("C:/Michelle/Michelle/R/Variables/tsm06_10")
wintsm06_10 <- raster("C:/Michelle/Michelle/R/Variables/winttsm06_10")
lonutm <- raster("C:/Michelle/Michelle/R/Variables/lonutm")
latutm <- raster("C:/Michelle/Michelle/R/Variables/latutm")

#Select values > 0 for remote sensing layers

wintsm06_10[wintsm06_10 == 0] <- NA
pp2010[pp2010 == 0] <- NA
pp06_10[pp06_10 == 0] <- NA
sst01_10[sst01_10 == 0] <- NA
chlmo03_12[chlmo03_12 == 0] <- NA
tsm06_10[tsm06_10 == 0] <- NA

# Select Depth Less than 1500 m
depth[depth < -1500] <- NA
sst01_10[sst01_10 > 12] <- NA
chlmo03_12[chlmo03_12 > 8] <- NA

#Set Raster Extent
ext <-extent(-68, -54, 41.0, 46)

#Crop Rasters
complc <-crop(compl, ext)
#plot(log(complc), col=pal)
#plot(coastline, add=TRUE, border='dark grey')
#points(cuskall$longitude, cuskall$latitude, pch = 19, cex = 0.2)
slopelc <- crop(slopel, ext)
botcurrc <-crop(botcurr, ext)
salvarc <-crop(salvar, ext)
salc <-crop(sal, ext)
tvarc <-crop(tvar, ext)
tc <-crop(t, ext)
rmsc <-crop(rms, ext)
stratc <- crop(strat, ext)
twinc <- crop(twin, ext)
tsumc <- crop(tsum, ext)
tsprc <- crop(tspr, ext)
tfallc <- crop(tfall, ext)
depthc <- crop(depth, ext)
pp2010c <- crop(pp2010, ext)
pp06_10c<- crop(pp06_10, ext)
sst01_10c<- crop(sst01_10, ext)
chlmo03_12c <- crop(chlmo03_12, ext)
tsm06_10c <- crop(tsm06_10, ext)
wintsm06_10c <- crop(wintsm06_10, ext)
lonutmc <- crop(lonutm, ext)
latutmc <- crop(latutm, ext)

#salvarc <- aggregate(salvarc, fact=4)
f= res(salvarc)/res(depth)

#aggregate (resample) rasters that are finer than the 1 km resolution
compla<-aggregate(complc, fact=f[1], fun=mean)
slopela<-aggregate(slopelc, fact=f[1], fun=mean)
deptha<-aggregate(depthc, fact=f[1], fun=mean)

#resample rasters
salvarr <- salvarc
salr <- resample(salc, salvarc, method='bilinear')
complr <- resample(compla, salvarc, method='bilinear')
slopelr <-resample(slopela, salvarc, method='bilinear')
stratr<-resample(stratc, salvarc, method='bilinear')
tvarr<-resample(tvarc, salvarc, method='bilinear')
tr<-resample(tc, salvarc, method='bilinear')
chlmo03_12r<-resample(chlmo03_12c, salvarc, method='bilinear')
rmsr<-resample(rmsc, salvarc, method='bilinear')
depthr<-resample(deptha, salvarc, method='bilinear')
twinr<-resample(twinc, salvarc, method='bilinear')
tsumr<-resample(tsumc, salvarc, method='bilinear')
tsprr<-resample(tsprc, salvarc, method='bilinear')
tfallr<-resample(tfallc, salvarc, method='bilinear')
sst01_10r<-resample(sst01_10c, salvarc, method='bilinear')
chlmo03_12r<-resample(chlmo03_12c, salvarc, method='bilinear')
tsm06_10r <-resample(tsm06_10c, salvarc, method='bilinear')
pp2010r <-resample(pp2010c, salvarc, method='bilinear')
pp06_10r <-resample(pp06_10c, salvarc, method='bilinear')
winttsm06_10r <-resample(wintsm06_10c, salvarc, method='bilinear')
botcurrr <- resample(botcurrc, salvarc, method='bilinear')
lonutm <- resample(lonutm, salvarc, method='bilinear')
latutm <- resample(latutm, salvarc, method='bilinear')

#writeRaster(salr, 'C:/Michelle/Michelle/Software/Maxent/ASCII_ALL/salr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(complr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/complr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(slopelr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/slopelr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(stratr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/stratr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(tvarr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/tvarr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(tr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/tr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(rmsr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/rmsr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(depthr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/depthr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(salvarr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/salvarr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(tsumr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/tsumr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(tsprr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/tsprr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(tfallr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/tfallr.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(sst01_10r, 'C:/Michelle/Michelle/Software/Maxent/ASCII/sst01_10r.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(chlmo03_12r, 'C:/Michelle/Michelle/Software/Maxent/ASCII/chlmo03_12r.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(tsm06_10r, 'C:/Michelle/Michelle/Software/Maxent/ASCII/tsm06_10r.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(pp2010r, 'C:/Michelle/Michelle/Software/Maxent/ASCII/pp2010r.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(pp06_10r, 'C:/Michelle/Michelle/Software/Maxent/ASCII/pp06_10r.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(wintsm06_10r, 'C:/Michelle/Michelle/Software/Maxent/ASCII/wintsm06_10r.asc', NAflag=-9999, overwrite=TRUE)
#writeRaster(botcurrr, 'C:/Michelle/Michelle/Software/Maxent/ASCII/botcurrr.asc', NAflag=-9999, overwrite=TRUE)

#####################
#create raster stack
predictorsrbig<- list(sst01_10r, chlmo03_12r, tsm06_10r, pp2010r, pp06_10r, winttsm06_10r, salvarr, salr, stratr, tvarr, tr, rmsr, depthr, slopelr, complr, twinr, tsumr, tsprr, tfallr, botcurrr)
predictorsr <- list(sst01_10r, chlmo03_12r, tsm06_10r, pp2010r, pp06_10r, winttsm06_10r, salvarr, stratr, tvarr, tr, rmsr, depthr, slopelr, complr, twinr, tsumr, tsprr, tfallr, botcurrr) 
# Low probability of selection were: tspr, salr, tsum, pp2010, botcurr
# Variables removed were salinity, all others had a correlation less than 0.7
pstack<- stack(predictorsr)
pstackbig<- stack(predictorsrbig)
names(pstackbig) <- c("sst01_10", "chlmo03_12", "tsm06_10", "pp2010", "pp06_10", "winttsm06_10", "salvar", "sal", "strat", "tvar", "t", "rms", "depth", "slopel", "compl", "twin", "tsum", "tspr", "tfall", "botcurr")
names(pstack) <- c("sst01_10", "chlmo03_12", "tsm06_10", "pp2010", "pp06_10", "winttsm06_10", "salvar", "strat", "tvar", "t", "rms", "depth", "slopel", "compl", "twin", "tsum", "tspr", "tfall", "botcurr")

#Project raster stack to UTM Coordinates
pstack <- projectRaster(pstack, crs="+proj=utm +zone=20 ellps=WGS84")
pstackbig <- projectRaster(pstackbig, crs="+proj=utm +zone=20 ellps=WGS84")

#Extract values from rasters
###################################################################      
l <- c("LONUTM", "LATUTM")
cuskall_llb <-cusk[,l]
head(cuskall_llb)
pavalbig <- extract(pstackbig, cuskall_llb)
#Extract catch values from rasters
l2 <- c("LONUTM", "LATUTM")
cusk.cllb <- cusk.c[,l2]
cvalsbig <- extract(pstackbig, cusk.cllb)
#Attach presence column
s <- c("pa", "lonutm", "latutm")
sdmdatabig<- data.frame(cbind(cusk[,s], pavalbig))
names(sdmdatabig)[1] <-"pa"

#create training and test data for Presence/Absence dataset 
# [change this to spatial cross validations]
# Anders uses 10-fold cross validation
c<- c("pa", "LONUTM", "LATUTM")
cusks1b <- cusk[, c]
presb <- cusks1b[cusks1b[,1] == 1, 2:3]
absb <- cusks1b[cusks1b[,1] == 0, 2:3]
groupb <- kfold(presb, 5)
groupab<- kfold(absb, 5)
pres_trainb <- presb[groupb != 1, ]
pres_testb <- presb[groupb == 1, ]
abs_trainb <- absb[groupab != 1, ]
abs_testb <- absb[groupab == 1, ]

trainb <- rbind(pres_trainb, abs_trainb)
c_trainb <- c(rep(1, nrow(pres_trainb)), rep(0, nrow(abs_trainb)))
envtrainbig <- extract(pstackbig, trainb)
envtrainbig <- data.frame(cbind(trainb,envtrainbig))
envtrainbig <- data.frame( cbind(pa=c_trainb, envtrainbig))
test_presbig <- data.frame(extract(pstackbig, pres_testb))
test_absbig <- data.frame(extract(pstackbig, abs_testb))
envtrainbig<- envtrainbig[complete.cases(envtrainbig),]

#Variable Selection Routine
# Maybe calculate the one that is least likely to be correlated with the response insead of
# the least likely to be selected by the model
#library(AUCRF)
#envtrainf <- as.factor(envtrainbig[,1]) # Convert the presence column to a factor
#envtrainf<- cbind(envtrainf, envtrainbig[3:23]) #Bind the presence column back to the data
#selection <- AUCRF(envtrainf~., data=envtrainf, pdel=0.2, ranking="MDG") # run the variable selection routine from the AUCRF package
#selection.cv <- AUCRFcv(selection)
#selection.o <- OptimalSet(selection.cv)
#sink(file="C:/Michelle/Michelle/R/Results/variablesel.txt")
#selection.o
#sink()
#plot(selection.cv, showOpt=TRUE, digits=4)

#######################################
# Anders says to use spearman correlation instead of pearson. To use with not normally distributed data
#Remove correlated varaibles with correlation > 0.7
#df <- data.frame(correlation=numeric (0), raster1=character(0), raster2=character(0), stringsAsFactors=FALSE) # create a new black data frame
#loop through rasters to calculate the correlation coefficients, write the correlation coefficient and raster names to a new data frame
#for(i in 1:length(predictorsr))
	#for(j in i:length(predictorsr))
	#	if(i != j){
	#	corl[i] <-	cor(getValues(pstack[[i]]), getValues(pstack[[j]]), use="complete.obs") # calculate the correlation between two rasters
	#	newrow = data.frame(correlation=as.numeric(corl[i]), raster1=as.character(names(pstack[[i]])), raster2=as.character(names(pstack[[j]])), stringsAsFactors=FALSE) # create a new row with the correlation and two raster names
	#	df <- rbind(df, newrow) # add the new row to the data frame
					}
#df <- df[with(df, order(correlation)),] # sort the dataframe by the correlation column
#head(df)

# Remove rasters with correlation coefficients > 0.6 to create the predictorss raster stack. 
#Benthic preserved over pelagic, annual preserved over variability, slope preserved over depth
#Removed: tvar, salvar, chlmo_03_12, pp06_10, tsm06_10
###############################################

# Export Raster Stack
#r.unstack = unstack(pstack)
#lapply(x=seq_len(length(r.unstack)), FUN= function(x, my.unstack, fn){
#  writeRaster(my.unstack[[x]], fn[x])
#}

#plots raster stack
#pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)
#plot(pstack, 1:16, col=pal)
#plot(pstack, 17:20, col=pal)
       
# Create Training and Testing Data
#Extract values from rasters
###################################################################      
l <- c("LONUTM", "LATUTM")
cuskall_ll <-cusk[,l]
head(cuskall_ll)
pavals <- extract(pstack, cuskall_ll)
write.table(cuskall_ll,file="cuskall_ll.csv",sep=",",row.names=T)

#Extract catch values from rasters
l2 <- c("LONUTM", "LATUTM")
cusk.cll <- cusk.c[,l2]
cvals <- extract(pstack, cusk.cll)

#Attach presence column
s <- c("pa", "LONUTM", "LATUTM")
sdmdata<- data.frame(cbind(cusk[,s], pavals))
names(sdmdata)[1] <-"pa"

#create training and test data for Presence/Absence dataset 
c<- c("pa", "LONUTM", "LATUTM")
cusks1 <- cusk[, c]
pres <- cusks1[cusks1[,1] == 1, 2:3]
abs <- cusks1[cusks1[,1] == 0, 2:3]
group <- kfold(pres, 5)
groupa<- kfold(abs, 5)
pres_train <- pres[group != 1, ]
pres_test <- pres[group == 1, ]
abs_train <- abs[groupa != 1, ]
abs_test <- abs[groupa == 1, ]

train <- rbind(pres_train, abs_train)
c_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(abs_train)))
envtrain <- extract(pstack, train)
envtrain <- data.frame(cbind(train,envtrain))
envtrain <- data.frame( cbind(pa=c_train, envtrain))
test_pres <- data.frame(extract(pstack, pres_test))
test_abs <- data.frame(extract(pstack, abs_test))
envtest <- rbind(test_pres, test_abs)
c_test <- c(rep(1, nrow(test_pres)), rep(0, nrow(test_abs)))
envtest <- data.frame(cbind(pa=c_test, envtest))
envtrain<- envtrain[complete.cases(envtrain),]
par(mfrow= c(1,1))
write.table(envtrain,file="C:/Michelle/Michelle/R/Results/Testing/cuskenvtrain.csv",sep=",",row.names=F)
  
#Create histogram of cusk catch
#par(mfrow = c(1,2))
#with(envtrainsub, hist(depth, prob=TRUE))
#curve(dnorm(x, mean=mean(envtrainsub$depth), sd=sd(envtrainsub$depth)), add=TRUE)

# Species Distribtuion Models
####################################################################
xmn <- 100000
ymn <- 4500000
xmx <- 1000000
ymx <- 5300000
ext <-extent(xmn, xmx, ymn, ymx)

#GAM with PA
##############################################################
#gam1 <- gam(pa ~ sst01_10 + chlmo03_12 + s(tsm06_10, sal) + winttsm06_10 + pp2010 + pp06_10 + salvar + sal + strat + tvar + t + rms + depth + slopel + compl + twin + tsum + tspr + tfall + botcurr, family = gaussian, data=envtrain)
#gam1 <- mgcv::gam(pa ~ sst01_10 + pp2010 + sal + t + rms + slopel + compl + twin + tsum + tspr + tfall + botcurr, family = gaussian, data=envtrain)
gam2 <- mgcv::gam(pa ~ s(sst01_10) + s(chlmo03_12) + s(tsm06_10) + s(pp2010) + s(pp06_10) + s(winttsm06_10) + s(salvar) + s(strat) + s(tvar) + s(t) + s(rms) + s(depth) +  s(compl) + s(twin) + s(tsum) + s(tspr) + s(tfall) + s(botcurr), family = binomial(link="logit"), data=envtrain)
AIC(gam2)
#gam3 <- mgcv::gam(pa ~ s(sst01_10), family = binomial(link = "logit"), data=envtrain)
#Removed:tvar, salvar, chlmo_03_12, pp06_10 
jpeg(file="C:/Michelle/Michelle/R/Results/PA/GAM/partialplots_gam_pa.jpg", height=1000, width=800)
par(mfrow=c(5,4))
plot(gam2, all.terms=TRUE, ylab="", shade=TRUE, scale=0, cex.lab=1.5)
#If you want to convert the y-axes labels to the probability scale (0 to 1) use the following function:
#Invlogit = function(x){exp(x)/(1+exp(x))}
#Invlogit(5) is .99
#Invlogit(-5) is .007
#Invlogit(0) is .5
#Using this function you can change the y axis labels
#plot(obj, trans=Invlogit, select=i) where i is the model term
#So if you have 9 model terms:
#layout(matrix(1:9,3,3, byrow=T))
#for(i in 1:9){ plot(obj, trans=Invlogit, select=i)}
dev.off()
summary(gam2)
sink(file="C:/Michelle/Michelle/R/Results/PA/GAM/summary.txt")
summary(gam2)
sink()
e2<- evaluate(test_pres, test_abs, gam2)
sink(file="C:/Michelle/Michelle/R/Results/PA/GAM/cmatrix.txt")
table(envtrain$pa>0,fitted(gam2)>0.5)
sink()
pg<- predict(pstack, gam2, type='response')
jpeg(file="C:/Michelle/Michelle/R/Results/PA/GAM/predict_gam_pa.jpg", height=400, width=500)
par(mfrow=c(1,1), oma=c(1,1,1,3))
plot(pg, col=pal, main=' ')
tre <- threshold(e2, 'spec_sens')
tre
plot(coastline, add=TRUE, border='dark grey') # fix the projection of the coastaline
dev.off()
jpeg(file="C:/Michelle/Michelle/R/Results/PA/GAM/predict_gam_pa_area.jpg", height=300, width=800)
par(mfrow=c(1,3), oma=c(1,1,1,1))
pgpa <- pg>tre
plot(pg > 0.5, main='GAM Presence')
plot(coastline, add=TRUE, border='dark grey')
plot(pg > 0.7, main = '0.7')
plot(coastline, add=TRUE, border='dark grey')
plot(pg > 0.8, main = '0.8')
plot(coastline, add=TRUE, border='dark grey')
dev.off()
pgam1 <- pg > 0.5
pgam2 <- pg > 0.7
pgam3 <- pg > 0.8
gam2.preds <- prediction(gam2$fitted, envtrain$pa)
performance(gam2.preds, "auc")
library(ROCR)
testpredictions <- predict.gam(gam2, envtest, type="response")
gam2.preds <- prediction(as.vector(testpredictions), pa)
performance(gam2.preds, "auc")
myplot <- performance(gam2.preds, "tpr", "fpr")
windows()
plot(myplot)
#points(pres_train, pch='+', cex=1)
#points(abs_train, pch='-', cex=0.75)
#still need partial dependance plots here
setwd("C:/Michelle/Michelle/R/Results/PA/GAM")
pras <- writeRaster(pg, filename="predict_gam_pa.tif", datatype='GTiff', overwrite=TRUE)

#Maxent with PA
#######################
#checking if the jar file is present. If not skip this bit
setwd("C:/Michelle/Michelle/R/Results/PA/Maxent")
par(mfrow=c(1,1))
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if(file.exists(jar)){
xm<-maxent(pstack, pres_train, a=abs_train, args=c("threads=4", "replicatetype=crossvalidate", "jackknife=true", "responsecurves=true"))
jpeg("C:/Michelle/Michelle/R/Results/PA/Maxent/maxent_xm.jpg", height = 400, width = 300)
plot(xm)
dev.off()
} else { 
	cat('cannot run this example because maxent is not available')
}
xm
# Plot Variable Response Curves
#response(xm, 1:9, expand = 0.2, ylim = c(0.2, 0.6))
#response(xm, 10:18, expand = 0.2, ylim = c(0.2, 0.6)) 
#abline(h=0.46, lty=2)
#response(xm, 19:20, expand = 0.2, ylim = c(0.2, 0.6))
# figure out how to loop through and write these using the partial dependance code
# write them in order of importance
jpeg(file="C:/Michelle/Michelle/R/Results/PA/Maxent/responseplots_maxent_pa.jpg", height=800, width=800)
par(mfrow=c(3,3))
response(xm, 1, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 2, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 3, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 4, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 5, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 6, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 7, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 8, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 9, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
dev.off()
jpeg(file="C:/Michelle/Michelle/R/Results/PA/Maxent/responseplots_maxent_pa2.jpg", height=1000, width=800)
par(mfrow=c(4,3))
response(xm, 10, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 11, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 12, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 13, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 14, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 15, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 16, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 17, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 18, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
response(xm, 19, expand=0, cex.lab=1.5, ylim = c(0.3, 0.8))
abline(h=0.46, lty=2)
dev.off()
#Evaluate the Threshold
e<- evaluate(pres_test, abs_test, xm, pstack)
sink(file="C:/Michelle/Michelle/R/Results/PA/Maxent/evaluate.txt")
e
sink()
#Predict the species distribution model
m2 = predict(pstack, xm, progress='', type='response')
# Plot the spatial species distribution model
jpeg(file="C:/Michelle/Michelle/R/Results/PA/Maxent/predict_maxent_pa.jpg", height=400, width=500)
par(mfrow=c(1,1), oma=c(1,1,1,3))
ext <-extent(-68, -54, 41.0, 46)
pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)
plot(m2, col=pal, main='')
plot(coastline, add=TRUE, border = 'dark grey')
dev.off()
jpeg(file="C:/Michelle/Michelle/R/Results/PA/Maxent/predict_maxent_pa_area.jpg", height=300, width=800)
par(mfrow=c(1,3), oma=c(1,1,1,3))
tr3 <- threshold(e, 'spec_sens')
plot(m2 > 0.5, main='Maxent 0.5')
plot(coastline, add=TRUE, border='dark grey')
plot(m2 > 0.6, main='0.6')
plot(coastline, add=TRUE, border='dark grey')
plot(m2 > 0.7, main='0.7')
plot(coastline, add=TRUE, border='dark grey')
dev.off()

#points(pres_train, pch='+', cex=1)
#points(abs_train, pch='-', cex=0.75)
#Write the prediction to a raster file
pras <- writeRaster(m2, filename="predictmaxent_pa.tif", datatype='GTiff', overwrite=TRUE)
                        
#Random Forest PA
#################################
#switch the pa variable to a factor and then run the model as type = prob, and then it will run a binomial
#envtrain$pa <- as.factor(envtrain$pa)
envtrainsub <- envtrain[sample(1:nrow(envtrain), 10000, replace=FALSE),]
setwd("C:/Michelle/Michelle/R/Results/PA/Randomforest")
model <- pa ~ sst01_10 + chlmo03_12 + tsm06_10 + pp2010 + pp06_10 + winttsm06_10 + salvar +  strat + tvar + t + rms + depth+ slopel + compl + twin + tsum + tspr + tfall + botcurr
#rf1 <- randomForest(model, data=envtrainsub, type = 'prob', proximity=TRUE)
rf1 <- randomForest(model, data=envtrainsub, proximity=TRUE, type = 'regression')
rf1
#model <- factor(pa) ~ salvar + sal + sal96 + strat + salr96 + tvar + t + tr96 + t96 + t96win + t96sum + t96spr + t96fall + rms + bathyo + slopeo + compo + twin + tsum + tspr + tfall + sst + sstr+ chl + chlr + botst_wt + k490a + botcurr + phia + phir + phid
#sink(file="C:/Michelle/Michelle/R/Results/PA/Randomforest/summary.txt")
sink(file="C:/Michelle/Michelle/R/Results/PA/Randomforest/summaryr.txt")
print(rf1)
sink()
tiff(filename="C:/Michelle/Michelle/R/Results/PA/Randomforest/var_imp_plot.tiff", height = 6, width = 5, units = 'cm', res=350, compression = c("lzw"), pointsize = 4)
varImpPlot(rf1)
dev.off()
er = evaluate(test_pres, test_abs, rf1)
sink(file="C:/Michelle/Michelle/R/Results/PA/Randomforest/evaluate.txt")
er
sink()
#this model assumes that the response is not a factor
#pr<- predict(pstack, rf1, type='response')
# type = prob assumes that the response is a factor
pr <- predict(pstack, rf1, type = 'prob', index = 2)

# Calculate the confusion matrix
categorical_predictions = predictions
categorical_predictions [which(categorical_predictions >=0.5)] = 1
categorical_predictions [which(categorical_predictions <=0.5)] = 0
observations = envtest$pa
predictions <- predict(rf1, newdata=envtest, type = 'prob')[,2]
confusion = table(observations, categorical_predictions)
confusion
accuracy = sum(diag(confusion))/sum(confusion)
errorrate = 1 - accuracy
errorrate
# Calculate AUC
auc1 = auc(observations, predictions)

sink(file="C:/Michelle/Michelle/R/Results/PA/Randomforest/confusion_error.txt")
print("Confusion Matrix")
print(confusion)
print("Error Rate")
print(errorrate)
print("AUC")
print(auc1)
sink()

tiff(filename= "C:/Michelle/Michelle/R/Results/PA/Randomforest/predict_rf_pa.tiff", height = 6, width = 7.5, units = 'cm', res=350, compression = c("lzw"), pointsize = 4)
#jpeg(file="C:/Michelle/Michelle/R/Results/PA/Randomforest/predict_rf_pa.jpg", height=400, width=500)
par(mfrow=c(1,1), oma=c(1,1,1,3))
plot(pr, col=pal)
plot(coastline, add=TRUE, border='dark grey', lwd=0.5)
dev.off()
tiff(filename="C:/Michelle/Michelle/R/Results/PA/Randomforest/predict_rf_pa_area.tiff", height=3.5, width=10, units='cm', res=350, compression = c("lzw"), pointsize=4)
par(mfrow=c(1,3), oma=c(1,1,1,3))
trf<- threshold(er, 'spec_sens')
plot(pr>trf, main='Random Forest 0.5')
plot(coastline, add=TRUE, border='dark grey')
plot(pr>0.6, main='0.6')
plot(coastline, add=TRUE, border='dark grey')
plot(pr>0.9, main='0.9')
plot(coastline, add=TRUE, border='dark grey')
dev.off()
#points(pres_train, pch='+', cex=0.75)
#points(abs_train, pch='-', cex=0.5)
rras <- writeRaster(pr, filename="predict_rf_pa.tif", datatype='GTiff', overwrite=TRUE)
#partialPlot(rf1, envtrainsub, salvar)
imp1 <- importance(rf1)

#PARTIAL dependance plots
#logarithms. how do we explain this plot
#log transformed presence probability, maybe with all other variables as a mean
# log of the fraction of the votes, so similar to log of the probability as fraction of votes = probability
impvar1 <- rownames(imp1)[order(imp1[, 1], decreasing=TRUE)]

impvarp <- impvar1[1:6]
tiff("C:/Michelle/Michelle/R/Results/PA/RandomForest/partialdependancesmall.tiff", height=8, width = 11, units='cm', res=350, compression = c("lzw"), pointsize=4)
par(mfrow=c(2,3))
for (i in seq_along(impvarp)){
	partialPlot(rf1, envtrain, impvarp[i], which.class = 1, xlab=impvarp[i], cex.lab=1.5,
							main=" ")
}
dev.off()
jpeg(file="C:/Michelle/Michelle/R/Results/PA/Randomforest/partialdependance.jpg", height=400, width=800)
op<- par(mfrow=c(1,3))

for (i in seq_along(impvar1[7:9])){
	partialPlot(rf1, envtrain, impvar1[i], which.class = 1, xlab=impvar1[i], cex.lab=1.5,
							main=" ")
}
par(op)
dev.off()

#Highlighting areas of extrapolation in environmental space
n_bootstraps = 5
vals = c()
for (run in 1:n_bootstraps) # Now do the following for each desired bootstrap sample
{
print(run)
# Define rows to sample (with replacement) from 'data'
rows = sample(1:nrow(envtrain), replace=TRUE)
# Create new data frame to hold bootstrapped data, using structure from 'data'
boot_data = data.frame(matrix(vector(), 0, ncol(envtrain), dimnames=list(c(), colnames(envtrain))), stringsAsFactors=F)
# Put the data from the 'rows' in 'data' into boot_data'
for (i in 1:nrow(envtrain)) {boot_data = rbind(boot_data, envtrain[rows[i],])}
# Create model using bootstrapped data
rferror = randomForest(model, data=boot_data, type="prob")
# Make predictions
map = predict(pstack, rferror, type="prob", index=2)
# Extract predicted values to vector, and add them to 'vals'
v = getValues(map)
vals = cbind(vals, v)
}
	
#Now we calculate the e.g. 5th and 95th percentiles for each cell
#Create empty containers for the percentile values
p5 = rep(NA, length(v))
p95 = rep(NA, length(v))

#Calculate the percentile values for each cell
for(cell in 1:length(v))
{
	print(cell)
	p5[cell] = quantile(vals[cell,], 0.05, na.rm=T)
	p95[cell] = quantile(vals[cell,], 0.95, na.rm = T)
}

#Put results into rasters and write them to a file
p5_raster = raster(pstack, 1)
p95_raster = raster(pstack, 1)
p5_raster[] = p5
p95_raster[] = p95
writeRaster(p5_raster, filename = "p5.tif", overwrite = T, format = "GTiff", datatype = "FLT4S")
writeRaster(p95_raster, filename = "p95.tif", overwrite = T, format = "GTiff", datatype = "FLT4S")

diff = p95_raster - p5_raster
writeRaster(diff, filename = "diff.tif", overwrite = T, format = "GTiff", datatype = "FLT4S")
# index = 2 is probability of presence or absence, which ever one is first in your model
# type = prob vs. type = response.......

######
#Plot all the SDMs together
#jpeg(file="C:/Michelle/Michelle/R/Results/PA/predict_combine.jpg", height=300, width=800)
#par(mfrow=c(2,2), oma=c(2, 0.5, 0.5, 2))
#plot(pr, col=pal, main='Random Forest')
#plot(coastline, add=TRUE, border='dark grey')
#plot(m2, col=pal, main='Maxent')
#plot(coastline, add=TRUE, border='dark grey')
#plot(pg, col=pal, main='GAM')
#plot(coastline, add=TRUE, border='dark grey')
######     
#Combine Presence/Absence Models
setwd("C:/Michelle/Michelle/R/Results/PA")
jpeg(file="C:/Michelle/Michelle/R/Results/PA/combined.jpg", height = 500, width = 800)
par(mfrow=c(2,2), oma=c(2, 0.5, 0.5, 2))
models <- stack(pg, m2, pr)
combined <- mean(models)
models <- stack(pg, m2, pr, combined)
names(models) <- c("GAM", "maxent", "random forest", "combined")                        
plot(models, col=pal) 
dev.off()
rras <- writeRaster(combined, filename="predictmean.tif", datatype='GTiff', overwrite=TRUE)
#####
# test whether the test and train points are working correctly
#r= raster(pstack, 1)
#plot(!is.na(r), col=c('white', 'light grey'), legend = FALSE)
#points(abs_train, pch='-', cex=0.5, col='yellow')
#points(abs_test, pch='-', cex=0.5, col='black')
#points(pres, pch='+', cex=0.5, col='green')
#points(pres_test, pch='+', ce=0.5, col='blue')

#Catch Models
#create training and testing data for catch dataset
################################################ 
c2<- c("CATCH", "LONUTM", "LATUTM")
cuskc2 <- cusk.c[, c2]
presc <- cuskc2[cuskc2[,1] > 0, 2:3]
absc <- cuskc2[cuskc2[,1] == 0, 2:3]
groupc<-kfold(presc, 5)
groupca<-kfold(absc, 5)
pres_trainc<-presc[groupc != 1, ]
pres_testc<-presc[groupc == 1, ]
abs_trainc <- absc[groupca != 1, ]
abs_testc <- absc[groupca ==1, ]
       
#extract catch values for GLM and RF
presc2 <- cusk.c[cusk.c[,3] > 0, 3:1]
#rite.table(presc2,file="C:/Michelle/Michelle/R/Results/Testing/presc2.csv",sep=",",row.names=F)
absc2 <- cusk.c[cusk.c[,3] == 0, 3:1]
groupc2 <- kfold(presc2, 5)
groupca2 <- kfold(absc2, 5)
pres_trainc2 <- presc2[groupc2 != 1, ]
pres_testc2<- presc2[groupc2== 1, ]
abs_trainc2 <- absc2[groupca2 != 1, ]
abs_testc2 <- absc2[groupca2 == 1, ]
       
trainc <- rbind(pres_trainc, abs_trainc)
#xxxxxxwrite.table(trainc,file="C:/Michelle/Michelle/R/Results/Testing/trainc.csv",sep=",",row.names=F)
c_trainc <- rbind(pres_trainc2[1], abs_trainc2[1])
envtrainc <- extract(pstack, trainc)
envtrainc <- data.frame(cbind(trainc,envtrainc))
envtrainc <- data.frame(cbind(ca=c_trainc, envtrainc))
test_presc <-data.frame(extract(pstack, pres_testc))
test_absc <- data.frame(extract(pstack, abs_testc))
envtrainc <- envtrainc[complete.cases(envtrainc), ]
envtrainc_test <- subset(envtrainc, depth < -900)

envtestc <- rbind(test_presc, test_absc)
c_testc <- rbind(pres_testc2[1], abs_testc2[1])
envtestc <- data.frame(cbind(ca=c_testc, envtestc))
envtestc <- envtestc[complete.cases(envtestc), ]

write.table(envtrainc,file="C:/Michelle/Michelle/R/Results/Testing/cuskenvtrainc.csv",sep=",",row.names=F)

# Catch Species Distribution Models ---------------------------------------

#GAM with Catch
#gam is like glm, but with extra facilities to allow smooth
#functions of covariates in the linear predictor.
##############################################################
#gam1 <- gam(pa ~ sst01_10 + chlmo03_12 + s(tsm06_10, sal) + winttsm06_10 + pp2010 + pp06_10 + salvar + sal + strat + tvar + t + rms + depth + slopel + compl + twin + tsum + tspr + tfall + botcurr, family = gaussian, data=envtrain)
gamc <- mgcv::gam(I(mean+.Machine$double.eps) ~ s(sst01_10) + s(chlmo03_12) + s(tsm06_10) + s(pp2010) + s(pp06_10) + s(winttsm06_10) + s(salvar) + s(strat) + s(tvar) + s(t) + s(rms) + s(depth) + s(slopel) + s(compl) + s(twin) + s(tsum) + s(tspr) + s(tfall) + s(botcurr), family = gaussian(link = "identity"), data=envtrainc)
#Removed:tvar, salvar, chlmo_03_12, tsm06_10, depth, pp06_10, strat, wintsm06_10
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/GAM/partialplots_gam_catch.jpg", height=1000, width=800)
par(mfrow=c(6,4))
# fitted(z)  # predicted values on the original scale
plot(gamc, scale=0, seWithMean=T, shade=T)
dev.off()
par(mfrow=c(1,1))
#fitted(gamc)
#plot(envtrainc$sst01_10~ fitted(gamc))
#plot(gamc, seWithMean=T, scale=0)
#plot(fitted(gamc), envtrainc$rms)
#plot(residuals(gamc), fitted(gamc))
#plot the partial dependance plots. These keep all other variables constant to determine the importance of a single predictor
#the scale of these is the transformed variables, in this case the log of the response variable
#Invlogit = function(x){exp(x)/(1+exp(x))} for a poisson distribution
#plot(gamc, all.terms=TRUE, ylab="", trans = exp, seWithMean=TRUE, shade=TRUE, scale=0, cex.lab=1.5)
#vis.gam(gamc, view=c("rms","tfall"),theta=30, thicktype='detailed')
summary(gamc)
sink(file="C:/Michelle/Michelle/R/Results/Catch/GAM/summary.txt")
summary(gamc)
sink()
ec<- evaluate(test_presc, test_absc, gamc)
sink(file="C:/Michelle/Michelle/R/Results/Catch/GAM/cmatrix.txt")
table(envtrainc$mean>0,fitted(gamc)>0.5)
sink()
# try including sefit=TRUE
# is this because I am trying to predict on values that are not splined????
pgc<- predict(pstack, gamc) # predicted values on the transformed scale unless you specify type=response
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/GAM/predict_gam_catch.jpg", height=400, width=500)
par(mfrow=c(1,1), oma=c(1,1,1,3))
plot(pgc, col=pal, main=' ')
plot(coastline, add=TRUE, border='dark grey') 
pgpa <- pgc>tre2
dev.off()
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/GAM/predict_gam_catch_area.jpg", height=300, width=800)
par(mfrow=c(1,3), oma=c(1,1,1,3))
tre2 <- threshold(ec, 'spec_sens')
tre2
plot(pgc > 125, main='GAM 125 ')
plot(coastline, add=TRUE, border='dark grey')
plot(pgc > 175, main='175')
plot(coastline, add=TRUE, border='dark grey')
plot(pgc > 200, main='200')
plot(coastline, add=TRUE, border='dark grey')
dev.off()
gamc.preds <- prediction(gamc$fitted, envtrainc$mean)
performance(gamc.preds, "auc")
library(ROCR)
testpredictionsc <- predict.gam(gamc, envtestc, type="response")
gam2.predsc <- prediction(as.vector(testpredictionsc), mean)
performance(gam2.predsc, "auc")
myplotc <- performance(gam2.predsc, "tpr", "fpr")
windows()
plot(myplotc)
#points(pres_train, pch='+', cex=1)
#points(abs_train, pch='-', cex=0.75)
setwd("C:/Michelle/Michelle/R/Results/Catch/GAM")
pras <- writeRaster(pgc, filename="predict_gam_catch.tif", datatype='GTiff', overwrite=TRUE)

#Random Forest Catch
########################################
envtraincsub <- envtrainc[sample(1:nrow(envtrainc), 15000, replace=FALSE),]
modelc <- CATCH ~ sst01_10 + chlmo03_12 + tsm06_10 + pp2010 + pp06_10 + winttsm06_10 + salvar+ strat + tvar + t + rms + depth + slopel + compl + twin + tsum + tspr + tfall + botcurr
#Removed:tvar, salvar, chlmo_03_12, tsm06_10, pp06_10 
#modelc <- mean ~ lonutm + latutm + sst01_10 + chlmo03_12 + tsm06_10 + winttsm06_10 + pp2010 + pp06_10 + salvar + sal + strat + tvar + t + rms + depth + slopel + compl + twin + tsum + tspr + tfall + botcurr
rfc <- randomForest(modelc, data=envtraincsub)
sink(file="C:/Michelle/Michelle/R/Results/Catch/RandomForest/summary.txt")
rfc
sink()
erc = evaluate(test_presc, test_absc, rfc)
sink(file="C:/Michelle/Michelle/R/Results/Catch/RandomForest/evaluate.txt")
erc
sink()
prc<- predict(pstack, rfc)
jpeg("C:/Michelle/Michelle/R/Results/Catch/RandomForest/predict_rf_c.jpg", height=400, width = 500)
par(mfrow=c(1,1), oma=c(1,1,1,3))
plot(prc, col=pal, main='Random Forest')
plot(coastline, add=TRUE, border='dark grey')
dev.off()
jpeg("C:/Michelle/Michelle/R/Results/Catch/RandomForest/predict_rf_c_area.jpg", height=300, width = 800)
par(mfrow=c(1,3), oma=c(1,1,1,3))
trc<- threshold(erc, 'spec_sens')
plot(prc > 125, main='Random Forest 125')
plot(coastline, add=TRUE, border='dark grey')
plot(prc > 175, main='100')
plot(coastline, add=TRUE, border='dark grey')
plot(prc > 200, main='200')
plot(coastline, add=TRUE, border='dark grey')
dev.off()

#points(pres_trainc, pch='+', cex=1)
#points(abs_trainc, pch='-', cex=0.75)
setwd("C:/Michelle/Michelle/R/Results/Catch/RandomForest")
rrasc <- writeRaster(prc, filename="predictrfc.tif", datatype='GTiff', overwrite=TRUE)
jpeg("C:/Michelle/Michelle/R/Results/Catch/RandomForest/varImpPlot.jpg", height=500, width = 400)
varImpPlot(rfc)
dev.off()
#partialPlot(rfc, envtrainc, salvar)
imp <- importance(rfc)
jpeg("C:/Michelle/Michelle/R/Results/Catch/RandomForest/partialdependance.jpg", height=800, width = 800)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
par(mfrow=c(4,3))
impvar1 <- impvar[1:12]
for (i in seq_along(impvar1)){
	partialPlot(rfc, envtrainc, impvar1[i], xlab=impvar1[i],
							main=" ")
}
dev.off()
impvar2 <- impvar[13:22]
jpeg("C:/Michelle/Michelle/R/Results/Catch/RandomForest/partialdependance2.jpg", height=800, width = 800)
par(mfrow=c(4,3))
for (i in seq_along(impvar2)){
	partialPlot(rfc, envtrainc, impvar2[i], xlab=impvar2[i],
							main=" ")
}
dev.off()
par(op)
par(mfrow=c(1,1))
################
 
# test whether the test and train points are working correctly
r= raster(pstack, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend = FALSE)
points(abs_trainc2, pch='-', cex=0.5, col='yellow')
points(abs_testc2, pch='-', cex=0.5, col='black')
points(presc2, pch='+', cex=0.5, col='green')
points(pres_testc2, pch='+', ce=0.5, col='blue')                        
#test
#setwd("C:/Michelle/Michelle/R/testing")
#write.table(cusks1,file="cusks1.csv",sep=",",row.names=F)
#write.table(pres,file="pres.csv",sep=",",row.names=F)
#Combine Catch Models
extprc <- extent(pgc)
prc <- crop(prc, extprc)
modelsc <- stack(pgc, prc)
names(modelsc) <- c("GLM", "random forest")                        
plot(modelsc, col=pal)
setwd("C:/Michelle/Michelle/R/Results/Catch")
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/combined.jpg", height = 250, width = 800)
par(mfrow = c(1,3))
plot(pgc, col= pal,  main = "GLM")
plot(prc, col = pal, main = "RF")
plot(mean(modelsc), col= pal, main='average score')                        
dev.off()
rrasc <- writeRaster(mean(modelsc), filename="predictmeanc.tif", datatype='GTiff', overwrite=TRUE)

# Kernel Density Comparisons
####################################################################
#Create histogram of cusk catch
# Set factor levels for the low, med and high catch values
quantile(envtrainc$mean, c(0.5, 0.6, 0.7, 0.9, 0.95))
max(envtrainc$mean)
envtrainc$meanf[envtrainc$mean == 0] <- 0
envtrainc$meanf[envtrainc$mean > 0 & envtrainc$mean <= 22.976]  <- 1 # values below the 70th percentile
envtrainc$meanf[envtrainc$mean > 22.976 & envtrainc$mean <= 373.01] <-2 # values between the 70th and 90th percentile
envtrainc$meanf[envtrainc$mean > 373.01] <- 3

########
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/KernelDensity/densitycurves.jpg", height=600, width=800)
envtrainc$id = 1:nrow(envtrainc)
require(reshape2)
envtrainc.m <- melt(envtrainc, id.vars= c('id', 'meanf'))
envtrainc.m$grp1 <- factor(gsub("\\..*$", "", envtrainc.m$meanf))
envtrainc.m$grp2 <- factor(gsub(".*\\.", "", envtrainc.m$variable))
m1 <- c("lonutm", "latutm", "sst01_10", "chlmo03_12", "tsm06_10", "pp2010", "pp06_10", "winttsm06_10","salvar", "strat", "tvar")
b <- envtrainc.m$grp2
envtrainc.m$grp1 <- factor(gsub("\\..*$", "", envtrainc.m$meanf))
envtrainc.m$grp2 <- factor(gsub(".*\\.", "", envtrainc.m$variable))
legend_title = "Catch"
l <- ylab("Density")
bg <- theme(panel.background=element_blank(), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),
		axis.line=element_line(colour='black')) 
cs <- scale_fill_manual(
	legend_title, 
	values=c("gray", "lightblue", "lightgreen", "peachpuff2"), 
	labels=c("absence", "low", "med", "high"))
p <- ggplot(envtrainc.m, aes(x=value)) +geom_density(aes(fill=grp1), alpha = 0.8) + l + bg + cs
p <- ggplot(envtrainc.m1, aes(x=value)) +geom_density(aes(fill=grp1), alpha = 0.8) + l + bg + cs
p <- p + facet_wrap (~ grp2, scales="free")
p
dev.off()
##########
#Split the variables in half for a smaller image
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/KernelDensity/densitycurves1.jpg", height=600, width=800)
envtrainc.m1 <- envtrainc.m[b =="lonutm" | b== "latutm" | b=="sst01_10" | b=="chlmo03_12" | b=="tsm06_10" |b== "pp2010"|b== "pp06_10"|b=="winttsm06_10"|b== "salvar"|b== "sal"|b == "botcurr"| b== "tfall" |b== "depth", ]
envtrainc.m1$grp1 <- factor(gsub("\\..*$", "", envtrainc.m1$meanf))
envtrainc.m1$grp2 <- factor(gsub(".*\\.", "", envtrainc.m1$variable))
legend_title = "Catch"
l <- ylab("Density")
bg <- theme(panel.background=element_blank(), 
		panel.grid.major=element_blank(), 
		panel.grid.minor=element_blank(), 
		panel.border=element_blank(),
		axis.line=element_line(colour='black')) 
cs <- scale_fill_manual(
	legend_title, 
	values=c("gray", "lightblue", "lightgreen", "coral2"), 
	labels=c("absence", "low", "med", "high"))
legend_title = "Catch"
p1 <- ggplot(envtrainc.m1, aes(x=value)) +geom_density(aes(fill=grp1), alpha = 0.5) + l + bg + cs
p1 <- p1 + facet_wrap (~ grp2, scales="free", nrow = 4, ncol=3)
p1
dev.off()
#########
#Second plot
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/KernelDensity/densitycurves2.jpg", height=600, width=800)
envtrainc.m2 <- envtrainc.m[b== "tspr"|b== "tsum"|b== "twin"|b== "compl"|b== "slopel"|b== "rms"|b== "t"|b== "tvar"|b== "strat", ]
envtrainc.m2$grp1 <- factor(gsub("\\..*$", "", envtrainc.m2$meanf))
envtrainc.m2$grp2 <- factor(gsub(".*\\.", "", envtrainc.m2$variable))
p2 <- ggplot(envtrainc.m2, aes(x=value)) +geom_density(aes(fill=grp1), alpha = 0.6) + l + bg + cs
p2 <- p2 + facet_wrap (~ grp2, scales="free", nrow=4, ncol=3)
p2
dev.off()
######
# Individual plots
ggplot(envtrainc, aes(x=rms, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs + bg + l
ggplot(envtrainc, aes(x=sst01_10, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=tspr, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=tfall, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=sal, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=twin, fill=as.factor(meanf))) + geom_density(alpha=0.6) + cs+ bg + l
ggplot(envtrainc, aes(x=tsum, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l + scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
ggplot(envtrainc, aes(x=botcurr, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=t, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l + scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))

jpeg(file="C:/Michelle/Michelle/R/Results/Catch/KernelDensity/densitycurves2.jpg", height=800, width=800)
par(mfrow=c(3,3))
ggplot(envtrainc, aes(x=slopel, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=compl, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l + scale_x_continuous(breaks=c( 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3))
ggplot(envtrainc, aes(x=salvar, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=strat, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=depth, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l + scale_x_continuous(breaks=c(0, -25, -50,-100, -200, -300, -400, -500, -700, -1000, -1500))
ggplot(envtrainc, aes(x=chlmo03_12, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=tsm06_10, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l
ggplot(envtrainc, aes(x=pp06_10, fill=as.factor(meanf))) + geom_density(alpha=0.6) + cs+ bg + l
ggplot(envtrainc, aes(x=pp2010, fill=as.factor(meanf))) + geom_density(alpha=0.6) + cs+ bg + l

dev.off()
jpeg(file="C:/Michelle/Michelle/R/Results/Catch/KernelDensity/densitycurves3.jpg", height=300, width=800)
par(mfrow=c(1,3))
ggplot(envtrainc, aes(x=tvar, fill=as.factor(meanf))) + geom_density(alpha=0.6) + cs+ bg + l
ggplot(envtrainc, aes(x=winttsm06_10, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l + scale_x_continuous(breaks=c(-0.5, -0.3, -0.2, -0.25, -0.15, -0.1, -0.5, 0, 0.25, 0.5, 0.75, 1))
ggplot(envtrainc, aes(x=winttsm06_10, fill=as.factor(meanf))) + geom_density(alpha=0.8) + cs+ bg + l + scale_x_continuous(breaks=c(-0.5, -0.3, -0.2, -0.25, -0.15, -0.1, -0.5, 0, 0.25, 0.5, 0.75, 1))

dev.off()
################################################################################
#plot first raster in raster stack with coastline and cusk points
coastline <- readShapeSpatial("GOMCoastline_WGS84")
pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)
plot(pstack, 1, col=pal)
plot(coastline, xlim=c(-68, -64), ylim=c(41, 45), axes=TRUE, col='gray83', add=TRUE)
presence<- as.factor(cusks[,17])
points(cusks$SlongDD, cusks$SlatDD, col=presence, pch = ifelse(presence==1, 19, 4), cex = ifelse(presence==1, 0.5, 0.2))

#write table for testing
#write.table(sdmdata,file="sdmdata.csv",sep=",",row.names=F)
#wrtie.table(pres,file="pres.csv",sep=",",row.names=F)
write.table(abs,file="abs.csv",sep=",",row.names=F)

#PCA
Trns_site <- predict(pstack, rf1)
Trns_grid <-rasterToPoints(pstack)
Trns_grid <- cbind(Trns_grid[, c("x", "y")], predict(rf1, Trns_grid))
PCs <- prcomp(na.omit(Trns_grid))
a1 <- PCs$x[, 1]
a2 <- PCs$x[, 2]
a3 <- PCs$x[, 3]
r <- a1 + a2
g <- -a2
b <- a3 + a2 - a1
r <- (r - min(r))/(max(r) - min(r)) * 255
g <- (g - min(g))/(max(g) - min(g)) * 255
b <- (b - min(b))/(max(b) - min(b)) * 255
nvs <- dim(PCs$rotation)[1]
vec <- c(names(pstack))
lv <- length(vec)
vind <- rownames(PCs$rotation) %in% vec
scal <- 40
xrng <- range(PCs$x[, 1], PCs$rotation[, 1]/scal)*1.1
yrng <- range(PCs$x[, 2], PCs$rotation[, 2]/scal)*1.1
plot((PCs$x[,1:2]), xlim=xrng, ylim = yrng, pch=".", cex=4, col=rgb(r, g, b, max=255), asp=1)
points(PCs$rotation[!vind, 1:2]/scal, pch = "+")
arrows(rep(0, lv), rep(0, lv), PCs$rotation[vec, 1]/scal, PCs$rotation[vec, 2]/scal, length = 0.0625)
jit <- 0.0015
text(PCs$rotation[vec, 1]/scal + jit * sign(PCs$rotation[vec, 1]), PCs$rotation[vec, 2]/scal + jit * sign(PCs$rotation[vec, 2]), labels = vecc)
PCsites <- predict(PCs, Trns_site)
points(PCsites[, 1:2])
SpsWtd <- sweep(rf1$y, 2, apply(rf$y, 2, min), "-")

plot(pr, col=rgb(r, g, b, max = 255), ext = extp, main='Random Forest')

# data statistics
# data imported
nrow(cusk)
# data selected with values better than 1300 m resolution
nrow(cusks)
# number of presence locations
nrow(pres)
# number of absence locations
nrow(abs)
# presence training locations
nrow(pres_train)
# presence testing locations
nrow(pres_test)
# absence training locations
nrow(abs_train)
# absence testing locations
nrow(abs_test)
# catches > 0
nrow(presc)
# absences
nrow(absc)
# training catches
nrow(pres_trainc)
# catch training absences
nrow(abs_trainc)
# catch testing absences
nrow(abs_testc)
# values included after extraction
nrow(envtrain[complete.cases(envtrain),])
envtrainp <- subset(envtrain, pa > 0)
nrow(envtrainp[complete.cases(envtrainp),])
envtraina <- subset(envtrain, pa == 0)
nrow(envtraina[complete.cases(envtraina),])
nrow(test_pres[complete.cases(test_pres),])
nrow(test_abs[complete.cases(test_abs),])

########
#old kernel density estimates
sm.density.compare(envtrainc$pp2010, envtrainc$meanf, main="", xlab = "Primary Productivity 2010" ,col=c("black", "blue", "green", "red"), lwd=1.7)
par(mfrow = c(1,1))
with(envtrainc, hist(log(mean), breaks=12, prob=F ))
mean(envtrainc$mean)
# Plot Kernel Density Estimates for catch data
m <- envtrainc$mean[envtrainc$mean > 0]
quantile(m, c(0.5, 0.8, 0.85, .90, 0.95, .99))


library(sm)
cyl.mf <- factor(envtrainc$meanf, levels= c(0, 1, 2, 3), labels = c("Absence", "Low Catch", "Med Catch", "High Catch"))

sm.density.compare(envtrainc$rms, envtrainc$meanf, main="", xlab = "RMS Current Stress", col=c("black", "blue", "green", "red"), cex.lab=3, lwd=1.7)
sm.density.compare(envtrainc$sst01_10, envtrainc$meanf, main="", xlab = "Sea Surface Temperature 2001-2010" ,col=c("black", "blue", "green", "red"), cex.lab=2.5, lwd=1.7)
sm.density.compare(envtrainc$tspr, envtrainc$meanf, main="", xlab = "Spring Temperature" ,col=c("black", "blue", "green", "red"), cex.lab=2.5, lwd=1.7)
legend("topright", levels(cyl.mf), fill=c("black", "blue", "green", "red"), cex=1.5)
sm.density.compare(envtrainc$tfall, envtrainc$meanf, main="", xlab = "Fall Temperature", col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)
sm.density.compare(envtrainc$sal, envtrainc$meanf, main="", xlab = "Salinity" ,col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)
sm.density.compare(envtrainc$twin, envtrainc$meanf, main="", xlab = "Winter Temperature" ,col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)
sm.density.compare(envtrainc$t, envtrainc$meanf, main="", xlab = "Temperature" ,col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)
sm.density.compare(envtrainc$tsum, envtrainc$meanf, main="", xlab = "Summer Temperature" ,col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)    
sm.density.compare(envtrainc$slopel, envtrainc$meanf, main="", xlab = "Slope" ,col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)

sm.density.compare(envtrainc$botcurr, envtrainc$meanf, main="", xlab = "Bottom Current" ,col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)
sm.density.compare(envtrainc$compl, envtrainc$meanf, main="", xlab = "Benthic Complexity" ,col=c("black", "blue", "green", "red"), cex.lab=2, lwd=1.7)
sm.density.compare(envtrainc$salvar, envtrainc$meanf, main="", xlab = "Salinity Variability" ,col=c("black", "blue", "green", "red"), lwd=1.7)       
legend("topright", levels(cyl.mf), fill=c("black", "blue", "green", "red"), cex=1.5)
sm.density.compare(envtrainc$strat, envtrainc$meanf, main="", xlab = "Stratification" ,col=c("black", "blue", "green", "red"), lwd=1.7)
sm.density.compare(envtrainc$tsm06_10, envtrainc$meanf, main="", xlab = "Total Suspended Matter 2006-2010" ,col=c("black", "blue", "green", "red"), lwd=1.7)
sm.density.compare(envtrainc$winttsm06_10, envtrainc$meanf, main="", xlab = "Winter Total Suspended Matter 2006-2010" ,col=c("black", "blue", "green", "red"), lwd=1.7)
sm.density.compare(envtrainc$depth, envtrainc$meanf, main="", xlab = "Depth" ,col=c("black", "blue", "green", "red"), lwd=1.7)
sm.density.compare(envtrainc$chlmo03_12, envtrainc$meanf, main="", xlab = "Chlorophyll 2003-2012" ,col=c("black", "blue", "green", "red"), lwd=1.7)
sm.density.compare(envtrainc$tvar, envtrainc$meanf, main="", xlab = "Temperature Variability" ,col=c("black", "blue", "green", "red"), lwd=1.7)

sm.density.compare(envtrainc$pp06_10, envtrainc$meanf, main="", xlab = "Primary Productivity 2006-2010" ,col=c("black", "blue", "green", "red"), lwd=1.7)

			 
			 #Plot single kernel density curves
# Plot Kernel Density Estimates
 d<- density(envtrainsub$depth)
plot(d, main="")
       
#Plot kernel density curve for presence and absence values
jpeg(file="C:/Michelle/Michelle/R/Results/PA/KernelDensity/densitycurves1.jpg", height=800, width=800)
par(mfrow = c(3,3))
cyl.f <- factor(envtrain$pa, levels= c(0, 1), labels = c("Absence", "Presence"))
sm.density.compare(envtrain$tfall, envtrain$pa, main="", xlab = "Fall Temperature", col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$sst01_10, envtrain$pa, main="", xlab = "Sea Surface Temperature 2001-2010" ,col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$rms, envtrain$pa, main="", xlab = "RMS Current Stress" ,col=c("black", "blue"), cex.lab=2, lwd=2)
legend("topright", levels(cyl.f), fill=c("black", "blue"), cex=1.5)
sm.density.compare(envtrain$twin, envtrain$pa, main="", xlab = "Winter Temperature" ,col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$t, envtrain$pa, main="", xlab = "Temperature" ,col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$tsum, envtrain$pa, main="", xlab = "Summer Temperature" ,col=c("black", "blue"), cex.lab=2, lwd=2)    
sm.density.compare(envtrain$tspr, envtrain$pa, main="", xlab = "Spring Temperature" ,col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$sal, envtrain$pa, main="", xlab = "Salinity" ,col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$slopel, envtrain$pa, main="", xlab = "Slope" ,col=c("black", "blue"), cex.lab=2, lwd=2)
dev.off()
jpeg(file="C:/Michelle/Michelle/R/Results/PA/KernelDensity/densitycurves2.jpg", height=300, width=800)
par(mfrow=c(1,3))
sm.density.compare(envtrain$botcurr, envtrain$pa, main="", xlab = "Bottom Current" ,col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$compl, envtrain$pa, main="", xlab = "Benthic Complexity" ,col=c("black", "blue"), cex.lab=2, lwd=2)
sm.density.compare(envtrain$tfall, envtrain$pa, main="", xlab = "Fall Temperature", col=c("black", "blue"), cex.lab=2, lwd=2)
legend("topright", levels(cyl.f), fill=c("black", "blue"), cex=1.5)
dev.off()
#sm.density.compare(envtrain$strat, envtrain$pa, main="", xlab = "Stratification" ,col=c("black", "blue"))
#sm.density.compare(envtrain$tsm06_10, envtrain$pa, main="", xlab = "Total Suspended Matter 2006-2010" ,col=c("black", "blue"))
#sm.density.compare(envtrain$pp2010, envtrain$pa, main="", xlab = "Primary Productivity 2010" ,col=c("black", "blue"))
#sm.density.compare(envtrain$winttsm06_10, envtrain$pa, main="", xlab = "Winter Total Suspended Matter 2006-2010" ,col=c("black", "blue"))
#sm.density.compare(envtrain$depth, envtrain$pa, main="", xlab = "Depth" ,col=c("black", "blue"))
#sm.density.compare(envtrain$chlmo03_12, envtrain$pa, main="", xlab = "Chlorophyll 2003-2012" ,col=c("black", "blue"))
#sm.density.compare(envtrain$tvar, envtrain$pa, main="", xlab = "Winter Variability" ,col=c("black", "blue"))
#sm.density.compare(envtrain$pp06_10, envtrain$pa, main="", xlab = "Primary Productivity 2006-2010" ,col=c("black", "blue"))
#sm.density.compare(envtrain$salvar, envtrain$pa, main="", xlab = "Salinity Variability" ,col=c("black", "blue"))       


#as.factor(meanf))

#mt <- ddply(envtrainc, "meanf", summarise, t.mean=mean(t))
# + geom_vline(data=mt, aes(xintercept=t.mean, yintercept=0, colour="red"), linetype="dashed", size=1)
max.depth <- max(envtrainc$depth)
lines(desnity(envtrainc$depth, na.rm = T, from = 0, to= max.depth))


plot(envtrainc$depth, envtrainc$mean)
lines(lowess(envtrainc$depth, envtrainc$mean), col="blue")

meandepth <- tapply(envtrainc$depth, envtrainc$meanf, mean)
catch <- factor(levels(envtrainc$meanf), levels = levels(envtrainc$meanf))
qplot(catch, meandepth)
envtrainc$posdepth  <- envtrainc$depth * -1
qplot(meanf, data=envtrainc, geom="bar", weight=posdepth, ylab="depth")
qplot(catch, meandepth, geom="bar", stat="identity")
qplot(envtrainc$meanf, envtrainc$depth, stat="identity")
highcatch <- subset(envtrainc, meanf ==3)
summary(highcatch$depth)

par(mfrow= c(3,4))
plot(pstack[[1:9]], axes=F, box=F, col=pal)
plot(pstack[[10:19]], axes=F, box=F, col=pal)
plot(pstack[[19]], axes=F, box=F, col=pal)



