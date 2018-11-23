habitat.analysis.seacucumber = function(){
  
  p = setup.parameters()
  
  #install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable", dep=TRUE, lib = "C:/Michelle/temp")
  library('INLA', lib.loc = "C:/Michelle/temp")
  
  library(gstat)
  
  setwd("C:/ss/R")
  source("HighstatLibV10.R")
  
  #Extract Cat data from groundfish survey 
  k = groundfish.db(DS="cat.base")
  species = c('6600', '6611') #sea cucumber
  j = survey.process(k, species) #make sure to set p$data.yrs
  #write.shapefile(j, 'seacucumber_groundfish', p)

  #Data Exploration
  #----------------------------------
  table(j$y)
  #Scale data  (x - mean(x)) / sd(x) or (x-min(x))/(max(x) - min(x))
  #colSums(is.na(j))
  #j %>% summarise_each(funs(sum(.==0)))
  j = j[which(j$totwgt_sd != 0), ]
  
  t <- as.data.frame(table(round(j$totwgt_sd, 0)))  
  plot(t)
  #-----------------------------------
  
  
  j = j[!is.na(j$lat),]
  j.cords <- j[, c("lon", "lat")]
  sfd <- SpatialPointsDataFrame(j.cords, data=j)
  proj4string(sfd) <- CRS('+proj=longlat +ellps=WGS84')  
  j.cnew <- spTransform(sfd, p$internal.crs)
  j <- as.data.frame(j.cnew)
  j$yr <- as.factor(j$yr)

  #IDW Interpolation
  #----------------------------------
  require(gstat)
  require(sp)
  data(meuse)
  data(meuse.grid)
  coordinates(meuse) = ~x+y
  coordinates(meuse.grid) = ~x+y 
  gridded(meuse.grid) = TRUE
  
  power = seq(from = 1, to = 1.6, by = 0.1)
  neigh = seq(from = 4, to = 5, by = 1)
  # January
  results=list()
  results.cv=list()
  for (i in power) {
    for (j in neigh) {
      results[[paste0(i,"_",j)]] = idw (zinc ~ 1, meuse, meuse.grid, nmax = i, idp = j)
      results.cv[[paste0(i,"_",j)]] = krige.cv (zinc ~ 1, meuse, nfold = 52)
    }
  } 
  
#INLA Model
  
  
  
  
  
  #INLA Model
  #---------------------------------
  cor1 <- round(j$lat.1/1000, -1)
  
  m1 <- totwgt_sd ~ yr + f(round(lat.1/1000, -1), model ="rw1") + f(round(lon.1/1000, -1), model = 'rw1')
  #m1 <- totwgt_sd ~ yr 
  
  I1 = inla(m1, family = 'gamma', data = j, control.compute = list(dic = T, waic = T), 
            control.family = list(link = 'log', hyper = list(prec = list(prior = 'loggamma', param = c(0.7, 0.05)))))
  
  summary(I1)

  
  mu <- I1$summary.fitted.values[, "mean"]
  phi <- I1$summary.hyperpar[, "mean"]
  vary <- mu^2 /phi
  E1 <- (j$totwgt_sd - mu / sqrt(vary))
  
  j$E1 <- E1

  myvar <- c("yr", "lat.1", "lon.1")
  
#Plot the pearson residuals
MyMultipanel.ggp2 (Z = j,
                     varx = myvar,
                     vary = 'E1',
                     ylab = 'Pearson residuals',
                     addSmoother = T,
                     addRegressionLine = F,
                     addHorizontalLine = T)
  
#Create the Variogram 
mydata <- data.frame(E1, j$lat.1/1000, j$lon.1/1000)
coordinates(mydata) <- c('j.lat.1.1000', 'j.lon.1.1000')
vario <- variogram(object = E1 ~ 1, data=mydata, cressie = T, cutoff = 10, width=  0.09)
#Plot the variogram
p <- ggplot(data = vario, aes(x=dist, y=gamma)) +
  geom_point() +
  geom_smooth(method= 'gam', formula = y ~ s(x, bs = 'cs'), colour = 'black') +
  ylim(0,100000) +
  theme(text = element_text(size = 15)) +
  xlab('Distance(km)')+
  ylab('Sample variogram')

p

  
  

 
}
