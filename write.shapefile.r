write.shapefile = function(x, name=name, p){
  
  #Write Shapefile of Fisheries Data
  fd <- x
  fd = fd[is.finite(fd$lat),]
  #fd$date.landed <- as.character(fd$date.landed)
  fd.cords <- fd[, c("lon", "lat")]
  
  sfd <- SpatialPointsDataFrame(fd.cords, data=fd)
  proj4string(sfd) <-p$geog.proj
  setwd(p$shpdir)
  writeOGR(sfd, ".", name, driver="ESRI Shapefile", overwrite=T)
  
}