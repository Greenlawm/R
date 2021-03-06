intersect.sp = function(x=x, y=y, var = var ){
  #browser()
  fd <- x
  to.extract = colnames(fd)
  length = ncol(fd)
  length = length + 1
  fd = fd[!is.na(fd$lat),]
  fd = fd[!is.na(fd$lon),]
  fd.cords <- fd[, c("lon", "lat")]
  sfd <- SpatialPointsDataFrame(fd.cords, data=fd, proj4string = p$geog.proj)
  
  sfp <- y
  
  sfp <- spTransform(sfp, p$geog.proj)
  o= point.in.poly(sfd, sfp)
  names( o ) = tolower( names( o ) )
    
  to.extract[[length]] <- var
  
  o = o[, to.extract]
  o= as.data.frame(o)

}
