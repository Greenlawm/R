intersect.sp.groundfish = function(x=x, y=y){
 #browser()
  # x = table and y = shapefile
  p = setup.parameters()
  
  fd <- x
  fd = fd[!is.na(fd$lat),]
  fd = fd[!is.na(fd$lon),]
  fd.cords <- fd[, c("lon", "lat")]
  sfd <- SpatialPointsDataFrame(fd.cords, data=fd, proj4string = p$geog.proj)
  
  sfp <- y
  
  sfp <- spTransform(sfp, p$geog.proj)
    o= point.in.poly(sfd, sfp)
  names( o ) = tolower( names( o ) )
    to.extract = c( "id", "strat", "yr","lat","lon", "lfa_nafo", "zone", "zone_1")
  
  o = o[, to.extract]
  o= as.data.frame(o)
}
