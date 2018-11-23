project.UTM = function(j){
  rv.utm <- project(as.matrix(j[c("lon", "lat")]), "+proj=utm +zone=20 ellps=WGS84")
  j <- cbind(rv.utm, j)
  names(j)[1] <- "lonutm"
  names(j)[2] <- "latutm"
  return (j)
}