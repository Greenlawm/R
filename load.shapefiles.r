load.shapefiles = function(){
    #Import coastline
  setwd(p$polydir)
  coast<-readOGR(".", "NY_to_Nova_UTM20")
  coast<-spTransform(coast, p$geog.proj)
  coast <- gSimplify(coast, tol=0.01, topologyPreserve=TRUE)
  print("Imported Coastline")
  
  #Import Groundfish Survey
  setwd(p$stratadir)
  strata.shp <- readOGR (".", "MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83")
  strata.shp <- spTransform(strata.shp, p$geog.proj)
  strata.shp <- fortify (strata.shp, region = "StrataID")
  print("Imported Groundfish Survey")
  
  save(strata.shp, coast, file="shapefiles")
  
  #load(file="shapefiles")
  
}