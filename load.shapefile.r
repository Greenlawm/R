load.shapefile = function(DS){
  p = setup.parameters()
    if (DS %in% c( "lfas" ) ) {
      setwd(p$polydir)
      s.polygon <- readOGR(".", "LFA_NAFO")
      print("Imported LFAs")

    }
    if (DS %in% c( "coastline" ) ) {
      setwd(p$polydir)
      coast<-readOGR(".", "NY_to_Nova_UTM20")
      coast<-spTransform(coast, p$geog.proj)
      s.polygon <- gSimplify(coast, tol=0.01, topologyPreserve = TRUE)
      print("Imported Coastline")
    }
  if (DS %in% c( "groundfish.survey" ) ) {
    setwd(p$stratadir)
    strata.shp <- readOGR (".", "MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83")
    strata.shp <- spTransform(strata.shp, p$geog.proj)
    s.polygon <- fortify (strata.shp, region = "StrataID")
    print("Imported Groundfish Survey")
    }
  if (DS %in% c( "contours" ) ) {
    setwd(p$polydir)
    contours<-readOGR(".", "contours")
    contours<-spTransform(contours, p$geog.proj)
    s.polygon <- gSimplify(contours, tol=0.01, topologyPreserve = TRUE)
    print("Imported Coastline")
    }
  if (DS %in% c( "sea.cucumber.fa" ) ) {
    setwd(p$polydir)
    sc.fa<-readOGR(".", "sea_cucumber_fa")
    sc.fa<-spTransform(sc.fa, p$geog.proj)
    s.polygon <- sc.fa
    print("Imported Sea Cucumber Fishing Areas")
  }
  if (DS %in% c( "sea.cucumber.old" ) ) {
    setwd(p$polydir)
    sc.fa<-readOGR(".", "sea_cucumber_old")
    sc.fa<-spTransform(sc.fa, p$geog.proj)
    s.polygon <- sc.fa
    print("Imported Old Sea Cucumber Fishing Areas")
  }
  if (DS %in% c( "sea.cucumber.older" ) ) {
    setwd(p$polydir)
    sc.fa<-readOGR(".", "sea_cucumber_older")
    sc.fa<-spTransform(sc.fa, p$geog.proj)
    s.polygon <- sc.fa
    print("Imported Very Old Sea Cucumber Fishing Areas")
  }
  if (DS %in% c( "whelk.fa" ) ) {
    setwd(p$polydir)
    fa<-readOGR(".", "whelk_fa")
    fa<-spTransform(fa, p$geog.proj)
    s.polygon <- fa
    print("Imported Whelk Fishing Areas")
  }
  if (DS %in% c( "nafo" ) ) {
    setwd(p$polydir)
    nafo<-readOGR(".", "nafo_mr")
    s.polygon<-spTransform(nafo, p$geog.proj)
    #s.polygon <- gSimplify(nafo, tol=0.01, topologyPreserve = TRUE)
    print("Imported NAFO Areas")
  }
  if (DS %in% c( "sc.habitat.4Vs" ) ) {
    setwd(p$polydir)
    hab<-readOGR(".", "sea_cucumber_habitat_4Vs1")
    s.polygon <-spTransform(hab, p$geog.proj)
    print("Imported Sea Cucumber Habitat")
  }
  if (DS %in% c( "sc.habitat.4W" ) ) {
    setwd(p$polydir)
    hab<-readOGR(".", "sea_cucumber_habitat_4W1")
    s.polygon <-spTransform(hab, p$geog.proj)
    print("Imported Sea Cucumber Habitat")
  }
  if (DS %in% c( "hagfish.z" ) ) {
    setwd(p$polydir)
    hab<-readOGR(".", "hagfish_zones")
    s.polygon <-spTransform(hab, p$geog.proj)
    print("Imported Hagfish Zones")
  }
  if (DS %in% c( "coastline.large" ) ) {
    setwd(p$polydir)
    coast<-readOGR(".", "NY_to_Nova_UTM20")
    s.polygon<-spTransform(coast, p$geog.proj)
    print("Imported Coastline")
  }
  if (DS %in% c( "coastline.medium" ) ) {
    setwd(p$polydir)
    coast<-readOGR(".", "NY_to_Nova_UTM20")
    coast<-spTransform(coast, p$geog.proj)
    s.polygon <- gSimplify(coast, tol=0.0004, topologyPreserve = TRUE)
    print("Imported Coastline Medium Size")
  }
  
 return(s.polygon)
  }
