setup.parameters = function(){
  #browser()
  p=list()
  p$data.yrs = 2000:2018

  p$polydir = file.path(ss.exdatadirectory, "spatial")
  p$datadir = file.path(ss.exdatadirectory, "data")
  p$stratadir = file.path(ss.exdatadirectory, "spatial", "survey")
  p$shpdir = file.path(ss.exdatadirectory, "output", "shapefiles")
  p$rasdir = file.path(ss.exdatadirectory, "output", "rasters")
  p$mapdir = file.path("C:", "ss", "inst", "output", "images")

  #p$ext = raster::extent(matrix(c(-68, 41, -58, 47.4), nrow=2, ncol=2))
  p$ext = extent(matrix(c(-68, 41, -59, 44), nrow=2, ncol=2))
  p$ext2 = extent(matrix(c(-68, 41, -63, 44), nrow=2, ncol=2))
  p$extsc = extent(matrix(c(-67.5, 42, -57, 47), nrow=2, ncol=2))
  p$extsc.swnb = extent(matrix(c(-67, 45, -66.87, 45.1), nrow=2, ncol=2))
  p$extsc.offshore = extent(matrix(c(-61.5, 43.7, -58, 45.3), nrow=2, ncol=2))
  
  p$ext.whelk.4V = extent(matrix(c(-59, 44, -57, 45), nrow=2, ncol=2))
  p$ext.inshorejonah = extent(matrix(c(-68, 42.5, -60, 46), nrow=2, ncol=2))
  p$ext.hagfish = extent(matrix(c(-67.5, 41.5, -57, 46.5), nrow=2, ncol=2))
  p$ext.urchin = extent(matrix(c(-67.5, 43, -59, 46.5), nrow=2, ncol=2))
  
  
  
  p$extlat = c(41, 47.4)
  p$extlong = c(-68.4, -58)

  cell=NULL
  p$internal.crs <- "+proj=utm +zone=20 ellps=WGS84"
  p$geog.proj <- sp::CRS("+proj=longlat +ellps=WGS84")
  p$seis <- colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")
  p$pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(100)

  p$line <- brewer.pal(n=10, name = 'Paired')

  #x = x [filter.region.polygon( x, region="isobath1000m"),]

  ## Create Plot ##
  # setting shape type and size for geom_point
  p$shapes <- c( 3, 16, 16, 16, 16)
  p$sizes <- c (2.5, 2, 3, 4, 5)

  #Designate Cell size
  if(is.null(cell)) {
    # 1 minute grid = 0.166, 2 minute grid = 0.033, 3minute grid = 0.05, 4 minute grid = 0.0666, 5 minute grid = 0.083
    p$cell<- 0.0666
    p$cell.big <- 0.083
  }
  return(p)
 }
