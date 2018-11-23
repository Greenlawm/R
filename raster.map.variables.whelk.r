
#Function to summarize and map raster variables
#Cell size can also be adjusted to change the default cell size that is mapped, default = 6km

	raster.map.variables.whelk = function(p=p, DS, y, yrs) {
	  #browser()

	  p = setup.parameters()
	  detach('package:ggplot2')
	  coast<-load.shapefile(DS = 'coastline')
	  contours <- load.shapefile(DS = 'contours')
	  nafo <- load.shapefile(DS= 'nafo')

	  grid.fun =  sum
	  grid.fun2 = mean

	  K = whelk.logbook.db( DS="logbook", p=p, yrs= yrs )
	  e = whelk.logbook.db(DS ="effort", p=p, yrs = yrs)

	  #Designate Cell size
	  # 1 minute grid = 0.0166, 2 minute grid = 0.033, 3minute grid = 0.05, 4 minute grid = 0.0666, 5 minute grid = 0.083
	  cell<- 0.0666
	  cell.big <- 0.083

	  K$sa = 1  # this a dummy variable required by the mapping routine
	  K$lon <- as.numeric(K$lon)
	  K$lat <- as.numeric(K$lat)
	  
	  e$sa = 1
	  e$lon <- as.numeric(e$lon)
	  e$lat <- as.numeric(e$lat)
	  
	  #Write Shapefile of Fisheries Data
	  fd <- K
	  fd$date.landed <- as.character(fd$date.fished)

	  fd = fd[!is.na(fd$lat),]
	  fd.cords <- fd[, c("lon", "lat")]
	  sfd <- SpatialPointsDataFrame(fd.cords, data=fd)
	  proj4string(sfd) <- p$geog.proj
	  setwd(p$shpdir)
	  writeOGR(sfd, ".", "WhelkFisheriesDataUpdate", driver="ESRI Shapefile", overwrite=T)

		#Set rows and columns for blank grid
	  extent = p$extsc
		ncols <- round((length(extent[2]:extent[1])/cell) , 0)
		nrows <- round(((length(extent[4]:extent[3])-1)/cell), 0)

		#Set rows and columns for the raster stack. Larger cell size because of smaller plots
		ncols.big <- length(extent[2]:extent[1])/cell.big
		nrows.big <- (length(extent[2]:extent[1])-1)/cell.big

		#Create the color palette for each variable
		setwd(p$shpdir)
		
		#Create a blank grid for plotting
		grid <- raster(nrows=nrows, ncols=ncols, ext=extent, crs=p$geog.proj)
		grid[]<- 1:ncell(grid)
		
		grid.big <- raster(nrows=nrows.big, ncols=ncols.big, ext=extent, crs=p$geog.proj)
		grid.big[]<- 1:ncell(grid.big)
		
		if (DS %in% c("landings")) {
		  
		  v= "landings"
			#Extract data for the raster creation
			M = K[, c("yr", "lon", "lat", v)]
			M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
			#convert landings from kg to mt
			M$landings = M$landings * 0.001

			rstack <- raster::stack()

			#--------------------------------------------------------------------------------
			  #Loop through and create a raster-based plot for each year
			for (i in 1:length(sort(unique(M$yr)))) {
				y= sort(unique(M$yr))
				name<- y[i]
				print(name)
				s= subset(M, M$yr==name)

				#Extract coordinates and create spatial points data frame to be gridded
				cor = s[, c("lon", "lat")]
				sPDF <- SpatialPointsDataFrame (cor, data=s)
				proj4string(sPDF) <- (p$geog.proj)
				spatial.name <- paste("whelk", v, name, sep="")
				writeOGR(sPDF, ".", spatial.name, driver="ESRI Shapefile", overwrite=T)

				#Create Rasters
				ras.name <- paste("whelk", v, name, ".tif", sep="")

				grid.sum <- rasterize(x=sPDF, y = grid, field = v, fun = grid.fun)
				grid.sum.big <- rasterize(x = sPDF, y = grid.big, field = v, fun = grid.fun)

				setwd(p$rasdir)
				writeRaster(grid.sum, filename = ras.name, datatype = 'GTiff', overwrite = T)

				#Create raster stack to calculate the max and min values for the colour scale
				rstack <- raster::stack(rstack, grid.sum)
				names(rstack[[i]]) <- name

				#Define colour scale
				z <- getValues(rstack)
				z <- z[is.finite(z)]
				#z <- round(z, digits = 0)
				
				#quant <- unique(quantile(z, seq(0,1, length.out = 75)))
				#quant.small <- unique(quantile(z, seq(0,1, length.out = 5)))
				#ckey <- list(at = quant, labels = list(at = quant.small))
				
				quant <- unique(quantile(z, seq(0,1, length.out=45)))
				quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = 0)
				my.brks = seq(0, max(z), by= (max(z)/13))
				s.brks = seq(0, max(z), by= (max(z)/45))
				ckey <- list(at=s.brks, labels=list(at=my.brks, labels=quant.labels))
				
				#Plot the variable
				setwd(p$mapdir)
			  fig.name <- paste("whelk", v, name, ".pdf", sep = "")
				pdf(file = fig.name, width = 6.5, height = 4.8, bg='white')

			  print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
				                margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
				               + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
    	                 + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
				               + layer(sp.polygons(nafo, col='grey', alpha = 0.7))
				)
				dev.off()
				
				p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
				               margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
				           + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
				           + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
				           + layer(sp.polygons(nafo, col='grey', alpha=0.7))
				)
				
			  }
		}
		
		if (DS %in% c("landings.year")) {
		  
		  v= "landings"
		  #Extract data for the raster creation
		  M = K[, c("yr", "lon", "lat", v)]
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
		  #convert landings from kg to mt
		  M$landings = M$landings * 0.001
		  
		  rstack <- raster::stack()
		  
		  #--------------------------------------------------------------------------------
		  #Loop through and create a raster-based plot for each year
		    y= y
		    name<- y
		    print(name)
		    s= subset(M, M$yr==name)
		    
		    #Extract coordinates and create spatial points data frame to be gridded
		    cor = s[, c("lon", "lat")]
		    sPDF <- SpatialPointsDataFrame (cor, data=s)
		    proj4string(sPDF) <- (p$geog.proj)
		    spatial.name <- paste("whelk", v, name, sep="")
		    writeOGR(sPDF, ".", spatial.name, driver="ESRI Shapefile", overwrite=T)
		    
		    #Create Rasters
		    ras.name <- paste("whelk", v, name, ".tif", sep="")
		    
		    grid.sum <- rasterize(x=sPDF, y = grid, field = v, fun = grid.fun)
		    grid.sum.big <- rasterize(x = sPDF, y = grid.big, field = v, fun = grid.fun)
		    
		    setwd(p$rasdir)
		    writeRaster(grid.sum, filename = ras.name, datatype = 'GTiff', overwrite = T)
		    
		    #Create raster stack to calculate the max and min values for the colour scale
		    rstack <- raster::stack(rstack, grid.sum)

		    #Define colour scale
		    z <- getValues(rstack)
		    z <- z[is.finite(z)]
		    #z <- round(z, digits = 0)
		    

		    quant <- unique(quantile(z, seq(0,1, length.out=45)))
		    #quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = 0)
		    quant.labels <-c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)
		    
		    my.brks = seq(0, max(z), by= (max(z)/13))
		    s.brks = seq(0, max(z), by= (max(z)/45))
		    ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
		    
		    #Plot the variable
		    setwd(p$mapdir)
		    fig.name <- paste("whelk", v, name, ".pdf", sep = "")
		    pdf(file = fig.name, width = 6.5, height = 4.8, bg='white')
		    
		    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                    margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
		          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
		          + layer(sp.polygons(nafo, col='grey', alpha = 0.7))
		          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		    )
		    dev.off()
		    
		    p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                         margin=F, xlab="", ylab="", main='', scales = list(x=list(cex=0.7), y=list(cex=0.7))) +
		                 layer(sp.polygons(coast, fill='lightgrey')) +
		                 layer(sp.lines(contours, col='dimgrey', alpha=0.4, lwd= 0.4)) +
		                 layer(sp.polygons(nafo, fill='grey', alpha=0.7))
		    )
		    
		  }
		
		
			
			#-----------------------------------------------------------------------
			#Fishery Footprint
		if (DS %in% c("fishery.footprint")) {
			 
		  v = 'landings'
		  M = K[, c("yr", "lon", "lat", v)]
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
		  M$landings = M$landings * 0.001
		  
		  
		  cor = M[, c("lon", "lat")]
			sPDF <- SpatialPointsDataFrame (cor, data=M)
			proj4string(sPDF)<-(p$geog.proj)
			spatial.name <- paste("whelk", "footprint", sep="")

			#Create Rasters
			ras.name <- paste("whelk", "footprint", ".tif", sep="")
			
			grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun=grid.fun)
			grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=grid.fun)
			
			setwd(p$rasdir)
			writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)
			
			#Create raster stack to calculate the max and min values for the colour scale
			rstack <- raster::stack()
			rstack <- raster::stack(rstack, grid.sum)

			#Define colour scale
			z <- getValues(rstack)
			z <- z[is.finite(z)]
			z <- round(z, digits=3)

			quant <- unique(quantile(z, seq(0,1, length.out=45)))
			#quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = 0)
			quant.labels <-c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
			
			my.brks = seq(0, max(z), by= (max(z)/13))
			s.brks = seq(0, max(z), by= (max(z)/45))
			ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
			
			#Plot the variable
			setwd(p$mapdir)
			fig.name <- paste("whelk", "footprint", ".pdf", sep="")
			#pdf(file=fig.name, width=6.5, height=4.8, bg='white')
			
			print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
			                margin=F, xlab="", ylab="", main = 'Fishery Footprint', scales = list(x=list(cex=0.7), y=list(cex=0.7)))
			      + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
			      + layer(sp.polygons(nafo, col='grey', alpha=0.7))
            + layer(sp.polygons(coast, fill='lightgrey'))
			)

			dev.off()
			
			p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
			                     margin=F, xlab="", ylab="", main='', scales = list(x=list(cex=0.7), y=list(cex=0.7))) +
			             layer(sp.polygons(coast, fill='lightgrey')) +
			             layer(sp.lines(contours, col='dimgrey', alpha=0.4, lwd= 0.4)) +
			             layer(sp.polygons(nafo, fill='grey', alpha=0.7))
			)
			nafo.4vs = nafo[nafo$ZONE == '4vS']
			grid.sum.4vs = crop(grid.sum, nafo.4vs)
		  
			}
	
			#-----------------------------------------------------------------------
	if (DS %in% c("cpue")) {
	  
	  v= "cpue" 
	  #Extract data for the raster creation
	  M = K[, c("yr", "lon", "lat", v)]
	  r = nrow(M)
	  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
	  n = nrow(M)
	  print(paste(r - n, "records are missing lat/long or cpue", sep = " "))
	  rstack <- raster::stack()
	  
	  
			#Loop through and create a raster-based plot for each year for cpue
			for (i in 1:length(sort(unique(M$yr)))) {
			  y = sort(unique(M$yr))
			  y = y[y > 2009]
			  name <- y[i]
			  print(name)
			  s = subset(M, M$yr == name)

			  #Extract coordinates and create spatial points data frame to be gridded
			  cor = s[, c("lon", "lat")]
			  sPDF <- SpatialPointsDataFrame (cor, data=s)
			  proj4string(sPDF)<-(p$geog.proj)
			  spatial.name <- paste("whelk", v, name, sep="")
			  writeOGR(sPDF, ".", spatial.name, driver = "ESRI Shapefile", overwrite = T)

			  #Create Rasters
			  ras.name <- paste("whelk", v, name, ".tif", sep="")

			  grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun= function(x, ...){mean(x)})
			  grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=function(x, ...){mean(x)})

			  setwd(p$rasdir)
			  writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)

			  #Create raster stack to calculate the max and min values for the colour scale
			  rstack <- raster::stack(rstack, grid.sum)
			  names(rstack[[i]]) <- name

			  #Define colour scale
			  z <- getValues(rstack)
			  z <- z[is.finite(z)]
			  z <- round(z, digits=6)

			  quant <- unique(quantile(z, seq(0,1, length.out=45)))
			  quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = 0)
			  my.brks = seq(0, max(z), by= (max(z)/13))
			  s.brks = seq(0, max(z), by= (max(z)/45))
			  ckey <- list(at=s.brks, labels=list(at=my.brks, labels=quant.labels))

			  #Plot the variable
			  setwd(p$mapdir)
			  fig.name <- paste("whelk", "cpue", name, ".pdf", sep="")
			  pdf(file=fig.name, width=6.5, height=4.8, bg='white')

			  print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
			                  margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
			        + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
			        + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
			        + layer(sp.polygons(nafo, col='grey', alpha=0.7))
			        
			  )


			  dev.off()
			  p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
			                       margin=F, xlab="", ylab="", main='', scales = list(x=list(cex=0.7), y=list(cex=0.7))) +
			               layer(sp.polygons(coast, fill='lightgrey')) +
			               layer(sp.lines(contours, col='dimgrey', alpha=0.4, lwd= 0.4)) +
			               layer(sp.polygons(nafo, fill='grey', alpha=0.7))
			  )
			}
	}

		#-----------------------------------------------------
		if (DS %in% c("cpue.year")) {
		  
		  v= "cpue" 
		  #Extract data for the raster creation
		  M = K[, c("yr", "lon", "lat", v)]
		  r = nrow(M)
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
		  n = nrow(M)
		  print(paste(r - n, "records are missing lat/long or cpue", sep = " "))
		  rstack <- raster::stack()
		  
	    y = y
	    name <- y
	    print(name)
	    s = subset(M, M$yr == name)
	    
	    #Extract coordinates and create spatial points data frame to be gridded
	    cor = s[, c("lon", "lat")]
	    sPDF <- SpatialPointsDataFrame (cor, data=s)
	    proj4string(sPDF)<-(p$geog.proj)
	    spatial.name <- paste("whelk", v, name, sep="")
	    writeOGR(sPDF, ".", spatial.name, driver = "ESRI Shapefile", overwrite = T)
	    
	    #Create Rasters
	    ras.name <- paste("whelk", v, name, ".tif", sep="")
	    
	    grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun= function(x, ...){mean(x)})
	    grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=function(x, ...){mean(x)})
	    
	    setwd(p$rasdir)
	    writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)
	    
	    #Create raster stack to calculate the max and min values for the colour scale
	    rstack <- raster::stack(rstack, grid.sum)

	    #Define colour scale
	    z <- getValues(rstack)
	    z <- z[is.finite(z)]
	    z <- round(z, digits=6)
	    
	    quant <- unique(quantile(z, seq(0,1, length.out=45)))
	    #quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = 0)
	    quant.labels <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26)
	    my.brks = seq(0, max(z), by= (max(z)/13))
	    s.brks = seq(0, max(z), by= (max(z)/45))
	    ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
	    
	    #Plot the variable
	    setwd(p$mapdir)
	    fig.name <- paste("whelk", "cpue", name, ".pdf", sep="")
	    pdf(file=fig.name, width=6.5, height=4.8, bg='white')
	    
	    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
	                    margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
	          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
	          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
	          + layer(sp.polygons(nafo, col='grey', alpha=0.7))
	          
	    )
	    
	    
	    dev.off()
	    
	    p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
	                         margin=F, xlab="", ylab="", main='', scales = list(x=list(cex=0.7), y=list(cex=0.7))) +
	                 layer(sp.polygons(coast, fill='lightgrey')) +
	                 layer(sp.lines(contours, col='dimgrey', alpha=0.4, lwd= 0.4)) +
	                 layer(sp.polygons(nafo, fill='grey', alpha=0.7))
	    )
	
	    
		    
		    
		  }

		#-----------------------------------------------------------
		if (DS %in% c("effort.year")) {
		  
		  v = 'trap_set'
		  M = e[, c("yr", "lon", "lat", v)]
		  
		  #assume that all NAs in traps set were 50 traps 
		  #(11  15  20  25  28  40  45  50  55  60  66  75  80 120 160 200)
		  #( 1  50   2  48   1 166  19 790 474   7   1   5   3   4   3   1)
		  M$trap_set[(is.na(M$trap_set))] <- 50
		  
		  r = nrow(M)
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
		  n = nrow(M)
		  print(paste(r - n, "records are missing lat/long or cpue", sep = " "))
		  rstack <- raster::stack()
		  
		  y = y
		  name <- y
		  print(name)
		  s = subset(M, M$yr == name)
		  
		  #Extract coordinates and create spatial points data frame to be gridded
		  cor = s[, c("lon", "lat")]
		  sPDF <- SpatialPointsDataFrame (cor, data=s)
		  proj4string(sPDF)<-(p$geog.proj)
		  spatial.name <- paste("whelk", v, name, sep="")
		  writeOGR(sPDF, ".", spatial.name, driver = "ESRI Shapefile", overwrite = T)
		  
		  #Create Rasters
		  ras.name <- paste("whelk", v, name, ".tif", sep="")
		  
		  grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun= function(x, ...){mean(x)})
		  grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=function(x, ...){mean(x)})
		  
		  setwd(p$rasdir)
		  writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)
		  
		  #Create raster stack to calculate the max and min values for the colour scale
		  rstack <- raster::stack(rstack, grid.sum)
		  
		  #Define colour scale
		  z <- getValues(rstack)
		  z <- z[is.finite(z)]
		  z <- round(z, digits=6)
		  
		  quant <- unique(quantile(z, seq(0,1, length.out=45)))
		  quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = 0)
		  #quant.labels <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26)
		  my.brks = seq(0, max(z), by= (max(z)/13))
		  s.brks = seq(0, max(z), by= (max(z)/45))
		  ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
		  
		  #Plot the variable
		  setwd(p$mapdir)
		  fig.name <- paste("whelk", "cpue", name, ".pdf", sep="")
		  pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		  
		  print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                  margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
		        + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
		        + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		        + layer(sp.polygons(nafo, col='grey', alpha=0.7))
		        
		  )
		  
		  
		  dev.off()
		  
		  p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                       margin=F, xlab="", ylab="", main='', scales = list(x=list(cex=0.7), y=list(cex=0.7))) +
		               layer(sp.polygons(coast, fill='lightgrey')) +
		               layer(sp.lines(contours, col='dimgrey', alpha=0.4, lwd= 0.4)) +
		               layer(sp.polygons(nafo, fill='grey', alpha=0.7))
		  )
		  
		  
		  
		  
		}
			#-----------------------------------------------------------------
	if (DS %in% c("cpue.allyears")) {
	    v = 'cpue'
	  
			#create a raster-based plot for all years for cpue
	    M = K[, c("yr", "lon", "lat", v)]
	    M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]	 
	    s= M
			rstack <- raster::stack()

			#Extract coordinates and create spatial points data frame to be gridded
			cor = s[, c("lon", "lat")]
			sPDF <- SpatialPointsDataFrame (cor, data=s)
			proj4string(sPDF)<-(p$geog.proj)
			spatial.name <- paste(v, 'allyears', sep="")
			writeOGR(sPDF, ".", spatial.name, driver="ESRI Shapefile", overwrite=T)

			#Create Rasters
			ras.name <- paste("whelk", "cpue.allyears", ".tif", sep="")
        
		  #Why was I dividing by 50 for the mean???????
		  grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun= function(x, ...){mean(x)})
		  grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=function(x, ...){mean(x)})

		  setwd(p$rasdir)
		  writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)

		  #Create raster stack to calculate the max and min values for the colour scale
		  rstack <- raster::stack(rstack, grid.sum)

		  #Define colour scale
		  z <- getValues(rstack)
		  z <- z[is.finite(z)]
		  z <- round(z, digits=6)

		  quant <- unique(quantile(z, seq(0,1, length.out=45)))
		  #quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = 0)
		  quant.labels <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26)
		  my.brks = seq(0, max(z), by= (max(z)/13))
		  s.brks = seq(0, max(z), by= (max(z)/45))
		  ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
		  
		  #Plot the variable
		  setwd(p$mapdir)
		  fig.name <- paste("whelk", "cpue.allyears", ".pdf", sep="")
		  #pdf(file=fig.name, width=6.5, height=4.8, bg='white')

		 p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                  margin=F, xlab="", ylab="", main='', scales = list(x=list(cex=0.7), y=list(cex=0.7))) +
		         layer(sp.polygons(coast, fill='lightgrey')) +
		         layer(sp.lines(contours, col='dimgrey', alpha=0.4, lwd= 0.4)) +
		         layer(sp.polygons(nafo, fill='grey', alpha=0.7))
		  )
		  #dev.off()
		 
		 #Calculate the Average Catch Per Unit effort over space
		 nafo.4vs = nafo[nafo$ZONE == '4Vs', ]
		 grid.sum.4vs = crop(grid.sum, nafo.4vs)
		 raster::plot(grid.sum.4vs)
		 avg.cpue = getValues(grid.sum.4vs)
		 avg.cpue = mean(avg.cpue[!is.na(avg.cpue)])
		 
		 nafo.4w = nafo[nafo$ZONE == '4W', ]
		 grid.sum.4w = crop(grid.sum, nafo.4w)
		 raster::plot(grid.sum.4w)
		 avg.cpue.4w = getValues(grid.sum.4w)
		 avg.cpue.4w = mean(avg.cpue.4w[!is.na(avg.cpue.4w)])
		 	}

		#----------------------------------------------------------------------------
		if (DS %in% c("cpue.stack")) {
		  
	  v= "cpue" 
	  #Extract data for the raster creation
	  M = K[, c("yr", "lon", "lat", v)]
	  r = nrow(M)
	  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
	  n = nrow(M)
	  print(paste(r - n, "records are missing lat/long or cpue", sep = " "))
	  rstack <- raster::stack()
		stack.name<- paste("whelk", "cpue", "stack", ".pdf", sep="")
		pdf(file=stack.name, width=8, height=9, bg='white')
 		par(mar=c(1,1,1,1))
 		par(oma=c(0,0,0,0))
		max.plot <- length(sort(unique(M$yr)))
		min.plot <- max.plot - 14
		main <- y
		print(levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
			margin=F, xlab="", ylab="", par.strip.text=list(cex=0.7), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5)))
			+ layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
			+ layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
			+ layer(sp.polygons(nafo, col='grey', alpha=0.5, labels = zone))
			
		)
		dev.off()

		to.print <- paste(v, "completed", sep=" ")
		print(to.print)
		
		p1 = levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		          margin=F, xlab="", ylab="", par.strip.text=list(cex=0.7), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5)))
		+ layer(sp.polygons(coast, fill='lightgrey'))
		+ layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		+ layer(sp.polygons(nafo, col='grey', alpha=0.5))
		

		}
		
		#-----------------------------------------------------------------------
		#Effort Footprint
		if (DS %in% c("effort.footprint")) {
		  
		  v = 'trap_set'
		  M = e[, c("yr", "lon", "lat", v)]
		  
		  #assume that all NAs in traps set were 50 traps 
		  #(11  15  20  25  28  40  45  50  55  60  66  75  80 120 160 200)
		  #( 1  50   2  48   1 166  19 790 474   7   1   5   3   4   3   1)
		  M$trap_set[(is.na(M$trap_set))] <- 50
		  
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]

		  cor = M[, c("lon", "lat")]
		  sPDF <- SpatialPointsDataFrame (cor, data=M)
		  proj4string(sPDF)<-(p$geog.proj)
		  spatial.name <- paste("whelk", "effort.footprint", sep="")
		  
		  #Create Rasters
		  ras.name <- paste("whelk", "effort.footprint", ".tif", sep="")
		  
		  grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun=grid.fun)

		  setwd(p$rasdir)
		  writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)
		  
		  #Create raster stack to calculate the max and min values for the colour scale
		  rstack <- raster::stack()
		  rstack <- raster::stack(rstack, grid.sum)
		  
		  #Define colour scale
		  z <- getValues(rstack)
		  z <- z[is.finite(z)]
		  z <- round(z, digits=0)
		  
		  quant <- unique(quantile(z, seq(0,1, length.out=45)))
		  quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = -2)

		  my.brks = seq(0, max(z), by= (max(z)/13))
		  s.brks = seq(0, max(z), by= (max(z)/45))
		  ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
		  
		  #Plot the variable
		  setwd(p$mapdir)
		  fig.name <- paste("whelk", "effort.footprint", ".pdf", sep="")
		  #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		  
		  print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                  margin=F, xlab="", ylab="", main = 'Effort Footprint', scales = list(x=list(cex=0.7), y=list(cex=0.7)))
		        + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		        + layer(sp.polygons(nafo, col='grey', alpha=0.7))
		        + layer(sp.polygons(coast, fill='lightgrey'))
		  )
		  
		  dev.off()
		  
		  p1 = print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                       margin=F, xlab="", ylab="", main='', scales = list(x=list(cex=0.7), y=list(cex=0.7))) +
		               layer(sp.polygons(coast, fill='lightgrey')) +
		               layer(sp.lines(contours, col='dimgrey', alpha=0.4, lwd= 0.4)) +
		               layer(sp.polygons(nafo, fill='grey', alpha=0.7))
		  )
		  nafo.4vs = nafo[nafo$ZONE == '4vS']
		  grid.sum.4vs = crop(grid.sum, nafo.4vs)
		  
		}
		
    require('ggplot2')
		return(p1)
	}
