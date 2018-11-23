
#Function to summarize and map raster variables
#Cell size can also be adjusted to change the default cell size that is mapped, default = 6km

	raster.map.variables.hagfish = function(p=p, DS, cell=NULL) {
	  #browser()
	  #look to see if all the gear codes are barrels, and if there is any bycatch from other fisheries in the data
	  p = setup.parameters()
	  extent = p$ext.hagfish
	  require('ggplot2')
	  detach('package:ggplot2')
	  
	  #Set Grid Size
	  # 1 minute grid = 0.0166, 2 minute grid = 0.033, 3minute grid = 0.05, 4 minute grid = 0.0666, 5 minute grid = 0.083
	  #0.1
	  cell<- 0.06
	  cell.big <- 0.18
	  #cell.big <- 0.05
	  
	  #Import Shapefilescoastline
	  coast<-load.shapefile(DS = 'coastline')
	  contours <- load.shapefile(DS = 'contours')
	  nafo <- load.shapefile(DS = 'nafo')

	  grid.fun= sum
	  K = process.hagfish.logbook()
	  K = intersect.sp(K, nafo, var='zone')

	  l.n = K
	  l.n = subset(l.n, is.na(zone)|zone == '4Vn' | zone == '4Vs' | zone == '4W'| zone == '5Ze'| zone == '4X')
	  c=l.n
	  
	  K$sa = 1  # this a dummy variable required by the mapping routine
	  K$lon <- as.numeric(K$lon)
	  K$lat <- as.numeric(K$lat)
	  
	  c$sa = 1  # this a dummy variable required by the mapping routine
	  c$lon <- as.numeric(c$lon)
	  c$lat <- as.numeric(c$lat)

	  #this part needs to be fixed to map c
	  #Write Shapefile of Fisheries Data
	  #fd <- K
	  #fd$date.landed <- as.character(fd$date.fished)

	  #fd = fd[!is.na(fd$lat),]
	  #fd.cords <- fd[, c("lon", "lat")]
	  #sfd <- SpatialPointsDataFrame(fd.cords, data=fd)
	  #proj4string(sfd) <- p$geog.proj
	  #setwd(p$shpdir)
	  #writeOGR(sfd, ".", "HafishFisheriesDataUpdate", driver="ESRI Shapefile", overwrite=T)

		#Set rows and columns for blank grid
		ncols <- length(extent[2]:extent[1])/cell
		nrows <- (length(extent[4]:extent[3])-1)/cell
		ncols.big <- length(extent[2]:extent[1])/cell.big
		nrows.big <- (length(extent[2]:extent[1])-1)/cell.big
		
		setwd(p$shpdir)
		#Create a blank grid for plotting
		grid <- raster(nrows=nrows, ncols=ncols, ext=extent, crs=p$geog.proj)
		grid[]<- 1:ncell(grid)
		grid.big <- raster(nrows=nrows.big, ncols=ncols.big, ext=extent, crs=p$geog.proj)
		grid.big[]<- 1:ncell(grid.big)
		rstack <- raster::stack()
		

		if (DS %in% c("landings.yearly")) {
		  setwd(p$shpdir)
		  v= "landings"
		  grid.fun= sum

		  #Extract data for the raster creation
		  M = K[, c("yr", "lon", "lat", v)]
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
		  
		  #Loop through and create a raster-based plot for each year
		  for (i in 1:length(sort(unique(M$yr)))) {
		    y= sort(unique(M$yr))
		    name<- y[i]
		    print(name)
		    s= subset(M, M$yr==name)
		    
		    #Extract coordinates and create spatial points data frame to be gridded
		    cor = s[, c("lon", "lat")]
		    sPDF <- SpatialPointsDataFrame (cor, data=s)
		    proj4string(sPDF)<-(p$geog.proj)
		    #spatial.name <- paste(v, name, sep="")
		    #writeOGR(sPDF, ".", spatial.name, driver="ESRI Shapefile", overwrite=T)
		    ras.name <- paste("hagfish", v, name, ".tif", sep="")
		    grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun=grid.fun)
		    grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=grid.fun)
		    setwd(p$rasdir)
		    writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)
		    
		    #Create raster stack to calculate the max and min values for the colour scale
		    rstack <- raster::stack(rstack, grid.sum.big)
		    names(rstack[[i]]) <- name
		    #Define colour scale
		    z <- getValues(rstack)
		    z <- z[is.finite(z)]
		    z <- round(z, digits=0)
		    
		    if (v == "landings") {
		      z <- z/100
		      z <- round(z, digits=0)
		      z <- z*100
		    } else {
		      z <- z/10
		      z <- round(z, digits=0)
		      z <- z*10
		    }
		    
		    #MG might want to convert landings /1000 to calculate tons for landings, right now it's using KG
		    quant <- unique(quantile(z, seq(0,1, length.out=45)))
		    quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = -2)
		    
		    my.brks = seq(0, max(z), by= (max(z)/13))
		    s.brks = seq(0, max(z), by= (max(z)/45))
		    ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels, cex=0.5))
		    
		    #Plot the variable	
		    setwd(p$mapdir)
		    #fig.name <- paste(v, name,"hagfish", ".pdf", sep="")
		    fig.name <- paste(v, name,"hagfish", ".png", sep="")
		    
		    png(filename=fig.name, width=6.5, height=5, units="in", res=450)
		    #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		    
		    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                    margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.5), y=list(cex=0.5)))
		          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
		          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4)) 
		          + layer(sp.lines(nafo, col='black', alpha=0.9, lwd= 0.4))
		    )
		    
		    dev.off()
		  }
		  
		  #Plot all the rasters in a stack, seperated into 2 images
		  #stack.name<- paste(v, "stack","hagfish", ".pdf", sep="")
		  stack.name<- paste(v, "stack","hagfish", ".png", sep="")
		  png(filename=stack.name, width=7, height=5, units="in", res=300)
		  #pdf(file=stack.name, width=8, height=9, bg='white')
		  par(mar=c(1,1,1,1))
		  par(oma=c(0,0,0,0))
		  max.plot <- length(sort(unique(M$yr)))
		  min.plot <- max.plot - 14
		  #max.plot2 <- min.plot -1
		  #min.plot2 <- min.plot -4
		  main <- y
		  print(levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                  margin=F, xlab="", ylab="", par.strip.text=list(cex=0.5), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5))) 
		        + layer(sp.polygons(coast, fill='lightgrey')) 
		        + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		        + layer(sp.lines(nafo, col='black', alpha=0.9, lwd= 0.4))
		        
		  )
		  dev.off()
		  
		  to.print <- paste(v, "completed", sep=" ")
		  print(to.print)
		  
		}
		
		if (DS %in% c("effort.yearly")) {
		  setwd(p$shpdir)
		  v= "effort"
		  grid.fun = sum
		  
		  K$effort = K$traps_retrieved

		  #Extract data for the raster creation
		  M = K[, c("yr", "lon", "lat", "effort")]
		  names(M) = c('yr', 'lon', 'lat', v)
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
		  
		  #Loop through and create a raster-based plot for each year
		  for (i in 1:length(sort(unique(M$yr)))) {
		    y= sort(unique(M$yr))
		    name<- y[i]
		    print(name)
		    s= subset(M, M$yr==name)
		    
		    #Extract coordinates and create spatial points data frame to be gridded
		    cor = s[, c("lon", "lat")]
		    sPDF <- SpatialPointsDataFrame (cor, data=s)
		    proj4string(sPDF)<-(p$geog.proj)
		    #spatial.name <- paste(v, name, sep="")
		    #writeOGR(sPDF, ".", spatial.name, driver="ESRI Shapefile", overwrite=T)
		    ras.name <- paste("hagfish", v, name, ".tif", sep="")
		    grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun=grid.fun)
		    grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=grid.fun)
		    setwd(p$rasdir)
		    writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)
		    
		    #Create raster stack to calculate the max and min values for the colour scale
		    rstack <- raster::stack(rstack, grid.sum.big)
		    names(rstack[[i]]) <- name
		    #Define colour scale
		    z <- getValues(rstack)
		    z <- z[is.finite(z)]
		    z <- round(z, digits=0)
		    
		    if (v == "landings") {
		      z <- z/100
		      z <- round(z, digits=0)
		      z <- z*100
		    } else {
		      z <- z/10
		      z <- round(z, digits=0)
		      z <- z*10
		    }
		    
		    #MG might want to convert landings /1000 to calculate tons for landings, right now it's using KG
		    quant <- unique(quantile(z, seq(0,1, length.out=45)))
		    quant.labels <- round(seq(0, max(z), by= (max(z)/13)), digits = -2)
		    
		    my.brks = seq(0, max(z), by= (max(z)/13))
		    s.brks = seq(0, max(z), by= (max(z)/45))
		    ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels, cex=0.5))
		    
		    #Plot the variable	
		    setwd(p$mapdir)
		    #fig.name <- paste(v, name,"hagfish", ".pdf", sep="")
		    fig.name <- paste(v, name,"hagfish", ".png", sep="")
		    
		    png(filename=fig.name, width=7, height=5, units="in", res=450)
		    #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		    
		    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                    margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.5), y=list(cex=0.5)))
		          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
		          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4)) 
		          + layer(sp.lines(nafo, col='black', alpha=0.9, lwd= 0.4))
		    )
		    
		    dev.off()
		  }
		  
		  
		  #Plot all the rasters in a stack, seperated into 2 images
		  #stack.name<- paste(v, "stack","hagfish", ".pdf", sep="")
		  stack.name<- paste(v, "stack","hagfish", ".png", sep="")
		  png(filename=stack.name, width=7
		      , height=5, units="in", res=300)
		  #pdf(file=stack.name, width=8, height=9, bg='white')
		  par(mar=c(1,1,1,1))
		  par(oma=c(0,0,0,0))
		  max.plot <- length(sort(unique(M$yr)))
		  min.plot <- max.plot - 11
		  #max.plot2 <- min.plot -1
		  #min.plot2 <- min.plot -4
		  main <- y
		  print(levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                  margin=F, xlab="", ylab="", par.strip.text=list(cex=0.5), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5))) 
		        + layer(sp.polygons(coast, fill='lightgrey')) 
		        + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		        + layer(sp.lines(nafo, col='black', alpha=0.9, lwd= 0.4))
		        
		  )
		  dev.off()
		  
		  to.print <- paste(v, "completed", sep=" ")
		  print(to.print)
		}
		
		if (DS %in% c("cpue.yearly")) {
		  setwd(p$shpdir)
		  v= "cpue"
		  grid.fun = fun= function(x, ...){mean(x)}
	
		  M = c[order(-c$cpue),]
		  #Extract data for the raster creation
		  M = M[, c("yr", "lon", "lat", v)]
		  names(M) = c('yr', 'lon', 'lat', 'cpue')
		  M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]
		  
		  #Loop through and create a raster-based plot for each year
		  for (i in 1:length(sort(unique(M$yr)))) {
		    y= sort(unique(M$yr))
		    name<- y[i]
		    print(name)
		    s= subset(M, M$yr==name)
		    
		    #Extract coordinates and create spatial points data frame to be gridded
		    cor = s[, c("lon", "lat")]
		    sPDF <- SpatialPointsDataFrame (cor, data=s)
		    proj4string(sPDF)<-(p$geog.proj)
		    #spatial.name <- paste(v, name, sep="")
		    #writeOGR(sPDF, ".", spatial.name, driver="ESRI Shapefile", overwrite=T)
		    ras.name <- paste("hagfish", v, name, ".tif", sep="")
		    grid.sum <- rasterize(x=sPDF, y=grid, field=v, fun=grid.fun)
		    grid.sum.big <- rasterize(x=sPDF, y=grid.big, field=v, fun=grid.fun)
		    setwd(p$rasdir)
		    writeRaster(grid.sum, filename=ras.name, datatype= 'GTiff', overwrite=T)
		    
		    #Create raster stack to calculate the max and min values for the colour scale
		    rstack <- raster::stack(rstack, grid.sum.big)
		    names(rstack[[i]]) <- name
		    #Define colour scale
		    z <- getValues(rstack)
		    z <- z[is.finite(z)]
		    z <- round(z, digits=2)
		    
		    #MG might want to convert landings /1000 to calculate tons for landings, right now it's using KG
		    quant <- unique(quantile(z, seq(0,1, length.out=45)))
		    #quant.labels <- round(seq(0, max(z), by= (max(z)/13)), 2)
		    quant.labels <- c(0, 2, 4, 6, 8, 10, 15, 20, 25, 35, 50, 75)
		    
		    my.brks = seq(0, max(z), by= (max(z)/13))
		    s.brks = seq(0, max(z), by= (max(z)/45))
		    ckey <- list(at=quant, labels=list(at=quant.labels, labels=quant.labels, cex=0.5))
		    
		    #Plot the variable	
		    setwd(p$mapdir)
		    #fig.name <- paste(v, name,"hagfish", ".pdf", sep="")
		    fig.name <- paste(v, name,"hagfish", ".png", sep="")
		    
		    png(filename=fig.name, width=6.5, height=5, units="in", res=450)
		    #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		    
		    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                    margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.5), y=list(cex=0.5)))
		          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
		          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4)) 
		          + layer(sp.lines(nafo, col='black', alpha=0.9, lwd= 0.4))
		    )
		    
		    dev.off()
		  }
		  

		  #Plot all the rasters in a stack, seperated into 2 images
		  #stack.name<- paste(v, "stack","hagfish", ".pdf", sep="")
		  stack.name<- paste(v, "stack","hagfish", ".png", sep="")
		  png(filename=stack.name, width=7, height=5, units="in", res=300)
		  #pdf(file=stack.name, width=8, height=9, bg='white')
		  par(mar=c(1,1,1,1))
		  par(oma=c(0,0,0,0))
		  max.plot <- length(sort(unique(M$yr)))
		  min.plot <- max.plot - 11
		  main <- y
		  print(levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                  margin=F, xlab="", ylab="", par.strip.text=list(cex=0.5), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5))) 
		        + layer(sp.polygons(coast, fill='lightgrey')) 
		        + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		        + layer(sp.lines(nafo, col='black', alpha=0.9, lwd= 0.4))
		        
		  )
		  dev.off()
		  
		  to.print <- paste(v, "completed", sep=" ")
		  print(to.print)
		}
		
		if (DS %in% c("cpue.allyears")) {
		  setwd(p$shpdir)
		  v= "cpue"
		  grid.fun = fun= function(x, ...){mean(x)}
		  
		  M = c[order(-c$cpue),]
		  #Extract data for the raster creation
		  M = M[, c("yr", "lon", "lat", v)]
		  names(M) = c('yr', 'lon', 'lat', 'cpue')
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
		  ras.name <- paste("hagfish", "cpue.allyears", ".tif", sep="")
		  
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
		  quant.labels <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150, 175)
		  my.brks = seq(0, max(z), by= (max(z)/13))
		  s.brks = seq(0, max(z), by= (max(z)/45))
		  ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
		  
		  #Plot the variable
		  setwd(p$mapdir)
		  #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		  
		    fig.name <- paste("hagfish", "cpue.allyears", ".png", sep="")
		    
		    png(filename=fig.name, width=6.5, height=5, units="in", res=450)
		    #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		    
		    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                    margin=F, xlab="", ylab="", main="", scales = list(x=list(cex=0.5), y=list(cex=0.5)))
		          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
		          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4)) 
		          + layer(sp.lines(nafo, col='black', alpha=0.9, lwd= 0.4))
		    )
		    
		    dev.off()
		  }
		  
		 		require('ggplot2')
		return("mapping completed")

	}
