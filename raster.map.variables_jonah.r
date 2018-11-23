
#Function to summarize and map raster variables
#Cell size can also be adjusted to change the default cell size that is mapped, default = 6km

	raster.map.variables.jonah = function(DS, p=p) {
    #browser()
    p = setup.parameters()
    library('ggplot2')
    detach('package:ggplot2')
    l =  jonah.logbook.db( DS="logbook")
    #l = l[which(l$yr != 2017),]
    l$lfa[l$lfa == "LOBSTER - GREY ZONE"] <- "Grey Zone"
    l$lfa[l$lfa == "NA"] <- "Unknown"
    l$cpue = as.numeric(l$cpue)
    l$effort = as.numeric(l$effort)
    l$landings = as.numeric(l$landings)
    l$doc_type[which(substr(l$doc_type, 1,2) == 'SD')] <- "lobster.log" #Anything starting with SD is lobster log
    l$doc_type[which(substr(l$doc_type, 1,2) == 'MD')] <- "crab.doc"
    l$doc_type[which(l$doc_type == 'NA')] <- "crab.doc"
    
    d.lic = jonah.logbook.db (DS = 'jonah.licence')
    names(d.lic)[names(d.lic) == "licence_id"] <- "licence"
    
    d = merge(d.lic, l, by= "licence", all.x = T)
    d$directed = "directed"
    d = d[, c('licence', 'directed')]
    d = unique(d)
    
    K = merge(l, d, by='licence', all.x = T)
    K$directed[which(is.na(K$directed))] <- "bycatch" 
    names(K)[names(K) == "directed"] <- "f.type"
    
    K$sa = 1  # this a dummy variable required by the mapping routine
    K = K[which(K$lfa != 41), ]
    K$lon <- as.numeric(K$lon)
    K$lat <- as.numeric(K$lat)
    
    #Write Shapefile of Fisheries Data
    fd <- K
    fd$date.landed <- as.character(fd$date.fished)
    
    fd = fd[!is.na(fd$lat),]
    fd.cords <- fd[, c("lon", "lat")]
    sfd <- SpatialPointsDataFrame(fd.cords, data=fd)
    proj4string(sfd) <- p$geog.proj
    setwd(p$shpdir)
    writeOGR(sfd, ".", "FisheriesDataUpdate", driver="ESRI Shapefile", overwrite=T)
    
    coast<-load.shapefile(DS = 'coastline')
    contours <- load.shapefile(DS = 'contours')
  	lfas = load.shapefile(DS = "lfas")
  	
  	#Designate Cell size
  	# 1 minute grid = 0.166, 2 minute grid = 0.033, 3minute grid = 0.05, 4 minute grid = 0.0666, 5 minute grid = 0.083
  	cell<- 0.05
  	cell.big <- 0.166
  
    ext = p$ext.inshorejonah
  
  	#Set rows and columns for blank grid
  	ncols <- length(ext[2]:ext[1])/cell
  	nrows <- (length(ext[4]:ext[3])-1)/cell
  	
  	#Set rows and columns for the raster stack. Larger cell size because of smaller plots
  	ncols.big <- length(ext[2]:ext[1])/cell.big
  	nrows.big <- (length(ext[2]:ext[1])-1)/cell.big
  	
  	#Create a blank grid for plotting
  	grid <- raster(nrows=nrows, ncols=ncols, ext=ext, crs=p$geog.proj)
  	grid[]<- 1:ncell(grid)
  	grid.big <- raster(nrows=nrows.big, ncols=ncols.big, ext=ext, crs=p$geog.proj)
  	grid.big[]<- 1:ncell(grid.big)
  	rstack <- raster::stack()
			
    if (DS %in% c("landings.yearly")) {
      setwd(p$shpdir)
      v= "landings"
      grid.fun= sum
      K = K[which(K$f.type == 'directed'), ] 
      
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
      			ras.name <- paste(v, name, ".tif", sep="")
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
    				ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
    				
    				#Plot the variable	
    				setwd(p$mapdir)
    			  #fig.name <- paste(v, name,"jonah", ".pdf", sep="")
    			  fig.name <- paste(v, name,"jonah", ".png", sep="")
    			  
    				png(filename=fig.name, width=6, height=5, units="in", res=450)
    				#pdf(file=fig.name, width=6.5, height=4.8, bg='white')
    
    				print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
    				                margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
    				     + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
      			     + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4)) 
    				     + layer(sp.lines(lfas, col='black', alpha=0.9, lwd= 0.4))
    				        )
    		
    				dev.off()
    			}
    
    			#Plot all the rasters in a stack, seperated into 2 images
    			#stack.name<- paste(v, "stack","jonah", ".pdf", sep="")
    			stack.name<- paste(v, "stack","jonah", ".png", sep="")
    			png(filename=stack.name, width=6.5, height=5, units="in", res=300)
    			#pdf(file=stack.name, width=8, height=9, bg='white')
     			par(mar=c(1,1,1,1))
     			par(oma=c(0,0,0,0))
    			max.plot <- length(sort(unique(M$yr)))
    			min.plot <- max.plot - 14
    			#max.plot2 <- min.plot -1
    			#min.plot2 <- min.plot -4
    			main <- y
    	  	print(levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
    				margin=F, xlab="", ylab="", par.strip.text=list(cex=0.7), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5))) 
    				+ layer(sp.polygons(coast, fill='lightgrey')) 
    			  + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
    				+ layer(sp.lines(lfas, col='black', alpha=0.9, lwd= 0.4))
    				
    				)
    			dev.off()
    			
    			to.print <- paste(v, "completed", sep=" ")
    			print(to.print)
    			
    }
			
		if (DS %in% c("effort.yearly")) {
		  setwd(p$shpdir)
		  v= "effort"
		  grid.fun = sum
		  K = K[which(K$f.type == 'directed'), ] 
		  
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
		    ras.name <- paste(v, name, ".tif", sep="")
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
		    ckey <- list(at=s.brks, labels=list(at=quant.labels, labels=quant.labels))
		    
		    #Plot the variable	
		    setwd(p$mapdir)
		    #fig.name <- paste(v, name,"jonah", ".pdf", sep="")
		    fig.name <- paste(v, name,"jonah", ".png", sep="")
		    
		    png(filename=fig.name, width=6, height=5, units="in", res=450)
		    #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
		    
		    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                    margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
		          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
		          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4)) 
		          + layer(sp.lines(lfas, col='black', alpha=0.9, lwd= 0.4))
		    )
		    
		    dev.off()
		  }
		
		
		#Plot all the rasters in a stack, seperated into 2 images
		#stack.name<- paste(v, "stack","jonah", ".pdf", sep="")
		stack.name<- paste(v, "stack","jonah", ".png", sep="")
		png(filename=stack.name, width=6.5, height=5, units="in", res=300)
		#pdf(file=stack.name, width=8, height=9, bg='white')
		par(mar=c(1,1,1,1))
		par(oma=c(0,0,0,0))
		max.plot <- length(sort(unique(M$yr)))
		min.plot <- max.plot - 14
		#max.plot2 <- min.plot -1
		#min.plot2 <- min.plot -4
		main <- y
		print(levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
		                margin=F, xlab="", ylab="", par.strip.text=list(cex=0.7), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5))) 
		      + layer(sp.polygons(coast, fill='lightgrey')) 
		      + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
		      + layer(sp.lines(lfas, col='black', alpha=0.9, lwd= 0.4))
		      
		)
		dev.off()
		
		to.print <- paste(v, "completed", sep=" ")
		print(to.print)
}
	
  	if (DS %in% c("cpue.yearly")) {
  	  setwd(p$shpdir)
  	  v= "cpue"
  	  grid.fun = fun= function(x, ...){mean(x)}
  	  
  	  K = K[which(K$f.type == 'directed'), ] 
  	  M = K[which(K$effort > 10) ,]
  	  M = M[which(M$cpue < 50),]
  	  #what is the maximum cpue I can have here...cut everything else out
  	  
  	  M = M[order(-M$cpue),]
  	  #Extract data for the raster creation
  	  M = M[, c("yr", "lon", "lat", v)]
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
  	    ras.name <- paste(v, name, ".tif", sep="")
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
  	    quant.labels <- c(0, 2, 4, 6, 8, 10, 15, 20, 25)
  	    
  	    my.brks = seq(0, max(z), by= (max(z)/13))
  	    s.brks = seq(0, max(z), by= (max(z)/45))
  	    ckey <- list(at=quant, labels=list(at=quant.labels, labels=quant.labels))
  	    
  	    #Plot the variable	
  	    setwd(p$mapdir)
  	    #fig.name <- paste(v, name,"jonah", ".pdf", sep="")
  	    fig.name <- paste(v, name,"jonah", ".png", sep="")
  	    
  	    png(filename=fig.name, width=6, height=5, units="in", res=450)
  	    #pdf(file=fig.name, width=6.5, height=4.8, bg='white')
  	    
  	    print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
  	                    margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
  	          + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
  	          + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4)) 
  	          + layer(sp.lines(lfas, col='black', alpha=0.9, lwd= 0.4))
  	    )
  	    
  	    dev.off()
  	  }
  	
  	
  	#Plot all the rasters in a stack, seperated into 2 images
  	#stack.name<- paste(v, "stack","jonah", ".pdf", sep="")
  	stack.name<- paste(v, "stack","jonah", ".png", sep="")
  	png(filename=stack.name, width=6.5, height=5, units="in", res=300)
  	#pdf(file=stack.name, width=8, height=9, bg='white')
  	par(mar=c(1,1,1,1))
  	par(oma=c(0,0,0,0))
  	max.plot <- length(sort(unique(M$yr)))
  	min.plot <- max.plot - 14
  	#max.plot2 <- min.plot -1
  	#min.plot2 <- min.plot -4
  	main <- y
  	print(levelplot(rstack[[min.plot:max.plot]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
  	                margin=F, xlab="", ylab="", par.strip.text=list(cex=0.7), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5))) 
  	      + layer(sp.polygons(coast, fill='lightgrey')) 
  	      + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
  	      + layer(sp.lines(lfas, col='black', alpha=0.9, lwd= 0.4))
  	      
  	)
  	dev.off()
  	
  	to.print <- paste(v, "completed", sep=" ")
  	print(to.print)
  	}
      
		require('ggplot2')
		return("mapping completed")
	}
