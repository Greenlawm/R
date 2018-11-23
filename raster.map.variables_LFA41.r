
#Function to summarize and map raster variables
#Cell size can also be adjusted to change the default cell size that is mapped, default = 6km

	raster.map.variables = function(p=p, grid.fun, variables, cell=NULL) {
	    #browser()
	  
	  p = setup.parameters()
	  K = sslogbook.db( DS="logbook" )
	  
	  K$sa = 1  # this a dummy variable required by the mapping routine
	  K = K[which(K$lfa == 41), ]
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
	  
	  #Import coastline
		#MG: Switch this to the smaller coastline with no islands
		setwd(p$polydir)
		coast<-readOGR(".", "NY_to_Nova_UTM20")
		coast<-spTransform(coast, p$geog.proj)
		coast <- rgeos::gSimplify(coast, tol=0.01, topologyPreserve=TRUE)
		
		#Import Contour
		contours <- readOGR(".", "contours")
		contours <- spTransform(contours, p$geog.proj)
		contours <- gSimplify(contours, tol=0.01, topologyPreserve = T)
		
		lfas = load.shapefile(DS = "lfas")
		
		#Designate Cell size
		#if(is.null(cell)) {
			# 1 minute grid = 0.166, 2 minute grid = 0.033, 3minute grid = 0.05, 4 minute grid = 0.0666, 5 minute grid = 0.083
			cell<- 0.083
			cell.big <- 0.166
		#}

		#Set rows and columns for blank grid
		ncols <- length(p$ext[2]:p$ext[1])/cell
		nrows <- (length(p$ext[4]:p$ext[3])-1)/cell
		
		#Set rows and columns for the raster stack. Larger cell size because of smaller plots
		ncols.big <- length(p$ext2[2]:p$ext2[1])/cell.big
		nrows.big <- (length(p$ext2[2]:p$ext2[1])-1)/cell.big
		
		grid.fun= sum

		#Create the color palette for each variable
		setwd(p$shpdir)
	#	for (v in variables) {
	#		print(v)
		  v= "landings"
			#Extract data for the raster creation
			M = K[, c("yr", "lon", "lat", v)]
			M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]

			#Create a blank grid for plotting
			grid <- raster(nrows=nrows, ncols=ncols, ext=p$ext, crs=p$geog.proj)
			grid[]<- 1:ncell(grid)
			grid.big <- raster(nrows=nrows.big, ncols=ncols.big, ext=p$ext2, crs=p$geog.proj)
			grid.big[]<- 1:ncell(grid.big)
			rstack <- raster::stack()
	
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
				quant <- unique(quantile(z, seq(0,1, length.out=75)))
			  #MG check to see if quantiles are being calculated properly for effort, there doesn't seem to be a lot of red on the maps
				quant.small <- unique(quantile(z, seq(0,1, length.out=5)))
				ckey <- list(at=quant, labels=list(at=quant.small))
				#Plot the variable	
				setwd(p$mapdir)
			    fig.name <- paste(v, name, ".pdf", sep="")
				#png(filename=fig.name, width=6, height=5, units="in", res=450)
				pdf(file=fig.name, width=6.5, height=4.8, bg='white')

				print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
				                margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7))) 
				      + layer(sp.polygons(coast, fill='lightgrey'))
				      + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
				      + layer(sp.lines(lfas, col='black', alpha=0.6, lwd= 0.4))
				        )

				dev.off()
			}
			
			#Plot all the rasters in a stack, seperated into 2 images
			stack.name<- paste(v, "stack", ".pdf", sep="")
			#png(filename=stack.name, width=6.5, height=5, units="in", res=300)
			pdf(file=stack.name, width=8, height=9, bg='white')
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
				)
			dev.off()
			
			to.print <- paste(v, "completed", sep=" ")
			print(to.print)
      #------------------------------------------
			#second raster stack
			stack.name<- paste(v, "stack2", ".pdf", sep="")
			#png(filename=stack.name, width=6.5, height=5, units="in", res=300)
			pdf(file=stack.name, width=9.5, height=6, bg='white')
			print(levelplot(rstack[[min.plot2:max.plot2]], at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
			                margin=F, xlab="", ylab="", par.strip.text=list(cex=0.7), scales = list(x=list(cex=0.5), y=list(cex=0.5), main=list(cex=0.5))) 
			      + layer(sp.polygons(coast, fill='lightgrey')) 
			      + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
			)
			dev.off()
			to.print <- paste(v, "2 completed", sep=" ")
			print(to.print)
		#}
		
		return("mapping completed")
	}
