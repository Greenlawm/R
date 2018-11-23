
#Function to summarize and map raster variables
#Cell size can also be adjusted to change the default cell size that is mapped, default = 6km

	raster.map.variables.seacucumber = function(p=p, grid.fun, variables, cell=NULL) {
	    #browser()
	  
	  #change the calculation for landings for fishing year
	  #calculate sq.km/100 mt and kg/sq.km
	  #look at detailed maps of where they are fishing for each of the zones. 

	  p = setup.parameters()
	  detach('package:ggplot2')

	  grid.fun= sum

	  K = process.sc.logbook()
	  
	  test = K[which(K$f.year == '2018'),]

	  #Import Shapefilescoastline
	  coast<-load.shapefile(DS = 'coastline')
    contours <- load.shapefile(DS = 'contours')
	  fa = load.shapefile(DS = "sea.cucumber.fa")

	  #Designate Cell size
	  #if(is.null(cell)) {
	  # 1 minute grid = 0.0166, 2 minute grid = 0.033, 3minute grid = 0.05, 4 minute grid = 0.0666, 5 minute grid = 0.083
	  cell<- 0.03
	  cell.big <- 0.053
	  #}

	  K$sa = 1  # this a dummy variable required by the mapping routine
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
	  writeOGR(sfd, ".", "SeaCucumberFisheriesDataUpdate", driver="ESRI Shapefile", overwrite=T)

		#Set rows and columns for blank grid
		ncols <- length(p$extsc[2]:p$extsc[1])/cell
		nrows <- (length(p$extsc[4]:p$extsc[3])-1)/cell

		#Set rows and columns for the raster stack. Larger cell size because of smaller plots
		ncols.big <- length(p$extsc[2]:p$extsc[1])/cell.big
		nrows.big <- (length(p$extsc[2]:p$extsc[1])-1)/cell.big

		#Create the color palette for each variable
		setwd(p$shpdir)
	#	for (v in variables) {
	#	print(v)
		  v= "landings"
			#Extract data for the raster creation
			M = K[, c("f.year", "lon", "lat", v)]
			M = M[is.finite(M[,v] *M[,"lon"] *M[,"lat"]),]

			#Create a blank grid for plotting
			grid <- raster(nrows=nrows, ncols=ncols, ext=p$extsc, crs=p$geog.proj)
			grid[]<- 1:ncell(grid)
			grid.big <- raster(nrows=nrows.big, ncols=ncols.big, ext=p$extsc, crs=p$geog.proj)
			grid.big[]<- 1:ncell(grid.big)
			rstack <- raster::stack()

			#Loop through and create a raster-based plot for each year
			for (i in 1:length(sort(unique(M$f.year)))) {
				y= sort(unique(M$f.year))
				name<- y[i]
				print(name)
				s= subset(M, M$f.year==name)

				#Extract coordinates and create spatial points data frame to be gridded
				cor = s[, c("lon", "lat")]
				sPDF <- SpatialPointsDataFrame (cor, data=s)
				proj4string(sPDF)<-(p$geog.proj)
				#spatial.name <- paste(v, name, sep="")
				#writeOGR(sPDF, ".", spatial.name, driver="ESRI Shapefile", overwrite=T)

				#Create Rasters
				ras.name <- paste("sc", v, name, ".tif", sep="")
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
			  fig.name <- paste("sc", v, name, ".pdf", sep="")
				#png(filename=fig.name, width=6, height=5, units="in", res=450)
				pdf(file=fig.name, width=6.5, height=4.8, bg='white')

				print(levelplot(grid.sum, at=quant, colorkey=ckey, col.regions=p$seis, alpha.regions=1,
				                margin=F, xlab="", ylab="", main=name, scales = list(x=list(cex=0.7), y=list(cex=0.7)))
				      + layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
			        + layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
				      + layer(sp.lines(fa, col='blue', alpha=0.6, lwd= 0.6))
				      )

				dev.off()
			}

			#Plot all the rasters in a stack, seperated into 2 images
			stack.name<- paste("sc", v, "stack", ".pdf", sep="")
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
				+ layer(sp.polygons(coast, fill='lightgrey', alpha = 0.2))
				+ layer(sp.lines(contours, col='dimgrey', alpha=0.6, lwd= 0.4))
				+ layer(sp.lines(fa, col='blue', alpha=0.6, lwd= 0.6))
				)
			dev.off()

			to.print <- paste(v, "completed", sep=" ")
			print(to.print)

		#}
library('ggplot2')
		return("mapping completed")
			
	}
