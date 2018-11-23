#Function to map proportional symbols over groundfish survey strata that represent their geometric mean catch from xxxx-xxxx

	map.spatial.groundfish.summary.whelk = function() {
	  
	  p = setup.parameters()
	  plot0 = T
	  species = c('4211', '4210', '4209') #whelk
	  #species = c('4221', '4220') #moonsnail
	  
	  v = 'totno_sd'
	  com.name = 'whelk'
	  
	  setwd(p$polydir)
	  coast<- load.shapefile(DS = 'coastline')
	  contours <- load.shapefile(DS = 'contours')
	  nafo <- load.shapefile(DS= 'nafo')
	  
	  coast.points = fortify(coast, region="Province")
	  nafo.points = fortify(nafo, region="ZONE")

	  #Extract Cat data from groundfish survey 
	  k = groundfish.db(DS="cat.base")
	  
	  #Extract the years requestd
	  k = k[which(k$yr >= min(p$data.yrs) & k$yr <= max(p$data.yrs)), ]
	  
	  #Add in all sets for a particular species, so zeros are present
	  all.sets = unique(k[c("id")])
	  all.sets = merge(all.sets, k[ , c("strat", "yr","id", "lat", "lon")])
	  all.sets = unique(all.sets)
	  expand <- expand.grid (spec = species, id = unique (all.sets$id))
	  all.sets <- merge(all.sets, expand, all.x=T)
	  
	  j <- merge (all.sets, k, all.x=T)
	  j = j[which(j$spec == species),]
	  j$syid = paste(j$stra, j$yr, sep=".")
	  j$totwgt_sd[is.na(j$totwgt_sd)] <- 0
	  j$totno_sd[is.na(j$totno_sd)] <- 0
	  
    j <- ddply (j,. (yr, strat, id), summarize, lat = mean (lat), lon = mean (lon), totno = mean(totno_sd))
	  #write.shapefile(j, "whelk", p)
	  
	  n=j
	  
	  setwd(p$stratadir)
	  strata.shp <- rgdal::readOGR (".", "MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83")
	  strata.shp$StrataID <- revalue (strata.shp$StrataID, c("5Z31"="5Z3", "5Z32"="5Z3", "5Z41"="5Z4", "5Z42"="5Z4"))
	  strata.shp = strata.shp[strata.shp$TYPE == 2 | strata.shp$TYPE ==3,]
	  strata.df <- ggplot2::fortify(strata.shp, region = "StrataID")
	  
	  strata <- strata.df
	  # Order so shape file works correctly
	  strata <- arrange (strata, order)

	 	setwd(p$mapdir)
		print(v)
		m=n
				
		m$tno[m$totno == 0] <- "0"
		m$tno[m$totno > 0 & m$totno < 1] <- "0-1"
		m$tno[m$totno >= 1 & m$totno < 10] <- "1-10"
		m$tno[m$totno >= 10 & m$totno < 50] <- "10-50"
		m$tno[m$totno >= 50 & m$totno < 100] <- "50-100"
		m$tno[m$totno >= 100] <- "100+"
		m$tno <- factor (m$tno, levels = c ("0", "0-1", "1-10", "10-50", "50-100", "100+"))
		m <- na.omit(m)
		
		#write.shapefile(m, name, p)
		
		labname = paste("Mean kg/tow")
		
		#Set Scales and Shapes
		shapes <- c( 21, 21, 21, 21, 21, 21)
		sizes <- c (0, 0.5, 1, 3, 4.5, 6)
		color <- c('white', 'red', 'red', 'red', 'red', 'red')
		
	  splot <- ggplot () +
    geom_point (data = m, aes (x = lon, y = lat, size = tno, shape = tno, fill = tno)) +
    scale_fill_manual(values = color) + scale_shape_manual(values = shapes) + scale_size_manual (values = sizes) +
    theme_bw () + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    coord_map(xlim = c(-68, -56.5), ylim = c(41, 47.5)) +
    theme (axis.title = element_text (size = 9), axis.text = element_text (size = 8), 
           legend.text = element_text (size = 8), legend.title = element_text (size = 9)) +
    geom_polygon(data = nafo.points, aes(x = long, y=lat, group = group), colour = 'grey20', fill = NA, size = 0.5) +
	  geom_polygon(data = coast.points, aes(x = long, y=lat, group = group), colour = 'grey20', fill= 'lightgrey', size = 0.2)
    #geom_polygon(data = strata, aes(x = long, y=lat, group = group), colour = 'grey', fill= 'NA', size = 0.5) +

    outfile = paste(v, com.name, "groundfish", sep=".")
    fn = file.path( p$mapdir, paste( outfile, "pdf", sep="." ) )
    pdf(file=fn, width=7, height=4, bg='white') 
    plot (splot)
    dev.off ()


		return(splot)
	}