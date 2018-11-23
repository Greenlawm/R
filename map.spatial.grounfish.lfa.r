#Function to map proportional symbols over groundfish survey strata that represent their geometric mean catch from xxxx-xxxx

	map.spatial.groundfish = function() {
	  
	  p = setup.parameters()
	  plot0 = T
	  species = c('2511') #Jonah Crab
	  #species = c('4211', '4210', '1510') #whelk
	  #species = c('25') #tilefish
	  
	  #Extract Cat data from groundfish survey 
	  k = groundfish.db(DS="cat.base")
	  y = load.shapefile( DS = "lfas")
	  to_extract = c('ZONE', 'ZONE_1', 'LFA_NAFO')
	  y = y[, to_extract]

	  #Extract the years requestd
	  k = k[which(k$yr >= min(p$data.yrs) & k$yr <= max(p$data.yrs)), ]
	  
	  #Add in all sets for a particular species, so zeros are present
	  all.sets = unique(k[c("id")])
	  all.sets = merge(all.sets, k[ , c("strat", "yr","id", "lat", "lon")])
	  all.sets = unique(all.sets)
	  all.sets = intersect.sp.groundfish (x = all.sets, y=y)
	  
	  expand <- expand.grid (spec = species, id = unique (all.sets$id))
	  all.sets <- merge(all.sets, expand, all.x=T)
	  
	  j <- merge (all.sets, k, all.x=T)
	  j = j[which(j$spec == species),]
	  j$syid = paste(j$stra, j$yr, sep=".")
	  j$totwgt_sd[is.na(j$totwgt_sd)] <- 0
	  j$totno_sd[is.na(j$totno_sd)] <- 0
	  
	  #fn.root =  file.path( ss.exdatadirectory)
	  #fny = file.path( fn.root, paste("groundfish.survey.rdata", sep="."))  
	  #save( j, file=fny, compress=T)
	  
	  #write.shapefile(j, "jonah", p)

	  j$totwgt_sd = j$totwgt_sd + 1
	  j$totno_sd = j$totno_sd + 1
	  
	  com.name = unique(j$name.common)
	  com.name = com.name[!is.na(com.name)]
	  
    #Take the geometric mean of weight and number by year, and then over years
	  yr.m <- ddply (j,. (yr, spec, lfa_nafo), summarize, lat = mean (lat), lon = mean (lon), 
	                  wgt = gm.mean (totwgt_sd, na.rm=T, zero.propagate = FALSE), tno = gm.mean(totno_sd, na.rm=T, zero.propagate = FALSE))
	  
	  tot.m <- ddply (j,. (spec, lfa_nafo), summarize, lat = mean (lat), lon = mean (lon), 
	                  wgt = gm.mean (totwgt_sd, na.rm=T, zero.propagate = FALSE), tno = gm.mean(totno_sd, na.rm=T, zero.propagate = FALSE))
	  
	  tot.m$wgt = tot.m$wgt - 1
	  tot.m$tno = tot.m$tno -1
	  
	  # Weights by group (for plotting)
	  tot.m$wgtg <- ifelse (tot.m$wgt == 0, "0", NA)
	  tot.m$wgtg <- ifelse (tot.m$wgt > 0, ifelse (tot.m$wgt, "0-0.3", tot.m$wgtg), tot.m$wgtg)
	  tot.m$wgtg <- ifelse (tot.m$wgt >= 0.3, ifelse (tot.m$wgt < 0.5, "0.3-0.5", tot.m$wgtg), tot.m$wgtg)
	  tot.m$wgtg <- ifelse (tot.m$wgt >= 0.5, ifelse (tot.m$wgt < 0.7, "0.5-0.7", tot.m$wgtg), tot.m$wgtg)
	  tot.m$wgtg <- ifelse (tot.m$wgt >= 0.7, "0.7+", tot.m$wgtg)
	  # Order factor levels
	  tot.m$wgtg <- factor (tot.m$wgtg, levels = c ("0", "0-0.3", "0.3-0.5", "0.5-0.7", "0.7+"))
	 
	  j$totwgt_sd = j$totwgt_sd - 1
	  j$totno_sd = j$totno_sd - 1
	  #variables = c("totno_sd", "totwgt_sd")
	  
	  #Intersect with LFAs
	  y <- spTransform(y, p$geog.proj)
	  v= "totwgt_sd"

	  #setwd(p$stratadir)
	  #strata.shp <- rgdal::readOGR (".", "MaritimesRegionEcosystemAssessmentStrata(2014-)NAD83")
	  #strata.shp$StrataID <- revalue (strata.shp$StrataID, c("5Z31"="5Z3", "5Z32"="5Z3", "5Z41"="5Z4", "5Z42"="5Z4"))
	  #strata.shp = strata.shp[strata.shp$TYPE == 2 | strata.shp$TYPE ==3,]
	  #strata.df <- ggplot2::fortify(strata.shp, region = "StrataID")
	  
	  lfa.df <- ggplot2::fortify(y, region = "LFA_NAFO")
	  
	  names(tot.m)[2] <- "id"
	  myvars <- c("id", "wgtg")
	  tot.s <- tot.m[myvars]
	  
	  lfa <- merge (lfa.df, tot.s, all = T)
	  # Order so shape file works correctly
	  lfa <- arrange (lfa, order)
	  lfa<-lfa[!is.na(lfa$wgtg),]

	  #Set Scales and Shapes
	  shapes <- c( 3, 16, 16, 16, 16)
	  sizes <- c (1, 1.5, 2.5, 5, 6)
	  
	 	setwd(p$mapdir)
	#	for (v in variables) {
			print(v)
		  n = j[, c("yr", "lon", "lat", v)]
		 	n = n[is.finite(n[,v] *n[,"lon"] *n[,"lat"]),]

		 	for (i in 1:length(sort(unique(n$yr)))) {
				y= sort(unique(n$yr))
				name<- y[i]
				print(name)
				m= subset(n, n$yr==name)
				
				m$wgtg[m$totwgt_sd == 0] <- "0"
				m$wgtg[m$totwgt_sd > 0 & m$totwgt_sd < 1] <- "0-1"
				m$wgtg[m$totwgt_sd >= 1 & m$totwgt_sd < 10] <- "1-10"
				m$wgtg[m$totwgt_sd >= 10 & m$totwgt_sd < 50] <- "10-50"
				m$wgtg[m$totwgt_sd >= 50 & m$totwgt_sd < 100] <- "50-100"
				m$wgtg[m$totwt_sd >= 100] <- "100+"
				m$wgtg <- factor (m$wgtg, levels = c ("0", "0-1", "1-10", "10-50", "50-100", "100+"))
				m <- na.omit(m)
				#write.shapefile(m, name, p)
				
				labname = paste(y[i], "Geometric Mean kg/tow", sep = " ")

		    splot <- ggplot () +
		    geom_polygon (data = lfa, aes (x = long, y = lat, group = group, fill = wgtg), colour = "white", size = 0.4) +
		    geom_point (data = m, aes (x = lon, y = lat, size = wgtg, shape = wgtg)) +
		    scale_shape_manual (values = shapes) + scale_size_manual (values = sizes) +
		    coord_map () + theme_bw () + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
		    scale_y_continuous ("Latitude", expand = c (0,0), limits = c (40, 46.5)) + 
		    scale_x_continuous ( "Longitude", expand = c (0,0), limits = c (-68, -56.5)) +
		    theme (axis.title = element_text (size = 9), axis.text = element_text (size = 8), legend.text = element_text (size = 8), legend.title = element_text (size = 9)) + #theme (legend.position = c(1,0), legend.justification = c(0.8, 0.05)) +
		    labs (shape=labname, size =labname, fill = "Geometric Mean Catch (kg/tow) ") +
		    scale_fill_brewer()
		  
		    outfile = paste(y[i], v, com.name, sep=".")
		    fn = file.path( p$mapdir, paste( outfile, "pdf", sep="." ) )
		    pdf(file=fn, width=7, height=4, bg='white') 
        plot (splot)
	      dev.off ()
		  
			}
#		}
		return("mapping completed")
	}