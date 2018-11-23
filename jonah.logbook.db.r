jonah.logbook.db = function( DS, p=NULL, yrs = yrs) {
#browser()
  #Species Codes
  # Jonh Crab 703, 1995 - 
  # Lobster 700
  #Sea Cucumber 619, whelk 615
  #yrs = 2002:2017
  
  if (DS %in% c("odbc.logbook", "odbc.logbook.redo")) {
    fn.root =  file.path( ss.exdatadirectory, "jonah.logbook", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    if (DS=="odbc.logbook") {
      out = NULL
      for ( YR in yrs ) {
        fny = file.path( fn.root, paste( YR, "rdata", sep="."))
        if (file.exists(fny)) {
          load (fny)
          out = rbind( out, logbook )
        }
      }
      return (out)
    }
    
    #New Database Export of logbooks
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    
    for ( YR in yrs ) {
      fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      query = paste(
        "SELECT * from marfissci.marfis_crab",
        "where SPECIES_CODE=703",
        "AND EXTRACT(YEAR from DATE_FISHED) = ", YR )
      
      logbook = NULL
      logbook = sqlQuery(con, query )
      save( logbook, file=fny, compress=T)
      gc()  # garbage collection
      print(YR)
    }
    odbcClose(con)
    return (yrs)
    
  }
  
  if (DS %in% c("odbc.lobster.logbook", "odbc.lobster.logbook.redo")) {
    fn.root =  file.path( ss.exdatadirectory, "lobster.logbook", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    if (DS=="odbc.lobster.logbook") {
      out = NULL
        fny = file.path( fn.root, paste("rdata", sep="."))
        if (file.exists(fny)) {
          load (fny)
          out = rbind( out, logbook )
       
      }
      return (out)
    }
    
    #New Database Export of logbooks
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    
      fny = file.path( fn.root, paste("rdata", sep="."))
      query = paste("SELECT * from marfissci.lobster_sd_log where DATE_FISHED > TO_DATE('2010-01-01', 'YYYY-MM-DD') order by DATE_FISHED")
      logbook = NULL
      logbook = sqlQuery(con, query )
      save( logbook, file=fny, compress=T)
      gc()  # garbage collection
    odbcClose(con)

  }
  
  if (DS %in% c("lobster.logbook", "lobster.logbook.redo")) {
    
    filename = file.path( ss.exdatadirectory, "lobster.logbook", "logbook.rdata" )
    
    if (DS=="lobster.logbook") {
      load( filename )
      return(logbook)
    }
    
    #---
    
    # Re-do logbooks 
    x = jonah.logbook.db( DS="odbc.lobster.logbook", p=p)
   
    names( x ) = tolower( names( x ) )
    x$month = month(x$date_fished)
    x$yr = year(x$date_fished)
    logbook = x
    save (logbook, file=filename, compress=T )
    
    return( "Complete" )
    
  }
  


  if (DS %in% c("odbc.historical.logbook.lfa41", "odbc.historical.logbook.lfa41.redo", "odbc.historical.otherlfa.logbook", "odbc.historical.otherlfa.logbook.redo")) {
   #browser()
    fn.root =  file.path( ss.exdatadirectory, "historical_logbook_lfa41", "datadump" )
    fn.root.other = file.path( ss.exdatadirectory, "historical_logbook_otherlfa", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fn.root.other, recursive = TRUE, showWarnings = FALSE )

    yrsh = 1995:2001

    if (DS=="odbc.historical.logbook.lfa41") {
      out = NULL
      for ( YR in yrsh ) {
        fny = file.path( fn.root, paste( YR, "rdata", sep="."))
        if (file.exists(fny)) {
          load (fny)
          out = rbind( out, logbook )
        }
      }
      return (out)
    }

    if(DS =="odbc.historical.otherlfa.logbook"){
      out = NULL
      for ( YR in yrsh ) {
        fny = file.path( fn.root.other, paste( YR, "rdata", sep="."))
        if (file.exists(fny)) {
          load (fny)
          out = rbind( out, logbook )
        }
      }
      return (out)
    }
    #---------
    #New Database Export of historical logbooks
    if(DS =="odbc.historical.logbook.lfa41.redo"){
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    for ( YR in yrsh ) {
      fny = file.path( fn.root, paste( YR,"rdata", sep="."))
      query = paste(
        "select * from LOBSTER.LFA41_LOGS ",
        "where EXTRACT(YEAR from DATE_FISHED) = ", YR )

      #Uncorrected Data
      #query = paste(
      #"select * from comland.I_1986-2001",
      #"where species_code = 703",
      #"and cfv_number in ('000530','001530','001532','004005','004034','004056','100989','101060','101315','129902')",
      #"and year_of_activity = ", YR)

      logbook = NULL
      logbook = sqlQuery(con, query )
      save( logbook, file=fny, compress=T)
      gc()  # garbage collection
      print(YR)
    }
    odbcClose(con)
    return (yrsh)
    }

    if(DS =="odbc.historical.otherlfa.logbook.redo"){
      con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    for ( YR in yrsh ) {
      fny = file.path( fn.root.other, paste( YR,"rdata", sep="."))
      query = paste(
        "select * from comland.I_1986_2001",
         "where species_code = 703",
         "and cfv_number not in ('000530','001530','001532','004005','004034','004056','100989','101060','101315','129902')",
         "and year_of_activity = ", YR)
      logbook = NULL
      logbook = sqlQuery(con, query )
      save( logbook, file=fny, compress=T)
      gc()  # garbage collection
      print(YR)
    }
    odbcClose(con)
    return (yrsh)
    }
}
 
  #-----------------
  if (DS %in% c("historical.logbook.lfa41", "historical.logbook.lfa41.redo")) {

    filename = file.path( ss.exdatadirectory, "historical_logbook_lfa41", "logbook.rdata" )
    if (DS=="historical.logbook.lfa41") {
      load( filename )
      return(logbook)
    }

    # Re-do historical logbooks from tables
    x = jonah.logbook.db( DS="odbc.historical.logbook.lfa41", p=p, yrs=1995:2001)
    names( x ) = tolower( names( x ) )
    x$year = format(x$date_fished, format="%Y")

    names(x)[names(x) == "t_latitude"] <- "lat"
    names(x)[names(x) == "t_longitude"] <- "lon"
    names(x)[names(x) == "t_nbr_traps"] <- "effort"
    names(x)[names(x) == "adj_crab_lbs"] <- "landings"
    names(x)[names(x) == "t_depth"] <- "depth"

    colnames(x)
    x$lon = -x$lon
    x$date.fished = lubridate::ymd( x$date_fished )
    x$landings = x$landings * 0.454
    x$cpue = x$landings / x$effort
    x$depth = x$depth * 1.83
    to.extract = c( "year","lat","lon","depth","landings","effort", "cpue",
                   "date.fished")
    x = x[, to.extract ]
    logbook = x
    save (logbook, file=filename, compress=T )  # this is for plotting maps, etc

    return( "Complete" )

  }
  
  
  #-------------------------
  if (DS %in% c("historical.otherlfa.logbook", "historical.otherlfa.logbook.redo")) {

    filename = file.path( ss.exdatadirectory, "historical_logbook_otherlfa", "logbook.rdata" )
   
    if (DS=="historical.otherlfa.logbook") {
      load( filename )
      return(logbook)
    }
    # Re-do historical logbooks from tables
    x = jonah.logbook.db( DS="odbc.historical.otherlfa.logbook", p=p, yrs=1995:2001)
    names( x ) = tolower( names( x ) )
    x$land_community_code = paste(formatC(x$land_community_code, width=2, flag="0"))
    x$port = paste(x$land_prov_code, x$land_stat_dist_code, x$land_community_code, sep="")

    h.areas = jonah.logbook.db(DS="odbc.lfa.areas")
    names(h.areas) = tolower(names(h.areas))

    x = merge(x, h.areas, by="port")
    x$year = format(x$land_date, format="%Y")
    names(x)[names(x) == "live_wt"] <- "landings"

    x$date.fished = lubridate::ymd( x$land_date )
    x$landings = x$landings * 1000 # convert mt to kilograms

    colnames(x)
    to.extract = c( "year","land_date","landings","lfa", "date.fished")
    x = x[, to.extract ]
    logbook = x
    save (logbook, file=filename, compress=T )  # this is for plotting maps, etc

    return( "Complete" )

  }
  
  #--------------
  
  if (DS %in% c("logbook", "logbook.current", "logbook.current.redo", "logbook.redo")) {
    #browser()
    
    filename = file.path( ss.exdatadirectory, "jonah.logbook", "logbook.rdata" )
    filename2 = file.path( ss.exdatadirectory, "jonah.logbook", "logbook.current.rdata" )
    
    if (DS=="logbook") {
      load( filename )
      return(logbook)
    }
    
    if (DS == 'logbook.current') {
      load(filename2)
      return(logbook)
    }
    
    #---
    if (DS == 'logbook.current.redo'){
      # Re-do logbooks from marfissci tables
      x = jonah.logbook.db( DS="odbc.logbook", p=p, yrs=yrs)
      names( x ) = tolower( names( x ) )
      names( x ) = rename.snowcrab.variables(names( x ))
      
      x$month = month(x$date.landed)
      x$soak.time = x$soak_days * 24  # make into hours
      x$landings = x$pro_rated_slip_wt_lbs #* 0.454  # convert to kg
      x$lat =   round( as.numeric(substring(x$lat, 1,2)) + as.numeric(substring(x$lat, 3,6))/6000 ,6)
      x$lon = - round((as.numeric(substring(x$lon, 1,2)) + as.numeric(substring(x$lon, 3,6))/6000), 6)
      
      #Add in Fishing Areas
      areas = jonah.logbook.db(DS="odbc.areas")
      licence = jonah.logbook.db(DS="odbc.licence")
      licence.area = merge(licence, areas, by="AREA_ID", all.x=TRUE)
      names( licence.area ) = tolower( names( licence.area ) )
      area.extract = c("licence_id", "area_id", "area", "desc_eng")
      licence.area = licence.area[, area.extract]
      names(licence.area)[names(licence.area) == "licence_id"] <- "licence"
      
      x = merge(x, licence.area, by="licence", all.x=TRUE)
      names(x)[names(x) == "area"] <- "lfa_add"
      
      to_extract = c('date.fished', 'date.landed', 'year', 'month', 'licence', "vessel", 
                     'captain', 'species_code', 'lfa_add', 'target_spc', 'doc_type', 'lat', 
                     'lon', 'effort', 'target_spc', 'soak.time', 'landings', 'est_weight_log_lbs', 'effort', 'slip_weight_lbs')
      
      x = x[, to_extract]
      
      #Incorporate Effort for bycatch from the lobster log data
      s = x[which(substr(x$doc_type, 1, 2) == 'SD'),] #Anything starting with SD is lobster log
      e = s[which(s$date.landed == s$date.fished),] #To calculate effort properly the date landed = date fished
      #to_extract = c("licence", 'date.fished', 'effort')
      e = e[,to_extract]
      #l = ddply (s, .(date.fished, licence, vessel, captain, species_code, effort, year, lfa_add, target_spc, doc_type, month, soak.time, lat, lon, est_weight_log_lbs), summarize, landings = sum(landings))
      #l.log = merge(e, l, by=c("date.fished", "licence", "effort"))
      
      #to_extract = c('licence', 'date', 'vessel_name', 'submitter_name', 'lfa', 'date_fished', 'grid_num', 'weight_lbs', 
      #               'num_of_traps', 'grid_num_b', 'weight_lbs_b', 'num_of_traps_b', 'grid_num_c', 'weight_lbs_c', 
      #               'num_of_traps_c', 'month.x', 'yr', 'date.fished', 'date.landed','species_code', 'lfa_add', 
      #               'target_spc', 'doc_type', 'jonah.landings', 'est_weight_log_lbs', 'effort')
      
      
      ###  Determine the licence IDs for the Jonah Crab fisherman
      
      #Extract the sold data identified by the Crab Monitoring Document
      c.mon = x[which(substr(x$doc_type, 1,2) == 'MD'),]
      to.extract = c( "date.fished",  "licence", "vessel", "captain", "species_code", "effort","year","lfa_add", "target_spc", "doc_type", "month", "soak.time", "lat","lon","landings")
      c.mon= c.mon[, to.extract]
      
      #Join Crab Monitoring Documents and Lobster Log Data
      x = rbind(l.log, c.mon)
      
      #Calculate Catch Per Unit Effort
      #x$cpue = x$landings / x$effort
      #x$depth = x$depth_fm*1.83
      #names(x)[names(x) == "lfa_add"] <- "lfa"
      
      #to.extract = c( "year","lat","lon","landings","effort","soak.time",
      #                "cpue","licence", "date.fished", "lfa", "target_spc", "doc_type")
      #lb.marfis = x[, to.extract ]
      
      logbook = x
      save (logbook, file=filename2, compress=T )
      
    }
    
    
    if (DS == 'logbook.redo'){
    # Re-do logbooks from marfissci tables
    x = jonah.logbook.db( DS="odbc.logbook", p=p, yrs=yrs)
    names( x ) = tolower( names( x ) )
    names( x ) = rename.snowcrab.variables(names( x ))
    
    x$month = month(x$date.landed)
    x$soak.time = x$soak_days * 24  # make into hours
    x$landings = x$pro_rated_slip_wt_lbs * 0.454  # convert to kg
    x$lat =   round( as.numeric(substring(x$lat, 1,2)) + as.numeric(substring(x$lat, 3,6))/6000 ,6)
    x$lon = - round((as.numeric(substring(x$lon, 1,2)) + as.numeric(substring(x$lon, 3,6))/6000), 6)
    
    #Add in Fishing Areas
    areas = jonah.logbook.db(DS="odbc.areas")
    licence = jonah.logbook.db(DS="odbc.licence")
    licence.area = merge(licence, areas, by="AREA_ID", all.x=TRUE)
    names( licence.area ) = tolower( names( licence.area ) )
    area.extract = c("licence_id", "area_id", "area", "desc_eng")
    licence.area = licence.area[, area.extract]
    names(licence.area)[names(licence.area) == "licence_id"] <- "licence"
    
    x = merge(x, licence.area, by="licence", all.x=TRUE)
    names(x)[names(x) == "area"] <- "lfa_add"
    to_extract = c('date.fished', 'date.landed', 'year', 'month', 'licence', 'lfa_add', 'target_spc', 'doc_type', 'lat', 'lon', 'effort', 'soak.time', 'landings')
    x = x[, to_extract]
    
    #Incorporate Effort for bycatch from the lobster log data
    s = x[which(substr(x$doc_type, 1, 2) == 'SD'),] #Anything starting with SD is lobster log
    e = s[which(s$date.landed == s$date.fished),] #To calculate effort properly the date landed = date fished
    to_extract = c("licence", 'date.fished', 'effort')
    e = e[,to_extract]
    l = ddply (s, .(date.fished, licence, year, lfa_add, target_spc, doc_type, month, soak.time, lat, lon), summarize, landings = sum(landings))
    l.log = merge(e, l, by=c("date.fished", "licence"))
    
    #Extract the directed fishing data identified by the Crab Monitoring Document
    c.mon = x[which(substr(x$doc_type, 1,2) == 'MD'),]
    to.extract = c( "date.fished",  "licence","effort","year","lfa_add", "target_spc", "doc_type", "month", "soak.time", "lat","lon","landings")
    c.mon= c.mon[, to.extract]
    
    #Join Crab Monitoring Documents and Lobster Log Data
    x = rbind(l.log, c.mon)
    
    #Calculate Catch Per Unit Effort
    x$cpue = x$landings / x$effort
    #x$depth = x$depth_fm*1.83
    names(x)[names(x) == "lfa_add"] <- "lfa"
    
    to.extract = c( "year","lat","lon","landings","effort","soak.time",
                    "cpue","licence", "date.fished", "lfa", "target_spc", "doc_type")
    lb.marfis = x[, to.extract ]
    
    #Input Historical Landings
    lb.historical = jonah.logbook.db( DS="historical.logbook.lfa41" )
    lb.historical = lb.historical[which(!is.na(lb.historical$landings)),]
    lb.historical$soak.time <- "NA"
    lb.historical$licence <- "NA"
    lb.historical$lfa <- "41"
    #MG how do I know if the target species is Jonah or Lobster in the historical LFA 41 data?
    #Check to see if the comland data has target species
    lb.historical$target_spc <- "NA"
    lb.historical$doc_type <- "NA"
    lb.historical = lb.historical[, to.extract]
    
    y = NULL
    y = rbind( lb.historical, lb.marfis )
    
    lb.ho = jonah.logbook.db(DS="historical.otherlfa.logbook")
    lb.ho$licence <- "NA"
    lb.ho$effort <- "NA"
    lb.ho$cpue <- "NA"
    lb.ho$lat <- "NA"
    lb.ho$lon <- "NA"
    lb.ho$target_spc <- "NA"
    lb.ho$doc_type <- "NA"
    lb.ho$soak.time <- "NA"
    
    to.extract = c( "year","lat","lon","landings","effort","soak.time","cpue","licence", "date.fished", "lfa", "target_spc", "doc_type")
    ho = lb.ho[, to.extract]
    
    u = rbind(y, ho)
    
    u$yr = u$year
    logbook = u
    save (logbook, file=filename, compress=T )
    
    return( "Complete" )
    }
  }
  
  #Other Stuff
  #-----------------------------------------------------
  #-----------------------------------------------------

  if (DS %in% c("odbc.licence.redo", "odbc.licence" ) ) {

    filename.licence = file.path( ss.exdatadirectory, "logbook", "lic.datadump.rdata" )


    if (DS=="odbc.licence") {
      load(filename.licence)
      return (lic)
    }

    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    lic = sqlQuery(con, "select * from marfissci.licence_areas")
    save(lic, file=filename.licence, compress=T)

  }
  #-------------------------
  
  if (DS %in% c("jonah.licence.redo", "jonah.licence" ) ) {
    
    filename.jlicence = file.path( ss.exdatadirectory, "logbook", "jlic.datadump.rdata" )
    
    
    if (DS=="jonah.licence") {
      load(filename.jlicence)
      return (x)
    }
    
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    lic = sqlQuery(con, "select * from marfissci.licences")
    area.id = sqlQuery(con, "select * from marfissci.licence_areas")
    desc = sqlQuery(con, "select * from marfissci.areas")
    
    x = merge(area.id, desc, by = 'AREA_ID') 
    lic = lic[which(lic$SPECIES_CODE == "703"), ]
    lic = lic[which(lic$EXPIRY_DATE == "4444-12-31"), ]
    
    x = merge(lic, x, by = 'LICENCE_ID', all.x = T)
    
    names(x) = tolower(names(x))
    
    to.extract = c('licence_id', 'area','species_code', 'start_date_time', 'licence_type_id', 'desc_eng', 
                   'season_start_mm', 'season_start_dd', 'season_end_mm', 'season_end_dd')
    
    x = x[ , to.extract]
    
    save(x, file=filename.jlicence, compress=T)
    
  }
  #-------------------------

  if (DS %in% c("odbc.areas.redo", "odbc.areas" ) ) {

    filename.areas = file.path( ss.exdatadirectory, "logbook", "areas.datadump.rdata" )

    if (DS=="odbc.areas") {
      load(filename.areas)
      return (areas)
    }

    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    areas = sqlQuery(con, "select * from marfissci.areas")
    save(areas, file=filename.areas, compress=T)

    return ("Complete")

  }

  if (DS %in% c("odbc.lfa.areas.redo", "odbc.lfa.areas" ) ) {

    filename.lfa.areas = file.path( ss.exdatadirectory, "logbook", "lfa.areas.datadump.rdata" )

    if (DS=="odbc.lfa.areas") {
      load(filename.lfa.areas)
      return (h.areas)
    }

    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    h.areas = sqlQuery(con, "select * from frailc.lfa_port")
    save(h.areas, file=filename.lfa.areas, compress=T)

    return ("Complete")

  }

  if (DS %in% c("odbc.species.redo", "odbc.species" ) ) {

    filename.species = file.path( ss.exdatadirectory, "logbook", "species.datadump.rdata" )

    if (DS=="odbc.species") {
      load(filename.areas)
      return (species)
    }

    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    species = sqlQuery(con, "select * from marfissci.species")
    species = species[order(species$DESC_ENG),]
    species = species[, c(1,3)]
    save(species, file=filename.species, compress=T)

    return ("Complete")

  }
  #-----------------------
    }


