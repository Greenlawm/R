urchin.logbook.db = function( DS, p=NULL, yrs = NULL) {
#browser()
  #Species Codes
  # Jonh Crab 703, Sea Cucumber 619, whelk 615, hagfish 197, urchin 930
  yrs = 2000:2017
#--------------------------------------------------------------------

  if (DS %in% c("odbc.logbook", "odbc.logbook.redo")) {
    fn.root =  file.path( ss.exdatadirectory, "urchin.logbook", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )

  if (DS=="odbc.logbook") {
    out = NULL
    for ( YR in yrs ) {
      fny = file.path( fn.root, paste(YR, "rdata", sep="."))
      if (file.exists(fny)) {
        load (fny)
        print(YR)
        out = rbind( out, logbook )
      }
    }
    return (out)
  }
    
   
  #New Database Export of logbooks
  require(RODBC)
  con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)

  for ( YR in yrs ) {
    fny = file.path( fn.root, paste(YR, "rdata", sep="."))
    query = paste(
      "SELECT * from marfissci.pro_spc_info",
      "where SPECIES_CODE=650",
      "AND EXTRACT(YEAR from DATE_FISHED) = ", YR )
    logbook = NULL
    logbook = sqlQuery(con, query )
    save( logbook, file=fny, compress=T)
    gc()  # garbage collection
    print(YR)
  }
  odbcClose(con)
  return(yrs)

  }
  
  if (DS %in% c("odbc.effort", "odbc.effort.redo")) {
    
    fn.root =  file.path( ss.exdatadirectory, "urchin.logbook", "effort" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    
    if (DS=="odbc.effort") {
      out = NULL
      fny = file.path( fn.root, paste("rdata", sep="."))
      if (file.exists(fny)) {
        load (fny)
        out = logbook
      }
      return (out)
    }
    
    #New Database Export of effort
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    
    fny = file.path( fn.root, paste( "rdata", sep="."))
    query = paste("SELECT * from marfissci.log_efrt_entrd_dets")
    logbook = NULL
    logbook = sqlQuery(con, query )
    save( logbook, file=fny, compress=T)
    gc()  # garbage collection
    
    odbcClose(con)
  }
  
  #--------------
  #Extract all the effort, even if there were no landings
  if (DS %in% c("effort", "effort.redo")) {
    #browser()
    
    filename = file.path( ss.exdatadirectory, "logbook", "urchin.effort.rdata" )
    
    if (DS=="effort") {
      load( filename )
      return(x)
    }
    
    #---
    # Re-do logbooks from marfissci tables
    #x = urchin.logbook.db( DS="odbc.licence.h", p=p, yrs=2000:2017)
    x = urchin.logbook.db(DS = "odbc.logbook")
    
    names( x ) = tolower( names( x ) )
    
    x$date.landed = x$date_fished
    x$month = month(x$date.landed)
    x$yr = year(x$date.landed)
    x$landings = x$rnd_weight_kgs  # check to see if this is prorated
    x$lat =   round( as.numeric(substring(x$latitude, 1,2)) + as.numeric(substring(x$latitude, 3,6))/6000 ,6)
    x$lon = - round((as.numeric(substring(x$longitude, 1,2)) + as.numeric(substring(x$longitude, 3,6))/6000), 6)
    
    y = urchin.logbook.db(DS = "odbc.effort",  p=p, yrs = 2000:2017)  
    z = urchin.logbook.db(DS = "odbc.columndefns")
    names(y) = tolower(names(y))
    names(z) = tolower(names(z))
    
    #---------------------------------------------------------------------- 
    to.extract = c('log_efrt_std_info_id', 'column_defn_id', 'data_value')
    y = y[, to.extract]
    y = merge(y, z, by='column_defn_id', all.x = T)
    
    x.1 = merge(x, y, by = 'log_efrt_std_info_id', all.x = T )
    
    to_extract = c('log_efrt_std_info_id', 'column_defn_id', 'data_value')
    z = x.1[, to.extract]
    
    #Find unique combinations of column_defn_id and desc_eng
    un = x.1[, c("column_defn_id", "desc_eng")]
    un = unique(un)
   
    z = unique(z)
    z = dcast(z, log_efrt_std_info_id ~ column_defn_id, value.var = "data_value",  drop = T)
    
    colnames(z)[1] <- "effort.id"
    colnames(z)[2] <- "avg.speed.kts"
    colnames(z)[3] <- 'start.time'
    colnames(z)[4] <- 'end.time'
    
    colnames(z)[5] <- 'avg.diver.hrs'
    colnames(z)[6] <- 'avg.diver.mins'
    colnames(z)[7] <- 'tow.dur.hrs'
    colnames(z)[8] <- 'tow.dur.mins'
    colnames(z)[9] <- 'no.tows'
    colnames(z)[10] <- 'no.divers'
    colnames(z)[11] <- 'avg.time.tow'
    colnames(z)[12] <- 'depth'
    colnames(z)[13] <-'comments'
    colnames(z)[14] <- 'date.fished.2'
    colnames(z)[15] <- 'no.tows.2'
    colnames(z)[16] <- 'uom.default.ft'
    colnames(z)[17] <- 'fishing.area.no'
    colnames(z)[18] <- 'bottom.temp'
    
    colnames(x)[5] <- 'effort.id'
    
    x = unique(x)

    x = merge(x, z, by = "effort.id", all.x = T)
    
    m = urchin.logbook.db(DS = "odbc.mondoc")
    names(m) = tolower(names(m))
 
    x = merge(x, m, by = "mon_doc_id", all.x = T)
     #Mon doc id 52: LFAs 36 & 38, 18: Nova Scotia
    
    #Fishing Area IDs: 621 - LFA 36, 627 - LFA 38 Zone 1, 628 - LFA 38 Zone 2
    x$zone[x$mon_doc_defn_id == "52"] <- "SWNB"
    x$zone[x$fishing_area_id == "621"] <- "LFA 36"
    x$zone[x$fishing_area_id == "627"] <- "LFA 38 Zone 1"
    x$zone[x$fishing_area_id == "628"] <- "LFA 38 Zone 2"
    x$zone[x$mon_doc_defn_id == "18"] <- "NS"
    x =  x[which(x$mon_doc_defn_id != "39"),]
    
    x$gear[x$gear_code == "76"] <- "Dive" 
    x$gear[x$gear_code == "71"] <- "Drag"
    x$gear[x$gear_code == "10"] <- "Drag"
    x$gear[x$gear_code == "75"] <- "Dive"
    
    x$zone_type <- paste(x$zone, x$gear, sep = " ") 
    
    #Testing
    #test = x[, c("zone", "gear_code", "gear")]
    test = x[, c("zone_type")]
    table(test)
    
    #test = x[, c("zone", "gear_code", "no.tows", "no.divers")]
    unique(test)
     
 
    #change to fishing year
    x$f.year = ""
    
    x$f.year <- x$yr
    
    s = x[which(x$zone== 'LFA 36' & x$month >= 10), c("yr")]
    x[which(x$zone== 'LFA 36' & x$month >= 10), c("f.year")] <- s + 1
    
    s = x[which(x$zone== 'LFA 38 Zone 1' & x$month == 12), c("yr")]
    x[which(x$zone== 'LFA 38 Zone 1' & x$month == 12), c("f.year")] <- s + 1
    
    s = x[which(x$zone== 'LFA 38 Zone 2' & x$month == 12), c("yr")]
    x[which(x$zone== 'LFA 38 Zone 2' & x$month == 12), c("f.year")] <- s + 1
    
    s = x[which(x$zone== 'NS' & x$month >= 9), c("yr")]
    x[which(x$zone== 'NS' & x$month >= 9), c("f.year")] <- s + 1
  
    save (x, file=filename, compress=T )
    
    return( "Complete" )
  }
  

    #if (DS %in% c("logbook", "logbook.redo")) {
    #browser()

    #filename = file.path( ss.exdatadirectory, "logbook", "urchin.logbook.rdata" )

    #if (DS=="logbook") {
    #  load( filename )
    #  return(logbook)
    #}

    #---
    # Re-do logbooks from marfissci tables
    #x = urchin.logbook.db( DS="odbc.logbook", p=p )
    #names( x ) = tolower( names( x ) )

    #x$date.landed = x$date_fished
    #x$month = month(x$date.landed)
    #x$yr = year(x$date.landed)
    #x$landings = x$rnd_weight_kgs  # check to see if this is prorated
    #x$lat =   round( as.numeric(substring(x$latitude, 1,2)) + as.numeric(substring(x$latitude, 3,6))/6000 ,6)
    #x$lon = - round((as.numeric(substring(x$longitude, 1,2)) + as.numeric(substring(x$longitude, 3,6))/6000), 6)
    
    #y = urchin.logbook.db(DS = "odbc.licence")
    #names(y) = tolower(names(y))
    #to_extract = c('licence_id', 'area_id')
    #y = y[, to_extract]
    #x = merge(x, y, by = "licence_id", all.x = T)

    #z = urchin.logbook.db(DS = "odbc.areas")
    #names(z) = tolower(names(z))
    #to_extract = c('area_id', 'area', 'desc_eng')
    #z = z[, to_extract]
    #x = merge(x, z, by ="area_id", all.x = T)

    #x$zone[x$area == "GUYSBOROUGH" |x$area == "CAPE BRETON" |x$area == "VICTORIA SOU CAPE NR RICHMOND" |
    #         x$area == "HALIFAX EAST" |x$area == "HFX EAST - EX" | x$area == "RICHMOND - EX" |x$area == "CB MAIN A DIEU" |
    #         x$area == "CB MAIN A DIEU BEACH" |x$area == "CB SCATARIE" |x$area == "HAL JEDDORE"|x$area == "GUYS SHEET HARBOUR"|
    #         x$area == "GUYS SOUTH MOOSEHEAD" |x$area == "GUYS MURPHY COVE"|x$area == "HAL OWL'S HEAD"|
    #         x$area == "GUYS CHEDABUCTO BAY"|x$area == "VICTORIA SOU CAPE NR"|x$area == "RICHMOND"|x$area == "CB COUNTY - EX"] <- "ENS"
   
    #x$zone[x$area == "SHELBURNE" |x$area == "DIGBY" |x$area == "HFX WEST PENNANT PT" |x$area == "HFX WEST - EX" |
    #           x$area == "SHELBURNE - EX" |x$area == "HAL PT PLEASANT PARK" |x$area == "HAL KETCH HEAD" |x$area == "HAL HALIFAX HBR WEST" |
    #           x$area == "SHEL WESTERN HEAD" |x$area == "HAL PENNANT POINT" |x$area == "SHEL CAPE SABLE ISL" |x$area == "SHEL LOCKEPORT HBR" |
    #           x$area == "SHEL EAST CAPE SABLE" |x$area == "SHEL RASPBERRY HEAD" |x$area == "SHEL NEGRO HARBOUR" |x$area == "GUYSBOROUGH"|
    #         x$area == "SHEL BARRINGTON BAY" |x$area == "SHELBURNE COUNTY BARRINGTON BAY" ] <- "SWNS"
    
    #$x$zone[x$area == "LFA 38"] <- "LFA38"
    
    #x$zone[x$area == "LFA 36"] <- "LFA36"
    
    #x$zone[x$area == "GENERAL"] <- "OTHER"
    
    #change to fishing year
    #x$f.year = ""
    
    #x$f.year <- x$yr
    
    #s = x[which(x$zone== 'LFA36' & x$month >= 10), c("yr")]
    #x[which(x$zone== 'LFA36' & x$month >= 10), c("f.year")] <- s + 1
    
    #s = x[which(x$zone== 'LFA38' & x$month == 12), c("yr")]
    #x[which(x$zone== 'LFA38' & x$month == 12), c("f.year")] <- s + 1
    
    #s = x[which(x$zone== 'SWNS' & x$month >= 9), c("yr")]
    #x[which(x$zone== 'SWNS' & x$month >= 9), c("f.year")] <- s + 1
    
  #  logbook = x
  #  save (logbook, file=filename, compress=T )

   # return( "Complete" )

  #}

 #-------------------------
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

    filename.areas = file.path( ss.exdatadirectory, "logbook", "lfa.areas.datadump.rdata" )

    if (DS=="odbc.lfa.areas") {
      load(filename.areas)
      return (areas)
    }

    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    h.areas = sqlQuery(con, "select * from frailc.lfa_port")
    save(h.areas, file=filename.areas, compress=T)

    return ("Complete")

  }

  if (DS %in% c("odbc.species.redo", "odbc.species" ) ) {

    filename.species = file.path( ss.exdatadirectory, "logbook", "species.datadump.rdata" )

    if (DS=="odbc.species") {
      load(filename.species)
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
  
  
  if (DS %in% c("odbc.columndefns.redo", "odbc.columndefns" ) ) {
    
    filename = file.path( ss.exdatadirectory, "logbook", "columndefns.datadump.rdata" )
    
    if (DS=="odbc.columndefns") {
      load(filename)
      return (species)
    }
    
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    species = sqlQuery(con, "select * from marfissci.column_defns")
    species = species[, c(1, 3)]
    save(species, file=filename, compress=T)
    
    
    return ("Complete")
    
  }
  
  
  if (DS %in% c("odbc.mondoc.redo", "odbc.mondoc" ) ) {
    
    filename = file.path( ss.exdatadirectory, "logbook", "mondoc.datadump.rdata" )
    
    if (DS=="odbc.mondoc") {
      load(filename)
      return (s)
    }
    
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    s = sqlQuery(con, "select * from marfissci.mon_docs")
    save(s, file=filename, compress=T)
    
    
    return ("Complete")
    
  }
  
  
  if (DS %in% c("odbc.licence.h.redo", "odbc.licence.h" ) ) {
    
    filename.licence = file.path( ss.exdatadirectory, "logbook", "hagfish.licence.datadump.rdata" )
    
    if (DS=="odbc.licence.h") {
      load(filename.licence)
      return (x)
    }
    
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    
    l   = sqlQuery(con, "select * from marfissci.licences")
    #lp  = sqlQuery(con, "select * from marfissci.licence_participants")
    #pt  = sqlQuery(con, "SELECT * FROM marfissci.participants", as.is = T )
    mdl = sqlQuery(con, "select * from marfissci.mon_doc_lics")
    m   = sqlQuery(con, "select * from marfissci.mon_docs")
    md  = sqlQuery(con, "select * from marfissci.mon_doc_defns")
    ed  = sqlQuery(con, "select * from marfissci.log_efrt_entrd_dets")
    ei = sqlQuery(con, "select * from marfissci.log_efrt_std_info")
    cs = sqlQuery (con, "select * from marfissci.catch_usages")
    cd = sqlQuery (con, "select * from marfissci.column_defns")
    
    names( l ) = tolower( names( l ) )
    names( pt ) = tolower( names( pt ) )
    names( lp ) = tolower( names( lp ) )
    names( mdl) = tolower(names (mdl))
    names(m) = tolower(names(m))
    names(ei) = tolower(names(ei))
    names(ed) = tolower(names(ed))
    names(md) = tolower(names(md))
    names(cd) = tolower(names(cd))
    
    
    l = l[l$species_code == 197, ]

    #x = merge(l, lp, by = "licence_id", all.x = T)
    #x = merge(x, pt, by = "fin", all.x = T)
    x = merge(l, mdl, by = "licence_id", all.x = T)
    x = merge(x, m, by = "mon_doc_id")
    x = merge(x, ei, by="mon_doc_id")
    x = merge(x, md, by="mon_doc_defn_id")
    x = merge(x, ed, by="log_efrt_std_info_id", all.x = T)
    
    #to_extract = c('licence_id', 'species_code', 'origin_date', 'licence_subtype_id', 
    #               'expiry_date', 'fin', 'surname', 'firstname', "mon_doc_id", "mon_doc_lic_id", 
    #               "document_title", "licence_type_id", "trip_id", "log_efrt_std_info_id", "mon_doc_defn_id", 
    #               'fv_fished_datetime', 'fv_num_of_gear_units', 'fv_gear_code.x', 'ent_latitude', 'ent_longitude', 'data_value')
    
    to_extract = c('licence_id', 'species_code', 'origin_date', 'licence_subtype_id', 
                   'expiry_date', "mon_doc_id", "mon_doc_lic_id", 
                   "document_title", "licence_type_id", "trip_id", "log_efrt_std_info_id", "mon_doc_defn_id", 
                   'fv_fished_datetime', 'fv_num_of_gear_units', 'fv_gear_code.x', 'ent_latitude', 'ent_longitude', 'data_value')
    
    x = x[, to_extract]
    save(x, file = filename.licence, compress = T)
    
    gc()  # garbage collection
    odbcClose(con)
    
    return ("Complete")
    
  }
    }