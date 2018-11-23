hagfish.logbook.db = function( DS, p=NULL, yrs = NULL) {
#browser()
  #Species Codes
  # Jonh Crab 703, Sea Cucumber 619, whelk 615, hagfish 197
  yrs = 2000:2017

  #--------------------------------------------------------------------
  if (DS %in% c("odbc.logbook", "odbc.logbook.redo")) {
    fn.root =  file.path( ss.exdatadirectory, "seacucumber.logbook", "datadump" )
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
      "SELECT * from marfissci.pro_spc_info",
      "where SPECIES_CODE=619",
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
  
  if (DS %in% c("odbc.effort", "odbc.effort.redo")) {
    
    fn.root =  file.path( ss.exdatadirectory, "hagfish.logbook", "effort" )
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
    
    filename = file.path( ss.exdatadirectory, "logbook", "hagfish.effort.rdata" )
    
    if (DS=="effort") {
      load( filename )
      return(x)
    }
    
    #---
    # Re-do logbooks from marfissci tables
    x = hagfish.logbook.db( DS="odbc.licence.h", p=p, yrs=2000:2017)
    y = hagfish.logbook.db(DS = "odbc.effort",  p=p, yrs = 2000:2017)   
    names(y) = tolower(names(y))
    
   #---------------------------------------------------------------------- 
    to.extract = c('log_efrt_std_info_id', 'column_defn_id', 'data_value')
    y = y[, to.extract]
    
    x$month = month(x$fv_fished_datetime)
    x$yr = year(x$fv_fished_datetime)
    x$lat =   round( as.numeric(substring(x$ent_latitude, 1,2)) + as.numeric(substring(x$ent_latitude, 3,6))/6000 ,6)
    x$lon = - round((as.numeric(substring(x$ent_longitude, 1,2)) + as.numeric(substring(x$ent_longitude, 3,6))/6000), 6)
    
    to_extract = c('log_efrt_std_info_id')
    
    x.s = x[, to_extract]
    x.s = as.data.frame(x.s)
    colnames(x.s) <- 'log_efrt_std_info_id'
    #x.s = x.s[sample(nrow(x.s), 200),]
    #x.s = as.data.frame(x.s)
    #colnames(x.s) <- 'log_efrt_std_info_id'
    
    #Process Effort
    x.m = merge(x.s, y, by = "log_efrt_std_info_id", all.x = T, na.rm = T)
    x.m = x.m[complete.cases(x.m), ]
    
    #column definition ids
    #marfissci.column_defns
    
    x.m = x.m[ which(x.m$column_defn_id == '19'| #date hauled
                       x.m$column_defn_id == '40'| #Depth
                       x.m$column_defn_id == '142'| #Time Set
                       x.m$column_defn_id == '115'| #Traps Set
                       x.m$column_defn_id == '116'| #Traps Lost
                       x.m$column_defn_id == '117'| #Traps Retrieved
                       x.m$column_defn_id == '828'), ] #Set number
    
    x.m = unique(x.m)
    x.t = dcast(x.m, log_efrt_std_info_id ~ column_defn_id, value.var = "data_value",  drop = F)

    colnames(x.t)[1] <- 'effort_id'
    colnames(x.t)[2] <- 'date_hauled'
    colnames(x.t)[3] <- 'depth'
    colnames(x.t)[7] <- 'time_set'
    colnames(x.t)[4] <- 'traps_set'
    colnames(x.t)[5] <- 'traps_lost'
    colnames(x.t)[6] <- 'traps_retrieved'
    colnames(x.t)[8] <- 'set_number'
    
    colnames(x)[11] <- "effort_id"
    
    x = merge(x, x.t, by = "effort_id", all.x = T)
    
    x$traps_retrieved = as.numeric(x$traps_retrieved)
    x$traps_set = as.numeric(x$traps_set)
    
    x = x[order(-x$traps_set),]
    #Fix some data errors
    x[1,27] <- 100
    
    t = x[is.na(x$traps_retrieved), 25]
    x[is.na(x$traps_retrieved), 27] <- t
    
    save (x, file=filename, compress=T )
    
    return( "Complete" )
  }
  
  #---------------------------------------------
   if (DS %in% c("logbook", "logbook.redo")) {
    #browser()

    filename = file.path( ss.exdatadirectory, "logbook", "seacucumber.logbook.rdata" )

    if (DS=="logbook") {
      load( filename )
      return(logbook)
    }

    #---

    # Re-do logbooks from marfissci tables
    x = seacucumber.logbook.db( DS="odbc.logbook", p=p, yrs=2000:2017)
    names( x ) = tolower( names( x ) )
    names( x ) = rename.snowcrab.variables(names( x ))
    
    y = hagfish.logbook.db(DS="odbc.effort", p=p, yrs = 2000:2017)
    names( y ) = tolower( names( y ) )
    to.extract = c('log_efrt_std_info_id', 'column_defn_id', 'data_value')
    y = y[, to.extract]

    #t8.log_efrt_std_info_id = t3.log_efrt_std_info_id(+)

    x$date.landed = x$landed_date
    x$month = month(x$date.fished)
    x$yr = year(x$date.fished)
    x$landings = x$rnd_weight_kgs  # check to see if this is prorated
    x$lat =   round( as.numeric(substring(x$lat, 1,2)) + as.numeric(substring(x$lat, 3,6))/6000 ,6)
    x$lon = - round((as.numeric(substring(x$lon, 1,2)) + as.numeric(substring(x$lon, 3,6))/6000), 6)

    to_extract = c('log_efrt_std_info_id')
    
    x.s = x[, to_extract]
    x.s = as.data.frame(x.s)
    colnames(x.s) <- 'log_efrt_std_info_id'
    #x.s = x.s[sample(nrow(x.s), 200),]
    #x.s = as.data.frame(x.s)
    #colnames(x.s) <- 'log_efrt_std_info_id'
    
    #logbook = x
    #save (logbook, file=filename, compress=T )
    
    #Process Effort
    x.m = merge(x.s, y, by = "log_efrt_std_info_id", all.x = T, na.rm = T)
    x.m = x.m[complete.cases(x.m), ]

    x.m = x.m[ which(x.m$column_defn_id == '19'| x.m$column_defn_id == '40'| x.m$column_defn_id == '142'| 
                       x.m$column_defn_id == '115'| x.m$column_defn_id == '116'| x.m$column_defn_id == '117'| 
                       x.m$column_defn_id == '828'), ]
    
    x.m = unique(x.m)
    x.t = dcast(x.m, log_efrt_std_info_id ~ column_defn_id, value.var = "data_value",  drop = F)
    

    colnames(x.t)[1] <- 'effort_id'
    colnames(x.t)[2] <- 'date_hauled'
    colnames(x.t)[3] <- 'depth'
    colnames(x.t)[7] <- 'time_set'
    colnames(x.t)[4] <- 'traps_set'
    colnames(x.t)[5] <- 'traps_lost'
    colnames(x.t)[6] <- 'traps_retrieved'
    colnames(x.t)[8] <- 'set_number'
    
    colnames(x)[5] <- "effort_id"
    
    x = merge(x, x.t, by = "effort_id", all = T)
    
    #Fix some data errors
    x[which(x$effort_id == '2243265'), 42] <- 100
    
    t = x[is.na(x$traps_retrieved), 42]
    x[is.na(x$traps_retrieved), 44] <- t
    
    
    to_extract = c('date.fished', 'date.landed', 'yr', 'month', 'lat', 'lon', 'landings', "effort_id", "date_hauled", 
                   "depth", "traps_set", "traps_lost", "traps_retrieved", "time_set", "set_number", 'licence', 'nafo_unit_area_id')

    x = x[, to_extract]
    logbook = x
    save (logbook, file=filename, compress=T )

    return( "Complete" )

  }

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
  if (DS %in% c("odbc.nafo.redo", "odbc.nafo" ) ) {
    
    filename.nafo = file.path( ss.exdatadirectory, "logbook", "nafo.datadump.rdata" )
    
    if (DS=="odbc.nafo") {
      load(filename.nafo)
      return (nafo)
    }
    
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    nafo = sqlQuery(con, "select * from marfissci.nafo_unit_areas")
    save(nafo, file=filename.licence, compress=T)
    
  }

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