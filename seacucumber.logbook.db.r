seacucumber.logbook.db = function( DS, p=NULL, yrs = yrs) {
#browser()
  #Species Codes
  # Jonh Crab 703, Sea Cucumber 619, whelk 615, hagfish 197
  yrs = 2000:2019
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

    fn.root =  file.path( ss.exdatadirectory, "seacucumber.logbook", "effort" )
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
    x = seacucumber.logbook.db( DS="odbc.logbook", p=p, yrs=2000:2018)
    names( x ) = tolower( names( x ) )
    names( x ) = rename.snowcrab.variables(names( x ))
    
    y = seacucumber.logbook.db(DS="odbc.effort", p=p, yrs = 2000:2018)
    names( y ) = tolower( names( y ) )
    to.extract = c('log_efrt_std_info_id', 'column_defn_id', 'data_value')
    y = y[, to.extract]

    x$date.landed = x$landed_date
    x$month = month(x$date.fished)
    x$yr = year(x$date.fished)
    x$vr_number = x$vr_number_fishing
    x$landings = x$rnd_weight_kgs  # check to see if this is prorated
    x$lat =   round( as.numeric(substring(x$lat, 1,2)) + as.numeric(substring(x$lat, 3,6))/6000 ,6)
    x$lon = - round((as.numeric(substring(x$lon, 1,2)) + as.numeric(substring(x$lon, 3,6))/6000), 6)
    
    #to_extract = c('log_efrt_std_info_id')
 
  #Run if you're looking for effort columns on log sheets
  #---------------------------------------------------   
    ##column definition ids
    #marfissci.column_defns
    c = seacucumber.logbook.db(DS = "odbc.columndefs")
    names( c ) = tolower( names( c ) )
    to.extract = c('column_defn_id', 'desc_eng')
    c = c[, to.extract]
    
    col.defs = merge(x, y , by = "log_efrt_std_info_id", all.x = T)
    col.defs = merge(col.defs, c, by = "column_defn_id", all.x = T)
    to.extract = c('column_defn_id', 'desc_eng')
    defs = col.defs[, to.extract]
    unique(defs)

  #Process Effort Data
  #---------------------------------------------------   
    to_extract = c('log_efrt_std_info_id', 'column_defn_id', 'data_value')
    x.s = col.defs[, to_extract]
    x.s = as.data.frame(x.s)
    
    x.m = x.s
    
    #Process Effort
    x.m = x.m[complete.cases(x.m), ]

    x.m = x.m[ which(x.m$column_defn_id == '184'| x.m$column_defn_id == '187'|x.m$column_defn_id == '167'| 
                       x.m$column_defn_id == '545'| x.m$column_defn_id == '813'| x.m$column_defn_id == '814'
                     | x.m$column_defn_id == '612' | x.m$column_defn_id == '565'), ]
    
    x.m = unique(x.m)
    x.t = dcast(x.m, log_efrt_std_info_id ~ column_defn_id, value.var = "data_value",  drop = F)
    

    colnames(x.t)[1] <- 'effort_id'
    colnames(x.t)[2] <- 'start_time'
    colnames(x.t)[3] <- 'tow_duration_mins'
    colnames(x.t)[4] <- 'straight_curve'
    colnames(x.t)[5] <- 'tow_dist_nm'
    colnames(x.t)[6] <- 'no.tows'
    colnames(x.t)[7] <- 'no.totes'
    colnames(x.t)[8] <- 'end_lat'
    colnames(x.t)[9] <- 'end_long'
    
    colnames(x)[5] <- "effort_id"
    
    x = merge(x, x.t, by = "effort_id", all.x = T)
    
    to_extract = c('date.fished', 'date.landed', 'vr_number', 'trip_id', 'yr', 'month', 'lat', 'lon', 'landings', 'licence', 'nafo_unit_area_id', 
                   "fishing_area_id", "start_time", "no.totes", "no.tows", "tow_duration_mins", "straight_curve", "tow_dist_nm", "end_lat", "end_long")

    
    #test.1 = x[which(x$licence == '306893' & x$yr == 2018),]
    #test.1 = test.1[ order(test.1$date.landed),]
    
    x = x[, to_extract]
    x$end_lat = as.numeric(x$end_lat)
    x$end_long = as.numeric(x$end_long)
    
    x$end_lat =   round( as.numeric(substring(x$end_lat, 1,2)) + as.numeric(substring(x$end_lat, 3,6))/6000 ,6)
    x$end_long = - round((as.numeric(substring(x$end_lon, 1,2)) + as.numeric(substring(x$end_lon, 3,6))/6000), 6)
    logbook = x
    save (logbook, file=filename, compress=T )

    return( "Complete" )

  }

  #-----------------------------------------------
  if (DS %in% c("historical.logbook", "historical.logbook.redo")){
    
    fn.root =  file.path( ss.exdatadirectory, "seacucumber.logbook", "historical.r" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    fny = file.path( fn.root, paste("rdata", sep="."))
    
    

    if (DS=="historical.logbook") {
      load( fny )
      return(x)
    }
    
    
    library(xlsx)
    z <- read.xlsx("C:/ss/data/seacucumber.logbook/historical/Ericlog0118_imp.xlsx", 1) #306893
    y <- read.xlsx("c:/ss/data/seacucumber.logbook/historical/LOG200118_imp.xlsx", 1) #306891
   
    names( z ) = tolower( names( z ) )
    names( y ) = tolower( names( y ) )
    drop = c("na.")
    z = z[, !(names(z) %in% drop)]
    z$licence_id = "306893"
    y$licence_id = "306891"
    y$area = ""
  
    x = rbind(z,y)
    x$lat = x$start_lat
    x$lon = x$start_long
    
    x$date.fished = lubridate::dmy( x$date_fished )
    #names( x ) = rename.snowcrab.variables(names( x ))

    x$month = lubridate::month(x$date.fished)
    x$yr = lubridate::year(x$date.fished)
    x$lat =   round( as.numeric(substring(x$lat, 1,2)) + as.numeric(substring(x$lat, 3,6))/60 ,6)
    x$lon = - round((as.numeric(substring(x$lon, 1,2)) + as.numeric(substring(x$lon, 3,6))/60), 6)
    x$end_lat =  round( as.numeric(substring(x$end_lat, 1,2)) + as.numeric(substring(x$end_lat, 3,6))/60 ,6)
    x$end_long = - round((as.numeric(substring(x$end_long, 1,2)) + as.numeric(substring(x$end_long, 3,6))/60), 6)
    x$tow_dist_nm = x$tow_distance_nm
    x$licence = x$licence_id
    
    to_extract = c("date.fished", "yr" ,"month", "lat","lon", "licence", "trip_id",  "gear_code", "port_code", "area", "set_time", "haul_time", 
                   "end_lat", "end_long", "tow_duration_mins", "no_tows","tow_dist_nm", 
                   "acon_distance_km", "curve","no_totes", "discards_kg" ,"max_depth" ,"min_depth" )
    
    x = x[, to_extract] 
    save( x, file=fny, compress=T)
    
    
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
  if (DS %in% c("odbc.columndefs.redo", "odbc.columndefs" ) ) {
    
    filename.licence = file.path( ss.exdatadirectory, "logbook", "coldef.datadump.rdata" )
    
    if (DS=="odbc.columndefs") {
      load(filename.licence)
      return (col.def)
    }
    
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
    col.def = sqlQuery(con, "select * from marfissci.column_defns")
    save(col.def, file=filename.licence, compress=T)
    
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