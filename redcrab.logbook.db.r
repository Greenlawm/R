redcrab.logbook.db = function( DS, p=NULL, yrs = NULL) {
#browser()
  #Species Codes
  # Jonh Crab 703, Sea Cucumber 619, whelk 615, hagfish 197, redcrab 706
  yrs = 2000:2017

  #--------------------------------------------------------------------

  if (DS %in% c("odbc.logbook", "odbc.logbook.redo")) {
    fn.root =  file.path( ss.exdatadirectory, "redcrab.logbook", "datadump" )
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
      "where SPECIES_CODE=706",
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

  #--------------

  if (DS %in% c("logbook", "logbook.redo")) {
    #browser()

    filename = file.path( ss.exdatadirectory, "logbook", "redcrab.logbook.rdata" )


    if (DS=="logbook") {
      load( filename )
      return(logbook)
    }

    #---

    # Re-do logbooks from marfissci tables
    x = redcrab.logbook.db( DS="odbc.logbook", p=p, yrs=2000:2017)
    names( x ) = tolower( names( x ) )
    names( x ) = rename.snowcrab.variables(names( x ))

    x$date.landed = x$landed_date
    x$month = month(x$date.fished)
    x$yr = year(x$date.fished)
    x$landings = x$rnd_weight_kgs  # check to see if this is prorated
    x$lat =   round( as.numeric(substring(x$lat, 1,2)) + as.numeric(substring(x$lat, 3,6))/6000 ,6)
    x$lon = - round((as.numeric(substring(x$lon, 1,2)) + as.numeric(substring(x$lon, 3,6))/6000), 6)


    to_extract = c('date.fished', 'date.landed', 'yr', 'month', 'lat', 'lon', 'landings')
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
    
    #add a table in for the gear codes to see what 62 is

  }
  #-----------------------
    }


