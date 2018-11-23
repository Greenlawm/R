sslogbook.db = function( DS, p=NULL, yrs = NULL) {
#browser()
  #Species Codes
  # Jonh Crab 703, Sea Cucumber 619, whelk 615
p = setup.parameters()
#--------------------------------------------------------------------

  if (DS %in% c("odbc.seacucumber.logbook", "odbc.seacucumber.logbook.redo")) {
    fn.root =  file.path( ss.exdatadirectory, "sc.logbook", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )

  if (DS=="odbc.seacucumber.logbook") {
    out = NULL
    for ( YR in p$data.yrs ) {
      fny = file.path( fn.root, paste( YR, "rdata", sep="."))
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

  #--------------

  if (DS %in% c("seacucumber.logbook", "seacucumber.logbook.redo")) {
    #browser()

    filename = file.path( ss.exdatadirectory, "logbook", "sc.logbook.rdata" )


    if (DS=="seacucumber.logbook") {
      load( filename )
      return(logbook)
    }

    #---

    # Re-do logbooks from marfissci tables
    x = sslogbook.db( DS="odbc.seacucumber.logbook", p=p)
    names( x ) = tolower( names( x ) )
    names( x ) = rename.snowcrab.variables(names( x ))

    x$date.landed = x$landed_date
    x$month = month(x$date.fished)
    x$yr = year(x$date.fished)
    x$landings = x$rnd_weight_kgs  # pro-rated over the number of efforts on a trip
    x$lat =   round( as.numeric(substring(x$lat, 1,2)) + as.numeric(substring(x$lat, 3,6))/6000 ,6)
    x$lon = - round((as.numeric(substring(x$lon, 1,2)) + as.numeric(substring(x$lon, 3,6))/6000), 6)


    to_extract = c('date.fished', 'date.landed', 'yr', 'month', 'lat', 'lon', 'landings')
    x = x[, to_extract]

    logbook = x
    save (logbook, file=filename, compress=T )

    return( "Complete" )

  }
 #------------------------------------------------------------------------------

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


