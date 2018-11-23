seacucumber.observer.db = function( DS, p=NULL) {
  
  if (DS %in% c("odbc.observer", "odbc.observer.redo")) {
    fn.root =  file.path( ss.exdatadirectory, "seacucumber.observer", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    
    if (DS=="odbc.observer") {
      out = NULL
        fny = file.path( fn.root, paste("rdata", sep="."))
        if (file.exists(fny)) {
          load (fny)
          out = observer
          }
         return (out)
    }
    
    #New Database Export of logbooks
    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    
 
      fny = file.path( fn.root, paste("rdata", sep="."))
      query = paste(
        "select t.trip, t.license_no, d.pntcd_id, d.setdate, 
        TO_CHAR(d.SETDATE,'MM')month,TO_CHAR(d.SETDATE,'YYYY')Year, 
        d.LATITUDE, d.LONGITUDE, t.trip_id, s.set_no, 
        s.NAFAREA_ID, c.speccd_id, c.est_kept_wt, c.est_discard_wt, 
        c.est_combined_wt, c.est_num_caught from observer.istrips t, 
        observer.isfishsets s, observer.iscatches c, observer.issetprofile d
        where t.tripcd_id = 6600
        and c.speccd_id in (6600, 6611)
        and t.trip_id = s.trip_id
        and d.pntcd_id = 2
        and s.fishset_id = c.fishset_id
        and c.fishset_id = d.fishset_id
        order by t.trip, s.set_no, c.speccd_id;")
      
      observer = NULL
      observer = sqlQuery(con, query )
      save( observer, file=fny, compress=T)
      gc()  # garbage collection
      print("Database export completed")
    
    odbcClose(con)

  }
  
  
  #---------------------------------------------
  if (DS %in% c("observer", "observer.redo")) {
    #browser()
    
    filename = file.path( ss.exdatadirectory, "observer", "seacucumber.observer.rdata" )
    
    if (DS=="observer") {
      load( filename )
      return(observer)
    }
    
    #---
    
    # Re-do logbooks from marfissci tables
    x = seacucumber.observer.db( DS= "odbc.observer", p=p)
    names( x ) = tolower( names( x ) )
    x$vr_number = x$license_no
    x$date.fished = x$setdate
    x$lat = x$latitude
    x$lon = x$longitude
    x$month = month(x$date.fished)
    x$yr = x$year
    x$discard = x$est_discard_wt
    x$kept = x$est_kept_wt
    x$combined = x$est_combined_wt
    x$num_caught = x$est_num_caught
    x$observed = 1

    to_extract = c("date.fished", "trip_id", "vr_number", "month", "yr", "lat", "lon",
                  "nafarea_id", "speccd_id", "observed")
    
    x = x[, to_extract]

    y = process.sc.logbook()
    y$marfis = 1
    
    #merge by vr_number, date.fished and trip_id
    x = merge(x, y, by = c('vr_number', 'trip_id', 'date.fished'), all = TRUE)
    x.observed = x[which(x$observed == 1), ]
    
    write.shapefile(x)
      
    save (x, file=filename, compress=T )
    
    return( "Complete" )
  }
    
  }
  