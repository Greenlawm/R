#require(bio.datawrangling)

#get_data(force.extract = FALSE)
#data_filter()
hagfish.observer.db = function( DS, p=NULL, yrs=NULL ) {
  
    if (DS %in% c("odbc.port.redo", "odbc.port") ) {
    
    fn.root =  file.path( ss.exdatadirectory, "observer.port", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    if (DS=="odbc.port") {
      out = NULL
        fny = file.path( fn.root, paste("rdata", sep="."))
        if (file.exists(fny)) {
          load (fny)
          out = rbind( out, odb )
        }
 
      return (out)
    }
    

require(RODBC)
con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
  fny = file.path( fn.root, paste("rdata", sep="."))
 
  odbq = paste("select a.sample, c.areacode, c.description, datelanded,",
               "b.lengroup, sum(b.numatlen), d.gearcode, d.description",
               "from MFD_PORT_SAMPLES.gpsamples a, MFD_PORT_SAMPLES.gplengths b,",
               "MFD_PORT_SAMPLES.GPUNIQ_AREA c, MFD_PORT_SAMPLES.GPUNIQ_GEAR d", 
               "where species=241 and a.sample=b.sample and a.area=c.areacode and a.fishing=d.gearcode",
               "group by a.sample, datelanded, b.lengroup, c.areacode, c.description, d.gearcode, d.description")
  
  odb = NULL
  odb = sqlQuery(con, odbq )
  save( odb, file=fny, compress=T)
  gc()  # garbage collection
 
odbcClose(con)
    }
  
  if (DS %in% c("odbc.redo", "odbc") ) {
    fn.root =  file.path( ss.exdatadirectory, "observer", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    if (DS=="odbc") {
      out = NULL
      fny = file.path( fn.root, paste("rdata", sep="."))
      if (file.exists(fny)) {
        load (fny)
        out = rbind( out, odb )
      }
       return (out)
    }

    require(RODBC)
    con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
    fny = file.path( fn.root, paste("rdata", sep="."))
    
    odbq = paste("select test.trip, test.tripcd_id, test.year, test.month, test.gearcd_id, test.set_no,
            test.latitude, test.longitude, test.nafarea_id, test.depth, test.duration, speccd_id,prodcd_id,sexcd_id,
            s.sexed_sample_weight,c.est_combined_wt,fish_length,num_at_length
                 from observer.test, observer.iscatches c, observer.issamples s, observer.isfishlengths fl
                 where
                 test.fishset_id=c.fishset_id and
                 c.catch_id=s.catch_id and
                 s.smpl_id=fl.smpl_id and
                 test.tripcd_id =241 and
                 speccd_id=241")
    
    odb = NULL
    odb = sqlQuery(con, odbq )
    save( odb, file=fny, compress=T)
    gc()  # garbage collection
    
    odbcClose(con)
  }
}


