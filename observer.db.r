#require(bio.datawrangling)

#get_data(force.extract = FALSE)
#data_filter()
observer.db = function( DS, p=NULL, yrs=NULL ) {
  
    if (DS %in% c("odbc.redo", "odbc") ) {
    
    if (  Sys.info()["sysname"] == "Windows" ) {
      .Library.site <- "D://R//library-local"
      .libPaths("D://R//library-local")
    }
    
    fn.root =  file.path( ss.exdatadirectory, "observer", "datadump" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    
    if (DS=="odbc") {
      out = NULL
      for ( YR in yrs ) {
        fny = file.path( fn.root, paste( YR, "rdata", sep="."))
        if (file.exists(fny)) {
          load (fny)
          out = rbind( out, odb )
        }
      }
      return (out)
    }
    

require(RODBC)
con=odbcConnect(oracle.snowcrab.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
#for ( YR in yrs ) {
#  fny = file.path( fn.root, paste( YR,"rdata", sep="."))
#  odbq = paste(
#    "SELECT d.LAT1, d.LONG1, d.LAT2, d.LONG2, d.LAT3, d.LONG3, d.LAT4, d.LONG4,", 
#    "s.LANDING_DATE, s.SET_NO, s.PRODCD_ID, s.EST_CATCH," ,
#    "s.NUM_HOOK_HAUL, d.BOARD_DATE, d.FISH_NO, d.SEXCD_ID, d.FISH_LENGTH, " ,
#    "d.FEMALE_ABDOMEN, d.CHELA_HEIGHT, d.SHELLCOND_CD, d.DUROMETRE, d.TRIP_ID, d.TRIP  " ,
#    "FROM ISSETPROFILE_WIDE d, SNOWCRAB.SNCRABSETS_OBS s " ,
#    "WHERE d.TRIP_ID = s.TRIP_ID  " ,
#    "AND d.SET_NO = s.SET_NO  " ,
#    "AND d.FISH_NO Is Not Null" ,
#    "AND EXTRACT(YEAR from d.BOARD_DATE) = ", YR )
#  odb = NULL
#  odb = sqlQuery(con, odbq )
#  save( odb, file=fny, compress=T)
#  gc()  # garbage collection
#  print(YR)
for (YR in yrs) {
  fny = file.path(fn.root, paste(YR, "rdata", sep="."))
  odbq = paste(
  "select to_number(to_char(setdate,'YYYY')) year, v.tonccd_id, v.grt, v.length, p.depth, substr(s.nafarea_id,1,3), g.gearcd_id, to_char(s.fishset_id) fishset_id, t.trip, s.set_no,", 
  "setdate, settime, longitude lon, latitude lat, tt.trip_type, common species,(est_kept_wt/1000) kept,est_num_caught, (est_discard_wt/1000) discarded",
  "from isdb.istrips t,", 
  "isdb.isfishsets s," ,
  "isdb.issetprofile p,", 
  "isdb.isgears g, isdb.isvessels v,", 
  "isdb.iscatches c,",   
  "isdb.isspeciescodes sc,",
  "isdb.istriptypecodes tt",
  "where s.fishset_id=p.fishset_id and s.fishset_id=c.fishset_id and c.speccd_id=sc.speccd_id and g.trip_id=t.trip_id", 
  "and t.tripcd_id=tt.tripcd_id and p.pntcd_id=DECODE(g.GearCd_Id,1,2,2,2,3,2,4,2,6,2,7,2,8,2,9,2,10,2,11,2,12,2,13,2,14,2,15,2,16,2,17,2,19,2,20,2,",
  "21,2,22,2,23,2,24,2,30,2,31,2,39,1,40,1,41,1,42,1,49,1,50,1,51,1,52,1,53,1,54,1,55,2,58,1,60,1,61,1,62,1,63,1,71,2,72,2,81,1,0)", 
  "and s.gear_id=g.gear_id and v.vess_id=t.vess_id and ctrycd_id = 2 and setdate between to_date(19800101,'YYYYMMDD') and to_date(20171231,'YYYYMMDD') ")
  odb=NULL
  odb = sqlQuery(con, odbq)
  save(odb, file=fny, compress=T)
       gc()
       print(YR)
}

odbcClose(con)
return (yrs)
}
}