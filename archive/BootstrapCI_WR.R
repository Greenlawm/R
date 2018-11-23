#install_github('Beothuk/bio.survey')
#require(bio.survey)
load('StrataDetails.rdata') # these are called st -- 
load('StrataData.rdata') #theses are called sc -- make your data files look like this
#----------------------------------------------------
st = Prepare.strata.file(st)
sc = sc[which(sc$type==1),]
sc = Prepare.strata.data(sc)

 sW = Stratify(sc,st,sc$totwgt)
 sN = Stratify(sc,st,sc$totno)
 ssW = summary.strata(sW) 
 ssN = summary.strata(SN)
 bsW = summary.boot(boot.strata(sW,method='BWR',nresamp=1000),ci.method='BC') #bootstrapping with replacement BWR
 bsN = summary.boot(boot.strata(sN,method='BWR',nresamp=1000),ci.method='BC')
 print(yr)
 print(bsW)


#c(ssN[[1]],bsN[[1]][1],bsN[[1]][2]) 



                  