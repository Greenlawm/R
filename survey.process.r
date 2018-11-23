survey.process = function(k, species, data.yrs) {
  #browser()
  
#update this to process stranal averages, or not
  
k = k[which(k$yr >= min(data.yrs) & k$yr <= max(data.yrs)), ]
  
#gscat$id = paste(gscat$mission, gscat$setno, sep=".")
#gscat$id2 = paste(gscat$mission, gscat$setno, gscat$spec, sep=".")

#Add in all sets for a particular species, so zeros are present
all.sets = unique(k[c("id")])
all.sets = merge(all.sets, k[ , c("strat", "yr","id", "lat", "lon", "mission", "setno", "sdate.x", "dist_pos", "sakm2", "dist_km", "sdepth", "temp", "sal", "oxyml", "settype", "oxysat", "cfset")])
all.sets = unique(all.sets)

expand <- expand.grid (spec = species, id = unique (all.sets$id))
all.sets <- merge(all.sets, expand, all.x = T)

j <- merge (all.sets, k, all.x = T)
j = j[which(j$spec == species),]
names = j[!is.na(j$name.scientific),]
name.scientific = names$name.scientific[1]
name.common = names$name.common[1]

j$syid = paste(j$stra, j$yr, sep = ".")
j$totwgt_sd[is.na(j$totwgt_sd)] <- 0
j$totno_sd[is.na(j$totno_sd)] <- 0
j$totno[is.na(j$totno)] <- 0
j$totwgt[is.na(j$totwgt)] <- 0
j$sampwgt[is.na(j$sampwgt)] <- 0
j$name.common[is.na(j$name.common)] <- name.common
j$name.scientific[is.na(j$name.scientific)] <- name.scientific

#Gulf Strata
j = subset(j, !(strat == '558' | strat == '559'))
#Georges Bank Strata
j = subset(j, !(strat == "5Z1"| strat == "5Z2"| strat == "5Z3"| strat == "5Z4"| strat == "5Z8"| strat == "5Z7" |
                  strat == "5Z6"| strat == "5Z5"| strat == "5Z9" ))
#Spring Strata
j = subset(j, !(strat == "406" | strat == "401" | strat == "402" | strat == "400" | strat == "397" | strat == "399" 
                | strat == "398" | strat ==  "404" | strat == "403"| strat ==  "407" | strat == "405" | strat == "408"
                | strat == "409" | strat ==  "410" | strat == "411" ))
#Deep Water Strata
j = subset(j, !(strat =="503" | strat == "501" | strat == "502" | strat == "505" | strat == "504" | strat == "558" | strat == "559"))
j = subset(j, !(strat =="498" |strat =="497" |strat == "496"))



j <- j[, c("strat", "yr","id", "lat", "lon", "mission", "setno", "sdate.x", "sakm2", 
           "dist_km", "sdepth", "settype", "cfset", "totwgt_sd", "totno_sd", "totwgt", "totno", "spec", "name.common", "name.scientific")]

return(j)

}
