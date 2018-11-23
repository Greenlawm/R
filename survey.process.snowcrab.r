survey.process.snowcrab = function(k, species) {
#browser()

k$yr = k$year
k$lon = k$long
k$totwgt_sd = k$est_catch
k$totwgt = k$wtkg
k$totno_sd = k$num

k = k[which(k$yr >= min(p$data.yrs) & k$yr <= max(p$data.yrs)), ]

#Add in all sets for a particular species, so zeros are present
k$id = paste(k$trip, k$set, sep="" )
all.sets = unique(k[c("id")])
all.sets = merge(all.sets, k[ , c("yr","id", "lat", "lon")])
all.sets = unique(all.sets)

expand <- expand.grid (species, id = unique (all.sets$id))
all.sets <- merge(all.sets, expand, all.x = T)

j <- merge (all.sets, k, all.x = T)
j = j[which(j$spec == species),]
j$totwgt_sd[is.na(j$totwgt_sd)] <- 0
j$totno_sd[is.na(j$totno_sd)] <- 0
j$totwgt[is.na(j$totwgt)] <- 0

j = j[, c('id', 'yr', 'date', 'lat', 'lon', 'totwgt_sd', 'totno_sd', 'totwgt')]

return(j)

}
