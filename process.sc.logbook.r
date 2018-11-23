process.sc.logbook = function(){
  
p = setup.parameters()
#seacucumber.logbook.db(DS="logbook.redo")
x = seacucumber.logbook.db(DS="logbook")
write.shapefile(x, 'sc.before.test.data', p=p)
x$id = seq.int(nrow(x))

#Figuring out Area Codes
##----------------------------------------------------
areas = x[, c('fishing_area_id', "nafo_unit_area_id")]
areas = unique(areas)
colnames(areas)[1] <- 'area_id'
a = seacucumber.logbook.db(DS = 'odbc.areas')
names(a) = tolower(names(a))
x.areas = merge(areas, a,  by= 'area_id', all.x = T)
to_extract = c("area_id", "area", "desc_eng")
x.areas = x.areas[, to_extract]
x.areas = unique(x.areas)
#-------------------------------------------------------

x$zone = ""
#4W Fishing Areas
x[which(x$fishing_area_id == 26|x$fishing_area_id == 1806|x$fishing_area_id == 1489|x$fishing_area_id == 1623|x$fishing_area_id == 1624 |x$fishing_area_id == 180), c('zone')] <- "4W"
#4Vs Fishing Areas
x[which(x$fishing_area_id == 25| x$fishing_area_id == 1488), c('zone')] <- "4Vs"
#SWNB Fishing Areas
x[which(x$fishing_area_id == 1483| x$fishing_area_id == 1484| x$fishing_area_id == 189 | x$fishing_area_id == 1487| 
          x$fishing_area_id == 27|x$fishing_area_id == 188 |x$fishing_area_id == 185 |x$fishing_area_id == 186 |x$fishing_area_id == 187 |
          x$fishing_area_id == 1846| x$fishing_area_id == 1843), c('zone')] <- "SWNB"

#4X Fishing Areas
#x[which(x$fishing_area_id == 188 |x$fishing_area_id == 185 |x$fishing_area_id == 186 |x$fishing_area_id == 187 |
#          x$fishing_area_id == 1846| x$fishing_area_id == 1843), 12] <- "4X"

#-----------------------------------------------------------------------------------
#Offshore May 1 - March 31st
#SWNB Second Tuesday in January - March31st

x$f.year = ""
s = x[which((x$zone == '4Vs'|x$zone == '4W')  & (x$month <= 3)), c('yr')]
x[which((x$zone == '4Vs'|x$zone == '4W')  & (x$month <= 3)), c('f.year')] <-  s - 1

s = x[which((x$zone == '4Vs'|x$zone == '4W')  & (x$month > 3)), c('yr')]
x[which((x$zone == '4Vs'|x$zone == '4W')  & (x$month > 3)), c('f.year')] <-  s

s = x[which(x$zone == 'SWNB'), c('yr')]
x[which(x$zone == 'SWNB'), c('f.year')] <-  s 


#Fishing Zone Changes
fa.older = load.shapefile(DS = 'sea.cucumber.older')
older = intersect.wNA.sp(x, fa.older, var = "fzone" )
older = older[which(older$f.year < 2016), ]

fa.old = load.shapefile(DS = 'sea.cucumber.old')
old = intersect.wNA.sp(x, fa.old, var = "fzone" )
old = old[which(old$f.year == 2016| old$f.year == 2017), ]

fa = load.shapefile(DS = 'sea.cucumber.fa')
new = intersect.wNA.sp(x, fa, var = 'fzone')
new = new[which(new$f.year >= 2018), ]

u = rbind(older, old)
u = rbind(u, new)

u$fzone = as.character(u$fzone)

#Fishing Areas Smaller
u[which(is.na(u$fzone) & u$fishing_area_id == 1483), c('fzone')] <- "Zone 1 The Passages"
u[which(u$fzone == "" & u$fishing_area_id == 1483), c('fzone')] <- "Zone 1 The Passages"
u[which(is.na(u$fzone) & u$fishing_area_id == 1484), c('fzone')] <- "Zone 2 Outside the Passages"
u[which(u$fzone == "" & u$fishing_area_id == 1484), c('fzone')]  <- "Zone 2 Outside the Passages"

u[which(is.na(u$fzone)),  c('fzone')] <- "Other"
u[which(u$fzone == ""),  c('fzone')] <- "Other"


write.shapefile(u, 'sc.test.after.data', p=p)

return(u)
}

